;;; project-persist.el --- A minor mode to allow loading and saving of project settings.

;; Copyright (C) 2013 Robert Dallas Gray

;; Author: Robert Dallas Gray
;; URL: https://github.com/rdallasgray/project-persist
;; Version: 0.3.0
;; Created: 2012-09-13
;; Keywords: project, persistence

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; # What is project-persist?
;; Project-persist is a simple, extensible Emacs package to allow persistence of a
;; list of projects with relevant settings.
;; 
;; # What does it do?
;; It allows you to create, open, save, close and delete simple projects based on
;; root directories.
;; 
;; # Is that it?
;; Pretty much. It also provides hooks around each of these functions so that you
;; could, for example, load and save an Emacs desktop in tandem with a project,
;; create and save a tags file, or load another project-management solution such as
;; [Projectile](https://github.com/bbatsov/projectile).
;; 
;; By default, only a project's name and root directory are saved, but you can
;; easily add other settings like this:
;; 
;; ```lisp
;; (add-to-list 'project-persist-additional-settings
;;   '(my-setting . (lambda () (read-from-minibuffer "My setting: "))))
;; ```
;; 
;; Each element of the list is a cons cell with car a symbol naming the new setting
;; and cdr a function to obtain the value of the setting. The function will be
;; called during project creation and the setting's value saved as normal.
;; 
;; The setting can be retrieved once a project is loaded by invoking:
;; 
;; ```lisp
;; (pp/settings-get 'my-setting)
;; ```
;; 
;; Project-persist is intentionally lightweight, in the spirit of Emacs, so that it
;; can be used to build a more complex project-management infrastructure tailored
;; to your needs. Other packages, like the aforementioned Projectile, handle things
;; like searching within a project, so there's no need to duplicate such
;; functionality.
;; 
;; It can be required and enabled as follows:
;; 
;; ```lisp
;; (require 'project-persist)
;; (project-persist-mode t)
;; ```
;;
;;; Code:

;; Customize options
(defgroup project-persist nil
  "Settings related to project-persist, a package to enable simple persistence of project settings."
  :group 'tools)

(defcustom project-persist-settings-dir (concat user-emacs-directory "project-persist")
  "The directory in which project-persist will save project settings files."
  :type 'directory
  :group 'project-persist)

(defcustom project-persist-keymap-prefix (kbd "C-c P")
  "Project-persist keymap prefix."
  :type 'sexp
  :group 'project-persist)

(defcustom project-persist-auto-save-global t
  "If non-nil, automatically save projects without prompting.

Can be overridden on a project-basis with
\(pp/settings-set 'auto-save VALUE), where VALUE is t or 'prompt

If the project setting `auto-save' is t or if the value of
variable `project-persist-auto-save-global' is non-nil, save the
project without prompting

If the project setting `auto-save' is 'prompt, always prompt before saving"
  :type 'boolean
  :group 'project-persist)


;; Hooks
(defvar project-persist-mode-hook nil
  "Run when entering project-persist-mode.")

(defvar project-persist-before-create-hook nil
  "A hook to be run before project-persist creates a project.")

(defvar project-persist-after-create-hook nil
  "A hook to be run after project-persist creates a project.")

(defvar project-persist-before-save-hook nil
  "A hook to be run before project-persist saves a project.")

(defvar project-persist-after-save-hook nil
  "A hook to be run after project-persist saves a project.")

(defvar project-persist-before-load-hook nil
  "A hook to be run before project-persist loads a project.")

(defvar project-persist-after-load-hook nil
  "A hook to be run after project-persist loads a project.")

(defvar project-persist-before-close-hook nil
  "A hook to be run before project-persist closes a project.")

(defvar project-persist-after-close-hook nil
  "A hook to be run after project-persist closes a project.")


;; Keymap
(defvar project-persist-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "f") 'project-persist-find)
      (define-key prefix-map (kbd "s") 'project-persist-save)
      (define-key prefix-map (kbd "k") 'project-persist-close)
      (define-key prefix-map (kbd "d") 'project-persist-delete)
      (define-key prefix-map (kbd "n") 'project-persist-create)
      (define-key map project-persist-keymap-prefix prefix-map))
    map)
  "Keymap for project-persist-mode.")


;; Global variables
(defvar project-persist-current-project-name nil
  "The name of the project currently loaded by project-persist.")

(defvar project-persist-current-project-root-dir nil
  "The root directory of the project currently loaded by project-persist.")

(defvar project-persist-current-project-settings-dir nil
  "The directory in which settings for the current project are stored.")

(defvar project-persist-additional-settings '()
  "A list of additional keys to store in the project settings file.
The defaults are 'name and 'root-dir. The format should be a cons cell:
\('key . read-function); e.g. ('name . (lambda () (read-from-buffer \"Project name: \"))).")

;; Internal variables
(defvar pp/lighter nil
  "Modeline lighter for minor mode.")

(defvar pp/project-list-cache '()
  "Cached list of projects.")

(defvar pp/project-list-cache-valid nil
  "Whether the cached project list is currently valid.")

(defvar pp/settings-file-name "pp-settings.txt"
  "Name of the default settings file to write in each project's settings directory.")

(defvar pp/settings-hash (make-hash-table :test 'equal)
  "Settings hashtable to be written to the project settings file.")


;; Interactive functions
(defun project-persist-create ()
  "Create a new project-persist project, giving a project name and root directory."
  (interactive)
  (pp/offer-save-if-open-project)
  (let ((root-dir (read-directory-name "Project root directory: ")))
    (let ((name
           (read-from-minibuffer
            "Project name: "
            (file-name-nondirectory (directory-file-name root-dir)))))
      (condition-case err
          (progn
            (pp/project-setup root-dir name)
            (pp/project-open name))
        (error (pp/signal-error err))))))

(defun project-persist-save ()
  "Save the project settings and run relevant hooks."
  (interactive)
  (when (not (pp/has-open-project)) (error "No project is currently open."))
  (let ((settings-dir (pp/settings-dir-from-name project-persist-current-project-name)))
    (pp/project-write settings-dir)))

(defun project-persist-find ()
  "Find and load the given project name."
  (interactive)
  (pp/offer-save-if-open-project)
  (pp/project-open (pp/read-project-name)))

(defun project-persist-close ()
  "Close the currently open project."
  (interactive)
  (when (not (pp/has-open-project)) (error "No project is currently open."))
  (pp/offer-save-if-open-project)
  (pp/close-current-project))

(defun project-persist-delete ()
  "Delete the given project name."
  (interactive)
  (let ((name (pp/read-project-name)))
    (when (eq name project-persist-current-project-name)
      (error "Can't delete the currently open project. Please close the project first."))
    (let ((confirm (yes-or-no-p (format "Are you sure you want to delete project %s?" name))))
      (when confirm
        (pp/project-destroy name)))))


;; Internal functions
(defun pp/offer-save-if-open-project ()
  "Offer to save the open project.
Depending on the value of the variable` project-persist-auto-save-global'
and the project setting `auto-save', save the project without asking."
  (when (pp/has-open-project)
    (let ((auto-save (pp/auto-save-value)))
      (when (or auto-save (y-or-n-p (format "Save project %s?" project-persist-current-project-name)))
	(project-persist-save)))))

(defun pp/auto-save-value ()
  "Get the auto-save setting; if set locally, use that, otherwise use the global setting."
  (let ((local-setting (pp/settings-get 'auto-save)))
    (if local-setting
	(not (eq local-setting 'prompt))
      project-persist-auto-save-global)))

(defun pp/disable-hooks ()
  "Disable all project-persist hooks (normally on disabling the minor mode)."
  (let ((hooks '(project-persist-before-create-hook
                 project-persist-after-create-hook
                 project-persist-before-load-hook
                 project-persist-after-load-hook
                 project-persist-before-save-hook
                 project-persist-after-save-hook)))
    (mapc (lambda (hook) (set hook nil)) hooks))
  (remove-hook 'kill-emacs-hook 'pp/offer-save-if-open-project))

(defun pp/reset-hashtable ()
  "Empty the hashtable containing project settings."
  (clrhash pp/settings-hash))

(defun pp/settings-get (key)
  "Get the value of setting KEY."
  (gethash key pp/settings-hash))

(defun pp/settings-set (key value)
  "Set project setting KEY to VALUE."
  (puthash key value pp/settings-hash))

(defun pp/read-project-name ()
  "Read the project name from user input using a choice of `completing-read' or `ido-completing-read'."
   (let ((func 'completing-read))
     (when (featurep 'ido) (setq func 'ido-completing-read))
     (funcall func "Project name: " (pp/project-list) nil t)))

(defun pp/signal-error (err &optional func)
  "Ding and message the error string, optionally continuing with a given function."
  (ding)
  (message "%s" (error-message-string err))
  (sit-for 1)
  (when func (funcall func)))

(defun pp/project-destroy (name)
  "Delete the settings directory for the given project NAME."
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (delete-directory settings-dir t t)
    (pp/invalidate-project-list-cache)))

(defun pp/close-current-project ()
  "Close the current project, setting relevant vars to nil."
  (run-hooks 'project-persist-before-close-hook)
  (pp/reset-hashtable)
  (pp/clear-project-vars)
  (setq pp/lighter nil)
  (run-hooks 'project-persist-after-close-hook))

(defun pp/clear-project-vars ()
  "Clear standard project variables."
    (let ((vars '(project-persist-current-project-name
                project-persist-current-project-root-dir
                project-persist-current-project-settings-dir)))
    (mapc (lambda (var) (set var nil)) vars)))

(defun pp/project-list ()
  "Get a list of names of existing projects."
  (when (not pp/project-list-cache-valid)
      (let ((settings-dir project-persist-settings-dir)(project-list '()))
        (let ((dirs (directory-files settings-dir)))
          (while dirs
            (let ((dir (car dirs)))
              (when (not (or (eq dir ".") (eq dir "..")))
                (let ((settings (pp/get-settings-in-dirname dir)))
                  (when settings
                    (add-to-list 'project-list (gethash 'name settings)))))
              (setq dirs (cdr dirs)))))
        (pp/set-project-list-cache project-list)))
  pp/project-list-cache)

(defun pp/set-project-list-cache (project-list)
  "Set the cached project list to PROJECT-LIST and make it valid."
  (setq pp/project-list-cache project-list)
  (setq pp/project-list-cache-valid t))

(defun pp/invalidate-project-list-cache ()
  "Make the cached project list invalid."
  (setq pp/project-list-cache-valid nil))

(defun pp/has-open-project ()
  "Whether a project is currently open."
  (not (null project-persist-current-project-name)))

(defun pp/project-exists (name)
  "Whether a project with the given NAME already exists.
\(I.e., an appropriately-named directory exists in the project settings
directory, and a valid settings file exists within that directory)."
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (let ((settings-file (expand-file-name pp/settings-file-name settings-dir)))
      (file-exists-p settings-file))))

(defun pp/get-settings-in-dirname (dirname)
  "Return the settings from the settings file in the given DIRNAME, or nil."
  (let ((dir (expand-file-name dirname project-persist-settings-dir))(settings nil))
    (if (file-directory-p dir)
        (let ((settings-file (expand-file-name pp/settings-file-name dir)))
          (if (file-exists-p settings-file)
              (let ((settings-string (pp/get-settings-file-contents settings-file)))
                (setq settings (pp/read-settings-from-string settings-string))))))
    settings))

(defun pp/project-setup (root-dir name)
  "Set up a project with root directory ROOT-DIR and name NAME."
  (if (string= name "") (error "Project name is empty"))
  (if (pp/project-exists name) (error "Project %s already exists." name))
  (run-hooks 'project-persist-before-create-hook)
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (pp/make-settings-dir settings-dir)
    (pp/reset-hashtable)
    (pp/settings-set 'root-dir root-dir)
    (pp/settings-set 'name name)
    (pp/set-additional-settings)
    (setq project-persist-current-project-settings-dir settings-dir)
    (pp/project-write settings-dir)
    (pp/invalidate-project-list-cache)
    (run-hooks 'project-persist-after-create-hook)))

(defun pp/set-additional-settings ()
  "Set any values given in `project-persist-additional-settings'."
  (let ((settings-keys project-persist-additional-settings))
    (while settings-keys
      (let ((setting (car settings-keys)))
        (let ((setting-key (car setting))(setting-value (funcall (cdr setting))))
          (pp/settings-set setting-key setting-value)
          (setq settings-keys (cdr settings-keys)))))))

(defun pp/project-open (name)
  "Open the project named NAME."
  (let ((settings-file
         (expand-file-name
          pp/settings-file-name (pp/settings-dir-from-name name))))
    (let ((settings (pp/read-settings-from-string
                     (pp/get-settings-file-contents settings-file))))
      (pp/apply-project-settings settings))))

(defun pp/apply-project-settings (settings)
  "Make the SETTINGS read from the project settings file current."
  (run-hooks 'project-persist-before-load-hook)
  (setq pp/settings-hash settings)
  (setq project-persist-current-project-name (gethash 'name settings))
  (setq project-persist-current-project-root-dir (gethash 'root-dir settings))
  (setq pp/lighter (format " pp:%s" project-persist-current-project-name))
  (setq project-persist-current-project-settings-dir (pp/settings-dir-from-name project-persist-current-project-name))
  (add-hook 'kill-emacs-hook 'pp/offer-save-if-open-project)
  (run-hooks 'project-persist-after-load-hook))

(defun pp/get-settings-file-contents (settings-file)
  "Read and return contents of SETTINGS-FILE."
  (with-temp-buffer
    (insert-file-contents settings-file)
    (buffer-string)))

(defun pp/read-settings-from-string (settings-string)
  "Read and return the project settings hash from the given SETTINGS-STRING."
  (read settings-string))

(defun pp/project-write (settings-dir)
  "Write project settings to the given SETTINGS-DIR."
  (let ((settings-file (expand-file-name pp/settings-file-name settings-dir)))
    (with-temp-buffer
      (print pp/settings-hash (current-buffer))
      (pp/write-to-settings settings-file (buffer-string)))))

(defun pp/write-to-settings (settings-file settings-string)
  "Write to SETTINGS-FILE with the given SETTINGS-STRING."
  (run-hooks 'project-persist-before-save-hook)
  (with-temp-file settings-file
    (insert settings-string))
  (run-hooks 'project-persist-after-save-hook))

(defun pp/settings-dir-from-name (name)
  "Return the settings directory for the project based on its NAME."
  (concat (expand-file-name name project-persist-settings-dir)))

(defun pp/make-settings-dir (settings-dir)
  "Create the project SETTINGS-DIR if it doesn't already exist.
Create the project-persist root settings directory if necessary."
  (unless (file-exists-p project-persist-settings-dir)
    (make-directory project-persist-settings-dir))
  (unless (file-exists-p settings-dir)
    (make-directory settings-dir)))

;;;###autoload
(define-minor-mode project-persist-mode
  "A minor mode to allow loading and saving of project settings."
  :global t
  :lighter pp/lighter
  :keymap project-persist-mode-map
  :group 'project-persist
  (unless project-persist-mode
    (pp/disable-hooks)
    (pp/close-current-project)))

(provide 'project-persist)
;;; project-persist.el ends here
