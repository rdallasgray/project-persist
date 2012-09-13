;; Customize options
(defgroup project-persist nil
  "Settings related to project-persist, a package to enable simple persistence of project settings.")

(defcustom project-persist-settings-dir (concat user-emacs-directory "project-persist")
  "The directory in which project-persist will save project settings files."
  :type 'directory
  :group 'project-persist)


;; Hooks
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


;; Global variables
(defvar project-persist-current-project-name nil
  "The name of the project currently loaded by project-persist.")

(defvar project-persist-current-project-root-dir nil
  "The root directory of the project currently loaded by project-persist.")

(defvar project-persist-additional-settings '()
  "A list of additional keys to store in the project settings file (the defaults are 'name and 'root-dir).
The format should be a cons cell ('key . read-function); e.g. ('name . (lambda () (read-from-buffer \"Project name: \"))).")


;; Internal variables
(defvar pp/project-list-cache '()
  "Cached list of projects.")

(defvar pp/project-list-cache-valid nil
  "Whether the cached project list is currently valid.")

(defvar pp/settings-file-name "pp-settings.txt"
  "Name of the default settings file to write in each project's settings directory.")

(defvar pp/settings-hash (make-hash-table :test 'equal)
  "Settings hashtable to be written to the project settings file.")

;; Interactive functions
(defun project-persist-create (root-dir name)
  "Create a new project-persist project, giving a project name and root directory."
  (interactive
   (let ((i-root-dir (read-directory-name "Project root directory: ")))
     (let ((i-name
            (read-from-minibuffer
             "Project name: "
             (file-name-nondirectory (directory-file-name i-root-dir)))))
       (list i-root-dir i-name))))
    (condition-case err
        (progn
          (pp/project-setup root-dir name)
          (pp/project-open name))
      (error (pp/signal-error err))))

(defun project-persist-save ()
  "Save the project settings and run relevant hooks."
  (interactive)
  (when (not (pp/has-open-project)) (error "No project is currently open."))
  (let ((settings-dir (pp/settings-dir-from-name project-persist-current-project-name)))
    (pp/project-write settings-dir)))

(defun project-persist-load (name)
  "Load the given project name."
  (interactive
   `(,(pp/read-project-name)))
  (pp/project-open name))

(defun project-persist-close()
  "Close the currently open project."
  (interactive)
  (when (not (pp/has-open-project)) (error "No project is currently open."))
  (pp/close-current-project))

(defun project-persist-delete (name confirm)
  "Delete the given project name."
  (interactive
   (let ((i-name (pp/read-project-name)))
     (let ((i-confirm (yes-or-no-p (format "Are you sure you want to delete project %s?" i-name))))
       `(,i-name ,i-confirm))))
  (when confirm
    (pp/project-destroy name)))


;; Internal functions
(defun pp/reset-hashtable ()
  "Empty the hashtable containing project settings."
  (clrhash pp/settings-hash))

(defun pp/read-project-name ()
  "Read the project name from user input using a choice of completing-read or ido-completing-read."
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
  "Delete the settings directory for the given project name."
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (delete-directory settings-dir t t)
    (pp/invalidate-project-list-cache)))

(defun pp/close-current-project ()
  "Close the current project, setting relevant vars to nil."
  (pp/reset-hashtable)
  (setq project-persist-current-project-name nil)
  (setq project-persist-current-project-root-dir nil))

(defun pp/project-list ()
  "Get a list of names of existing projects."
  (when (not pp/project-list-cache-valid)
      (let ((settings-dir project-persist-settings-dir)(project-list '()))
        (let ((dirs (directory-files settings-dir)))
          (while dirs
            (let ((dir (car dirs)))
              (when (not (or (equalp dir ".") (equalp dir "..")))
                (let ((settings (pp/get-settings-in-dirname dir)))
                  (when settings
                    (add-to-list 'project-list (gethash 'name settings)))))
              (setq dirs (cdr dirs)))))
        (pp/set-project-list-cache project-list)))
  pp/project-list-cache)

(defun pp/set-project-list-cache (project-list)
  "Set the cached project list to project-list and make it valid."
  (setq pp/project-list-cache project-list)
  (setq pp/project-list-cache-valid t))

(defun pp/invalidate-project-list-cache ()
  "Make the cached project list invalid."
  (setq pp/project-list-cache-valid nil))

(defun pp/has-open-project ()
  "Whether a project is currently open."
  (not (equalp nil project-persist-current-project-name)))

(defun pp/project-exists (name)
  "Whether a project with the given name already exists (i.e., an appropriately-named directory
exists in the project settings directory, and a valid settings file exists within that directory)."
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (let ((settings-file (expand-file-name pp/settings-file-name settings-dir)))
      (file-exists-p settings-file))))

(defun pp/get-settings-in-dirname (dirname)
  "Return the settings from the settings file in the given directory, or nil."
  (let ((dir (expand-file-name dirname project-persist-settings-dir))(settings nil))
    (if (file-directory-p dir)
        (let ((settings-file (expand-file-name pp/settings-file-name dir)))
          (if (file-exists-p settings-file)
              (let ((settings-string (pp/get-settings-file-contents settings-file)))
                (setq settings (pp/read-settings-from-string settings-string))))))
    settings))

(defun pp/project-setup (root-dir name)
  "Set up a project with name name and root directory root-dir."
  (if (string= name "") (error "Project name is empty"))
  (if (pp/project-exists name) (error "Project %s already exists." name))
  (run-hooks 'project-persist-before-create-hook)
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (pp/make-settings-dir settings-dir)
    (pp/reset-hashtable)
    (pp/settings-set 'root-dir root-dir)
    (pp/settings-set 'name name)
    (pp/set-additional-settings)
    (pp/project-write settings-dir)
    (pp/invalidate-project-list-cache)
    (run-hooks 'project-persist-after-create-hook)))

(defun pp/set-additional-settings ()
  (let ((settings-keys project-persist-additional-settings))
    (message "setting additional settings: %S" settings-keys)
    (while settings-keys
      (let ((setting (car settings-keys)))
        (message "setting: %S" setting)
        (let ((setting-key (car setting))(setting-value (funcall (cdr setting))))
          (message "key: %S value: %S" setting-key setting-value)
          (pp/settings-set setting-key setting-value)
          (setq settings-keys (cdr settings-keys)))))))

(defun pp/settings-set (key value)
  "Set project setting key to value."
  (puthash key value pp/settings-hash))

(defun pp/project-open (name)
  "Open the project named name."
  (let ((settings-file
         (expand-file-name
          pp/settings-file-name (pp/settings-dir-from-name name))))
    (let ((settings (pp/read-settings-from-string
                     (pp/get-settings-file-contents settings-file))))
      (pp/apply-project-settings settings))))

(defun pp/apply-project-settings (settings)
  "Make the settings read from the project settings file current."
  (run-hooks 'project-persist-before-load-hook)
  (setq pp/settings-hash settings)
  (setq project-persist-current-project-name (gethash 'name pp/settings-hash))
  (setq project-persist-current-project-root-dir (gethash 'root-dir pp/settings-hash))
  (run-hooks 'project-persist-after-load-hook))

(defun pp/get-settings-file-contents (settings-file)
  "Read and return contents of settings-file"
  (with-temp-buffer
    (insert-file-contents settings-file)
    (buffer-string)))

(defun pp/read-settings-from-string (settings-string)
  "Read and return the project settings hash from the given file."
  (read settings-string))

(defun pp/project-write (settings-dir)
  "Write project settings to the given settings directory."
  (let ((settings-file (expand-file-name pp/settings-file-name settings-dir)))
    (with-temp-buffer
      (print pp/settings-hash (current-buffer))
      (pp/write-to-settings settings-file (buffer-string)))))

(defun pp/write-to-settings (settings-file settings-string)
  "Write the given string representing project settings to the given file."
  (run-hooks 'project-persist-before-save-hook)
  (with-temp-file settings-file
    (insert settings-string))
  (run-hooks 'project-persist-after-save-hook))

(defun pp/settings-dir-from-name (name)
  "Return the settings directory for the project based on its name."
  (concat (expand-file-name name project-persist-settings-dir)))

(defun pp/make-settings-dir (settings-dir)
  "Create the project settings directory if it doesn't already exist, creating the project-persist root settings directory if necessary."
  (unless (file-exists-p project-persist-settings-dir)
    (make-directory project-persist-settings-dir))
  (unless (file-exists-p settings-dir)
    (make-directory settings-dir)))

(provide 'project-persist)
