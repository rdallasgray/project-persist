;; Customize options
(defgroup project-persist nil
  "Settings related to project-persist, a package to enable simple persistence of project settings.")

(defcustom project-persist-desktop t
  "Whether project-persist should load and save the desktop along with the project."
  :type 'boolean
  :group 'project-persist)

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
          (pp/project-setup name root-dir)
          (pp/project-open name))
      (error (progn
               (ding)
               (message "%s" (error-message-string err))
               (sit-for 1)
               (project-persist-create)))))

(defun project-persist-save ())

(defun project-persist-load (name))

(defun project-persist-list ())

;; Internal functions
(defun pp/project-exists (name) nil)

(defun pp/project-setup (name root-dir)
  "Set up a project with name name and root directory dir."
  (if (string= name "") (error "Project name is empty"))
  (if (pp/project-exists name) (error "Project %s already exists." name))
  (run-hooks 'project-persist-before-create-hook)
  (let ((settings-dir (pp/settings-dir-from-name name)))
    (pp/make-settings-dir settings-dir)
    (pp/project-write name root-dir settings-dir)
    (run-hooks 'project-persist-after-create-hook)))

(defun pp/project-write (name root-dir settings-dir)
  (let ((settings `((project-name . ,name)(project-root-dir . ,root-dir)))
        (settings-file (expand-file-name "pp-settings.txt" settings-dir)))
    (with-temp-buffer
      (print settings (current-buffer))
      (pp/write-to-settings settings-file (buffer-string)))))

(defun pp/write-to-settings (settings-file settings-string)
  (with-temp-file settings-file (insert settings-string)))

(defun pp/settings-dir-from-name (name)
  (concat (expand-file-name name project-persist-settings-dir)))

(defun pp/project-open (name)
  "Open the project named name."
  (message (format "Opening project %s" name)))

(defun pp/make-settings-dir (settings-dir)
  (unless (file-exists-p project-persist-settings-dir)
    (make-directory project-persist-settings-dir))
  (unless (file-exists-p settings-dir)
    (make-directory settings-dir)))

(provide 'project-persist)
