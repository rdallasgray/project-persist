
(defvar project-persist-dir (concat user-emacs-directory "project-persist")
  "The directory in which project-persist will save project settings files.")

(defvar project-persist-current-project-name nil
  "The name of the project currently loaded by project-persist.")

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

;; Interactive functions
(defun project-persist-create ()
  "Create a new project-persist project, giving a project name and root directory."
  (interactive)
  (condition-case err
        (let ((dir (read-directory-name "Project root directory: ")))
          (let ((name
                 (read-from-minibuffer "Project name: "
                                       (file-name-nondirectory (directory-file-name dir)))))
            (message "dir: %s" dir)
            (when (pp-project-exists name) (error "A project with that name already exists."))
            (when (string= "" name) (error "Please enter a valid project name."))
            (pp-project-setup name dir)
            (pp-project-open name)))
    (error (progn
             (ding)
             (message "%s" (error-message-string err))
             (sit-for 1)
             (project-persist-create)))))

(defun project-persist-save ())

(defun project-persist-load (name))

(defun project-persist-list ())

;; Internal functions
(defun pp-project-exists (name) nil)

(defun pp-project-setup (name dir)
  "Set up a project with name name and root directory dir."
  (message (format "Creating project %s at root %s" name dir)))

(defun pp-project-open (name)
  "Open the project named name."
  (message (format "Opening project %s at root %s" name dir)))
