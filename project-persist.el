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

(defvar project-persist-additional-settings '()
  "A list of additional keys to store in the project settings file (the defaults are 'name and 'root-dir).
The format should be a cons cell ('key . read-function); e.g. ('name . (lambda () (read-from-buffer \"Project name: \"))).")


;; Internal variables
(defvar pp/settings-file-name "pp-settings.txt"
  "Name of the default settings file to write in each project's settings directory.")

(defvar pp/settings-hash (pp/reset-hashtable)
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

(defun project-persist-save ())

(defun project-persist-load (name))

(defun project-persist-list ())

;; Internal functions
(defun pp/reset-hashtable ()
  "Empty the hashtable containing project settings."
  (setq pp/settings-hash (make-hash-table :test 'equal)))

(defun pp/signal-error (err &optional func)
  "Ding and message the error string, optionally continuing with a given function."
  (ding)
  (message "%s" (error-message-string err))
  (sit-for 1)
  (when func (funcall func)))

(defun pp/project-exists (name) nil)

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
    (let ((settings (pp/project-read-settings-from-string
                     (pp/get-settings-file-contents settings-file))))
      (pp/apply-project-settings settings))))

(defun pp/apply-project-settings (settings)
  "Make the settings read from the project settings file current."
  ;; set hashtable then set global vars
  )

(defun pp/get-settings-file-contents (settings-file)
  "Read and return contents of settings-file"
  (with-temp-buffer
    (insert-file-contents settings-file)
    (buffer-string)))

(defun pp/project-read-settings-from-string (settings-string)
  "Read and return the project settings hash from the given file."
  (read-from-string settings-string))

(defun pp/project-write (settings-dir)
  "Write project settings to the given settings directory."
  (let ((settings-file (expand-file-name pp/settings-file-name settings-dir)))
    (with-temp-buffer
      (print pp/settings-hash (current-buffer))
      (pp/write-to-settings settings-file (buffer-string)))))

(defun pp/write-to-settings (settings-file settings-string)
  "Write the given string representing project settings to the given file."
  (with-temp-file settings-file (insert settings-string)))

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
