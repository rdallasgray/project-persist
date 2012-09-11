(let ((current-directory (file-name-directory (if load-file-name load-file-name buffer-file-name))))
  (setq pp/test-path (expand-file-name "." current-directory))
  (setq pp/root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path pp/root-path)

(require 'project-persist)

(ert-deftest pp-test/empty-project-name-signals-error ()
  "Test that attempting to create a project with an empty name signals an error."
  (flet ((pp/make-settings-dir (sd) t)(pp/project-write (n rd sd) t))
    (should-error (project-persist-create "/test" ""))))

(ert-deftest pp-test/existing-project-name-signals-error ()
  "Test that attempting to create a project with an existing name signals an error."
  (flet ((pp/project-exists (name) t)(pp/make-settings-dir (sd) t)(pp/project-write (n rd sd) t))
    (should-error (project-persist-create "/test" "test"))))

(ert-deftest pp-test/correct-settings-dir-from-name ()
  "Test that the correct directory name for the settings file is returned."
  (setq project-persist-settings-dir "/test/settings-dir")
  (should (equal (pp/settings-dir-from-name "name") "/test/settings-dir/name")))

(ert-deftest pp-test/settings-written-to-correct-file ()
  "Test that pp/project-write writes to the correct file and directory."
  (let
      ((settings-dir "/test/settings-dir")
       (project-name "test-project-name")
       (project-root-dir "/test/project-root-dir"))
    (flet ((pp/write-to-settings (settings-file settings-string)
               (should (equal settings-file "/test/settings-dir/pp-settings.txt"))))
      (pp/project-write project-name project-root-dir settings-dir))))

(ert-deftest pp-test/settings-written-correctly ()
  "Test that project settings are written correctly."
  (let
      ((settings-dir "/test/settings-dir")
       (project-name "test-project-name")
       (project-root-dir "/test/project-root-dir")
       (settings-text "\n((project-name . \"test-project-name\") (project-root-dir . \"/test/project-root-dir\"))\n"))
    (flet ((pp/write-to-settings (settings-file settings-string)
                                 (with-temp-buffer
                                   (insert settings-string)
                                   (should (equal (buffer-string) settings-text)))))
      (pp/project-write project-name project-root-dir settings-dir))))
