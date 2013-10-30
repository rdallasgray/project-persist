(let ((current-directory (file-name-directory (if load-file-name load-file-name buffer-file-name))))
  (setq pp/test-path (expand-file-name "." current-directory))
  (setq pp/root-path (expand-file-name ".." current-directory)))

(add-to-list 'load-path pp/root-path)

(require 'project-persist)
(require 'cl)

(ert-deftest pp-test/empty-project-name-signals-error ()
  "Test that attempting to create a project with an empty name signals an error."
  (project-persist-mode 1)
  (flet ((pp/make-settings-dir (sd) t)(pp/project-write (n rd sd) t))
    (should-error (pp/project-setup "/test" ""))))

(ert-deftest pp-test/existing-project-name-signals-error ()
  "Test that attempting to create a project with an existing name signals an error."
  (project-persist-mode 1)
  (flet ((pp/project-exists (name) t)(pp/make-settings-dir (sd) t)(pp/project-write (n rd sd) t))
    (should-error (pp/project-setup "/test" "test"))))

(ert-deftest pp-test/correct-settings-dir-from-name ()
  "Test that the correct directory name for the settings file is returned."
  (project-persist-mode 1)
  (setq project-persist-settings-dir "/test/settings-dir")
  (should (equal (pp/settings-dir-from-name "name") "/test/settings-dir/name")))

(ert-deftest pp-test/settings-written-to-correct-file ()
  "Test that pp/project-write writes to the correct file and directory."
  (project-persist-mode 1)
  (let
      ((settings-dir "/test/settings-dir")
       (project-name "test-project-name")
       (project-root-dir "/test/project-root-dir"))
    (flet ((pp/write-to-settings (settings-file settings-string)
				 (should (equal settings-file "/test/settings-dir/pp-settings.txt"))))
      (pp/project-write settings-dir))))

(ert-deftest pp-test/settings-written-correctly ()
  "Test that project settings are stored correctly."
  (project-persist-mode 1)
  (setq project-persist-additional-settings nil)
  (pp/reset-hashtable)
  (let
      ((project-name "test-project-name")
       (project-root-dir "/test/project-root-dir")
       (settings-text "\n#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (root-dir \"/test/project-root-dir\" name \"test-project-name\"))\n"))
    (flet ((pp/write-to-settings (settings-file settings-string)
				 (with-temp-buffer
				   (insert settings-string)
				   (should (equal (buffer-string) settings-text)))))
      (pp/settings-set 'root-dir project-root-dir)
      (pp/settings-set 'name project-name)
      (pp/project-write "/test-settings-dir"))))

(ert-deftest pp-test/additional-settings-written-correctly ()
  "Test that additional project settings are stored correctly."
  (project-persist-mode 1)
  (setq project-persist-additional-settings nil)
  (pp/reset-hashtable)
  (let
      ((project-name "test-project-name")
       (project-root-dir "/test/project-root-dir")
       (settings-text "\n#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (root-dir \"/test/project-root-dir\" name \"test-project-name\" test \"test setting\"))\n"))
    (flet ((pp/write-to-settings (settings-file settings-string)
				 (with-temp-buffer
				   (insert settings-string)
				   (should (equal (buffer-string) settings-text)))))
      (pp/settings-set 'root-dir project-root-dir)
      (pp/settings-set 'name project-name)
      (add-to-list 'project-persist-additional-settings '(test . (lambda () (concat "test setting"))))
      (pp/set-additional-settings)
      (pp/project-write "/test-settings-dir"))))

(ert-deftest pp-test/settings-read-correctly ()
  "Test that project settings are read back in correctly."
  (project-persist-mode 1)
  (let ((settings-string "\n#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (root-dir \"/test/project-root-dir\" name \"test-project-name\" test \"test setting\"))\n"))
    (let ((settings (pp/read-settings-from-string settings-string)))
      (should (equal (gethash 'name settings) "test-project-name"))
      (should (equal (gethash 'root-dir settings) "/test/project-root-dir"))
      (should (equal (gethash 'test settings) "test setting")))))

(ert-deftest pp-test/settings-applied-correctly ()
  "Test that project settings are applied correctly."
  (project-persist-mode 1)
  (let ((settings-string "\n#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (root-dir \"/test/project-root-dir\" name \"test-project-name\" test \"test setting\"))\n"))
    (let ((settings (pp/read-settings-from-string settings-string)))
      (flet ((add-hook (hook func)))
        (pp/apply-project-settings settings)
        (should (equal project-persist-current-project-name "test-project-name"))
        (should (equal project-persist-current-project-root-dir "/test/project-root-dir"))))))

(ert-deftest pp-test/settings-read-from-hash-correctly ()
  "Test that settings are stored in the hash and read back correctly via the pp/settings-get method."
  (project-persist-mode 1)
  (let ((settings-string "\n#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (root-dir \"/test/project-root-dir\" name \"test-project-name\" test \"test setting\"))\n"))
    (let ((settings (pp/read-settings-from-string settings-string)))
      (flet ((add-hook (hook func)))
        (pp/apply-project-settings settings)
        (should (equal (pp/settings-get 'name) "test-project-name"))
        (should (equal (pp/settings-get 'root-dir) "/test/project-root-dir"))
        (should (equal (pp/settings-get 'test) "test setting"))))))

(ert-deftest pp-test/project-closed-correctly ()
  "Test that variables are set to nil when a project is closed."
  (project-persist-mode 1)
  (let ((settings-string "\n#s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8 data (root-dir \"/test/project-root-dir\" name \"test-project-name\" test \"test setting\"))\n"))
    (let ((settings (pp/read-settings-from-string settings-string)))
      (pp/apply-project-settings settings)
      (pp/close-current-project)
      (should (equal nil project-persist-current-project-name))
      (should (equal nil project-persist-current-project-root-dir)))))

(ert-deftest pp-test/project-mode-off-removes-hooks ()
  "Test that hooks are set to nil when a project is closed."
  (project-persist-mode 1)
  (add-hook 'project-persist-before-load-hook (lambda () (concat "test")))
  (project-persist-mode -1)
  (should (equal nil project-persist-before-load-hook)))

(ert-deftest pp-test/global-auto-save-setting-nil ()
  "Test that the global auto-save setting is honoured when set to nil."
  (project-persist-mode 1)
  (let ((project-persist-auto-save-global nil)(prompt-called nil)(project-persist-current-project-name "test"))
    (flet ((y-or-n-p (prompt) (setq prompt-called t))(pp/write-to-settings (sf ss) t)(pp/has-open-project () t))
      (message (format "open? %s" (pp/has-open-project)))
      (project-persist-close)
      (should (equal t prompt-called)))))

(ert-deftest pp-test/global-auto-save-setting-t ()
  "Test that the global auto-save setting is honoured when set to t."
  (project-persist-mode 1)
  (let ((project-persist-auto-save-global t)(prompt-called nil)(project-persist-current-project-name "test"))
    (flet ((y-or-n-p (prompt) (setq prompt-called t))(pp/write-to-settings (sf ss) t)(pp/has-open-project () t))
      (project-persist-close)
      (should (equal nil prompt-called)))))

(ert-deftest pp-test/local-auto-save-setting-t ()
  "Test that the local auto-save setting is honoured when set to t."
  (project-persist-mode 1)
  (let ((prompt-called nil)(project-persist-current-project-name "test"))
    (pp/settings-set 'auto-save t)
    (flet ((y-or-n-p (prompt) (setq prompt-called t))(pp/write-to-settings (sf ss) t)(pp/has-open-project () t))
      (project-persist-close)
      (should (equal nil prompt-called)))))

(ert-deftest pp-test/local-auto-save-setting-prompt ()
  "Test that the local auto-save setting is honoured when set to prompt."
  (project-persist-mode 1)
  (let ((prompt-called nil)(project-persist-current-project-name "test"))
    (pp/settings-set 'auto-save 'prompt)
    (flet ((y-or-n-p (prompt) (setq prompt-called t))(pp/write-to-settings (sf ss) t)(pp/has-open-project () t))
      (project-persist-close)
      (should (equal t prompt-called)))))
