;;; beads-project-test.el --- Tests for beads-project.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'beads-project)
(require 'beads-list)

(ert-deftest beads-project-test-root-defined ()
  "Test that beads-project-root is defined."
  (should (fboundp 'beads-project-root)))

(ert-deftest beads-project-test-name-defined ()
  "Test that beads-project-name is defined."
  (should (fboundp 'beads-project-name)))

(ert-deftest beads-project-test-buffer-name-defined ()
  "Test that beads-project-buffer-name is defined."
  (should (fboundp 'beads-project-buffer-name)))

(ert-deftest beads-project-test-list-defined ()
  "Test that beads-project-list is defined as a command."
  (should (fboundp 'beads-project-list))
  (should (commandp 'beads-project-list)))

(ert-deftest beads-project-test-name-extracts-dirname ()
  "Test that beads-project-name extracts directory name."
  (should (string= (beads-project-name "/foo/bar/myproject/")
                   "myproject"))
  (should (string= (beads-project-name "/foo/bar/myproject")
                   "myproject")))

(ert-deftest beads-project-test-default-buffer-name ()
  "Test default buffer name generation."
  (should (string= (beads-project-default-buffer-name "/foo/myproject/")
                   "*Beads: myproject*")))

(ert-deftest beads-project-test-buffer-name-without-project ()
  "Test buffer name when not in a project."
  (let ((beads-project-per-project-buffers t))
    (cl-letf (((symbol-function 'beads-project-root)
               (lambda () nil)))
      (should (string= (beads-project-buffer-name) "*Beads Issues*")))))

(ert-deftest beads-project-test-buffer-name-with-project ()
  "Test buffer name when in a project."
  (let ((beads-project-per-project-buffers t))
    (cl-letf (((symbol-function 'beads-project-root)
               (lambda () "/home/user/myproject/")))
      (should (string= (beads-project-buffer-name) "*Beads: myproject*")))))

(ert-deftest beads-project-test-buffer-name-disabled ()
  "Test buffer name when per-project buffers disabled."
  (let ((beads-project-per-project-buffers nil))
    (cl-letf (((symbol-function 'beads-project-root)
               (lambda () "/home/user/myproject/")))
      (should (string= (beads-project-buffer-name) "*Beads Issues*")))))

(ert-deftest beads-project-test-custom-buffer-name-function ()
  "Test custom buffer name function."
  (let ((beads-project-per-project-buffers t)
        (beads-project-buffer-name-function
         (lambda (root)
           (format "*Issues: %s*" (beads-project-name root)))))
    (cl-letf (((symbol-function 'beads-project-root)
               (lambda () "/code/awesome/")))
      (should (string= (beads-project-buffer-name) "*Issues: awesome*")))))

(ert-deftest beads-list-test-project-root-var-exists ()
  "Test that beads-list--project-root buffer-local variable exists."
  (with-temp-buffer
    (should (local-variable-if-set-p 'beads-list--project-root))))

(provide 'beads-project-test)
;;; beads-project-test.el ends here
