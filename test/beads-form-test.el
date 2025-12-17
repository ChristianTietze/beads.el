;;; beads-form-test.el --- Tests for beads-form.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'beads-form)
(require 'beads-detail)

(ert-deftest beads-form-test-mode-defined ()
  "Test that beads-form-mode is defined."
  (should (fboundp 'beads-form-mode)))

(ert-deftest beads-form-test-open-defined ()
  "Test that beads-form-open is defined."
  (should (fboundp 'beads-form-open)))

(ert-deftest beads-form-test-commit-defined ()
  "Test that beads-form-commit is defined as a command."
  (should (fboundp 'beads-form-commit))
  (should (commandp 'beads-form-commit)))

(ert-deftest beads-form-test-cancel-defined ()
  "Test that beads-form-cancel is defined as a command."
  (should (fboundp 'beads-form-cancel))
  (should (commandp 'beads-form-cancel)))

(ert-deftest beads-form-test-keybindings ()
  "Test that keybindings are set up correctly."
  (should (eq (lookup-key beads-form-mode-map (kbd "C-c C-c"))
              #'beads-form-commit))
  (should (eq (lookup-key beads-form-mode-map (kbd "C-c C-k"))
              #'beads-form-cancel)))

(ert-deftest beads-form-test-open-creates-buffer ()
  "Test that beads-form-open creates a buffer."
  (let ((buffer nil)
        (issue '((id . "test-123")
                 (title . "Test Issue")
                 (status . "open")
                 (priority . 2)
                 (issue_type . "task"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-form-open issue))
          (should buffer)
          (should (buffer-live-p buffer))
          (should (string-match-p "beads-form.*test-123"
                                  (buffer-name buffer))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-form-test-open-sets-buffer-locals ()
  "Test that beads-form-open sets buffer-local variables."
  (let ((buffer nil)
        (issue '((id . "test-456")
                 (title . "Test Issue")
                 (status . "open")
                 (priority . 2)
                 (issue_type . "task"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-form-open issue))
          (with-current-buffer buffer
            (should (string= beads-form--issue-id "test-456"))
            (should beads-form--original-issue)
            (should beads-form--widgets)))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-form-test-open-renders-fields ()
  "Test that beads-form-open renders all expected fields."
  (let ((buffer nil)
        (issue '((id . "test-789")
                 (title . "Test Title")
                 (status . "in_progress")
                 (priority . 1)
                 (issue_type . "feature")
                 (description . "Test description"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-form-open issue))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match-p "Title:" content))
              (should (string-match-p "Status:" content))
              (should (string-match-p "Priority:" content))
              (should (string-match-p "Type:" content))
              (should (string-match-p "Description:" content)))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-form-test-collect-changes-empty ()
  "Test that collect-changes returns nil when nothing changed."
  (let ((buffer nil)
        (issue '((id . "test-unchanged")
                 (title . "Test")
                 (status . "open")
                 (priority . 2)
                 (issue_type . "task"))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-form-open issue))
          (with-current-buffer buffer
            (should (null (beads-form--collect-changes)))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-form-test-detail-edit-form-keybinding ()
  "Test that E is bound to beads-detail-edit-form in detail mode."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "E"))
                #'beads-detail-edit-form))))

(ert-deftest beads-form-test-detail-edit-form-defined ()
  "Test that beads-detail-edit-form is defined as a command."
  (should (fboundp 'beads-detail-edit-form))
  (should (commandp 'beads-detail-edit-form)))

(provide 'beads-form-test)
;;; beads-form-test.el ends here
