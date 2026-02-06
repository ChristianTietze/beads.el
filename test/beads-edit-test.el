;;; beads-edit-test.el --- Tests for beads-edit -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'beads-edit)

(ert-deftest beads-edit-test-mode-defined ()
  "Test that beads-edit-mode is defined."
  (should (fboundp 'beads-edit-mode)))

(ert-deftest beads-edit-test-commit-defined ()
  "Test that beads-edit-commit is defined."
  (should (fboundp 'beads-edit-commit))
  (should (commandp 'beads-edit-commit)))

(ert-deftest beads-edit-test-abort-defined ()
  "Test that beads-edit-abort is defined."
  (should (fboundp 'beads-edit-abort))
  (should (commandp 'beads-edit-abort)))

(ert-deftest beads-edit-test-keybindings ()
  "Test that keybindings are set up correctly."
  (should (eq (lookup-key beads-edit-mode-map (kbd "C-c C-c"))
              #'beads-edit-commit))
  (should (eq (lookup-key beads-edit-mode-map (kbd "C-c C-k"))
              #'beads-edit-abort)))

(ert-deftest beads-edit-test-field-creates-buffer ()
  "Test that beads-edit-field creates a buffer."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-edit-field "test-123" :description "Initial content"))
          (should buffer)
          (should (buffer-live-p buffer))
          (should (string-match-p "beads-edit.*test-123.*description"
                                  (buffer-name buffer))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-field-sets-buffer-locals ()
  "Test that beads-edit-field sets buffer-local variables."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-edit-field "test-456" :design "Design content"))
          (with-current-buffer buffer
            (should (string= beads-edit--issue-id "test-456"))
            (should (eq beads-edit--field :design))
            (should (string= beads-edit--original-content "Design content"))
            (should beads-edit--allow-write-back)
            (should beads-edit-mode)))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-field-inserts-content ()
  "Test that beads-edit-field inserts initial content."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-edit-field "test-789" :notes "Some notes here"))
          (with-current-buffer buffer
            (should (string= (buffer-substring-no-properties (point-min) (point-max))
                             "Some notes here"))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-field-empty-content ()
  "Test that beads-edit-field handles nil content."
  (let ((buffer nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf))))
            (beads-edit-field "test-empty" :description nil))
          (with-current-buffer buffer
            (should (string= (buffer-substring-no-properties (point-min) (point-max))
                             ""))
            (should (string= beads-edit--original-content ""))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-mode-sets-header-line ()
  "Test that enabling mode sets header-line-format."
  (with-temp-buffer
    (beads-edit-mode 1)
    (should header-line-format)
    (should (string-match-p "save" header-line-format))
    (should (string-match-p "discard" header-line-format))))

(ert-deftest beads-edit-test-mode-lighter ()
  "Test that beads-edit-mode has correct lighter."
  (with-temp-buffer
    (beads-edit-mode 1)
    (let ((mode-line (format-mode-line minor-mode-alist)))
      (should (string-match-p "BeadsEdit" mode-line)))))

(ert-deftest beads-edit-test-abort-sets-flag ()
  "Test that abort sets allow-write-back to nil."
  (let ((buffer nil)
        (exit-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf)))
                    ((symbol-function 'beads-edit--exit)
                     (lambda ()
                       (setq exit-called t)
                       (should-not beads-edit--allow-write-back))))
            (beads-edit-field "test-abort" :description "Content")
            (with-current-buffer buffer
              (beads-edit-abort))
            (should exit-called)))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-commit-keeps-flag ()
  "Test that commit keeps allow-write-back as t."
  (let ((buffer nil)
        (exit-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf)))
                    ((symbol-function 'beads-edit--exit)
                     (lambda ()
                       (setq exit-called t)
                       (should beads-edit--allow-write-back))))
            (beads-edit-field "test-commit" :description "Content")
            (with-current-buffer buffer
              (beads-edit-commit))
            (should exit-called)))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-exit-no-rpc-when-unchanged ()
  "Test that exit doesn't call RPC when content unchanged."
  (let ((buffer nil)
        (rpc-called nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf)))
                    ((symbol-function 'beads-client-update)
                     (lambda (&rest _)
                       (setq rpc-called t)))
                    ((symbol-function 'kill-buffer)
                     (lambda (_) nil))
                    ((symbol-function 'set-window-configuration)
                     (lambda (_) nil)))
            (beads-edit-field "test-unchanged" :description "Same content")
            (with-current-buffer buffer
              (beads-edit--exit))
            (should-not rpc-called)))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-exit-calls-rpc-when-changed ()
  "Test that exit calls RPC when content changed."
  (let ((buffer nil)
        (rpc-called nil)
        (rpc-args nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buf) (setq buffer buf)))
                    ((symbol-function 'beads-client-update)
                     (lambda (id field content)
                       (setq rpc-called t)
                       (setq rpc-args (list id field content))))
                    ((symbol-function 'kill-buffer)
                     (lambda (_) nil))
                    ((symbol-function 'set-window-configuration)
                     (lambda (_) nil)))
            (beads-edit-field "test-changed" :description "Original")
            (with-current-buffer buffer
              (erase-buffer)
              (insert "Modified content")
              (beads-edit--exit))
            (should rpc-called)
            (should (equal (car rpc-args) "test-changed"))
            (should (equal (cadr rpc-args) :description))
            (should (equal (caddr rpc-args) "Modified content"))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

(ert-deftest beads-edit-test-field-minibuffer-unchanged ()
  "Test minibuffer editing when value unchanged."
  (let ((rpc-called nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt default)
                 default))
              ((symbol-function 'beads-client-update)
               (lambda (&rest _)
                 (setq rpc-called t))))
      (let ((result (beads-edit-field-minibuffer "test-id" :title "Same" "Title: ")))
        (should-not result)
        (should-not rpc-called)))))

(ert-deftest beads-edit-test-field-minibuffer-changed ()
  "Test minibuffer editing when value changed."
  (let ((rpc-called nil)
        (rpc-args nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_prompt _default)
                 "New title"))
              ((symbol-function 'beads-client-update)
               (lambda (id field value)
                 (setq rpc-called t)
                 (setq rpc-args (list id field value)))))
      (let ((result (beads-edit-field-minibuffer "test-id" :title "Old" "Title: ")))
        (should (equal result "New title"))
        (should rpc-called)
        (should (equal rpc-args '("test-id" :title "New title")))))))

(ert-deftest beads-edit-test-field-completing-unchanged ()
  "Test completing-read editing when value unchanged."
  (let ((rpc-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _)
                 "open"))
              ((symbol-function 'beads-client-update)
               (lambda (&rest _)
                 (setq rpc-called t))))
      (let ((result (beads-edit-field-completing
                     "test-id" :status "open" "Status: "
                     '("open" "in_progress" "closed"))))
        (should-not result)
        (should-not rpc-called)))))

(ert-deftest beads-edit-test-field-completing-changed ()
  "Test completing-read editing when value changed."
  (let ((rpc-called nil)
        (rpc-args nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _choices &rest _)
                 "closed"))
              ((symbol-function 'beads-client-update)
               (lambda (id field value)
                 (setq rpc-called t)
                 (setq rpc-args (list id field value)))))
      (let ((result (beads-edit-field-completing
                     "test-id" :status "open" "Status: "
                     '("open" "in_progress" "closed"))))
        (should (equal result "closed"))
        (should rpc-called)
        (should (equal rpc-args '("test-id" :status "closed")))))))

(provide 'beads-edit-test)
;;; beads-edit-test.el ends here
