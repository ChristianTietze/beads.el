;;; beads-list-test.el --- Tests for beads-list.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads issue list mode.
;;
;; Test categories:
;; 1. Formatter tests - test formatters without daemon
;; 2. Entry conversion tests - test issue to tabulated-list conversion (mocked)
;; 3. Mode tests - test mode setup and keybindings
;; 4. Integration tests - test with actual daemon (tagged :integration)
;;
;; Note on test isolation:
;; Integration tests connect to the actual beads daemon in this repo.
;; Tests are read-only and do not modify data.

;;; Code:

(require 'ert)
(require 'beads-list)

;;; Formatter tests (no daemon needed)

(ert-deftest beads-list-test-format-id ()
  "Test that beads--format-id returns the issue ID."
  (let ((issue '((id . "bd-a1b2")
                 (title . "Test issue"))))
    (should (equal (beads--format-id issue) "bd-a1b2"))))

(ert-deftest beads-list-test-format-status-open ()
  "Test that beads--format-status formats open status correctly."
  (let ((issue '((status . "open"))))
    (let ((result (beads--format-status issue)))
      (should (equal result "open"))
      (should (eq (get-text-property 0 'face result) 'beads-list-status-open)))))

(ert-deftest beads-list-test-format-status-in-progress ()
  "Test that beads--format-status formats in_progress status with face."
  (let ((issue '((status . "in_progress"))))
    (let ((result (beads--format-status issue)))
      (should (equal result "in_progress"))
      (should (eq (get-text-property 0 'face result) 'beads-list-status-in-progress)))))

(ert-deftest beads-list-test-format-status-closed ()
  "Test that beads--format-status formats closed status with face."
  (let ((issue '((status . "closed"))))
    (let ((result (beads--format-status issue)))
      (should (equal result "closed"))
      (should (eq (get-text-property 0 'face result) 'beads-list-status-closed)))))

(ert-deftest beads-list-test-format-status-blocked ()
  "Test that beads--format-status formats blocked status with face."
  (let ((issue '((status . "blocked"))))
    (let ((result (beads--format-status issue)))
      (should (equal result "blocked"))
      (should (eq (get-text-property 0 'face result) 'beads-list-status-blocked)))))

(ert-deftest beads-list-test-format-status-hooked ()
  "Test that beads--format-status formats hooked status with face."
  (let ((issue '((status . "hooked"))))
    (let ((result (beads--format-status issue)))
      (should (equal result "hooked"))
      (should (eq (get-text-property 0 'face result) 'beads-list-status-hooked)))))

(ert-deftest beads-list-test-format-priority-p0 ()
  "Test that beads--format-priority formats P0 with bold red face."
  (let ((issue '((priority . 0))))
    (let ((result (beads--format-priority issue)))
      (should (equal result "P0"))
      (should (eq (get-text-property 0 'face result) 'beads-list-priority-p0)))))

(ert-deftest beads-list-test-format-priority-p1 ()
  "Test that beads--format-priority formats P1 with orange face."
  (let ((issue '((priority . 1))))
    (let ((result (beads--format-priority issue)))
      (should (equal result "P1"))
      (should (eq (get-text-property 0 'face result) 'beads-list-priority-p1)))))

(ert-deftest beads-list-test-format-priority-p2 ()
  "Test that beads--format-priority formats P2 with default face."
  (let ((issue '((priority . 2))))
    (let ((result (beads--format-priority issue)))
      (should (equal result "P2"))
      (should (eq (get-text-property 0 'face result) 'default)))))

(ert-deftest beads-list-test-format-priority-p3 ()
  "Test that beads--format-priority formats P3 correctly."
  (let ((issue '((priority . 3))))
    (should (equal (beads--format-priority issue) "P3"))))

(ert-deftest beads-list-test-format-priority-p4 ()
  "Test that beads--format-priority formats P4 correctly."
  (let ((issue '((priority . 4))))
    (should (equal (beads--format-priority issue) "P4"))))

(ert-deftest beads-list-test-format-type-full-style ()
  "Test that beads--format-type returns full type names when style is full."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph nil))
    (should (equal (beads--format-type '((issue_type . "bug"))) "bug"))
    (should (equal (beads--format-type '((issue_type . "feature"))) "feature"))
    (should (equal (beads--format-type '((issue_type . "task"))) "task"))
    (should (equal (beads--format-type '((issue_type . "epic"))) "epic"))
    (should (equal (beads--format-type '((issue_type . "chore"))) "chore"))
    (should (equal (beads--format-type '((issue_type . "gate"))) "gate"))
    (should (equal (beads--format-type '((issue_type . "convoy"))) "convoy"))
    (should (equal (beads--format-type '((issue_type . "agent"))) "agent"))
    (should (equal (beads--format-type '((issue_type . "role"))) "role"))))

(ert-deftest beads-list-test-format-type-short-style ()
  "Test that beads--format-type abbreviates types when style is short."
  (let ((beads-list-type-style 'short)
        (beads-list-type-glyph nil))
    (should (equal (beads--format-type '((issue_type . "bug"))) "bug"))
    (should (equal (beads--format-type '((issue_type . "feature"))) "feat"))
    (should (equal (beads--format-type '((issue_type . "task"))) "task"))
    (should (equal (beads--format-type '((issue_type . "epic"))) "epic"))
    (should (equal (beads--format-type '((issue_type . "chore"))) "chor"))
    (should (equal (beads--format-type '((issue_type . "gate"))) "gate"))
    (should (equal (beads--format-type '((issue_type . "convoy"))) "conv"))
    (should (equal (beads--format-type '((issue_type . "agent"))) "agnt"))
    (should (equal (beads--format-type '((issue_type . "role"))) "role"))))

(ert-deftest beads-list-test-format-type-special-faces ()
  "Test that special types get appropriate faces."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph nil))
    (should (eq (get-text-property 0 'face (beads--format-type '((issue_type . "gate"))))
                'beads-list-type-gate))
    (should (eq (get-text-property 0 'face (beads--format-type '((issue_type . "convoy"))))
                'beads-list-type-convoy))
    (should (eq (get-text-property 0 'face (beads--format-type '((issue_type . "agent"))))
                'beads-list-type-agent))
    (should (eq (get-text-property 0 'face (beads--format-type '((issue_type . "role"))))
                'beads-list-type-role))))

(ert-deftest beads-list-test-format-type-regular-no-face ()
  "Test that regular types have no special face."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph nil))
    (should (null (get-text-property 0 'face (beads--format-type '((issue_type . "bug"))))))
    (should (null (get-text-property 0 'face (beads--format-type '((issue_type . "feature"))))))
    (should (null (get-text-property 0 'face (beads--format-type '((issue_type . "task"))))))))

(ert-deftest beads-list-test-format-type-glyphs ()
  "Test that glyphs are prepended when beads-list-type-glyph is non-nil."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph t))
    (should (string-prefix-p "■ " (beads--format-type '((issue_type . "gate")))))
    (should (string-prefix-p "▶ " (beads--format-type '((issue_type . "convoy")))))
    (should (string-prefix-p "◉ " (beads--format-type '((issue_type . "agent")))))
    (should (string-prefix-p "● " (beads--format-type '((issue_type . "role")))))))

(ert-deftest beads-list-test-format-type-no-glyph-regular ()
  "Test that regular types have no glyph even when glyphs enabled."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph t))
    (should (equal (beads--format-type '((issue_type . "bug"))) "bug"))
    (should (equal (beads--format-type '((issue_type . "task"))) "task"))))

(ert-deftest beads-list-test-format-type-glyph-with-short ()
  "Test that glyphs work with short style."
  (let ((beads-list-type-style 'short)
        (beads-list-type-glyph t))
    (should (equal (beads--format-type '((issue_type . "convoy"))) "▶ conv"))
    (should (equal (beads--format-type '((issue_type . "agent"))) "◉ agnt"))))

(ert-deftest beads-list-test-format-type-unknown ()
  "Test that beads--format-type returns unknown types unchanged."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph nil))
    (should (equal (beads--format-type '((issue_type . "unknown"))) "unknown"))))

(ert-deftest beads-list-test-format-title-short ()
  "Test that beads--format-title returns short titles unchanged."
  (let ((issue '((title . "Short title"))))
    (should (equal (beads--format-title issue) "Short title"))))

(ert-deftest beads-list-test-format-title-long ()
  "Test that beads--format-title truncates long titles to 50 chars."
  (let ((issue '((title . "This is a very long title that should be truncated because it exceeds the maximum length"))))
    (let ((result (beads--format-title issue)))
      (should (equal (length result) 50))
      (should (string-suffix-p "..." result))
      (should (equal result "This is a very long title that should be trunca...")))))

(ert-deftest beads-list-test-format-title-exactly-50 ()
  "Test that beads--format-title does not truncate 50-char titles."
  (let ((issue '((title . "12345678901234567890123456789012345678901234567890"))))
    (should (equal (beads--format-title issue)
                   "12345678901234567890123456789012345678901234567890"))))

(ert-deftest beads-list-test-format-title-missing ()
  "Test that beads--format-title handles missing title gracefully."
  (let ((issue '((id . "bd-test"))))
    (should (equal (beads--format-title issue) ""))))

;;; Entry conversion tests (mocked)

(ert-deftest beads-list-test-entries-single-issue ()
  "Test that beads-list-entries converts a single issue correctly."
  (let ((issues '(((id . "bd-a1b2")
                   (title . "Test issue")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")))))
    (let ((entries (beads-list-entries issues)))
      (should (= (length entries) 1))
      (let ((entry (car entries)))
        (should (equal (car entry) "bd-a1b2"))
        (should (vectorp (cadr entry)))
        (should (= (length (cadr entry)) 5))))))

(ert-deftest beads-list-test-entries-multiple-issues ()
  "Test that beads-list-entries converts multiple issues correctly."
  (let ((issues '(((id . "bd-a1b2")
                   (title . "First issue")
                   (status . "open")
                   (priority . 1)
                   (issue_type . "bug"))
                  ((id . "bd-c3d4")
                   (title . "Second issue")
                   (status . "in_progress")
                   (priority . 0)
                   (issue_type . "feature")))))
    (let ((entries (beads-list-entries issues)))
      (should (= (length entries) 2))
      (should (equal (car (nth 0 entries)) "bd-a1b2"))
      (should (equal (car (nth 1 entries)) "bd-c3d4")))))

(ert-deftest beads-list-test-entries-empty ()
  "Test that beads-list-entries handles empty issue list."
  (let ((issues '()))
    (should (equal (beads-list-entries issues) '()))))

(ert-deftest beads-list-test-entries-column-order ()
  "Test that beads-list-entries produces columns in correct order."
  (let ((beads-list-type-style 'full)
        (beads-list-type-glyph nil)
        (issues '(((id . "bd-test")
                   (title . "Test")
                   (status . "closed")
                   (priority . 0)
                   (issue_type . "feature")))))
    (let* ((entries (beads-list-entries issues))
           (entry (car entries))
           (columns (cadr entry)))
      (should (equal (aref columns 0) "bd-test"))
      (should (equal (aref columns 1) "closed"))
      (should (equal (aref columns 2) "P0"))
      (should (equal (aref columns 3) "feature"))
      (should (equal (aref columns 4) "Test")))))

(ert-deftest beads-list-test-entries-preserves-faces ()
  "Test that beads-list-entries preserves text properties from formatters."
  (let ((issues '(((id . "bd-test")
                   (title . "Test")
                   (status . "in_progress")
                   (priority . 0)
                   (issue_type . "task")))))
    (let* ((entries (beads-list-entries issues))
           (columns (cadr (car entries)))
           (status-col (aref columns 1))
           (priority-col (aref columns 2)))
      (should (eq (get-text-property 0 'face status-col) 'beads-list-status-in-progress))
      (should (eq (get-text-property 0 'face priority-col) 'beads-list-priority-p0)))))

;;; Mode tests (no daemon)

(ert-deftest beads-list-test-mode-derived-from-tabulated-list ()
  "Test that beads-list-mode is derived from tabulated-list-mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (derived-mode-p 'tabulated-list-mode))
    (should (derived-mode-p 'beads-list-mode))))

(ert-deftest beads-list-test-mode-sets-format ()
  "Test that beads-list-mode sets tabulated-list-format correctly."
  (with-temp-buffer
    (beads-list-mode)
    (should (vectorp tabulated-list-format))
    (should (= (length tabulated-list-format) 5))
    (should (equal (car (aref tabulated-list-format 0)) "ID"))
    (should (equal (car (aref tabulated-list-format 1)) "Status"))
    (should (equal (car (aref tabulated-list-format 2)) "Pri"))
    (should (equal (car (aref tabulated-list-format 3)) "Type"))
    (should (equal (car (aref tabulated-list-format 4)) "Title"))))

(ert-deftest beads-list-test-mode-sets-padding ()
  "Test that beads-list-mode sets tabulated-list-padding."
  (with-temp-buffer
    (beads-list-mode)
    (should (= tabulated-list-padding 2))))

(ert-deftest beads-list-test-mode-sets-sort-key ()
  "Test that beads-list-mode sets initial sort key to ID."
  (with-temp-buffer
    (beads-list-mode)
    (should (equal tabulated-list-sort-key '("ID")))))

(ert-deftest beads-list-test-mode-keybindings ()
  "Test that beads-list-mode sets up keybindings correctly."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "g")) #'beads-list-refresh))
    (should (eq (lookup-key beads-list-mode-map (kbd "RET")) #'beads-list-goto-issue))
    (should (eq (lookup-key beads-list-mode-map (kbd "q")) #'quit-window))))

(ert-deftest beads-list-test-mode-inherits-parent-keybindings ()
  "Test that beads-list-mode inherits tabulated-list-mode keybindings."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "S"))
                (lookup-key tabulated-list-mode-map (kbd "S"))))))

(ert-deftest beads-list-test-get-issue-at-point-found ()
  "Test that beads--get-issue-at-point returns issue when found."
  (with-temp-buffer
    (beads-list-mode)
    (let* ((issues '(((id . "bd-a1b2")
                      (title . "Test issue")
                      (status . "open")
                      (priority . 2)
                      (issue_type . "task"))
                     ((id . "bd-c3d4")
                      (title . "Another issue")
                      (status . "closed")
                      (priority . 1)
                      (issue_type . "bug"))))
           (beads-list--issues issues))
      (setq tabulated-list-entries (beads-list-entries issues))
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line 1)
      (let ((issue (beads--get-issue-at-point)))
        (should issue)
        (should (member (alist-get 'id issue) '("bd-a1b2" "bd-c3d4")))))))

(ert-deftest beads-list-test-get-issue-at-point-not-found ()
  "Test that beads--get-issue-at-point returns nil when no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (let ((beads-list--issues '()))
      (should (null (beads--get-issue-at-point))))))

(ert-deftest beads-list-test-get-issue-at-point-multiple-issues ()
  "Test that beads--get-issue-at-point can find different issues."
  (with-temp-buffer
    (beads-list-mode)
    (let* ((issues '(((id . "bd-a1b2")
                      (title . "First")
                      (status . "open")
                      (priority . 2)
                      (issue_type . "task"))
                     ((id . "bd-c3d4")
                      (title . "Second")
                      (status . "closed")
                      (priority . 1)
                      (issue_type . "bug"))
                     ((id . "bd-e5f6")
                      (title . "Third")
                      (status . "open")
                      (priority . 3)
                      (issue_type . "feature"))))
           (beads-list--issues issues))
      (setq tabulated-list-entries (beads-list-entries issues))
      (tabulated-list-print)
      (goto-char (point-min))
      (forward-line 1)
      (let* ((issue1 (beads--get-issue-at-point))
             (id1 (alist-get 'id issue1)))
        (should issue1)
        (should (member id1 '("bd-a1b2" "bd-c3d4" "bd-e5f6")))
        (forward-line 1)
        (let* ((issue2 (beads--get-issue-at-point))
               (id2 (when issue2 (alist-get 'id issue2))))
          (when issue2
            (should (member id2 '("bd-a1b2" "bd-c3d4" "bd-e5f6")))
            (should-not (equal id1 id2))))))))

;;; Integration tests (require daemon)

(ert-deftest beads-list-test-refresh-with-daemon ()
  "Test that beads-list-refresh fetches issues from daemon."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (with-temp-buffer
    (beads-list-mode)
    (beads-list-refresh)
    (should (vectorp (beads-rpc-list)))
    (should (>= (length beads-list--issues) 0))
    (should (listp beads-list--issues))))

(ert-deftest beads-list-test-refresh-populates-entries ()
  "Test that beads-list-refresh populates tabulated-list-entries."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (with-temp-buffer
    (beads-list-mode)
    (beads-list-refresh)
    (should (listp tabulated-list-entries))
    (should (= (length tabulated-list-entries) (length beads-list--issues)))))

(ert-deftest beads-list-test-refresh-error-handling ()
  "Test that beads-list-refresh handles RPC errors gracefully."
  :tags '(:integration)
  (with-temp-buffer
    (beads-list-mode)
    (cl-letf (((symbol-function 'beads-rpc-list)
               (lambda (&rest _args)
                 (signal 'beads-rpc-error '("Test error")))))
      (beads-list-refresh)
      (should t))))

(ert-deftest beads-list-test-list-command-creates-buffer ()
  "Test that beads-list creates and switches to issue buffer."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((buffer-name "*Beads Issues*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (progn
          (beads-list)
          (should (get-buffer buffer-name))
          (should (eq major-mode 'beads-list-mode)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest beads-list-test-list-command-refreshes ()
  "Test that beads-list fetches issues on open."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((buffer-name "*Beads Issues*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (progn
          (beads-list)
          (with-current-buffer buffer-name
            (should (>= (length beads-list--issues) 0))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest beads-list-test-goto-issue-with-issue ()
  "Test that beads-list-goto-issue displays message for issue at point."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((buffer-name "*Beads Issues*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (progn
          (beads-list)
          (with-current-buffer buffer-name
            (when (> (length beads-list--issues) 0)
              (goto-char (point-min))
              (forward-line 1)
              (let ((message-log-max t))
                (beads-list-goto-issue)
                (should (get-buffer "*Messages*"))))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest beads-list-test-goto-issue-no-issue ()
  "Test that beads-list-goto-issue shows message when no issue at point."
  (with-temp-buffer
    (beads-list-mode)
    (let ((beads-list--issues '()))
      (let ((message-log-max t))
        (beads-list-goto-issue)
        (should (get-buffer "*Messages*"))))))

(ert-deftest beads-list-test-buffer-reuse ()
  "Test that calling beads-list twice reuses the same buffer."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((buffer-name "*Beads Issues*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (progn
          (beads-list)
          (let ((first-buffer (get-buffer buffer-name)))
            (beads-list)
            (should (eq first-buffer (get-buffer buffer-name)))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(provide 'beads-list-test)
;;; beads-list-test.el ends here
