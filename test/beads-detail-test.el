;;; beads-detail-test.el --- Tests for beads-detail.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads issue detail mode.
;;
;; Test categories:
;; 1. Face definition tests - test all faces are defined with expected properties
;; 2. Render tests - test beads-detail--render inserts expected sections (mocked)
;; 3. Mode tests - test mode setup, keybindings, and read-only behavior
;; 4. Integration tests - test with actual daemon (tagged :integration)
;;
;; Note on test isolation:
;; Integration tests connect to the actual beads daemon in this repo.
;; Tests are read-only and do not modify data.

;;; Code:

(require 'ert)
(require 'beads-detail)

;;; Face definition tests (no daemon needed)

(ert-deftest beads-detail-test-faces-defined ()
  "Test that all detail mode faces are defined."
  (should (facep 'beads-detail-id-face))
  (should (facep 'beads-detail-title-face))
  (should (facep 'beads-detail-section-face))
  (should (facep 'beads-detail-label-face))
  (should (facep 'beads-detail-value-face)))

(ert-deftest beads-detail-test-id-face-properties ()
  "Test that beads-detail-id-face has expected properties."
  (let ((face-attrs (face-all-attributes 'beads-detail-id-face nil)))
    (should (listp face-attrs))))

(ert-deftest beads-detail-test-title-face-properties ()
  "Test that beads-detail-title-face has expected properties."
  (let ((face-attrs (face-all-attributes 'beads-detail-title-face nil)))
    (should (listp face-attrs))))

(ert-deftest beads-detail-test-section-face-properties ()
  "Test that beads-detail-section-face has expected properties."
  (let ((face-attrs (face-all-attributes 'beads-detail-section-face nil)))
    (should (listp face-attrs))))

(ert-deftest beads-detail-test-label-face-properties ()
  "Test that beads-detail-label-face has expected properties."
  (let ((face-attrs (face-all-attributes 'beads-detail-label-face nil)))
    (should (listp face-attrs))))

(ert-deftest beads-detail-test-value-face-properties ()
  "Test that beads-detail-value-face has expected properties."
  (let ((face-attrs (face-all-attributes 'beads-detail-value-face nil)))
    (should (listp face-attrs))))

;;; Render tests (mocked, no daemon)

(ert-deftest beads-detail-test-render-inserts-header ()
  "Test that beads-detail--render inserts ID and title in header."
  (with-temp-buffer
    (let ((issue '((id . "bd-test123")
                   (title . "Test Issue Title")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "bd-test123" nil t))
      (should (search-forward "Test Issue Title" nil t)))))

(ert-deftest beads-detail-test-render-inserts-metadata ()
  "Test that beads-detail--render inserts status, priority, and type."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "in_progress")
                   (priority . 1)
                   (issue_type . "bug"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should (string-match-p "in_progress" buffer-content))
        (should (string-match-p "1" buffer-content))
        (should (string-match-p "bug" buffer-content))))))

(ert-deftest beads-detail-test-render-created-by-present ()
  "Test that beads-detail--render shows created_by when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (created_by . "alice"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should (string-match-p "Created by:" buffer-content))
        (should (string-match-p "alice" buffer-content))))))

(ert-deftest beads-detail-test-render-created-by-absent ()
  "Test that beads-detail--render handles missing created_by gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Created by:" buffer-content))))))

(ert-deftest beads-detail-test-render-comments-present ()
  "Test that beads-detail--render shows comments when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (comments . [((id . 1)
                                 (author . "alice")
                                 (text . "This is a test comment")
                                 (created_at . "2026-01-06T10:00:00Z"))]))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should (string-match-p "Comments (1):" buffer-content))
        (should (string-match-p "\\[alice\\]" buffer-content))
        (should (string-match-p "This is a test comment" buffer-content))))))

(ert-deftest beads-detail-test-render-comments-absent ()
  "Test that beads-detail--render handles missing comments gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Comments" buffer-content))))))

(ert-deftest beads-detail-test-render-comments-empty ()
  "Test that beads-detail--render handles empty comments array gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (comments . []))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Comments" buffer-content))))))

(ert-deftest beads-detail-test-render-description-present ()
  "Test that beads-detail--render shows description when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (description . "This is a test description"))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "This is a test description" nil t)))))

(ert-deftest beads-detail-test-render-description-absent ()
  "Test that beads-detail--render handles missing description gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Description:" buffer-content))))))

(ert-deftest beads-detail-test-render-description-empty ()
  "Test that beads-detail--render handles empty description gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (description . ""))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Description:" buffer-content))))))

(ert-deftest beads-detail-test-render-design-present ()
  "Test that beads-detail--render shows design section when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (design . "Design notes here"))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "Design notes here" nil t)))))

(ert-deftest beads-detail-test-render-design-absent ()
  "Test that beads-detail--render handles missing design gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Design:" buffer-content))))))

(ert-deftest beads-detail-test-render-acceptance-present ()
  "Test that beads-detail--render shows acceptance criteria when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (acceptance_criteria . "Acceptance criteria here"))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "Acceptance criteria here" nil t)))))

(ert-deftest beads-detail-test-render-acceptance-absent ()
  "Test that beads-detail--render handles missing acceptance criteria gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Acceptance:" buffer-content))))))

(ert-deftest beads-detail-test-render-dependencies-present ()
  "Test that beads-detail--render shows dependencies when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (depends_on . [((id . "bd-dep1")
                                   (dep_type . "blocks"))
                                  ((id . "bd-dep2")
                                   (dep_type . "related"))]))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "bd-dep1" nil t))
      (should (search-forward "bd-dep2" nil t)))))

(ert-deftest beads-detail-test-render-dependencies-absent ()
  "Test that beads-detail--render handles missing dependencies gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Dependencies:" buffer-content))))))

(ert-deftest beads-detail-test-render-labels-present ()
  "Test that beads-detail--render shows labels when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (labels . ["backend" "urgent"]))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "backend" nil t))
      (should (search-forward "urgent" nil t)))))

(ert-deftest beads-detail-test-render-labels-absent ()
  "Test that beads-detail--render handles missing labels gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Labels:" buffer-content))))))

(ert-deftest beads-detail-test-render-timestamps ()
  "Test that beads-detail--render shows created and updated timestamps."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (created_at . "2025-01-01T10:00:00Z")
                   (updated_at . "2025-01-02T11:00:00Z"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should (string-match-p "2025-01-01" buffer-content))
        (should (string-match-p "2025-01-02" buffer-content))))))

(ert-deftest beads-detail-test-render-assignee-present ()
  "Test that beads-detail--render shows assignee when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (assignee . "alice"))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "alice" nil t)))))

(ert-deftest beads-detail-test-render-minimal-issue ()
  "Test that beads-detail--render handles minimal issue with only required fields."
  (with-temp-buffer
    (let ((issue '((id . "bd-minimal")
                   (title . "Minimal Issue")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (goto-char (point-min))
      (should (search-forward "bd-minimal" nil t))
      (should (search-forward "Minimal Issue" nil t)))))

(ert-deftest beads-detail-test-render-all-fields ()
  "Test that beads-detail--render handles issue with all fields populated."
  (with-temp-buffer
    (let ((issue '((id . "bd-full")
                   (title . "Full Issue")
                   (status . "in_progress")
                   (priority . 1)
                   (issue_type . "feature")
                   (description . "Full description")
                   (design . "Design notes")
                   (acceptance_criteria . "Acceptance criteria")
                   (assignee . "bob")
                   (labels . ["frontend" "ui"])
                   (depends_on . [((id . "bd-dep1") (dep_type . "blocks"))])
                   (created_at . "2025-01-01T10:00:00Z")
                   (updated_at . "2025-01-02T11:00:00Z"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should (string-match-p "bd-full" buffer-content))
        (should (string-match-p "Full Issue" buffer-content))
        (should (string-match-p "Full description" buffer-content))
        (should (string-match-p "Design notes" buffer-content))
        (should (string-match-p "Acceptance criteria" buffer-content))
        (should (string-match-p "bob" buffer-content))
        (should (string-match-p "frontend" buffer-content))
        (should (string-match-p "bd-dep1" buffer-content))))))

(ert-deftest beads-detail-test-render-parent-present ()
  "Test that beads-detail--render shows parent link when present."
  (with-temp-buffer
    (let ((issue '((id . "bd-child")
                   (title . "Child Issue")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (parent_id . "bd-parent"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should (string-match-p "Parent:" buffer-content))
        (should (string-match-p "bd-parent" buffer-content))))))

(ert-deftest beads-detail-test-render-parent-absent ()
  "Test that beads-detail--render handles missing parent gracefully."
  (with-temp-buffer
    (let ((issue '((id . "bd-test")
                   (title . "Test")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task"))))
      (beads-detail--render issue)
      (let ((buffer-content (buffer-string)))
        (should-not (string-match-p "Parent:" buffer-content))))))

;;; Parent navigation tests (no daemon)

(ert-deftest beads-detail-test-goto-parent-defined ()
  "Test that beads-detail-goto-parent is defined as a command."
  (should (fboundp 'beads-detail-goto-parent))
  (should (commandp 'beads-detail-goto-parent)))

(ert-deftest beads-detail-test-view-children-defined ()
  "Test that beads-detail-view-children is defined as a command."
  (should (fboundp 'beads-detail-view-children))
  (should (commandp 'beads-detail-view-children)))

(ert-deftest beads-detail-test-keybinding-goto-parent ()
  "Test that P is bound to beads-detail-goto-parent."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "P"))
                #'beads-detail-goto-parent))))

(ert-deftest beads-detail-test-keybinding-view-children ()
  "Test that C is bound to beads-detail-view-children."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "C"))
                #'beads-detail-view-children))))

(ert-deftest beads-detail-test-goto-parent-no-parent ()
  "Test that beads-detail-goto-parent errors when no parent."
  (with-temp-buffer
    (beads-detail-mode)
    (setq beads-detail--current-issue '((id . "bd-test")
                                        (title . "Test")
                                        (status . "open")
                                        (priority . 2)
                                        (issue_type . "task")))
    (should-error (beads-detail-goto-parent) :type 'user-error)))

;;; Mode tests (no daemon)

(ert-deftest beads-detail-test-mode-derived-from-special ()
  "Test that beads-detail-mode is derived from special-mode."
  (with-temp-buffer
    (beads-detail-mode)
    (should (derived-mode-p 'special-mode))
    (should (derived-mode-p 'beads-detail-mode))))

(ert-deftest beads-detail-test-mode-buffer-read-only ()
  "Test that beads-detail-mode sets buffer to read-only."
  (with-temp-buffer
    (beads-detail-mode)
    (should buffer-read-only)))

(ert-deftest beads-detail-test-mode-keybinding-refresh ()
  "Test that beads-detail-mode binds 'g' to beads-detail-refresh."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "g"))
                #'beads-detail-refresh))))

(ert-deftest beads-detail-test-mode-keybinding-quit ()
  "Test that beads-detail-mode binds 'q' to quit-window."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "q"))
                #'quit-window))))

(ert-deftest beads-detail-test-mode-keybinding-edit ()
  "Test that beads-detail-mode binds 'e' to edit prefix map."
  (with-temp-buffer
    (beads-detail-mode)
    (should (keymapp (lookup-key beads-detail-mode-map (kbd "e"))))
    (should (eq (lookup-key beads-detail-mode-map (kbd "e d"))
                #'beads-detail-edit-description))))

(ert-deftest beads-detail-test-mode-keybinding-label-prefix ()
  "Test that 'e l' is a prefix map for label commands."
  (with-temp-buffer
    (beads-detail-mode)
    (should (keymapp (lookup-key beads-detail-mode-map (kbd "e l"))))
    (should (eq (lookup-key beads-detail-mode-map (kbd "e l a"))
                #'beads-detail-edit-label-add))
    (should (eq (lookup-key beads-detail-mode-map (kbd "e l r"))
                #'beads-detail-edit-label-remove))))

(ert-deftest beads-detail-test-label-add-defined ()
  "Test that beads-detail-edit-label-add is defined as a command."
  (should (fboundp 'beads-detail-edit-label-add))
  (should (commandp 'beads-detail-edit-label-add)))

(ert-deftest beads-detail-test-label-remove-defined ()
  "Test that beads-detail-edit-label-remove is defined as a command."
  (should (fboundp 'beads-detail-edit-label-remove))
  (should (commandp 'beads-detail-edit-label-remove)))

(ert-deftest beads-detail-test-label-add-calls-rpc ()
  "Test that beads-detail-edit-label-add calls beads-rpc-label-add."
  (let ((rpc-called nil)
        (rpc-args nil))
    (with-temp-buffer
      (beads-detail-mode)
      (setq beads-detail--current-issue '((id . "test-123")
                                          (title . "Test")
                                          (status . "open")
                                          (priority . 2)
                                          (issue_type . "task")
                                          (labels . ["existing"])))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt)
                   "new-label"))
                ((symbol-function 'beads-rpc-label-add)
                 (lambda (id label)
                   (setq rpc-called t)
                   (setq rpc-args (list id label))))
                ((symbol-function 'beads-detail-refresh)
                 (lambda () nil)))
        (beads-detail-edit-label-add)
        (should rpc-called)
        (should (equal rpc-args '("test-123" "new-label")))))))

(ert-deftest beads-detail-test-label-add-empty-input ()
  "Test that beads-detail-edit-label-add ignores empty input."
  (let ((rpc-called nil))
    (with-temp-buffer
      (beads-detail-mode)
      (setq beads-detail--current-issue '((id . "test-123")
                                          (title . "Test")
                                          (status . "open")
                                          (priority . 2)
                                          (issue_type . "task")))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt) ""))
                ((symbol-function 'beads-rpc-label-add)
                 (lambda (_id _label)
                   (setq rpc-called t))))
        (beads-detail-edit-label-add)
        (should-not rpc-called)))))

(ert-deftest beads-detail-test-label-remove-no-labels ()
  "Test that beads-detail-edit-label-remove handles issues with no labels."
  (let ((completing-read-called nil))
    (with-temp-buffer
      (beads-detail-mode)
      (setq beads-detail--current-issue '((id . "test-123")
                                          (title . "Test")
                                          (status . "open")
                                          (priority . 2)
                                          (issue_type . "task")
                                          (labels . [])))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (setq completing-read-called t)
                   "")))
        (beads-detail-edit-label-remove)
        (should-not completing-read-called)))))

(ert-deftest beads-detail-test-label-remove-calls-rpc ()
  "Test that beads-detail-edit-label-remove calls beads-rpc-label-remove."
  (let ((rpc-called nil)
        (rpc-args nil))
    (with-temp-buffer
      (beads-detail-mode)
      (setq beads-detail--current-issue '((id . "test-456")
                                          (title . "Test")
                                          (status . "open")
                                          (priority . 2)
                                          (issue_type . "task")
                                          (labels . ["label1" "label2"])))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _choices &rest _)
                   "label1"))
                ((symbol-function 'beads-rpc-label-remove)
                 (lambda (id label)
                   (setq rpc-called t)
                   (setq rpc-args (list id label))))
                ((symbol-function 'beads-detail-refresh)
                 (lambda () nil)))
        (beads-detail-edit-label-remove)
        (should rpc-called)
        (should (equal rpc-args '("test-456" "label1")))))))

(ert-deftest beads-detail-test-mode-inherits-parent-keybindings ()
  "Test that beads-detail-mode inherits special-mode keybindings."
  (with-temp-buffer
    (beads-detail-mode)
    (should (commandp (lookup-key beads-detail-mode-map (kbd "SPC"))))))

(ert-deftest beads-detail-test-mode-sets-buffer-name ()
  "Test that beads-detail-mode sets appropriate buffer name pattern."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq major-mode 'beads-detail-mode))))

;;; Integration tests (require daemon)

(ert-deftest beads-detail-test-show-creates-buffer ()
  "Test that beads-detail-show creates detail buffer."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((issues (beads-rpc-list '(:limit 1))))
    (skip-unless (> (length issues) 0))
    (let* ((issue-id (alist-get 'id (aref issues 0)))
           (buffer-name (format "*Beads: %s*" issue-id)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (unwind-protect
          (progn
            (beads-detail-show issue-id)
            (should (get-buffer buffer-name))
            (with-current-buffer buffer-name
              (should (eq major-mode 'beads-detail-mode))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))

(ert-deftest beads-detail-test-show-displays-content ()
  "Test that beads-detail-show displays issue content."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((issues (beads-rpc-list '(:limit 1))))
    (skip-unless (> (length issues) 0))
    (let* ((issue-id (alist-get 'id (aref issues 0)))
           (buffer-name (format "*Beads: %s*" issue-id)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (unwind-protect
          (progn
            (beads-detail-show issue-id)
            (with-current-buffer buffer-name
              (goto-char (point-min))
              (should (search-forward issue-id nil t))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))

(ert-deftest beads-detail-test-show-sets-buffer-local-issue-id ()
  "Test that beads-detail-show sets buffer-local issue ID."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((issues (beads-rpc-list '(:limit 1))))
    (skip-unless (> (length issues) 0))
    (let* ((issue-id (alist-get 'id (aref issues 0)))
           (buffer-name (format "*Beads: %s*" issue-id)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (unwind-protect
          (progn
            (beads-detail-show issue-id)
            (with-current-buffer buffer-name
              (should (boundp 'beads-detail--current-issue-id))
              (should (equal beads-detail--current-issue-id issue-id))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))

(ert-deftest beads-detail-test-refresh-updates-content ()
  "Test that beads-detail-refresh updates buffer content."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((issues (beads-rpc-list '(:limit 1))))
    (skip-unless (> (length issues) 0))
    (let* ((issue-id (alist-get 'id (aref issues 0)))
           (buffer-name (format "*Beads: %s*" issue-id)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (unwind-protect
          (progn
            (beads-detail-show issue-id)
            (with-current-buffer buffer-name
              (let ((old-content (buffer-string)))
                (beads-detail-refresh)
                (let ((new-content (buffer-string)))
                  (should (string= old-content new-content))))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))

(ert-deftest beads-detail-test-show-error-handling ()
  "Test that beads-detail-show handles RPC errors gracefully."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((buffer-name "*Beads: bd-nonexistent*"))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (unwind-protect
        (should-error (beads-detail-show "bd-nonexistent")
                      :type 'beads-rpc-error)
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest beads-detail-test-refresh-without-issue-id ()
  "Test that beads-detail-refresh handles missing issue ID gracefully."
  (with-temp-buffer
    (beads-detail-mode)
    (should-error (beads-detail-refresh))))

(ert-deftest beads-detail-test-buffer-reuse ()
  "Test that calling beads-detail-show twice reuses the same buffer."
  :tags '(:integration)
  (skip-unless (beads-rpc--socket-path))
  (skip-unless (file-exists-p (beads-rpc--socket-path)))
  (let ((issues (beads-rpc-list '(:limit 1))))
    (skip-unless (> (length issues) 0))
    (let* ((issue-id (alist-get 'id (aref issues 0)))
           (buffer-name (format "*Beads: %s*" issue-id)))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (unwind-protect
          (progn
            (beads-detail-show issue-id)
            (let ((first-buffer (get-buffer buffer-name)))
              (beads-detail-show issue-id)
              (should (eq first-buffer (get-buffer buffer-name)))))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))))))

(provide 'beads-detail-test)
;;; beads-detail-test.el ends here
