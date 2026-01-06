;;; beads-activity-test.el --- Tests for beads-activity.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads activity feed.
;;
;; Test categories:
;; 1. Mode definition tests - test beads-activity-mode is defined correctly
;; 2. Keybinding tests - test RET, f, F, g, l, q bindings
;; 3. Command availability tests - test commands are defined
;; 4. Glyph and face tests - test event visualization

;;; Code:

(require 'ert)
(require 'beads-activity)

;;; Mode definition tests

(ert-deftest beads-activity-test-mode-defined ()
  "Test that beads-activity-mode is defined."
  (should (fboundp 'beads-activity-mode)))

(ert-deftest beads-activity-test-mode-is-special ()
  "Test that beads-activity-mode is derived from special-mode."
  (with-temp-buffer
    (beads-activity-mode)
    (should (derived-mode-p 'special-mode))
    (should (derived-mode-p 'beads-activity-mode))))

(ert-deftest beads-activity-test-mode-readonly ()
  "Test that beads-activity-mode sets buffer to read-only."
  (with-temp-buffer
    (beads-activity-mode)
    (should buffer-read-only)))

;;; Keybinding tests

(ert-deftest beads-activity-test-keybinding-return ()
  "Test that RET is bound to beads-activity-goto-issue."
  (with-temp-buffer
    (beads-activity-mode)
    (should (eq (lookup-key beads-activity-mode-map (kbd "RET"))
                #'beads-activity-goto-issue))))

(ert-deftest beads-activity-test-keybinding-follow ()
  "Test that f is bound to beads-activity-toggle-follow."
  (with-temp-buffer
    (beads-activity-mode)
    (should (eq (lookup-key beads-activity-mode-map (kbd "f"))
                #'beads-activity-toggle-follow))))

(ert-deftest beads-activity-test-keybinding-filter ()
  "Test that F is bound to beads-activity-set-filter."
  (with-temp-buffer
    (beads-activity-mode)
    (should (eq (lookup-key beads-activity-mode-map (kbd "F"))
                #'beads-activity-set-filter))))

(ert-deftest beads-activity-test-keybinding-refresh ()
  "Test that g is bound to beads-activity-refresh."
  (with-temp-buffer
    (beads-activity-mode)
    (should (eq (lookup-key beads-activity-mode-map (kbd "g"))
                #'beads-activity-refresh))))

(ert-deftest beads-activity-test-keybinding-limit ()
  "Test that l is bound to beads-activity-set-limit."
  (with-temp-buffer
    (beads-activity-mode)
    (should (eq (lookup-key beads-activity-mode-map (kbd "l"))
                #'beads-activity-set-limit))))

(ert-deftest beads-activity-test-keybinding-quit ()
  "Test that q is bound to beads-activity-quit."
  (with-temp-buffer
    (beads-activity-mode)
    (should (eq (lookup-key beads-activity-mode-map (kbd "q"))
                #'beads-activity-quit))))

;;; Command availability tests

(ert-deftest beads-activity-test-command-defined ()
  "Test that beads-activity is defined as a command."
  (should (fboundp 'beads-activity))
  (should (commandp 'beads-activity)))

(ert-deftest beads-activity-test-goto-issue-defined ()
  "Test that beads-activity-goto-issue is defined as a command."
  (should (fboundp 'beads-activity-goto-issue))
  (should (commandp 'beads-activity-goto-issue)))

(ert-deftest beads-activity-test-refresh-defined ()
  "Test that beads-activity-refresh is defined as a command."
  (should (fboundp 'beads-activity-refresh))
  (should (commandp 'beads-activity-refresh)))

(ert-deftest beads-activity-test-toggle-follow-defined ()
  "Test that beads-activity-toggle-follow is defined as a command."
  (should (fboundp 'beads-activity-toggle-follow))
  (should (commandp 'beads-activity-toggle-follow)))

(ert-deftest beads-activity-test-set-filter-defined ()
  "Test that beads-activity-set-filter is defined as a command."
  (should (fboundp 'beads-activity-set-filter))
  (should (commandp 'beads-activity-set-filter)))

(ert-deftest beads-activity-test-set-limit-defined ()
  "Test that beads-activity-set-limit is defined as a command."
  (should (fboundp 'beads-activity-set-limit))
  (should (commandp 'beads-activity-set-limit)))

(ert-deftest beads-activity-test-quit-defined ()
  "Test that beads-activity-quit is defined as a command."
  (should (fboundp 'beads-activity-quit))
  (should (commandp 'beads-activity-quit)))

;;; Glyph tests

(ert-deftest beads-activity-test-glyph-create ()
  "Test glyph for create event."
  (let ((event '((type . "create"))))
    (should (stringp (beads-activity--glyph-for-event event)))))

(ert-deftest beads-activity-test-glyph-status-closed ()
  "Test glyph for status change to closed."
  (let ((event '((type . "status") (new_status . "closed"))))
    (should (stringp (beads-activity--glyph-for-event event)))))

(ert-deftest beads-activity-test-glyph-comment ()
  "Test glyph for comment event."
  (let ((event '((type . "comment"))))
    (should (stringp (beads-activity--glyph-for-event event)))))

(ert-deftest beads-activity-test-glyph-custom ()
  "Test that custom glyphs are used."
  (let ((beads-activity-glyphs '((create . "NEW"))))
    (should (equal (beads-activity--glyph-for-event '((type . "create")))
                   "NEW"))))

;;; Face tests

(ert-deftest beads-activity-test-face-create ()
  "Test face for create event."
  (let ((event '((type . "create"))))
    (should (eq (beads-activity--face-for-event event) 'beads-activity-create))))

(ert-deftest beads-activity-test-face-delete ()
  "Test face for delete event."
  (let ((event '((type . "delete"))))
    (should (eq (beads-activity--face-for-event event) 'beads-activity-delete))))

(ert-deftest beads-activity-test-face-completed ()
  "Test face for completed status."
  (let ((event '((type . "status") (new_status . "closed"))))
    (should (eq (beads-activity--face-for-event event) 'beads-activity-completed))))

(ert-deftest beads-activity-test-face-blocked ()
  "Test face for blocked status."
  (let ((event '((type . "status") (new_status . "blocked"))))
    (should (eq (beads-activity--face-for-event event) 'beads-activity-blocked))))

(ert-deftest beads-activity-test-face-comment ()
  "Test face for comment event."
  (let ((event '((type . "comment"))))
    (should (eq (beads-activity--face-for-event event) 'beads-activity-comment))))

;;; Render tests

(ert-deftest beads-activity-test-render-empty ()
  "Test that beads-activity--render handles empty list."
  (with-temp-buffer
    (beads-activity-mode)
    (beads-activity--render nil)
    (should (string-match-p "No activity events found" (buffer-string)))))

(ert-deftest beads-activity-test-render-events ()
  "Test that beads-activity--render displays event data."
  (with-temp-buffer
    (beads-activity-mode)
    (beads-activity--render '(((type . "create")
                               (issue_id . "bd-test")
                               (timestamp . "2025-01-01T12:00:00Z")
                               (message . "Test event"))))
    (let ((content (buffer-string)))
      (should (string-match-p "bd-test" content))
      (should (string-match-p "Test event" content)))))

(ert-deftest beads-activity-test-render-sets-text-properties ()
  "Test that beads-activity--render sets text properties for navigation."
  (with-temp-buffer
    (beads-activity-mode)
    (beads-activity--render '(((type . "create")
                               (issue_id . "bd-test")
                               (timestamp . "2025-01-01T12:00:00Z"))))
    (goto-char (point-min))
    (search-forward "bd-test")
    (should (equal (get-text-property (point) 'beads-activity-id) "bd-test"))))

(ert-deftest beads-activity-test-render-follow-indicator ()
  "Test that follow mode shows [LIVE] indicator."
  (with-temp-buffer
    (beads-activity-mode)
    (setq beads-activity--follow-mode t)
    (beads-activity--render nil)
    (should (string-match-p "\\[LIVE\\]" (buffer-string)))))

;;; Error handling tests

(ert-deftest beads-activity-test-goto-no-issue ()
  "Test that beads-activity-goto-issue errors when no issue at point."
  (with-temp-buffer
    (beads-activity-mode)
    (beads-activity--render nil)
    (should-error (beads-activity-goto-issue) :type 'user-error)))

(ert-deftest beads-activity-test-refresh-wrong-mode ()
  "Test that beads-activity-refresh errors outside activity mode."
  (with-temp-buffer
    (should-error (beads-activity-refresh) :type 'user-error)))

(ert-deftest beads-activity-test-toggle-follow-wrong-mode ()
  "Test that beads-activity-toggle-follow errors outside activity mode."
  (with-temp-buffer
    (should-error (beads-activity-toggle-follow) :type 'user-error)))

;;; Customization tests

(ert-deftest beads-activity-test-default-limit ()
  "Test that beads-activity-limit has sensible default."
  (should (integerp beads-activity-limit))
  (should (> beads-activity-limit 0)))

(ert-deftest beads-activity-test-default-poll-interval ()
  "Test that beads-activity-poll-interval has sensible default."
  (should (integerp beads-activity-poll-interval))
  (should (> beads-activity-poll-interval 0)))

(ert-deftest beads-activity-test-glyphs-defined ()
  "Test that default glyphs are defined."
  (should (listp beads-activity-glyphs))
  (should (alist-get 'create beads-activity-glyphs))
  (should (alist-get 'delete beads-activity-glyphs))
  (should (alist-get 'comment beads-activity-glyphs)))

(ert-deftest beads-activity-test-ascii-glyphs-defined ()
  "Test that ASCII glyphs alternative is defined."
  (should (listp beads-activity-glyphs-ascii))
  (should (alist-get 'create beads-activity-glyphs-ascii)))

;;; Timestamp formatting tests

(ert-deftest beads-activity-test-format-timestamp-valid ()
  "Test timestamp formatting with valid input."
  (let ((result (beads-activity--format-timestamp "2025-01-01T12:30:00Z")))
    (should (stringp result))
    (should (= (length result) 5))))

(ert-deftest beads-activity-test-format-timestamp-nil ()
  "Test timestamp formatting handles nil."
  (should (equal (beads-activity--format-timestamp nil) "??:??")))

(provide 'beads-activity-test)
;;; beads-activity-test.el ends here
