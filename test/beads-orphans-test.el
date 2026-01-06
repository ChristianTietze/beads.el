;;; beads-orphans-test.el --- Tests for beads-orphans.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads orphans view.
;;
;; Test categories:
;; 1. Mode definition tests - test beads-orphans-mode is defined correctly
;; 2. Keybinding tests - test RET, c, g, q bindings
;; 3. Command availability tests - test commands are defined

;;; Code:

(require 'ert)
(require 'beads-orphans)

;;; Mode definition tests

(ert-deftest beads-orphans-test-mode-defined ()
  "Test that beads-orphans-mode is defined."
  (should (fboundp 'beads-orphans-mode)))

(ert-deftest beads-orphans-test-mode-is-special ()
  "Test that beads-orphans-mode is derived from special-mode."
  (with-temp-buffer
    (beads-orphans-mode)
    (should (derived-mode-p 'special-mode))
    (should (derived-mode-p 'beads-orphans-mode))))

(ert-deftest beads-orphans-test-mode-readonly ()
  "Test that beads-orphans-mode sets buffer to read-only."
  (with-temp-buffer
    (beads-orphans-mode)
    (should buffer-read-only)))

;;; Keybinding tests

(ert-deftest beads-orphans-test-keybinding-return ()
  "Test that RET is bound to beads-orphans-goto-issue."
  (with-temp-buffer
    (beads-orphans-mode)
    (should (eq (lookup-key beads-orphans-mode-map (kbd "RET"))
                #'beads-orphans-goto-issue))))

(ert-deftest beads-orphans-test-keybinding-close ()
  "Test that c is bound to beads-orphans-close."
  (with-temp-buffer
    (beads-orphans-mode)
    (should (eq (lookup-key beads-orphans-mode-map (kbd "c"))
                #'beads-orphans-close))))

(ert-deftest beads-orphans-test-keybinding-refresh ()
  "Test that g is bound to beads-orphans-refresh."
  (with-temp-buffer
    (beads-orphans-mode)
    (should (eq (lookup-key beads-orphans-mode-map (kbd "g"))
                #'beads-orphans-refresh))))

(ert-deftest beads-orphans-test-keybinding-quit ()
  "Test that q is bound to quit-window."
  (with-temp-buffer
    (beads-orphans-mode)
    (should (eq (lookup-key beads-orphans-mode-map (kbd "q"))
                #'quit-window))))

;;; Command availability tests

(ert-deftest beads-orphans-test-command-defined ()
  "Test that beads-orphans is defined as a command."
  (should (fboundp 'beads-orphans))
  (should (commandp 'beads-orphans)))

(ert-deftest beads-orphans-test-goto-issue-defined ()
  "Test that beads-orphans-goto-issue is defined as a command."
  (should (fboundp 'beads-orphans-goto-issue))
  (should (commandp 'beads-orphans-goto-issue)))

(ert-deftest beads-orphans-test-close-defined ()
  "Test that beads-orphans-close is defined as a command."
  (should (fboundp 'beads-orphans-close))
  (should (commandp 'beads-orphans-close)))

(ert-deftest beads-orphans-test-refresh-defined ()
  "Test that beads-orphans-refresh is defined as a command."
  (should (fboundp 'beads-orphans-refresh))
  (should (commandp 'beads-orphans-refresh)))

;;; Render tests

(ert-deftest beads-orphans-test-render-empty ()
  "Test that beads-orphans--render handles empty list."
  (with-temp-buffer
    (beads-orphans-mode)
    (beads-orphans--render nil)
    (should (string-match-p "No orphaned issues found" (buffer-string)))))

(ert-deftest beads-orphans-test-render-orphans ()
  "Test that beads-orphans--render displays orphan data."
  (with-temp-buffer
    (beads-orphans-mode)
    (beads-orphans--render '(((issue_id . "bd-test")
                              (title . "Test orphan")
                              (status . "open")
                              (latest_commit . "abc123")
                              (latest_commit_message . "Fix something"))))
    (let ((content (buffer-string)))
      (should (string-match-p "bd-test" content))
      (should (string-match-p "Test orphan" content))
      (should (string-match-p "abc123" content)))))

(ert-deftest beads-orphans-test-render-sets-text-properties ()
  "Test that beads-orphans--render sets text properties for navigation."
  (with-temp-buffer
    (beads-orphans-mode)
    (beads-orphans--render '(((issue_id . "bd-test")
                              (title . "Test orphan")
                              (status . "open")
                              (latest_commit . "abc123")
                              (latest_commit_message . "Fix"))))
    (goto-char (point-min))
    (search-forward "bd-test")
    (should (equal (get-text-property (point) 'beads-orphan-id) "bd-test"))))

;;; Error handling tests

(ert-deftest beads-orphans-test-goto-no-orphan ()
  "Test that beads-orphans-goto-issue errors when no orphan at point."
  (with-temp-buffer
    (beads-orphans-mode)
    (beads-orphans--render nil)
    (should-error (beads-orphans-goto-issue) :type 'user-error)))

(ert-deftest beads-orphans-test-close-no-orphan ()
  "Test that beads-orphans-close errors when no orphan at point."
  (with-temp-buffer
    (beads-orphans-mode)
    (beads-orphans--render nil)
    (should-error (beads-orphans-close) :type 'user-error)))

(ert-deftest beads-orphans-test-refresh-wrong-mode ()
  "Test that beads-orphans-refresh errors outside orphans mode."
  (with-temp-buffer
    (should-error (beads-orphans-refresh) :type 'user-error)))

(provide 'beads-orphans-test)
;;; beads-orphans-test.el ends here
