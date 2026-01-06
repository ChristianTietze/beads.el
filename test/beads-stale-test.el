;;; beads-stale-test.el --- Tests for beads-stale.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads stale issues view.
;;
;; Test categories:
;; 1. Mode definition tests - test beads-stale-mode is defined correctly
;; 2. Keybinding tests - test RET, c, g, d, f, q bindings
;; 3. Command availability tests - test commands are defined
;; 4. Days calculation tests - test beads-stale--days-ago

;;; Code:

(require 'ert)
(require 'beads-stale)

;;; Mode definition tests

(ert-deftest beads-stale-test-mode-defined ()
  "Test that beads-stale-mode is defined."
  (should (fboundp 'beads-stale-mode)))

(ert-deftest beads-stale-test-mode-is-special ()
  "Test that beads-stale-mode is derived from special-mode."
  (with-temp-buffer
    (beads-stale-mode)
    (should (derived-mode-p 'special-mode))
    (should (derived-mode-p 'beads-stale-mode))))

(ert-deftest beads-stale-test-mode-readonly ()
  "Test that beads-stale-mode sets buffer to read-only."
  (with-temp-buffer
    (beads-stale-mode)
    (should buffer-read-only)))

;;; Keybinding tests

(ert-deftest beads-stale-test-keybinding-return ()
  "Test that RET is bound to beads-stale-goto-issue."
  (with-temp-buffer
    (beads-stale-mode)
    (should (eq (lookup-key beads-stale-mode-map (kbd "RET"))
                #'beads-stale-goto-issue))))

(ert-deftest beads-stale-test-keybinding-claim ()
  "Test that c is bound to beads-stale-claim."
  (with-temp-buffer
    (beads-stale-mode)
    (should (eq (lookup-key beads-stale-mode-map (kbd "c"))
                #'beads-stale-claim))))

(ert-deftest beads-stale-test-keybinding-refresh ()
  "Test that g is bound to beads-stale-refresh."
  (with-temp-buffer
    (beads-stale-mode)
    (should (eq (lookup-key beads-stale-mode-map (kbd "g"))
                #'beads-stale-refresh))))

(ert-deftest beads-stale-test-keybinding-days ()
  "Test that d is bound to beads-stale-set-days."
  (with-temp-buffer
    (beads-stale-mode)
    (should (eq (lookup-key beads-stale-mode-map (kbd "d"))
                #'beads-stale-set-days))))

(ert-deftest beads-stale-test-keybinding-filter ()
  "Test that f is bound to beads-stale-set-filter."
  (with-temp-buffer
    (beads-stale-mode)
    (should (eq (lookup-key beads-stale-mode-map (kbd "f"))
                #'beads-stale-set-filter))))

(ert-deftest beads-stale-test-keybinding-quit ()
  "Test that q is bound to quit-window."
  (with-temp-buffer
    (beads-stale-mode)
    (should (eq (lookup-key beads-stale-mode-map (kbd "q"))
                #'quit-window))))

;;; Command availability tests

(ert-deftest beads-stale-test-command-defined ()
  "Test that beads-stale is defined as a command."
  (should (fboundp 'beads-stale))
  (should (commandp 'beads-stale)))

(ert-deftest beads-stale-test-goto-issue-defined ()
  "Test that beads-stale-goto-issue is defined as a command."
  (should (fboundp 'beads-stale-goto-issue))
  (should (commandp 'beads-stale-goto-issue)))

(ert-deftest beads-stale-test-claim-defined ()
  "Test that beads-stale-claim is defined as a command."
  (should (fboundp 'beads-stale-claim))
  (should (commandp 'beads-stale-claim)))

(ert-deftest beads-stale-test-refresh-defined ()
  "Test that beads-stale-refresh is defined as a command."
  (should (fboundp 'beads-stale-refresh))
  (should (commandp 'beads-stale-refresh)))

(ert-deftest beads-stale-test-set-days-defined ()
  "Test that beads-stale-set-days is defined as a command."
  (should (fboundp 'beads-stale-set-days))
  (should (commandp 'beads-stale-set-days)))

(ert-deftest beads-stale-test-set-filter-defined ()
  "Test that beads-stale-set-filter is defined as a command."
  (should (fboundp 'beads-stale-set-filter))
  (should (commandp 'beads-stale-set-filter)))

;;; Days calculation tests

(ert-deftest beads-stale-test-days-ago-nil ()
  "Test that beads-stale--days-ago handles nil gracefully."
  (should (= (beads-stale--days-ago nil) 0)))

(ert-deftest beads-stale-test-days-ago-empty-string ()
  "Test that beads-stale--days-ago handles empty string."
  (should (= (beads-stale--days-ago "") 0)))

(ert-deftest beads-stale-test-days-ago-recent ()
  "Test that beads-stale--days-ago calculates correctly for recent date."
  (let* ((now (current-time))
         (yesterday (time-subtract now (days-to-time 1)))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S" yesterday)))
    (should (>= (beads-stale--days-ago timestamp) 0))
    (should (<= (beads-stale--days-ago timestamp) 2))))

;;; Render tests

(ert-deftest beads-stale-test-render-empty ()
  "Test that beads-stale--render handles empty list."
  (with-temp-buffer
    (beads-stale-mode)
    (setq beads-stale--days 30)
    (beads-stale--render nil)
    (should (string-match-p "No stale issues found" (buffer-string)))))

(ert-deftest beads-stale-test-render-issues ()
  "Test that beads-stale--render displays issue data."
  (with-temp-buffer
    (beads-stale-mode)
    (setq beads-stale--days 30)
    (beads-stale--render '(((id . "bd-test")
                            (title . "Test stale issue")
                            (status . "open")
                            (updated_at . "2025-01-01T00:00:00Z"))))
    (let ((content (buffer-string)))
      (should (string-match-p "bd-test" content))
      (should (string-match-p "Test stale issue" content))
      (should (string-match-p "open" content)))))

(ert-deftest beads-stale-test-render-sets-text-properties ()
  "Test that beads-stale--render sets text properties for navigation."
  (with-temp-buffer
    (beads-stale-mode)
    (setq beads-stale--days 30)
    (beads-stale--render '(((id . "bd-test")
                            (title . "Test issue")
                            (status . "open")
                            (updated_at . "2025-01-01T00:00:00Z"))))
    (goto-char (point-min))
    (search-forward "bd-test")
    (should (equal (get-text-property (point) 'beads-stale-id) "bd-test"))))

;;; Error handling tests

(ert-deftest beads-stale-test-goto-no-issue ()
  "Test that beads-stale-goto-issue errors when no issue at point."
  (with-temp-buffer
    (beads-stale-mode)
    (beads-stale--render nil)
    (should-error (beads-stale-goto-issue) :type 'user-error)))

(ert-deftest beads-stale-test-claim-no-issue ()
  "Test that beads-stale-claim errors when no issue at point."
  (with-temp-buffer
    (beads-stale-mode)
    (beads-stale--render nil)
    (should-error (beads-stale-claim) :type 'user-error)))

(ert-deftest beads-stale-test-refresh-wrong-mode ()
  "Test that beads-stale-refresh errors outside stale mode."
  (with-temp-buffer
    (should-error (beads-stale-refresh) :type 'user-error)))

(ert-deftest beads-stale-test-set-days-wrong-mode ()
  "Test that beads-stale-set-days errors outside stale mode."
  (with-temp-buffer
    (should-error (beads-stale-set-days 14) :type 'user-error)))

(ert-deftest beads-stale-test-set-filter-wrong-mode ()
  "Test that beads-stale-set-filter errors outside stale mode."
  (with-temp-buffer
    (should-error (beads-stale-set-filter "open") :type 'user-error)))

;;; Customization tests

(ert-deftest beads-stale-test-default-days ()
  "Test that beads-stale-days has sensible default."
  (should (integerp beads-stale-days))
  (should (> beads-stale-days 0)))

(provide 'beads-stale-test)
;;; beads-stale-test.el ends here
