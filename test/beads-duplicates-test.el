;;; beads-duplicates-test.el --- Tests for beads-duplicates.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads duplicate detection view.
;;
;; Test categories:
;; 1. Mode definition tests - test beads-duplicates-mode is defined correctly
;; 2. Keybinding tests - test RET, m, M, g, q bindings
;; 3. Command availability tests - test commands are defined
;; 4. Render tests - test display formatting

;;; Code:

(require 'ert)
(require 'beads-duplicates)

;;; Mode definition tests

(ert-deftest beads-duplicates-test-mode-defined ()
  "Test that beads-duplicates-mode is defined."
  (should (fboundp 'beads-duplicates-mode)))

(ert-deftest beads-duplicates-test-mode-is-special ()
  "Test that beads-duplicates-mode is derived from special-mode."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should (derived-mode-p 'special-mode))
    (should (derived-mode-p 'beads-duplicates-mode))))

(ert-deftest beads-duplicates-test-mode-readonly ()
  "Test that beads-duplicates-mode sets buffer to read-only."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should buffer-read-only)))

;;; Keybinding tests

(ert-deftest beads-duplicates-test-keybinding-return ()
  "Test that RET is bound to beads-duplicates-goto-issue."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should (eq (lookup-key beads-duplicates-mode-map (kbd "RET"))
                #'beads-duplicates-goto-issue))))

(ert-deftest beads-duplicates-test-keybinding-merge ()
  "Test that m is bound to beads-duplicates-merge-at-point."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should (eq (lookup-key beads-duplicates-mode-map (kbd "m"))
                #'beads-duplicates-merge-at-point))))

(ert-deftest beads-duplicates-test-keybinding-merge-group ()
  "Test that M is bound to beads-duplicates-merge-group."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should (eq (lookup-key beads-duplicates-mode-map (kbd "M"))
                #'beads-duplicates-merge-group))))

(ert-deftest beads-duplicates-test-keybinding-refresh ()
  "Test that g is bound to beads-duplicates-refresh."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should (eq (lookup-key beads-duplicates-mode-map (kbd "g"))
                #'beads-duplicates-refresh))))

(ert-deftest beads-duplicates-test-keybinding-quit ()
  "Test that q is bound to quit-window."
  (with-temp-buffer
    (beads-duplicates-mode)
    (should (eq (lookup-key beads-duplicates-mode-map (kbd "q"))
                #'quit-window))))

;;; Command availability tests

(ert-deftest beads-duplicates-test-command-defined ()
  "Test that beads-duplicates is defined as a command."
  (should (fboundp 'beads-duplicates))
  (should (commandp 'beads-duplicates)))

(ert-deftest beads-duplicates-test-goto-issue-defined ()
  "Test that beads-duplicates-goto-issue is defined as a command."
  (should (fboundp 'beads-duplicates-goto-issue))
  (should (commandp 'beads-duplicates-goto-issue)))

(ert-deftest beads-duplicates-test-merge-at-point-defined ()
  "Test that beads-duplicates-merge-at-point is defined as a command."
  (should (fboundp 'beads-duplicates-merge-at-point))
  (should (commandp 'beads-duplicates-merge-at-point)))

(ert-deftest beads-duplicates-test-merge-group-defined ()
  "Test that beads-duplicates-merge-group is defined as a command."
  (should (fboundp 'beads-duplicates-merge-group))
  (should (commandp 'beads-duplicates-merge-group)))

(ert-deftest beads-duplicates-test-refresh-defined ()
  "Test that beads-duplicates-refresh is defined as a command."
  (should (fboundp 'beads-duplicates-refresh))
  (should (commandp 'beads-duplicates-refresh)))

;;; Render tests

(ert-deftest beads-duplicates-test-render-empty ()
  "Test that beads-duplicates--render handles empty data."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render '((duplicate_groups . 0) (groups . [])))
    (should (string-match-p "No duplicate issues found" (buffer-string)))))

(ert-deftest beads-duplicates-test-render-nil-groups ()
  "Test that beads-duplicates--render handles nil groups."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render '((duplicate_groups . 0) (groups . nil)))
    (should (string-match-p "No duplicate issues found" (buffer-string)))))

(ert-deftest beads-duplicates-test-render-groups ()
  "Test that beads-duplicates--render displays group data."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render
     '((duplicate_groups . 1)
       (groups . [((title . "Test duplicate")
                   (suggested_target . "bd-001")
                   (suggested_sources . ["bd-002"])
                   (issues . [((id . "bd-001")
                               (status . "open")
                               (references . 2)
                               (is_merge_target . t))
                              ((id . "bd-002")
                               (status . "open")
                               (references . 0)
                               (is_merge_target . :json-false))]))])))
    (let ((content (buffer-string)))
      (should (string-match-p "Group 1" content))
      (should (string-match-p "Test duplicate" content))
      (should (string-match-p "bd-001" content))
      (should (string-match-p "bd-002" content)))))

(ert-deftest beads-duplicates-test-render-sets-text-properties ()
  "Test that beads-duplicates--render sets text properties."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render
     '((duplicate_groups . 1)
       (groups . [((title . "Test")
                   (suggested_target . "bd-001")
                   (suggested_sources . ["bd-002"])
                   (issues . [((id . "bd-001") (status . "open") (references . 0))
                              ((id . "bd-002") (status . "open") (references . 0))]))])))
    (goto-char (point-min))
    (search-forward "bd-001")
    (should (equal (get-text-property (point) 'beads-duplicate-id) "bd-001"))))

;;; Error handling tests

(ert-deftest beads-duplicates-test-goto-no-issue ()
  "Test that beads-duplicates-goto-issue errors when no issue at point."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render '((duplicate_groups . 0) (groups . [])))
    (should-error (beads-duplicates-goto-issue) :type 'user-error)))

(ert-deftest beads-duplicates-test-merge-no-issue ()
  "Test that beads-duplicates-merge-at-point errors when no issue at point."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render '((duplicate_groups . 0) (groups . [])))
    (should-error (beads-duplicates-merge-at-point) :type 'user-error)))

(ert-deftest beads-duplicates-test-merge-group-no-group ()
  "Test that beads-duplicates-merge-group errors when no group at point."
  (with-temp-buffer
    (beads-duplicates-mode)
    (beads-duplicates--render '((duplicate_groups . 0) (groups . [])))
    (should-error (beads-duplicates-merge-group) :type 'user-error)))

(ert-deftest beads-duplicates-test-refresh-wrong-mode ()
  "Test that beads-duplicates-refresh errors outside duplicates mode."
  (with-temp-buffer
    (should-error (beads-duplicates-refresh) :type 'user-error)))

;;; Face tests

(ert-deftest beads-duplicates-test-faces-defined ()
  "Test that custom faces are defined."
  (should (facep 'beads-duplicates-target))
  (should (facep 'beads-duplicates-source))
  (should (facep 'beads-duplicates-group-header)))

(provide 'beads-duplicates-test)
;;; beads-duplicates-test.el ends here
