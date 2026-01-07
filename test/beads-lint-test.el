;;; beads-lint-test.el --- Tests for beads-lint.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads issue lint report.
;;
;; Test categories:
;; 1. Mode definition tests - test beads-lint-mode is defined correctly
;; 2. Keybinding tests - test RET, g, f, n, p, q bindings
;; 3. Command availability tests - test commands are defined
;; 4. Render tests - test display formatting
;; 5. Grouping tests - test type grouping logic

;;; Code:

(require 'ert)
(require 'beads-lint)

;;; Mode definition tests

(ert-deftest beads-lint-test-mode-defined ()
  "Test that beads-lint-mode is defined."
  (should (fboundp 'beads-lint-mode)))

(ert-deftest beads-lint-test-mode-is-special ()
  "Test that beads-lint-mode is derived from special-mode."
  (with-temp-buffer
    (beads-lint-mode)
    (should (derived-mode-p 'special-mode))
    (should (derived-mode-p 'beads-lint-mode))))

(ert-deftest beads-lint-test-mode-readonly ()
  "Test that beads-lint-mode sets buffer to read-only."
  (with-temp-buffer
    (beads-lint-mode)
    (should buffer-read-only)))

;;; Keybinding tests

(ert-deftest beads-lint-test-keybinding-return ()
  "Test that RET is bound to beads-lint-goto-issue."
  (with-temp-buffer
    (beads-lint-mode)
    (should (eq (lookup-key beads-lint-mode-map (kbd "RET"))
                #'beads-lint-goto-issue))))

(ert-deftest beads-lint-test-keybinding-refresh ()
  "Test that g is bound to beads-lint-refresh."
  (with-temp-buffer
    (beads-lint-mode)
    (should (eq (lookup-key beads-lint-mode-map (kbd "g"))
                #'beads-lint-refresh))))

(ert-deftest beads-lint-test-keybinding-filter ()
  "Test that f is bound to beads-lint-filter-type."
  (with-temp-buffer
    (beads-lint-mode)
    (should (eq (lookup-key beads-lint-mode-map (kbd "f"))
                #'beads-lint-filter-type))))

(ert-deftest beads-lint-test-keybinding-next ()
  "Test that n is bound to beads-lint-next-issue."
  (with-temp-buffer
    (beads-lint-mode)
    (should (eq (lookup-key beads-lint-mode-map (kbd "n"))
                #'beads-lint-next-issue))))

(ert-deftest beads-lint-test-keybinding-prev ()
  "Test that p is bound to beads-lint-prev-issue."
  (with-temp-buffer
    (beads-lint-mode)
    (should (eq (lookup-key beads-lint-mode-map (kbd "p"))
                #'beads-lint-prev-issue))))

(ert-deftest beads-lint-test-keybinding-quit ()
  "Test that q is bound to quit-window."
  (with-temp-buffer
    (beads-lint-mode)
    (should (eq (lookup-key beads-lint-mode-map (kbd "q"))
                #'quit-window))))

;;; Command availability tests

(ert-deftest beads-lint-test-command-defined ()
  "Test that beads-lint is defined as a command."
  (should (fboundp 'beads-lint))
  (should (commandp 'beads-lint)))

(ert-deftest beads-lint-test-goto-issue-defined ()
  "Test that beads-lint-goto-issue is defined as a command."
  (should (fboundp 'beads-lint-goto-issue))
  (should (commandp 'beads-lint-goto-issue)))

(ert-deftest beads-lint-test-refresh-defined ()
  "Test that beads-lint-refresh is defined as a command."
  (should (fboundp 'beads-lint-refresh))
  (should (commandp 'beads-lint-refresh)))

(ert-deftest beads-lint-test-filter-type-defined ()
  "Test that beads-lint-filter-type is defined as a command."
  (should (fboundp 'beads-lint-filter-type))
  (should (commandp 'beads-lint-filter-type)))

(ert-deftest beads-lint-test-next-issue-defined ()
  "Test that beads-lint-next-issue is defined as a command."
  (should (fboundp 'beads-lint-next-issue))
  (should (commandp 'beads-lint-next-issue)))

(ert-deftest beads-lint-test-prev-issue-defined ()
  "Test that beads-lint-prev-issue is defined as a command."
  (should (fboundp 'beads-lint-prev-issue))
  (should (commandp 'beads-lint-prev-issue)))

;;; Render tests

(ert-deftest beads-lint-test-render-empty ()
  "Test that beads-lint--render handles empty data."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render '((total . 0) (results . [])))
    (should (string-match-p "All issues pass lint checks" (buffer-string)))))

(ert-deftest beads-lint-test-render-nil-results ()
  "Test that beads-lint--render handles nil results."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render '((total . 0) (results . nil)))
    (should (string-match-p "All issues pass lint checks" (buffer-string)))))

(ert-deftest beads-lint-test-render-issues ()
  "Test that beads-lint--render displays issue data."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render
     '((total . 1)
       (results . [((id . "bd-001")
                    (title . "Test bug")
                    (type . "bug")
                    (missing . ["## Acceptance Criteria"]))])))
    (let ((content (buffer-string)))
      (should (string-match-p "bd-001" content))
      (should (string-match-p "Test bug" content))
      (should (string-match-p "Bug" content))
      (should (string-match-p "Acceptance Criteria" content)))))

(ert-deftest beads-lint-test-render-multiple-missing ()
  "Test that beads-lint--render shows multiple missing sections."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render
     '((total . 1)
       (results . [((id . "bd-001")
                    (title . "Test bug")
                    (type . "bug")
                    (missing . ["## Steps to Reproduce" "## Acceptance Criteria"]))])))
    (let ((content (buffer-string)))
      (should (string-match-p "Steps to Reproduce" content))
      (should (string-match-p "Acceptance Criteria" content)))))

(ert-deftest beads-lint-test-render-sets-text-properties ()
  "Test that beads-lint--render sets text properties."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render
     '((total . 1)
       (results . [((id . "bd-001")
                    (title . "Test")
                    (type . "task")
                    (missing . ["## Acceptance Criteria"]))])))
    (goto-char (point-min))
    (search-forward "bd-001")
    (should (equal (get-text-property (point) 'beads-lint-id) "bd-001"))))

(ert-deftest beads-lint-test-render-filter-indicator ()
  "Test that type filter is shown in header."
  (with-temp-buffer
    (beads-lint-mode)
    (setq beads-lint--type-filter "bug")
    (beads-lint--render
     '((total . 1)
       (results . [((id . "bd-001")
                    (title . "Test")
                    (type . "bug")
                    (missing . ["## Acceptance Criteria"]))])))
    (should (string-match-p "filtered: bug" (buffer-string)))))

;;; Grouping tests

(ert-deftest beads-lint-test-group-by-type-empty ()
  "Test grouping with empty results."
  (should (null (beads-lint--group-by-type nil))))

(ert-deftest beads-lint-test-group-by-type-single ()
  "Test grouping with single issue."
  (let ((results '(((id . "bd-001") (type . "bug")))))
    (let ((grouped (beads-lint--group-by-type results)))
      (should (= (length grouped) 1))
      (should (equal (caar grouped) "bug"))
      (should (= (length (cdar grouped)) 1)))))

(ert-deftest beads-lint-test-group-by-type-multiple ()
  "Test grouping with multiple types."
  (let ((results '(((id . "bd-001") (type . "bug"))
                   ((id . "bd-002") (type . "task"))
                   ((id . "bd-003") (type . "bug")))))
    (let ((grouped (beads-lint--group-by-type results)))
      (should (= (length grouped) 2))
      (should (= (length (cdr (assoc "bug" grouped))) 2))
      (should (= (length (cdr (assoc "task" grouped))) 1)))))

(ert-deftest beads-lint-test-group-by-type-sorted ()
  "Test that groups are sorted alphabetically."
  (let ((results '(((id . "bd-001") (type . "task"))
                   ((id . "bd-002") (type . "bug"))
                   ((id . "bd-003") (type . "feature")))))
    (let ((grouped (beads-lint--group-by-type results)))
      (should (equal (mapcar #'car grouped) '("bug" "feature" "task"))))))

;;; Error handling tests

(ert-deftest beads-lint-test-goto-no-issue ()
  "Test that beads-lint-goto-issue errors when no issue at point."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render '((total . 0) (results . [])))
    (should-error (beads-lint-goto-issue) :type 'user-error)))

(ert-deftest beads-lint-test-refresh-wrong-mode ()
  "Test that beads-lint-refresh errors outside lint mode."
  (with-temp-buffer
    (should-error (beads-lint-refresh) :type 'user-error)))

(ert-deftest beads-lint-test-filter-wrong-mode ()
  "Test that beads-lint-filter-type errors outside lint mode."
  (with-temp-buffer
    (should-error (beads-lint-filter-type) :type 'user-error)))

;;; Face tests

(ert-deftest beads-lint-test-faces-defined ()
  "Test that custom faces are defined."
  (should (facep 'beads-lint-issue-id))
  (should (facep 'beads-lint-type-header))
  (should (facep 'beads-lint-missing))
  (should (facep 'beads-lint-warning-count)))

;;; Navigation tests

(ert-deftest beads-lint-test-next-issue-at-end ()
  "Test that beads-lint-next-issue errors at end of buffer."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render '((total . 0) (results . [])))
    (goto-char (point-max))
    (should-error (beads-lint-next-issue) :type 'user-error)))

(ert-deftest beads-lint-test-prev-issue-at-start ()
  "Test that beads-lint-prev-issue errors at start of buffer."
  (with-temp-buffer
    (beads-lint-mode)
    (beads-lint--render '((total . 0) (results . [])))
    (goto-char (point-min))
    (should-error (beads-lint-prev-issue) :type 'user-error)))

(provide 'beads-lint-test)
;;; beads-lint-test.el ends here
