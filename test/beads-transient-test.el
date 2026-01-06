;;; beads-transient-test.el --- Tests for beads-transient.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads transient menu system.
;;
;; Test categories:
;; 1. Transient definition tests - test beads-menu is a transient prefix (no daemon)
;; 2. Keybinding tests - test ? and C-c m in beads-list-mode and beads-detail-mode (no daemon)
;; 3. Placeholder command tests - test placeholder commands exist (no daemon)
;;
;; Note on test isolation:
;; All tests in this file are unit tests that do not require the daemon.
;; They test menu definitions, keybindings, and command availability only.

;;; Code:

(require 'ert)
(require 'beads-transient)
(require 'beads-list)
(require 'beads-detail)

;;; Transient definition tests (no daemon needed)

(ert-deftest beads-transient-test-menu-defined ()
  "Test that beads-menu is defined as a command."
  (should (commandp 'beads-menu)))

(ert-deftest beads-transient-test-menu-is-transient-prefix ()
  "Test that beads-menu is a transient prefix command."
  (should (get 'beads-menu 'transient--prefix)))

(ert-deftest beads-transient-test-menu-has-prefix-object ()
  "Test that beads-menu has a transient prefix object."
  (let ((prefix-obj (get 'beads-menu 'transient--prefix)))
    (should prefix-obj)))

;;; Keybinding tests (no daemon)

(ert-deftest beads-transient-test-list-mode-help-key ()
  "Test that ? is bound to beads-menu in beads-list-mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "?"))
                #'beads-menu))))

(ert-deftest beads-transient-test-list-mode-menu-key ()
  "Test that C-c m is bound to beads-menu in beads-list-mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "C-c m"))
                #'beads-menu))))

(ert-deftest beads-transient-test-detail-mode-help-key ()
  "Test that ? is bound to beads-menu in beads-detail-mode."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "?"))
                #'beads-menu))))

(ert-deftest beads-transient-test-detail-mode-menu-key ()
  "Test that C-c m is bound to beads-menu in beads-detail-mode."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "C-c m"))
                #'beads-menu))))

(ert-deftest beads-transient-test-list-mode-keybindings-interactive ()
  "Test that beads-menu can be called interactively from beads-list-mode."
  (with-temp-buffer
    (beads-list-mode)
    (let ((cmd (lookup-key beads-list-mode-map (kbd "?"))))
      (should (commandp cmd))
      (should (eq cmd #'beads-menu)))))

(ert-deftest beads-transient-test-detail-mode-keybindings-interactive ()
  "Test that beads-menu can be called interactively from beads-detail-mode."
  (with-temp-buffer
    (beads-detail-mode)
    (let ((cmd (lookup-key beads-detail-mode-map (kbd "?"))))
      (should (commandp cmd))
      (should (eq cmd #'beads-menu)))))

;;; Placeholder command tests (no daemon)

(ert-deftest beads-transient-test-create-issue-defined ()
  "Test that beads-create-issue is defined."
  (should (fboundp 'beads-create-issue)))

(ert-deftest beads-transient-test-create-issue-interactive ()
  "Test that beads-create-issue is an interactive command."
  (should (commandp 'beads-create-issue)))

(ert-deftest beads-transient-test-create-issue-requires-title ()
  "Test that beads-create-issue requires a title."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
            ((symbol-function 'completing-read) (lambda (&rest _) "task")))
    (let ((message-log-max t))
      (beads-create-issue)
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (forward-line -1)
        (should (string-match-p "Title is required"
                               (buffer-substring (line-beginning-position)
                                                (line-end-position))))))))

(ert-deftest beads-transient-test-close-issue-defined ()
  "Test that beads-close-issue is defined."
  (should (fboundp 'beads-close-issue)))

(ert-deftest beads-transient-test-close-issue-interactive ()
  "Test that beads-close-issue is an interactive command."
  (should (commandp 'beads-close-issue)))

(ert-deftest beads-transient-test-close-issue-requires-context ()
  "Test that beads-close-issue requires an issue context."
  (with-temp-buffer
    (let ((message-log-max t))
      (beads-close-issue)
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (forward-line -1)
        (should (string-match-p "No issue at point"
                               (buffer-substring (line-beginning-position)
                                                (line-end-position))))))))

(ert-deftest beads-transient-test-filter-status-defined ()
  "Test that beads-filter-status is defined."
  (should (fboundp 'beads-filter-status)))

(ert-deftest beads-transient-test-filter-status-interactive ()
  "Test that beads-filter-status is an interactive command."
  (should (commandp 'beads-filter-status)))

(ert-deftest beads-transient-test-filter-status-requires-list-mode ()
  "Test that beads-filter-status requires beads-list-mode."
  (with-temp-buffer
    (should-error (beads-filter-status) :type 'user-error)))

(ert-deftest beads-transient-test-filter-priority-defined ()
  "Test that beads-filter-priority is defined."
  (should (fboundp 'beads-filter-priority)))

(ert-deftest beads-transient-test-filter-priority-interactive ()
  "Test that beads-filter-priority is an interactive command."
  (should (commandp 'beads-filter-priority)))

(ert-deftest beads-transient-test-filter-priority-requires-list-mode ()
  "Test that beads-filter-priority requires beads-list-mode."
  (with-temp-buffer
    (should-error (beads-filter-priority) :type 'user-error)))

;;; Integration tests for menu structure

(ert-deftest beads-transient-test-menu-contains-list-command ()
  "Test that beads-menu includes beads-list command."
  (should (commandp 'beads-list)))

(ert-deftest beads-transient-test-menu-contains-refresh-command ()
  "Test that beads-menu includes beads-list-refresh command."
  (should (commandp 'beads-list-refresh)))

(ert-deftest beads-transient-test-menu-contains-edit-command ()
  "Test that beads-detail-edit-description command exists."
  (should (commandp 'beads-detail-edit-description)))

(ert-deftest beads-transient-test-menu-contains-describe-mode ()
  "Test that beads-menu includes describe-mode command."
  (should (commandp 'describe-mode)))

(ert-deftest beads-transient-test-menu-contains-quit-command ()
  "Test that beads-menu includes transient-quit-one command."
  (should (commandp 'transient-quit-one)))

;;; Command availability tests

(ert-deftest beads-transient-test-all-placeholder-commands-available ()
  "Test that all placeholder commands are available and interactive."
  (should (commandp 'beads-create-issue))
  (should (commandp 'beads-close-issue))
  (should (commandp 'beads-filter-status))
  (should (commandp 'beads-filter-priority)))

(ert-deftest beads-transient-test-orphans-defined ()
  "Test that beads-orphans is defined."
  (should (fboundp 'beads-orphans)))

(ert-deftest beads-transient-test-orphans-interactive ()
  "Test that beads-orphans is an interactive command."
  (should (commandp 'beads-orphans)))

(ert-deftest beads-transient-test-keybindings-dont-conflict ()
  "Test that ? keybinding doesn't conflict with other bindings in list mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "?")) #'beads-menu))
    (should (eq (lookup-key beads-list-mode-map (kbd "g")) #'beads-list-refresh))
    (should (eq (lookup-key beads-list-mode-map (kbd "RET")) #'beads-list-goto-issue))
    (should (eq (lookup-key beads-list-mode-map (kbd "q")) #'quit-window))))

(ert-deftest beads-transient-test-detail-keybindings-dont-conflict ()
  "Test that ? keybinding doesn't conflict with other bindings in detail mode."
  (with-temp-buffer
    (beads-detail-mode)
    (should (eq (lookup-key beads-detail-mode-map (kbd "?")) #'beads-menu))
    (should (eq (lookup-key beads-detail-mode-map (kbd "g")) #'beads-detail-refresh))
    (should (keymapp (lookup-key beads-detail-mode-map (kbd "e"))))
    (should (eq (lookup-key beads-detail-mode-map (kbd "q")) #'quit-window))))

(provide 'beads-transient-test)
;;; beads-transient-test.el ends here
