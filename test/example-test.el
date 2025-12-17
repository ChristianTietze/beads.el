;;; example-test.el --- Example ERT tests for beads.el -*- lexical-binding: t; -*-

;;; Commentary:
;; This is an example test file showing how to use ERT (Emacs Lisp Regression Testing).
;; Real tests should be added as development progresses.

;;; Code:

(require 'ert)

(ert-deftest beads-test-example ()
  "Example test to verify ERT setup works."
  (should (equal (+ 1 1) 2))
  (should (stringp "hello"))
  (should-not (null t)))

(ert-deftest beads-test-list-operations ()
  "Test basic list operations."
  (let ((test-list '(1 2 3)))
    (should (= (length test-list) 3))
    (should (equal (car test-list) 1))
    (should (equal (cdr test-list) '(2 3)))))

(provide 'example-test)
;;; example-test.el ends here
