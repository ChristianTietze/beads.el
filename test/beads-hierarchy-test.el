;;; beads-hierarchy-test.el --- Tests for beads-hierarchy -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'hierarchy)
(require 'beads-hierarchy)

(defvar beads-hierarchy-test--mock-issues
  (let ((h (make-hash-table :test 'equal)))
    (puthash "ROOT-1"
             '((id . "ROOT-1")
               (title . "Root issue")
               (status . "open")
               (priority . 1)
               (issue_type . "epic")
               (dependents . [((id . "CHILD-1")
                               (title . "Child one")
                               (status . "open")
                               (dependency_type . "parent-child"))
                              ((id . "CHILD-2")
                               (title . "Child two")
                               (status . "closed")
                               (dependency_type . "parent-child"))]))
             h)
    (puthash "CHILD-1"
             '((id . "CHILD-1")
               (title . "Child one")
               (status . "open")
               (priority . 2)
               (issue_type . "task")
               (dependents . [((id . "GRANDCHILD-1")
                               (title . "Grandchild")
                               (status . "in_progress")
                               (dependency_type . "parent-child"))]))
             h)
    (puthash "CHILD-2"
             '((id . "CHILD-2")
               (title . "Child two")
               (status . "closed")
               (priority . 2)
               (issue_type . "task")
               (dependents . nil))
             h)
    (puthash "GRANDCHILD-1"
             '((id . "GRANDCHILD-1")
               (title . "Grandchild")
               (status . "in_progress")
               (priority . 3)
               (issue_type . "task")
               (dependents . nil))
             h)
    (puthash "LONE-1"
             '((id . "LONE-1")
               (title . "Lone issue")
               (status . "open")
               (priority . 2)
               (issue_type . "task")
               (dependents . nil))
             h)
    h)
  "Mock issue data for testing.")

(defun beads-hierarchy-test--mock-show (id)
  "Mock beads-rpc-show for testing."
  (or (gethash id beads-hierarchy-test--mock-issues)
      (signal 'beads-rpc-error (list (format "Issue not found: %s" id)))))

(ert-deftest beads-hierarchy-test-find-parent-nil-for-root ()
  "Test that root issues have no parent."
  (let ((by-id (make-hash-table :test 'equal))
        (root '((id . "ROOT") (title . "Root"))))
    (puthash "ROOT" root by-id)
    (should (null (beads-hierarchy--find-parent root by-id)))))

(ert-deftest beads-hierarchy-test-find-parent-returns-parent ()
  "Test that child issues return their parent."
  (let ((by-id (make-hash-table :test 'equal))
        (root '((id . "ROOT") (title . "Root")))
        (child '((beads--parent-id . "ROOT") (id . "CHILD") (title . "Child"))))
    (puthash "ROOT" root by-id)
    (puthash "CHILD" child by-id)
    (should (equal (beads-hierarchy--find-parent child by-id) root))))

(ert-deftest beads-hierarchy-test-collect-dependents-empty ()
  "Test collecting dependents from issue with no dependents."
  (let ((by-id (make-hash-table :test 'equal))
        (issue '((id . "LONE") (title . "Lone") (dependents . nil))))
    (beads-hierarchy--collect-dependents issue by-id)
    (should (= (hash-table-count by-id) 0))))

(ert-deftest beads-hierarchy-test-collect-dependents-adds-children ()
  "Test that collect-dependents adds children to hash table."
  (cl-letf (((symbol-function 'beads-rpc-show) #'beads-hierarchy-test--mock-show))
    (let ((by-id (make-hash-table :test 'equal))
          (root (gethash "ROOT-1" beads-hierarchy-test--mock-issues)))
      (beads-hierarchy--collect-dependents root by-id)
      (should (gethash "CHILD-1" by-id))
      (should (gethash "CHILD-2" by-id)))))

(ert-deftest beads-hierarchy-test-collect-dependents-recursive ()
  "Test that collect-dependents recursively collects grandchildren."
  (cl-letf (((symbol-function 'beads-rpc-show) #'beads-hierarchy-test--mock-show))
    (let ((by-id (make-hash-table :test 'equal))
          (root (gethash "ROOT-1" beads-hierarchy-test--mock-issues)))
      (beads-hierarchy--collect-dependents root by-id)
      (should (gethash "GRANDCHILD-1" by-id)))))

(ert-deftest beads-hierarchy-test-build-creates-hierarchy ()
  "Test that build creates a valid hierarchy."
  (cl-letf (((symbol-function 'beads-rpc-show) #'beads-hierarchy-test--mock-show))
    (let ((result (beads-hierarchy--build "ROOT-1")))
      (should result)
      (should (consp result))
      (let ((h (car result))
            (by-id (cdr result)))
        (should (>= (hierarchy-length h) 1))
        (should (hash-table-p by-id))))))

(ert-deftest beads-hierarchy-test-build-with-no-dependents ()
  "Test building hierarchy for issue with no dependents."
  (cl-letf (((symbol-function 'beads-rpc-show) #'beads-hierarchy-test--mock-show))
    (let ((result (beads-hierarchy--build "LONE-1")))
      (should result)
      (let ((h (car result)))
        (should (= (hierarchy-length h) 1))))))

(ert-deftest beads-hierarchy-test-build-has-correct-structure ()
  "Test that built hierarchy has correct parent-child structure."
  (cl-letf (((symbol-function 'beads-rpc-show) #'beads-hierarchy-test--mock-show))
    (let* ((result (beads-hierarchy--build "ROOT-1"))
           (h (car result)))
      (let ((roots (hierarchy-roots h)))
        (should (= (length roots) 1))
        (should (string= (alist-get 'id (car roots)) "ROOT-1"))))))

(ert-deftest beads-hierarchy-test-status-face-open ()
  "Test status face for open issues."
  (should (eq (beads-hierarchy--status-face "open") 'beads-list-status-open)))

(ert-deftest beads-hierarchy-test-status-face-closed ()
  "Test status face for closed issues."
  (should (eq (beads-hierarchy--status-face "closed") 'beads-list-status-closed)))

(ert-deftest beads-hierarchy-test-status-face-in-progress ()
  "Test status face for in-progress issues."
  (should (eq (beads-hierarchy--status-face "in_progress") 'beads-list-status-in-progress)))

(provide 'beads-hierarchy-test)
;;; beads-hierarchy-test.el ends here
