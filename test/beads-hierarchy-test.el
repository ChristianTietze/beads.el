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
               (dependencies . [((id . "ROOT-1")
                                 (title . "Root issue")
                                 (dependency_type . "parent-child"))])
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
               (dependencies . [((id . "ROOT-1")
                                 (title . "Root issue")
                                 (dependency_type . "parent-child"))])
               (dependents . nil))
             h)
    (puthash "GRANDCHILD-1"
             '((id . "GRANDCHILD-1")
               (title . "Grandchild")
               (status . "in_progress")
               (priority . 3)
               (issue_type . "task")
               (dependencies . [((id . "CHILD-1")
                                 (title . "Child one")
                                 (dependency_type . "parent-child"))])
               (dependents . nil))
             h)
    (puthash "LONE-1"
             '((id . "LONE-1")
               (title . "Lone issue")
               (status . "open")
               (priority . 2)
               (issue_type . "task")
               (dependencies . nil)
               (dependents . nil))
             h)
    h)
  "Mock issue data for testing.")

(defun beads-hierarchy-test--mock-show (id)
  "Mock beads-client-show for testing."
  (or (gethash id beads-hierarchy-test--mock-issues)
      (signal 'beads-client-error (list (format "Issue not found: %s" id)))))

(ert-deftest beads-hierarchy-test-find-parent-nil-for-root ()
  "Test that root issues have no parent."
  (let ((by-id (make-hash-table :test 'equal))
        (root '((id . "ROOT") (title . "Root"))))
    (puthash "ROOT" root by-id)
    (should (null (beads-hierarchy--find-parent root by-id)))))

(ert-deftest beads-hierarchy-test-find-parent-returns-parent-for-descendant ()
  "Test that descendant issues return their parent via beads--parent-id."
  (let ((by-id (make-hash-table :test 'equal))
        (root '((id . "ROOT") (title . "Root")))
        (child '((beads--parent-id . "ROOT") (id . "CHILD") (title . "Child"))))
    (puthash "ROOT" root by-id)
    (puthash "CHILD" child by-id)
    (should (equal (beads-hierarchy--find-parent child by-id) root))))

(ert-deftest beads-hierarchy-test-find-parent-returns-parent-for-ancestor ()
  "Test that ancestor issues find their child via beads--child-id lookup."
  (let ((by-id (make-hash-table :test 'equal))
        (ancestor '((id . "ANCESTOR") (title . "Ancestor")))
        (focus '((beads--child-id . "ANCESTOR") (id . "FOCUS") (title . "Focus"))))
    (puthash "ANCESTOR" ancestor by-id)
    (puthash "FOCUS" focus by-id)
    (should (equal (beads-hierarchy--find-parent ancestor by-id) focus))))

(ert-deftest beads-hierarchy-test-collect-descendants-empty ()
  "Test collecting descendants from issue with no dependents."
  (let ((by-id (make-hash-table :test 'equal))
        (issue '((id . "LONE") (title . "Lone") (dependents . nil))))
    (beads-hierarchy--collect-descendants issue by-id)
    (should (= (hash-table-count by-id) 0))))

(ert-deftest beads-hierarchy-test-collect-descendants-adds-children ()
  "Test that collect-descendants adds children to hash table."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let ((by-id (make-hash-table :test 'equal))
          (root (gethash "ROOT-1" beads-hierarchy-test--mock-issues)))
      (beads-hierarchy--collect-descendants root by-id)
      (should (gethash "CHILD-1" by-id))
      (should (gethash "CHILD-2" by-id)))))

(ert-deftest beads-hierarchy-test-collect-descendants-recursive ()
  "Test that collect-descendants recursively collects grandchildren."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let ((by-id (make-hash-table :test 'equal))
          (root (gethash "ROOT-1" beads-hierarchy-test--mock-issues)))
      (beads-hierarchy--collect-descendants root by-id)
      (should (gethash "GRANDCHILD-1" by-id)))))

(ert-deftest beads-hierarchy-test-collect-ancestors-empty ()
  "Test collecting ancestors from issue with no dependencies."
  (let ((by-id (make-hash-table :test 'equal))
        (issue '((id . "LONE") (title . "Lone") (dependencies . nil))))
    (beads-hierarchy--collect-ancestors issue by-id)
    (should (= (hash-table-count by-id) 0))))

(ert-deftest beads-hierarchy-test-collect-ancestors-adds-parents ()
  "Test that collect-ancestors adds parent issues to hash table."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let ((by-id (make-hash-table :test 'equal))
          (child (gethash "CHILD-1" beads-hierarchy-test--mock-issues)))
      (beads-hierarchy--collect-ancestors child by-id)
      (should (gethash "ROOT-1" by-id)))))

(ert-deftest beads-hierarchy-test-build-creates-hierarchy ()
  "Test that build creates a valid hierarchy."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let ((result (beads-hierarchy--build "ROOT-1")))
      (should result)
      (should (consp result))
      (let ((h (car result))
            (by-id (cdr result)))
        (should (>= (hierarchy-length h) 1))
        (should (hash-table-p by-id))))))

(ert-deftest beads-hierarchy-test-build-with-no-dependents ()
  "Test building hierarchy for issue with no dependents."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let ((result (beads-hierarchy--build "LONE-1")))
      (should result)
      (let ((h (car result)))
        (should (= (hierarchy-length h) 1))))))

(ert-deftest beads-hierarchy-test-build-has-correct-structure ()
  "Test that built hierarchy has correct parent-child structure."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let* ((result (beads-hierarchy--build "ROOT-1"))
           (h (car result)))
      (let ((roots (hierarchy-roots h)))
        (should (= (length roots) 1))
        (should (string= (alist-get 'id (car roots)) "ROOT-1"))))))

(ert-deftest beads-hierarchy-test-build-from-leaf-shows-ancestors ()
  "Test that building from leaf includes ancestors."
  (cl-letf (((symbol-function 'beads-client-show) #'beads-hierarchy-test--mock-show))
    (let* ((result (beads-hierarchy--build "GRANDCHILD-1"))
           (by-id (cdr result)))
      (should (gethash "GRANDCHILD-1" by-id))
      (should (gethash "CHILD-1" by-id))
      (should (gethash "ROOT-1" by-id)))))

(provide 'beads-hierarchy-test)
;;; beads-hierarchy-test.el ends here
