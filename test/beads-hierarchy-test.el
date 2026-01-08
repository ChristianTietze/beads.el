;;; beads-hierarchy-test.el --- Tests for beads-hierarchy -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'hierarchy)
(require 'beads-hierarchy)

(defvar beads-hierarchy-test--mock-tree-data
  '(((id . "ROOT-1")
     (title . "Root issue")
     (status . "open")
     (priority . 1)
     (issue_type . "epic")
     (depth . 0)
     (parent_id . "ROOT-1"))
    ((id . "CHILD-1")
     (title . "Child one")
     (status . "open")
     (priority . 2)
     (issue_type . "task")
     (depth . 1)
     (parent_id . "ROOT-1"))
    ((id . "CHILD-2")
     (title . "Child two")
     (status . "closed")
     (priority . 2)
     (issue_type . "task")
     (depth . 1)
     (parent_id . "ROOT-1"))
    ((id . "GRANDCHILD-1")
     (title . "Grandchild")
     (status . "in_progress")
     (priority . 3)
     (issue_type . "task")
     (depth . 2)
     (parent_id . "CHILD-1")))
  "Mock tree data simulating bd dep tree --direction=both output.")

(defvar beads-hierarchy-test--mock-single-issue
  '(((id . "LONE-1")
     (title . "Lone issue")
     (status . "open")
     (priority . 2)
     (issue_type . "task")
     (depth . 0)
     (parent_id . "LONE-1")))
  "Mock tree data for issue with no dependencies.")

(defun beads-hierarchy-test--mock-fetch-tree (id)
  "Mock beads-hierarchy--fetch-tree for testing."
  (cond
   ((string= id "ROOT-1") beads-hierarchy-test--mock-tree-data)
   ((string= id "LONE-1") beads-hierarchy-test--mock-single-issue)
   (t (signal 'beads-rpc-error (list (format "Issue not found: %s" id))))))

(ert-deftest beads-hierarchy-test-find-parent-nil-for-root ()
  "Test that root issues have no parent (parent_id equals own id)."
  (let ((by-id (make-hash-table :test 'equal))
        (root '((id . "ROOT") (parent_id . "ROOT") (title . "Root"))))
    (puthash "ROOT" root by-id)
    (should (null (beads-hierarchy--find-parent root by-id)))))

(ert-deftest beads-hierarchy-test-find-parent-returns-parent ()
  "Test that child issues return their parent."
  (let ((by-id (make-hash-table :test 'equal))
        (root '((id . "ROOT") (parent_id . "ROOT") (title . "Root")))
        (child '((id . "CHILD") (parent_id . "ROOT") (title . "Child"))))
    (puthash "ROOT" root by-id)
    (puthash "CHILD" child by-id)
    (should (equal (beads-hierarchy--find-parent child by-id) root))))

(ert-deftest beads-hierarchy-test-find-parent-nil-when-missing ()
  "Test that find-parent returns nil when parent not in hash table."
  (let ((by-id (make-hash-table :test 'equal))
        (child '((id . "CHILD") (parent_id . "MISSING") (title . "Child"))))
    (puthash "CHILD" child by-id)
    (should (null (beads-hierarchy--find-parent child by-id)))))

(ert-deftest beads-hierarchy-test-build-creates-hierarchy ()
  "Test that build creates a valid hierarchy."
  (cl-letf (((symbol-function 'beads-hierarchy--fetch-tree)
             #'beads-hierarchy-test--mock-fetch-tree))
    (let ((result (beads-hierarchy--build "ROOT-1")))
      (should result)
      (should (consp result))
      (let ((h (car result))
            (by-id (cdr result)))
        (should (>= (hierarchy-length h) 1))
        (should (hash-table-p by-id))))))

(ert-deftest beads-hierarchy-test-build-with-no-dependents ()
  "Test building hierarchy for issue with no dependents."
  (cl-letf (((symbol-function 'beads-hierarchy--fetch-tree)
             #'beads-hierarchy-test--mock-fetch-tree))
    (let ((result (beads-hierarchy--build "LONE-1")))
      (should result)
      (let ((h (car result)))
        (should (= (hierarchy-length h) 1))))))

(ert-deftest beads-hierarchy-test-build-has-correct-structure ()
  "Test that built hierarchy has correct parent-child structure."
  (cl-letf (((symbol-function 'beads-hierarchy--fetch-tree)
             #'beads-hierarchy-test--mock-fetch-tree))
    (let* ((result (beads-hierarchy--build "ROOT-1"))
           (h (car result)))
      (let ((roots (hierarchy-roots h)))
        (should (= (length roots) 1))
        (should (string= (alist-get 'id (car roots)) "ROOT-1"))))))

(ert-deftest beads-hierarchy-test-build-contains-all-issues ()
  "Test that built hierarchy contains all issues from tree data."
  (cl-letf (((symbol-function 'beads-hierarchy--fetch-tree)
             #'beads-hierarchy-test--mock-fetch-tree))
    (let* ((result (beads-hierarchy--build "ROOT-1"))
           (by-id (cdr result)))
      (should (gethash "ROOT-1" by-id))
      (should (gethash "CHILD-1" by-id))
      (should (gethash "CHILD-2" by-id))
      (should (gethash "GRANDCHILD-1" by-id)))))

(ert-deftest beads-hierarchy-test-build-has-grandchildren ()
  "Test that hierarchy correctly nests grandchildren."
  (cl-letf (((symbol-function 'beads-hierarchy--fetch-tree)
             #'beads-hierarchy-test--mock-fetch-tree))
    (let* ((result (beads-hierarchy--build "ROOT-1"))
           (h (car result))
           (by-id (cdr result))
           (root (gethash "ROOT-1" by-id))
           (child1 (gethash "CHILD-1" by-id))
           (grandchild (gethash "GRANDCHILD-1" by-id)))
      (should (member child1 (hierarchy-children h root)))
      (should (member grandchild (hierarchy-children h child1))))))

(ert-deftest beads-hierarchy-test-status-face-open ()
  "Test status face for open issues."
  (should (eq (beads-hierarchy--status-face "open") 'beads-list-status-open)))

(ert-deftest beads-hierarchy-test-status-face-closed ()
  "Test status face for closed issues."
  (should (eq (beads-hierarchy--status-face "closed") 'beads-list-status-closed)))

(ert-deftest beads-hierarchy-test-status-face-in-progress ()
  "Test status face for in-progress issues."
  (should (eq (beads-hierarchy--status-face "in_progress") 'beads-list-status-in-progress)))

(ert-deftest beads-hierarchy-test-status-face-blocked ()
  "Test status face for blocked issues."
  (should (eq (beads-hierarchy--status-face "blocked") 'beads-list-status-blocked)))

(ert-deftest beads-hierarchy-test-status-face-hooked ()
  "Test status face for hooked issues."
  (should (eq (beads-hierarchy--status-face "hooked") 'beads-list-status-hooked)))

(provide 'beads-hierarchy-test)
;;; beads-hierarchy-test.el ends here
