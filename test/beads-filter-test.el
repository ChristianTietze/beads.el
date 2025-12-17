;;; beads-filter-test.el --- Tests for beads-filter -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'beads-filter)

(defvar beads-filter-test--issues
  '(((id . "TEST-1") (title . "Bug one") (status . "open")
     (priority . 0) (issue_type . "bug") (assignee . "alice"))
    ((id . "TEST-2") (title . "Feature two") (status . "in_progress")
     (priority . 1) (issue_type . "feature") (assignee . "bob"))
    ((id . "TEST-3") (title . "Task three") (status . "closed")
     (priority . 2) (issue_type . "task") (assignee . nil))
    ((id . "TEST-4") (title . "Bug four") (status . "open")
     (priority . 1) (issue_type . "bug") (assignee . "alice")))
  "Test issue data.")

(ert-deftest beads-filter-test-make ()
  "Test filter creation."
  (let ((filter (beads-filter-make "test" (lambda (_) t))))
    (should (equal (beads-filter-name filter) "test"))
    (should (functionp (beads-filter-predicate filter)))))

(ert-deftest beads-filter-test-apply ()
  "Test filter application."
  (let* ((filter (beads-filter-by-status "open"))
         (result (beads-filter-apply filter beads-filter-test--issues)))
    (should (= (length result) 2))
    (should (string= (alist-get 'id (car result)) "TEST-1"))))

(ert-deftest beads-filter-test-by-status ()
  "Test status filter."
  (let ((filter (beads-filter-by-status "closed")))
    (should (equal (beads-filter-name filter) "status:closed"))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 1))
      (should (string= (alist-get 'id (car result)) "TEST-3")))))

(ert-deftest beads-filter-test-by-priority ()
  "Test priority filter."
  (let ((filter (beads-filter-by-priority 1)))
    (should (equal (beads-filter-name filter) "priority:P1"))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 2)))))

(ert-deftest beads-filter-test-by-type ()
  "Test type filter."
  (let ((filter (beads-filter-by-type "bug")))
    (should (equal (beads-filter-name filter) "type:bug"))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 2)))))

(ert-deftest beads-filter-test-by-assignee ()
  "Test assignee filter."
  (let ((filter (beads-filter-by-assignee "alice")))
    (should (equal (beads-filter-name filter) "assignee:alice"))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 2)))))

(ert-deftest beads-filter-test-unassigned ()
  "Test unassigned filter."
  (let ((filter (beads-filter-unassigned)))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 1))
      (should (string= (alist-get 'id (car result)) "TEST-3")))))

(ert-deftest beads-filter-test-not-closed ()
  "Test not-closed filter."
  (let ((filter (beads-filter-not-closed)))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 3)))))

(ert-deftest beads-filter-test-compose ()
  "Test filter composition with AND."
  (let* ((status-filter (beads-filter-by-status "open"))
         (type-filter (beads-filter-by-type "bug"))
         (composed (beads-filter-compose status-filter type-filter)))
    (should (equal (beads-filter-name composed) "status:open+type:bug"))
    (let ((result (beads-filter-apply composed beads-filter-test--issues)))
      (should (= (length result) 2)))))

(ert-deftest beads-filter-test-compose-or ()
  "Test filter composition with OR."
  (let* ((status-filter (beads-filter-by-status "closed"))
         (priority-filter (beads-filter-by-priority 0))
         (composed (beads-filter-compose-or status-filter priority-filter)))
    (let ((result (beads-filter-apply composed beads-filter-test--issues)))
      (should (= (length result) 2)))))

(ert-deftest beads-filter-test-negate ()
  "Test filter negation."
  (let* ((filter (beads-filter-by-status "closed"))
         (negated (beads-filter-negate filter)))
    (should (equal (beads-filter-name negated) "!status:closed"))
    (let ((result (beads-filter-apply negated beads-filter-test--issues)))
      (should (= (length result) 3)))))

(ert-deftest beads-filter-test-pipeline ()
  "Test filter pipeline."
  (let* ((type-filter (beads-filter-by-type "bug"))
         (assignee-filter (beads-filter-by-assignee "alice"))
         (result (beads-filter-apply-pipeline
                  beads-filter-test--issues
                  type-filter
                  assignee-filter)))
    (should (= (length result) 2))))

(ert-deftest beads-filter-test-identity ()
  "Test identity filter."
  (let ((filter (beads-filter-identity)))
    (should (equal (beads-filter-name filter) "all"))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 4)))))

(ert-deftest beads-filter-test-from-state-empty ()
  "Test building filter from empty state."
  (let ((filter (beads-filter-from-state '())))
    (should (equal (beads-filter-name filter) "all"))))

(ert-deftest beads-filter-test-from-state-single ()
  "Test building filter from state with single filter."
  (let ((filter (beads-filter-from-state '(:status-filter "open"))))
    (should (equal (beads-filter-name filter) "status:open"))))

(ert-deftest beads-filter-test-from-state-multiple ()
  "Test building filter from state with multiple filters."
  (let ((filter (beads-filter-from-state
                 '(:status-filter "open" :type-filter "bug"))))
    (let ((result (beads-filter-apply filter beads-filter-test--issues)))
      (should (= (length result) 2)))))

(provide 'beads-filter-test)
;;; beads-filter-test.el ends here
