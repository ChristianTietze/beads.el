;;; beads-state-test.el --- Tests for beads-state -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'beads-state)

(ert-deftest beads-state-test-create ()
  "Test state creation."
  (let ((state (beads-state-create)))
    (should (functionp state))
    (should (eq (funcall state 'get-view) 'all))))

(ert-deftest beads-state-test-view ()
  "Test view get/set."
  (let ((beads--state nil))
    (beads-state-reset)
    (should (eq (beads-state-get-view) 'all))
    (beads-state-set-view 'open)
    (should (eq (beads-state-get-view) 'open))
    (beads-state-set-view 'closed)
    (should (eq (beads-state-get-view) 'closed))))

(ert-deftest beads-state-test-type-filter ()
  "Test type filter get/set."
  (let ((beads--state nil))
    (beads-state-reset)
    (should (null (beads-state-get-type-filter)))
    (beads-state-set-type-filter "bug")
    (should (equal (beads-state-get-type-filter) "bug"))
    (beads-state-set-type-filter "feature")
    (should (equal (beads-state-get-type-filter) "feature"))))

(ert-deftest beads-state-test-priority-filter ()
  "Test priority filter get/set."
  (let ((beads--state nil))
    (beads-state-reset)
    (should (null (beads-state-get-priority-filter)))
    (beads-state-set-priority-filter 0)
    (should (eq (beads-state-get-priority-filter) 0))
    (beads-state-set-priority-filter 2)
    (should (eq (beads-state-get-priority-filter) 2))))

(ert-deftest beads-state-test-assignee-filter ()
  "Test assignee filter get/set."
  (let ((beads--state nil))
    (beads-state-reset)
    (should (null (beads-state-get-assignee-filter)))
    (beads-state-set-assignee-filter "alice")
    (should (equal (beads-state-get-assignee-filter) "alice"))))

(ert-deftest beads-state-test-status-filter ()
  "Test status filter get/set."
  (let ((beads--state nil))
    (beads-state-reset)
    (should (null (beads-state-get-status-filter)))
    (beads-state-set-status-filter "open")
    (should (equal (beads-state-get-status-filter) "open"))))

(ert-deftest beads-state-test-clear-filters ()
  "Test clearing all filters."
  (let ((beads--state nil))
    (beads-state-reset)
    (beads-state-set-type-filter "bug")
    (beads-state-set-priority-filter 1)
    (beads-state-set-assignee-filter "bob")
    (beads-state-set-status-filter "open")
    (beads-state-clear-filters)
    (should (null (beads-state-get-type-filter)))
    (should (null (beads-state-get-priority-filter)))
    (should (null (beads-state-get-assignee-filter)))
    (should (null (beads-state-get-status-filter)))))

(ert-deftest beads-state-test-get-all ()
  "Test getting all state."
  (let ((beads--state nil))
    (beads-state-reset)
    (beads-state-set-view 'open)
    (beads-state-set-type-filter "task")
    (let ((state (beads-state-get-all)))
      (should (eq (plist-get state :view) 'open))
      (should (equal (plist-get state :type-filter) "task")))))

(ert-deftest beads-state-test-has-filters-p ()
  "Test filter predicate."
  (let ((beads--state nil))
    (beads-state-reset)
    (should-not (beads-state-has-filters-p))
    (beads-state-set-type-filter "bug")
    (should (beads-state-has-filters-p))
    (beads-state-clear-filters)
    (should-not (beads-state-has-filters-p))))

(ert-deftest beads-state-test-observers ()
  "Test observer notification."
  (let* ((beads--state nil)
         (beads-state--observers nil)
         (result (list nil)))
    (beads-state-reset)
    (beads-state-add-observer (lambda () (setcar result t)))
    (beads-state-set-view 'closed)
    (should (car result))))

(ert-deftest beads-state-test-remove-observer ()
  "Test observer removal."
  (let* ((beads--state nil)
         (beads-state--observers nil)
         (result (list nil))
         (fn (lambda () (setcar result t))))
    (beads-state-reset)
    (beads-state-add-observer fn)
    (beads-state-remove-observer fn)
    (beads-state-set-view 'closed)
    (should-not (car result))))

(provide 'beads-state-test)
;;; beads-state-test.el ends here
