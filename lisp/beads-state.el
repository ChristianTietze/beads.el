;;; beads-state.el --- State management for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Beads Contributors

;; Author: Beads Contributors
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Global state management for Beads using closure pattern.

;;; Code:

(defgroup beads-state nil
  "State management for Beads."
  :group 'beads)

(defvar beads--state nil
  "Global state manager instance.")

(defun beads-state-create ()
  "Create state manager using closure pattern.
Returns a dispatcher function that handles state messages."
  (let ((view 'all)
        (type-filter nil)
        (priority-filter nil)
        (assignee-filter nil)
        (status-filter nil))
    (lambda (message &rest args)
      (pcase message
        ('get-view view)
        ('set-view (setq view (car args)))
        ('get-type-filter type-filter)
        ('set-type-filter (setq type-filter (car args)))
        ('get-priority-filter priority-filter)
        ('set-priority-filter (setq priority-filter (car args)))
        ('get-assignee-filter assignee-filter)
        ('set-assignee-filter (setq assignee-filter (car args)))
        ('get-status-filter status-filter)
        ('set-status-filter (setq status-filter (car args)))
        ('clear-filters
         (setq type-filter nil
               priority-filter nil
               assignee-filter nil
               status-filter nil))
        ('get-all
         (list :view view
               :type-filter type-filter
               :priority-filter priority-filter
               :assignee-filter assignee-filter
               :status-filter status-filter))))))

(defun beads-state--ensure ()
  "Ensure state manager is initialized."
  (unless beads--state
    (setq beads--state (beads-state-create))))

(defun beads-state-get-view ()
  "Get current view state."
  (beads-state--ensure)
  (funcall beads--state 'get-view))

(defun beads-state-set-view (view)
  "Set VIEW state."
  (beads-state--ensure)
  (funcall beads--state 'set-view view)
  (beads-state--notify-change))

(defun beads-state-get-type-filter ()
  "Get current type filter."
  (beads-state--ensure)
  (funcall beads--state 'get-type-filter))

(defun beads-state-set-type-filter (type)
  "Set TYPE filter."
  (beads-state--ensure)
  (funcall beads--state 'set-type-filter type)
  (beads-state--notify-change))

(defun beads-state-get-priority-filter ()
  "Get current priority filter."
  (beads-state--ensure)
  (funcall beads--state 'get-priority-filter))

(defun beads-state-set-priority-filter (priority)
  "Set PRIORITY filter."
  (beads-state--ensure)
  (funcall beads--state 'set-priority-filter priority)
  (beads-state--notify-change))

(defun beads-state-get-assignee-filter ()
  "Get current assignee filter."
  (beads-state--ensure)
  (funcall beads--state 'get-assignee-filter))

(defun beads-state-set-assignee-filter (assignee)
  "Set ASSIGNEE filter."
  (beads-state--ensure)
  (funcall beads--state 'set-assignee-filter assignee)
  (beads-state--notify-change))

(defun beads-state-get-status-filter ()
  "Get current status filter."
  (beads-state--ensure)
  (funcall beads--state 'get-status-filter))

(defun beads-state-set-status-filter (status)
  "Set STATUS filter."
  (beads-state--ensure)
  (funcall beads--state 'set-status-filter status)
  (beads-state--notify-change))

(defun beads-state-clear-filters ()
  "Clear all filters."
  (beads-state--ensure)
  (funcall beads--state 'clear-filters)
  (beads-state--notify-change))

(defun beads-state-get-all ()
  "Get all state as a plist."
  (beads-state--ensure)
  (funcall beads--state 'get-all))

(defun beads-state-has-filters-p ()
  "Return non-nil if any filters are active."
  (beads-state--ensure)
  (let ((state (funcall beads--state 'get-all)))
    (or (plist-get state :type-filter)
        (plist-get state :priority-filter)
        (plist-get state :assignee-filter)
        (plist-get state :status-filter))))

(defvar beads-state--observers nil
  "List of observer functions to call on state changes.")

(defun beads-state-add-observer (fn)
  "Add FN as an observer for state changes."
  (unless (memq fn beads-state--observers)
    (push fn beads-state--observers)))

(defun beads-state-remove-observer (fn)
  "Remove FN from state change observers."
  (setq beads-state--observers (delq fn beads-state--observers)))

(defun beads-state--notify-change ()
  "Notify all observers of state change."
  (dolist (observer beads-state--observers)
    (funcall observer)))

(defun beads-state-reset ()
  "Reset state manager to initial state."
  (setq beads--state nil)
  (beads-state--ensure))

(provide 'beads-state)
;;; beads-state.el ends here
