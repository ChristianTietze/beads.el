;;; beads-filter.el --- Filter system for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, ui, search

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

;; Composable filter system for Beads issue lists.

;;; Code:

(require 'seq)

(defgroup beads-filter nil
  "Filter system for Beads issues."
  :group 'beads)

(defun beads-filter-make (name predicate-fn &optional config)
  "Create a filter with NAME and PREDICATE-FN.
Optional CONFIG is a plist of additional filter settings."
  (list :name name :predicate predicate-fn :config (or config '())))

(defun beads-filter-name (filter)
  "Return the name of FILTER."
  (plist-get filter :name))

(defun beads-filter-predicate (filter)
  "Return the predicate function of FILTER."
  (plist-get filter :predicate))

(defun beads-filter-apply (filter issues)
  "Apply FILTER to ISSUES, returning matching issues."
  (seq-filter (plist-get filter :predicate) issues))

(defun beads-filter-by-status (status)
  "Create filter for STATUS (open, in_progress, closed, blocked)."
  (beads-filter-make
   (format "status:%s" status)
   (lambda (issue)
     (string= (alist-get 'status issue) status))
   (list :type 'status :value status)))

(defun beads-filter-by-priority (priority)
  "Create filter for PRIORITY (0-4)."
  (beads-filter-make
   (format "priority:P%d" priority)
   (lambda (issue)
     (= (alist-get 'priority issue) priority))
   (list :type 'priority :value priority)))

(defun beads-filter-by-type (type)
  "Create filter for TYPE (bug, feature, task, epic, chore)."
  (beads-filter-make
   (format "type:%s" type)
   (lambda (issue)
     (string= (alist-get 'issue_type issue) type))
   (list :type 'issue-type :value type)))

(defun beads-filter-by-assignee (assignee)
  "Create filter for ASSIGNEE."
  (beads-filter-make
   (format "assignee:%s" assignee)
   (lambda (issue)
     (string= (alist-get 'assignee issue) assignee))
   (list :type 'assignee :value assignee)))

(defun beads-filter-unassigned ()
  "Create filter for unassigned issues."
  (beads-filter-make
   "assignee:none"
   (lambda (issue)
     (null (alist-get 'assignee issue)))
   (list :type 'assignee :value nil)))

(defun beads-filter-by-label (label)
  "Create filter for issues with LABEL."
  (beads-filter-make
   (format "label:%s" label)
   (lambda (issue)
     (member label (alist-get 'labels issue)))
   (list :type 'label :value label)))

(defun beads-filter-ready ()
  "Create filter for ready issues (open/in_progress with no blockers)."
  (beads-filter-make
   "ready"
   (lambda (issue)
     (and (member (alist-get 'status issue) '("open" "in_progress"))
          (zerop (or (alist-get 'dependency_count issue) 0))))
   (list :type 'ready :value t)))

(defun beads-filter-blocked ()
  "Create filter for blocked issues."
  (beads-filter-make
   "blocked"
   (lambda (issue)
     (string= (alist-get 'status issue) "blocked"))
   (list :type 'blocked :value t)))

(defun beads-filter-by-search (query)
  "Create filter for issues matching QUERY in title or description.
Search is case-insensitive."
  (let ((query-re (regexp-quote (downcase query))))
    (beads-filter-make
     (format "search:%s" query)
     (lambda (issue)
       (let ((title (or (alist-get 'title issue) ""))
             (description (or (alist-get 'description issue) "")))
         (or (string-match-p query-re (downcase title))
             (string-match-p query-re (downcase description)))))
     (list :type 'search :value query))))

(defun beads-filter-by-parent (parent-id)
  "Create filter for issues with PARENT-ID as their parent.
Enables epic-scoped views by filtering to children of a specific issue."
  (beads-filter-make
   (format "parent:%s" parent-id)
   (lambda (issue)
     (string= (alist-get 'parent_id issue) parent-id))
   (list :type 'parent :value parent-id)))

(defun beads-filter-not-closed ()
  "Create filter for non-closed issues (open, in_progress, blocked)."
  (beads-filter-make
   "status:!closed"
   (lambda (issue)
     (not (string= (alist-get 'status issue) "closed")))
   (list :type 'status :value 'not-closed)))

(defun beads-filter-compose (&rest filters)
  "Compose FILTERS with logical AND."
  (beads-filter-make
   (mapconcat #'beads-filter-name filters "+")
   (lambda (issue)
     (seq-every-p (lambda (f) (funcall (beads-filter-predicate f) issue))
                  filters))
   (list :type 'composed :filters filters)))

(defun beads-filter-compose-or (&rest filters)
  "Compose FILTERS with logical OR."
  (beads-filter-make
   (mapconcat #'beads-filter-name filters "|")
   (lambda (issue)
     (seq-some (lambda (f) (funcall (beads-filter-predicate f) issue))
               filters))
   (list :type 'composed-or :filters filters)))

(defun beads-filter-negate (filter)
  "Negate FILTER (logical NOT)."
  (beads-filter-make
   (concat "!" (beads-filter-name filter))
   (lambda (issue)
     (not (funcall (beads-filter-predicate filter) issue)))
   (list :type 'negated :original filter)))

(defun beads-filter-apply-pipeline (issues &rest filters)
  "Apply FILTERS to ISSUES in sequence."
  (let ((result issues))
    (dolist (filter filters result)
      (setq result (beads-filter-apply filter result)))))

(defun beads-filter-identity ()
  "Create identity filter that matches all issues."
  (beads-filter-make
   "all"
   (lambda (_issue) t)
   (list :type 'identity)))

(defun beads-filter-from-state (state)
  "Build filter from STATE plist."
  (let ((filters nil))
    (when-let ((status (plist-get state :status-filter)))
      (push (beads-filter-by-status status) filters))
    (when-let ((type (plist-get state :type-filter)))
      (push (beads-filter-by-type type) filters))
    (when-let ((priority (plist-get state :priority-filter)))
      (push (beads-filter-by-priority priority) filters))
    (when-let ((assignee (plist-get state :assignee-filter)))
      (push (beads-filter-by-assignee assignee) filters))
    (if filters
        (apply #'beads-filter-compose filters)
      (beads-filter-identity))))

(provide 'beads-filter)
;;; beads-filter.el ends here
