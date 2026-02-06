;;; beads-backend-br.el --- br (beads_rust) backend for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
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

;; Backend implementation for the br (beads_rust) CLI.
;; CLI-only (no daemon support), with a reduced operation set.

;;; Code:

(require 'beads-backend)

(defun beads-backend-br--operation-to-cli-args (operation args)
  "Convert RPC OPERATION and ARGS to CLI arguments for br."
  (pcase operation
    ("list"
     (beads-backend--build-cli-args "list" args
                                    '(status priority issue_type assignee
                                      labels limit title_contains parent)))
    ("show"
     (let ((id (alist-get 'id args)))
       (list "show" id)))
    ("ready"
     (beads-backend--build-cli-args "ready" args
                                    '(assignee priority limit sort_policy parent)))
    ("create"
     (let ((title (alist-get 'title args))
           (other-args (assq-delete-all 'title (copy-alist args))))
       (append (list "create" title)
               (beads-backend--alist-to-cli-flags other-args))))
    ("update"
     (let ((id (alist-get 'id args))
           (other-args (assq-delete-all 'id (copy-alist args))))
       (append (list "update" id)
               (beads-backend--alist-to-cli-flags other-args))))
    ("close"
     (let ((id (alist-get 'id args))
           (reason (alist-get 'reason args)))
       (if reason
           (list "close" id "--reason" reason)
         (list "close" id))))
    ("delete"
     (let ((ids (alist-get 'ids args))
           (force (alist-get 'force args)))
       (append (list "delete")
               (if (listp ids) ids (list ids))
               (when force '("--force")))))
    ("stats"
     '("stats"))
    ("count"
     (beads-backend--build-cli-args "count" args '(status group_by)))
    ("dep_add"
     (let ((from-id (alist-get 'from_id args))
           (to-id (alist-get 'to_id args))
           (dep-type (alist-get 'dep_type args)))
       (if dep-type
           (list "dep" "add" from-id to-id "--type" dep-type)
         (list "dep" "add" from-id to-id))))
    ("dep_remove"
     (let ((from-id (alist-get 'from_id args))
           (to-id (alist-get 'to_id args)))
       (list "dep" "remove" from-id to-id)))
    ("dep_tree"
     (let ((id (alist-get 'id args))
           (max-depth (alist-get 'max_depth args)))
       (if max-depth
           (list "dep" "tree" id "--max-depth" (number-to-string max-depth))
         (list "dep" "tree" id))))
    ("label_add"
     (let ((id (alist-get 'id args))
           (label (alist-get 'label args)))
       (list "label" "add" id label)))
    ("label_remove"
     (let ((id (alist-get 'id args))
           (label (alist-get 'label args)))
       (list "label" "remove" id label)))
    (_
     (signal 'beads-backend-error
             (list (format "Unknown operation for br backend: %s" operation))))))

(defconst beads-backend-br
  (make-beads-backend
   :name "br"
   :cli-program "br"
   :supports-daemon nil
   :socket-name nil
   :daemon-start-args nil
   :supported-ops '("list" "show" "ready" "create" "update" "close"
                     "delete" "stats" "count" "dep_add" "dep_remove"
                     "dep_tree" "label_add" "label_remove")
   :op-to-cli-args #'beads-backend-br--operation-to-cli-args
   :cli-extra-flags nil))

(beads-backend-register beads-backend-br)

(provide 'beads-backend-br)
;;; beads-backend-br.el ends here
