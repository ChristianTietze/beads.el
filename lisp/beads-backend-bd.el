;;; beads-backend-bd.el --- bd backend for Beads -*- lexical-binding: t -*-

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

;; Backend implementation for the bd (beads) CLI.
;; Uses CLI JSON commands; legacy RPC is detected from the installed CLI.

;;; Code:

(require 'beads-backend)

(defun beads-backend-bd--supports-daemon-p ()
  "Return whether the installed bd still provides legacy RPC."
  (beads-backend-cli-supports-command-p "daemon" beads-backend-bd))

(defun beads-backend-bd--operation-to-cli-args (operation args)
  "Convert RPC OPERATION and ARGS to CLI arguments for bd."
  (setq args (mapcar
               (lambda (pair)
                 (cons (pcase (car pair)
                         ('issue_type 'type)
                         ('acceptance_criteria 'acceptance)
                         ('dependencies 'deps)
                         ('sort_policy 'sort)
                         ('add_labels 'add_label)
                         ('remove_labels 'remove_label)
                         ('labels (if (equal operation "list") 'label 'labels))
                         (key key))
                       (cdr pair)))
               args))
  (pcase operation
    ("health"
     (if (beads-backend-bd--supports-daemon-p)
         '("daemon" "status")
       '("ping")))
    ("list"
     (append '("list" "--all" "--limit" "0")
             (cdr (beads-backend--build-cli-args
                   "list" args '(status priority type assignee
                                 label limit title_contains parent)))))
    ("show"
     (let ((id (alist-get 'id args)))
       (list "show" id)))
    ("ready"
     (beads-backend--build-cli-args "ready" args
                                    '(assignee priority limit sort parent)))
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
     (let ((group (alist-get 'group_by args)))
       (when (and group (not (member group '("status" "priority" "type" "assignee" "label"))))
         (signal 'beads-backend-error (list "Unsupported count grouping" group)))
       (append (beads-backend--build-cli-args "count" args '(status))
               (when group (list (concat "--by-" group))))))
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
    ("get_mutations"
     (unless (beads-backend-bd--supports-daemon-p)
       (signal 'beads-backend-error
               '("This bd has no mutation stream; use automatic list refresh")))
     (let ((since-id (alist-get 'since_id args)))
       (if since-id
           (list "mutations" "--since" since-id)
         '("mutations"))))
    ("types"
     '("types"))
    ("config_get"
     (let ((key (alist-get 'key args)))
       (list "config" "get" key)))
    ("config_set"
     (let ((key (alist-get 'key args))
           (value (alist-get 'value args)))
       (list "config" "set" key value)))
    ("config_unset"
     (let ((key (alist-get 'key args)))
       (list "config" "unset" key)))
    ("resolve-conflicts"
     (if (beads-backend-cli-supports-command-p "conflicts")
         (if (alist-get 'dry_run args)
             '("conflicts" "show")
           (let ((strategy (alist-get 'strategy args)))
             (unless (member strategy '("ours" "theirs"))
               (signal 'beads-backend-error
                       '("Choose ours or theirs to resolve Dolt conflicts")))
             (list "conflicts" "resolve" "--all" "--strategy" strategy)))
       (if (alist-get 'dry_run args)
           '("resolve-conflicts" "--dry-run")
         '("resolve-conflicts"))))
    ("duplicates"
     '("duplicates"))
    ("duplicate"
     (let ((source-id (alist-get 'source_id args))
           (target-id (alist-get 'target_id args)))
       (list "duplicate" source-id "--of" target-id)))
    ("comments-add"
     (let ((id (alist-get 'id args))
           (text (alist-get 'text args)))
       (list "comments" "add" id text)))
    (_
     (signal 'beads-backend-error
             (list (format "Unknown operation for bd backend: %s" operation))))))

(defun beads-backend-bd--cli-extra-flags (operation)
  "Return extra CLI flags for OPERATION on bd."
  (pcase operation
    ("duplicates" (when (beads-backend-bd--supports-daemon-p) '("--no-daemon")))
    (_ nil)))

(defconst beads-backend-bd
  (make-beads-backend
   :name "bd"
   :cli-program "bd"
   :supports-daemon #'beads-backend-bd--supports-daemon-p
   :socket-name "bd.sock"
   :daemon-start-args '("daemon" "start" "--foreground")
   :supported-ops '("health" "list" "show" "ready" "create" "update" "close"
                     "delete" "stats" "count" "dep_add" "dep_remove" "dep_tree"
                     "label_add" "label_remove" "get_mutations" "types"
                     "config_get" "config_set" "config_unset"
                     "resolve-conflicts" "duplicates" "duplicate"
                     "comments-add")
   :op-to-cli-args #'beads-backend-bd--operation-to-cli-args
   :cli-extra-flags #'beads-backend-bd--cli-extra-flags))

(beads-backend-register beads-backend-bd)

(provide 'beads-backend-bd)
;;; beads-backend-bd.el ends here
