;;; beads-transient.el --- Transient menus for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'transient)

(declare-function beads-list "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-list-edit-form "beads-list")
(declare-function beads-preview-mode "beads-preview")
(declare-function beads-detail-refresh "beads-detail")
(declare-function beads-detail-edit-form "beads-detail")

(defgroup beads-transient nil
  "Transient menus for Beads issue tracker."
  :group 'beads)

(defun beads-create-issue ()
  "Create a new issue."
  (interactive)
  (message "Create issue not yet implemented"))

(defun beads-close-issue ()
  "Close the current issue."
  (interactive)
  (message "Close issue not yet implemented"))

(defun beads-filter-status ()
  "Filter issues by status."
  (interactive)
  (message "Filter by status not yet implemented"))

(defun beads-filter-priority ()
  "Filter issues by priority."
  (interactive)
  (message "Filter by priority not yet implemented"))

(transient-define-prefix beads-list-menu ()
  "Beads list mode menu."
  ["Navigation"
   ("g" "Refresh" beads-list-refresh)
   ("P" "Toggle preview" beads-preview-mode)]
  ["Actions"
   ("c" "Create issue" beads-create-issue)
   ("E" "Edit issue" beads-list-edit-form)
   ("x" "Close issue" beads-close-issue)]
  ["Filter"
   ("s" "By status" beads-filter-status)
   ("p" "By priority" beads-filter-priority)]
  ["Help"
   ("?" "Describe mode" describe-mode)
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix beads-detail-menu ()
  "Beads detail mode menu."
  ["Navigation"
   ("l" "List issues" beads-list)
   ("g" "Refresh" beads-detail-refresh)]
  ["Edit"
   ("E" "Edit form" beads-detail-edit-form)
   ("e d" "Description" beads-detail-edit-description)
   ("e s" "Status" beads-detail-edit-status)
   ("e p" "Priority" beads-detail-edit-priority)
   ("e t" "Title" beads-detail-edit-title)]
  ["Actions"
   ("x" "Close issue" beads-close-issue)]
  ["Help"
   ("?" "Describe mode" describe-mode)
   ("q" "Quit" transient-quit-one)])

(defun beads-menu ()
  "Show context-appropriate Beads menu."
  (interactive)
  (cond
   ((derived-mode-p 'beads-detail-mode)
    (beads-detail-menu))
   ((derived-mode-p 'beads-list-mode)
    (beads-list-menu))
   (t
    (beads-list-menu))))

(provide 'beads-transient)
;;; beads-transient.el ends here
