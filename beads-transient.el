;;; beads-transient.el --- Transient menus for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'transient)
(require 'beads-list)
(require 'beads-detail)
(require 'beads-preview)

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

(transient-define-prefix beads-menu ()
  "Beads issue tracker menu."
  ["Navigation"
   ("l" "List issues" beads-list)
   ("g" "Refresh" beads-list-refresh)
   ("P" "Toggle preview" beads-preview-mode :transient t)]
  ["Actions"
   ("c" "Create issue" beads-create-issue)
   ("E" "Edit form" beads-detail-edit-form)
   ("x" "Close issue" beads-close-issue)]
  ["Filter"
   ("s" "By status" beads-filter-status)
   ("p" "By priority" beads-filter-priority)]
  ["Help"
   ("?" "Describe mode" describe-mode)
   ("q" "Quit" transient-quit-one)])

(provide 'beads-transient)
;;; beads-transient.el ends here
