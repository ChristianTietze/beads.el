;;; beads-transient.el --- Transient menus for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'transient)
(require 'beads-rpc)
(require 'beads-filter)

(defvar beads-list--filter)

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
  "Create a new issue interactively.
Prompts for title (required), type, and priority."
  (interactive)
  (let* ((title (read-string "Title: "))
         (type (completing-read "Type: "
                                '("task" "bug" "feature" "epic" "chore")
                                nil t "task"))
         (priority-str (completing-read "Priority: "
                                         '("P0" "P1" "P2" "P3" "P4")
                                         nil t "P2"))
         (priority (string-to-number (substring priority-str 1))))
    (if (string-empty-p title)
        (message "Title is required")
      (condition-case err
          (let ((issue (beads-rpc-create title
                                         :issue-type type
                                         :priority priority)))
            (message "Created issue %s" (alist-get 'id issue))
            (when (derived-mode-p 'beads-list-mode)
              (beads-list-refresh)))
        (beads-rpc-error
         (message "Failed to create issue: %s" (error-message-string err)))))))

(defun beads-close-issue ()
  "Close the issue at point or in current detail buffer.
Prompts for an optional close reason."
  (interactive)
  (let ((id (cond
             ((derived-mode-p 'beads-detail-mode)
              (bound-and-true-p beads-detail--current-issue-id))
             ((derived-mode-p 'beads-list-mode)
              (tabulated-list-get-id))
             (t nil))))
    (if (not id)
        (message "No issue at point")
      (let ((reason (read-string (format "Close %s reason (optional): " id))))
        (condition-case err
            (progn
              (beads-rpc-close id (unless (string-empty-p reason) reason))
              (message "Closed issue %s" id)
              (cond
               ((derived-mode-p 'beads-list-mode)
                (beads-list-refresh))
               ((derived-mode-p 'beads-detail-mode)
                (beads-detail-refresh))))
          (beads-rpc-error
           (message "Failed to close issue: %s" (error-message-string err))))))))

(defun beads-delete-issue ()
  "Permanently delete the issue at point.
Prompts for confirmation with `yes-or-no-p'."
  (interactive)
  (let ((id (cond
             ((derived-mode-p 'beads-detail-mode)
              (bound-and-true-p beads-detail--current-issue-id))
             ((derived-mode-p 'beads-list-mode)
              (tabulated-list-get-id))
             (t nil))))
    (if (not id)
        (message "No issue at point")
      (when (yes-or-no-p (format "Permanently delete issue %s? " id))
        (condition-case err
            (progn
              (beads-rpc-delete (list id))
              (message "Deleted issue %s" id)
              (cond
               ((derived-mode-p 'beads-list-mode)
                (beads-list-refresh))
               ((derived-mode-p 'beads-detail-mode)
                (quit-window t))))
          (beads-rpc-error
           (message "Failed to delete issue: %s" (error-message-string err))))))))

(defun beads-reopen-issue ()
  "Reopen the closed issue at point.
Sets status to open and clears closed_at timestamp."
  (interactive)
  (let ((id (cond
             ((derived-mode-p 'beads-detail-mode)
              (bound-and-true-p beads-detail--current-issue-id))
             ((derived-mode-p 'beads-list-mode)
              (tabulated-list-get-id))
             (t nil))))
    (if (not id)
        (message "No issue at point")
      (condition-case err
          (progn
            (beads-rpc-update id :status "open")
            (message "Reopened issue %s" id)
            (cond
             ((derived-mode-p 'beads-list-mode)
              (beads-list-refresh))
             ((derived-mode-p 'beads-detail-mode)
              (beads-detail-refresh))))
        (beads-rpc-error
         (message "Failed to reopen issue: %s" (error-message-string err)))))))

(defun beads-filter-status ()
  "Filter issues by status.
Select a status to filter, or \"all\" to clear the filter."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((choices '("all" "open" "in_progress" "blocked" "closed"))
         (current (when beads-list--filter
                    (plist-get (plist-get beads-list--filter :config) :value)))
         (status (completing-read "Filter by status: " choices nil t
                                  (or current ""))))
    (setq beads-list--filter
          (unless (string= status "all")
            (beads-filter-by-status status)))
    (beads-list-refresh)))

(defun beads-filter-priority ()
  "Filter issues by priority.
Select a priority to filter, or \"all\" to clear the filter."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((choices '("all" "P0" "P1" "P2" "P3" "P4"))
         (current (when beads-list--filter
                    (let ((val (plist-get (plist-get beads-list--filter :config) :value)))
                      (when (numberp val) (format "P%d" val)))))
         (priority-str (completing-read "Filter by priority: " choices nil t
                                        (or current ""))))
    (setq beads-list--filter
          (unless (string= priority-str "all")
            (beads-filter-by-priority
             (string-to-number (substring priority-str 1)))))
    (beads-list-refresh)))

(transient-define-prefix beads-list-menu ()
  "Beads list mode menu."
  ["Navigation"
   ("g" "Refresh" beads-list-refresh)
   ("P" "Toggle preview" beads-preview-mode)]
  ["Actions"
   ("c" "Create issue" beads-create-issue)
   ("E" "Edit issue" beads-list-edit-form)
   ("x" "Close issue" beads-close-issue)
   ("R" "Reopen issue" beads-reopen-issue)
   ("D" "Delete issue" beads-delete-issue)]
  ["Filter"
   ("f s" "By status" beads-filter-status)
   ("f p" "By priority" beads-filter-priority)]
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
   ("x" "Close issue" beads-close-issue)
   ("R" "Reopen issue" beads-reopen-issue)
   ("D" "Delete issue" beads-delete-issue)]
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
