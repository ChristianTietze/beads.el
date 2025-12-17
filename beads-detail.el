;;; beads-detail.el --- Issue detail view for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'beads-rpc)

(declare-function beads-menu "beads-transient")

(defgroup beads-detail nil
  "Issue detail display for Beads."
  :group 'beads)

(defface beads-detail-id-face
  '((t :weight bold))
  "Face for issue ID in detail view.")

(defface beads-detail-title-face
  '((t :height 1.2 :weight bold))
  "Face for issue title in detail view.")

(defface beads-detail-header-face
  '((t :weight bold :underline t))
  "Face for section headers in detail view.")

(defface beads-detail-label-face
  '((t :weight bold))
  "Face for field labels in detail view.")

(defface beads-detail-value-face
  '((t :inherit default))
  "Face for field values in detail view.")

(defvar-local beads-detail--current-issue-id nil
  "Issue ID currently displayed in this buffer.")

(defvar beads-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-detail-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "e") #'beads-detail-edit-issue)
    (define-key map (kbd "?") #'beads-menu)
    (define-key map (kbd "C-c m") #'beads-menu)
    map)
  "Keymap for beads-detail-mode.")

(define-derived-mode beads-detail-mode special-mode "Beads-Detail"
  "Major mode for displaying Beads issue details.

\\{beads-detail-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

(defun beads-detail-show (issue)
  "Display ISSUE in detail buffer."
  (let* ((id (alist-get 'id issue))
         (buffer (get-buffer-create "*beads-detail*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'beads-detail-mode)
        (beads-detail-mode))
      (setq beads-detail--current-issue-id id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (beads-detail--render issue)
        (goto-char (point-min))))
    (display-buffer buffer '((display-buffer-in-side-window)
                             (side . right)
                             (window-width . 0.4)))))

(defun beads-detail-refresh ()
  "Re-fetch and redisplay current issue."
  (interactive)
  (unless beads-detail--current-issue-id
    (user-error "No issue to refresh"))
  (condition-case err
      (let ((issue (beads-rpc-show beads-detail--current-issue-id)))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (beads-detail--render issue)
          (goto-char (point-min)))
        (message "Refreshed issue %s" beads-detail--current-issue-id))
    (beads-rpc-error
     (message "Failed to refresh issue: %s" (error-message-string err)))))

(defun beads-detail-edit-issue ()
  "Edit the current issue."
  (interactive)
  (message "Edit functionality not yet implemented"))

(defun beads-detail--render (issue)
  "Insert formatted ISSUE content into current buffer."
  (beads-detail--insert-header issue)
  (insert "\n")
  (beads-detail--insert-separator ?═)
  (insert "\n")
  (beads-detail--insert-metadata issue)
  (insert "\n")
  (beads-detail--insert-separator ?─)
  (insert "\n\n")

  (when-let ((description (alist-get 'description issue)))
    (unless (string-empty-p description)
      (beads-detail--insert-section "Description" description)))

  (when-let ((design (alist-get 'design issue)))
    (unless (string-empty-p design)
      (beads-detail--insert-section "Design Notes" design)))

  (when-let ((acceptance (alist-get 'acceptance_criteria issue)))
    (unless (string-empty-p acceptance)
      (beads-detail--insert-section "Acceptance Criteria" acceptance))))

(defun beads-detail--insert-header (issue)
  "Insert ID and title for ISSUE."
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue "")))
    (insert (propertize (format "[%s] " id) 'face 'beads-detail-id-face))
    (insert (propertize title 'face 'beads-detail-title-face))))

(defun beads-detail--insert-metadata (issue)
  "Insert status, priority, type, assignee, and labels for ISSUE."
  (let ((status (alist-get 'status issue))
        (priority (alist-get 'priority issue))
        (type (alist-get 'issue_type issue))
        (assignee (alist-get 'assignee issue))
        (created (alist-get 'created_at issue))
        (labels (alist-get 'labels issue)))

    (beads-detail--insert-field "Status" status)
    (insert "     ")
    (beads-detail--insert-field "Priority" (format "P%d" priority))
    (insert "     ")
    (beads-detail--insert-field "Type" type)
    (insert "\n")

    (when assignee
      (beads-detail--insert-field "Assignee" assignee)
      (insert "  "))
    (when created
      (beads-detail--insert-field "Created" (beads-detail--format-timestamp created)))
    (insert "\n")

    (when (and labels (> (length labels) 0))
      (beads-detail--insert-field "Labels"
                                  (mapconcat #'identity (append labels nil) ", "))
      (insert "\n"))))

(defun beads-detail--insert-field (label value)
  "Insert a LABEL: VALUE pair."
  (insert (propertize (concat label ": ") 'face 'beads-detail-label-face))
  (insert (propertize (or value "") 'face 'beads-detail-value-face)))

(defun beads-detail--insert-section (title content)
  "Insert a section with TITLE and CONTENT."
  (beads-detail--insert-separator ?─)
  (insert "\n")
  (insert (propertize (concat title ":\n") 'face 'beads-detail-header-face))
  (insert "\n")
  (insert content)
  (insert "\n\n"))

(defun beads-detail--insert-separator (char)
  "Insert a separator line using CHAR."
  (insert (make-string 60 char)))

(defun beads-detail--format-timestamp (timestamp)
  "Format TIMESTAMP string for display."
  (if (stringp timestamp)
      (let ((parts (split-string timestamp "T")))
        (if (car parts)
            (car parts)
          timestamp))
    (format "%s" timestamp)))

(provide 'beads-detail)
;;; beads-detail.el ends here
