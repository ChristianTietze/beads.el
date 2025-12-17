;;; beads-detail.el --- Issue detail view for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'beads-rpc)
(require 'beads-edit)

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

(defvar-local beads-detail--current-issue nil
  "Full issue data currently displayed in this buffer.")

(defvar beads-detail-label-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'beads-detail-edit-label-add)
    (define-key map (kbd "r") #'beads-detail-edit-label-remove)
    map)
  "Keymap for label commands in beads-detail-mode.")

(defvar beads-detail-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'beads-detail-edit-description)
    (define-key map (kbd "D") #'beads-detail-edit-design)
    (define-key map (kbd "a") #'beads-detail-edit-acceptance)
    (define-key map (kbd "n") #'beads-detail-edit-notes)
    (define-key map (kbd "t") #'beads-detail-edit-title)
    (define-key map (kbd "s") #'beads-detail-edit-status)
    (define-key map (kbd "p") #'beads-detail-edit-priority)
    (define-key map (kbd "T") #'beads-detail-edit-type)
    (define-key map (kbd "A") #'beads-detail-edit-assignee)
    (define-key map (kbd "x") #'beads-detail-edit-external-ref)
    (define-key map (kbd "l") beads-detail-label-map)
    map)
  "Keymap for edit commands in beads-detail-mode.")

(defvar beads-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'beads-detail-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "e") beads-detail-edit-map)
    (define-key map (kbd "?") #'beads-menu)
    (define-key map (kbd "C-c m") #'beads-menu)
    map)
  "Keymap for beads-detail-mode.")

(define-derived-mode beads-detail-mode special-mode "Beads-Detail"
  "Major mode for displaying Beads issue details.

\\{beads-detail-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

(defun beads-detail-open (issue)
  "Open ISSUE in a dedicated detail buffer in bottom window.
Creates a unique buffer per issue and focuses it."
  (let* ((id (alist-get 'id issue))
         (buffer-name (format "*beads-detail: %s*" id))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'beads-detail-mode)
        (beads-detail-mode))
      (setq beads-detail--current-issue-id id)
      (setq beads-detail--current-issue issue)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (beads-detail--render issue)
        (goto-char (point-min))))
    (let ((window (display-buffer buffer
                                  '((display-buffer-reuse-mode-window
                                     display-buffer-below-selected)
                                    (mode . beads-detail-mode)
                                    (window-height . 0.4)))))
      (when window
        (select-window window)))))

(defun beads-detail-show (issue)
  "Display ISSUE in preview buffer (for preview mode).
Uses a single reusable buffer in a side window without focusing."
  (let* ((id (alist-get 'id issue))
         (buffer (get-buffer-create "*beads-preview*")))
    (with-current-buffer buffer
      (unless (eq major-mode 'beads-detail-mode)
        (beads-detail-mode))
      (setq beads-detail--current-issue-id id)
      (setq beads-detail--current-issue issue)
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
        (setq beads-detail--current-issue issue)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (beads-detail--render issue)
          (goto-char (point-min)))
        (message "Refreshed issue %s" beads-detail--current-issue-id))
    (beads-rpc-error
     (message "Failed to refresh issue: %s" (error-message-string err)))))

(defun beads-detail--require-issue ()
  "Return current issue or signal error."
  (unless beads-detail--current-issue
    (user-error "No issue in current buffer"))
  beads-detail--current-issue)

(defun beads-detail-edit-description ()
  "Edit the description of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (description (alist-get 'description issue)))
    (beads-edit-field-markdown id :description description)))

(defun beads-detail-edit-design ()
  "Edit the design notes of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (design (alist-get 'design issue)))
    (beads-edit-field-markdown id :design design)))

(defun beads-detail-edit-acceptance ()
  "Edit the acceptance criteria of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (acceptance (alist-get 'acceptance_criteria issue)))
    (beads-edit-field-markdown id :acceptance-criteria acceptance)))

(defun beads-detail-edit-notes ()
  "Edit the notes of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (notes (alist-get 'notes issue)))
    (beads-edit-field-markdown id :notes notes)))

(defun beads-detail-edit-title ()
  "Edit the title of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (title (alist-get 'title issue)))
    (when (beads-edit-field-minibuffer id :title title "Title: ")
      (beads-detail-refresh))))

(defun beads-detail-edit-status ()
  "Edit the status of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (status (alist-get 'status issue)))
    (when (beads-edit-field-completing
           id :status status "Status: "
           '("open" "in_progress" "blocked" "closed"))
      (beads-detail-refresh))))

(defun beads-detail-edit-priority ()
  "Edit the priority of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (priority (alist-get 'priority issue))
         (priority-str (format "P%d" priority))
         (choices '("P0" "P1" "P2" "P3" "P4")))
    (when-let ((new-value (completing-read "Priority: " choices nil t priority-str)))
      (unless (string= new-value priority-str)
        (let ((new-priority (string-to-number (substring new-value 1))))
          (condition-case err
              (progn
                (beads-rpc-update id :priority new-priority)
                (message "Updated priority for %s" id)
                (beads-detail-refresh))
            (beads-rpc-error
             (message "Failed to update: %s" (error-message-string err)))))))))

(defun beads-detail-edit-type ()
  "Edit the type of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (type (alist-get 'issue_type issue)))
    (when (beads-edit-field-completing
           id :issue-type type "Type: "
           '("bug" "feature" "task" "epic" "chore"))
      (beads-detail-refresh))))

(defun beads-detail-edit-assignee ()
  "Edit the assignee of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (assignee (alist-get 'assignee issue)))
    (when (beads-edit-field-minibuffer id :assignee assignee "Assignee: ")
      (beads-detail-refresh))))

(defun beads-detail-edit-external-ref ()
  "Edit the external reference of the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (external-ref (alist-get 'external_ref issue)))
    (when (beads-edit-field-minibuffer id :external-ref external-ref "External ref: ")
      (beads-detail-refresh))))

(defun beads-detail-edit-label-add ()
  "Add a label to the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (labels (alist-get 'labels issue))
         (labels-str (if (and labels (> (length labels) 0))
                         (format " [current: %s]" (mapconcat #'identity (append labels nil) ", "))
                       ""))
         (label (read-string (format "Add label%s: " labels-str))))
    (when (and label (not (string-empty-p label)))
      (condition-case err
          (progn
            (beads-rpc-label-add id label)
            (message "Added label '%s' to %s" label id)
            (beads-detail-refresh))
        (beads-rpc-error
         (message "Failed to add label: %s" (error-message-string err)))))))

(defun beads-detail-edit-label-remove ()
  "Remove a label from the current issue."
  (interactive)
  (let* ((issue (beads-detail--require-issue))
         (id (alist-get 'id issue))
         (labels (alist-get 'labels issue))
         (labels-list (when labels (append labels nil))))
    (if (not labels-list)
        (message "Issue %s has no labels to remove" id)
      (let ((label (completing-read "Remove label: " labels-list nil t)))
        (when (and label (not (string-empty-p label)))
          (condition-case err
              (progn
                (beads-rpc-label-remove id label)
                (message "Removed label '%s' from %s" label id)
                (beads-detail-refresh))
            (beads-rpc-error
             (message "Failed to remove label: %s" (error-message-string err)))))))))

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
