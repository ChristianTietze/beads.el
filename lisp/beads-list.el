;;; beads-list.el --- Issue list mode for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'beads-rpc)
(require 'beads-detail)
(require 'beads-filter)
(require 'beads-preview)
(require 'tabulated-list)

(declare-function beads-menu "beads-transient")
(declare-function beads-show-hint "beads")
(declare-function beads-form-open "beads-form")
(declare-function beads-edit-field-minibuffer "beads-edit")
(declare-function beads-edit-field-completing "beads-edit")
(declare-function beads-edit-field-markdown "beads-edit")
(declare-function beads-project-buffer-name "beads-project")

(defgroup beads-list nil
  "Issue list display for Beads."
  :group 'beads)

(defcustom beads-list-show-header-stats t
  "Whether to show statistics in the header line.
When non-nil, displays issue counts (total, open, blocked, ready)
in the header line of the list view."
  :type 'boolean
  :group 'beads-list)

(defface beads-list-status-open
  '((t :inherit default))
  "Face for open status.")

(defface beads-list-status-in-progress
  '((t :foreground "yellow"))
  "Face for in_progress status.")

(defface beads-list-status-closed
  '((t :foreground "green"))
  "Face for closed status.")

(defface beads-list-priority-p0
  '((t :foreground "red" :weight bold))
  "Face for P0 priority.")

(defface beads-list-priority-p1
  '((t :foreground "orange"))
  "Face for P1 priority.")

(defface beads-list-header-line
  '((t :inherit header-line))
  "Face for stats header line.")

(defface beads-list-header-count
  '((t :inherit bold))
  "Face for counts in header line.")

(defvar beads-list--issues nil
  "Cached list of issues for current buffer.")

(defvar-local beads-list--filter nil
  "Current filter applied to issue list.
Created via `beads-filter-make' functions.")

(defvar-local beads-list--project-root nil
  "Project root for this beads list buffer.
Used to ensure refresh uses the correct project context.")

(declare-function beads-filter-menu "beads-transient")
(declare-function beads-delete-issue "beads-transient")
(declare-function beads-reopen-issue "beads-transient")
(declare-function beads-search "beads-transient")
(declare-function beads-stats "beads-transient")
(declare-function beads-hierarchy-show "beads-hierarchy")

(defvar beads-list-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'beads-list-edit-title)
    (define-key map (kbd "s") #'beads-list-edit-status)
    (define-key map (kbd "p") #'beads-list-edit-priority)
    (define-key map (kbd "T") #'beads-list-edit-type)
    (define-key map (kbd "d") #'beads-list-edit-description)
    map)
  "Keymap for edit commands in beads-list-mode.")

(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "RET") #'beads-list-goto-issue)
    (define-key map (kbd "e") beads-list-edit-map)
    (define-key map (kbd "E") #'beads-list-edit-form)
    (define-key map (kbd "f") #'beads-filter-menu)
    (define-key map (kbd "/") #'beads-search)
    (define-key map (kbd "H") #'beads-hierarchy-show)
    (define-key map (kbd "P") #'beads-preview-mode)
    (define-key map (kbd "S") #'beads-stats)
    (define-key map (kbd "D") #'beads-delete-issue)
    (define-key map (kbd "R") #'beads-reopen-issue)
    (define-key map (kbd "o") #'beads-list-cycle-sort)
    (define-key map (kbd "O") #'beads-list-reverse-sort)
    (define-key map (kbd "q") #'beads-list-quit)
    (define-key map (kbd "?") #'beads-menu)
    (define-key map (kbd "C-c m") #'beads-menu)
    map)
  "Keymap for beads-list-mode.")

(defun beads-list--format-header-line (stats)
  "Format STATS for display in header line."
  (let ((total (alist-get 'total_issues stats 0))
        (open (alist-get 'open_issues stats 0))
        (in-progress (alist-get 'in_progress_issues stats 0))
        (blocked (alist-get 'blocked_issues stats 0))
        (ready (alist-get 'ready_issues stats 0)))
    (format " %s total | %s open | %s in progress | %s blocked | %s ready"
            (propertize (number-to-string total) 'face 'beads-list-header-count)
            (propertize (number-to-string open) 'face 'beads-list-header-count)
            (propertize (number-to-string in-progress) 'face 'beads-list-header-count)
            (propertize (number-to-string blocked) 'face 'beads-list-header-count)
            (propertize (number-to-string ready) 'face 'beads-list-header-count))))

(defun beads-list--update-header-line ()
  "Update the header line with current stats.
Respects `beads-list-show-header-stats'."
  (if beads-list-show-header-stats
      (condition-case nil
          (let ((stats (beads-rpc-stats)))
            (setq header-line-format (beads-list--format-header-line stats)))
        (beads-rpc-error
         (setq header-line-format nil)))
    (setq header-line-format nil)))

(define-derived-mode beads-list-mode tabulated-list-mode "Beads-List"
  "Major mode for displaying Beads issues in a table.

\\{beads-list-mode-map}"
  (setq tabulated-list-format
        [("ID" 10 t)
         ("Date" 10 beads-list--sort-by-date)
         ("Status" 12 t)
         ("Pri" 4 t)
         ("Type" 8 t)
         ("Title" 50 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (add-hook 'tabulated-list-revert-hook #'beads-list-refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (beads-show-hint))

(defun beads-list-refresh (&optional silent)
  "Fetch issues from daemon and refresh the display.
When SILENT is non-nil, don't show message.
Applies `beads-list--filter' if set."
  (interactive)
  (let ((saved-id (tabulated-list-get-id))
        (saved-line (line-number-at-pos))
        (saved-start (window-start)))
    (condition-case err
        (let* ((all-issues (beads-rpc-list))
               (issues (if beads-list--filter
                           (beads-filter-apply beads-list--filter all-issues)
                         all-issues)))
          (setq beads-list--issues (append issues nil))
          (setq tabulated-list-entries (beads-list-entries beads-list--issues))
          (tabulated-list-print t)
          (if saved-id
              (unless (beads-list-goto-id saved-id)
                (goto-char (point-min))
                (forward-line (1- (min saved-line (line-number-at-pos (point-max))))))
            (goto-char (point-min)))
          (when-let ((win (get-buffer-window (current-buffer))))
            (set-window-start win (min saved-start (point-max))))
          (beads-list--update-header-line)
          (unless silent
            (let ((filter-msg (if beads-list--filter
                                  (format " [%s]" (beads-filter-name beads-list--filter))
                                "")))
              (message "Refreshed %d issues%s" (length beads-list--issues) filter-msg))))
      (beads-rpc-error
       (message "Failed to fetch issues: %s" (error-message-string err))))))

(defun beads-list-goto-id (id)
  "Move point to the line with issue ID.
Returns t if found, nil otherwise."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found) (not (eobp)))
      (if (equal id (tabulated-list-get-id))
          (setq found t)
        (forward-line 1)))
    found))

(defun beads-list-entries (issues)
  "Convert ISSUES to tabulated-list entries."
  (mapcar (lambda (issue)
            (let ((id (alist-get 'id issue)))
              (list id
                    (vector
                     (beads--format-id issue)
                     (beads--format-date issue)
                     (beads--format-status issue)
                     (beads--format-priority issue)
                     (beads--format-type issue)
                     (beads--format-title issue)))))
          issues))

(defun beads--format-id (issue)
  "Format ID column for ISSUE."
  (alist-get 'id issue))

(defun beads--format-date (issue)
  "Format date column for ISSUE.
Displays YYYY-MM-DD from created_at timestamp."
  (let ((created (alist-get 'created_at issue)))
    (if (and created (stringp created))
        (let ((parts (split-string created "T")))
          (or (car parts) ""))
      "")))

(defun beads-list--sort-by-date (a b)
  "Compare entries A and B by their date column for sorting.
Returns non-nil if A should come before B."
  (let ((date-a (aref (cadr a) 1))
        (date-b (aref (cadr b) 1)))
    (string< date-a date-b)))

(defun beads--format-status (issue)
  "Format status column for ISSUE with face."
  (let ((status (alist-get 'status issue)))
    (propertize status
                'face (pcase status
                        ("closed" 'beads-list-status-closed)
                        ("in_progress" 'beads-list-status-in-progress)
                        (_ 'beads-list-status-open)))))

(defun beads--format-priority (issue)
  "Format priority column for ISSUE as P0-P4 with face."
  (let* ((priority (alist-get 'priority issue))
         (priority-str (format "P%d" priority)))
    (propertize priority-str
                'face (pcase priority
                        (0 'beads-list-priority-p0)
                        (1 'beads-list-priority-p1)
                        (_ 'default)))))

(defun beads--format-type (issue)
  "Format type column for ISSUE with abbreviation."
  (let ((type (alist-get 'issue_type issue)))
    (pcase type
      ("bug" "bug")
      ("feature" "feat")
      ("task" "task")
      ("epic" "epic")
      ("chore" "chor")
      (_ type))))

(defun beads--format-title (issue)
  "Format title column for ISSUE, truncating if needed."
  (let ((title (alist-get 'title issue "")))
    (if (> (length title) 50)
        (concat (substring title 0 47) "...")
      title)))

(defun beads--get-issue-at-point ()
  "Get issue data at current line.
Returns the issue alist or nil if not found."
  (when-let ((id (tabulated-list-get-id)))
    (seq-find (lambda (issue)
                (string= (alist-get 'id issue) id))
              beads-list--issues)))

(defun beads-list-quit ()
  "Quit beads list, closing preview first if active."
  (interactive)
  (if beads-preview-mode
      (beads-preview-mode -1)
    (quit-window)))

(defun beads-list-cycle-sort ()
  "Cycle through sort columns."
  (interactive)
  (let* ((columns '("ID" "Date" "Status" "Pri" "Type" "Title"))
         (current (car tabulated-list-sort-key))
         (flip (cdr tabulated-list-sort-key))
         (idx (or (seq-position columns current #'string=) 0))
         (next-idx (mod (1+ idx) (length columns)))
         (next-col (nth next-idx columns)))
    (setq tabulated-list-sort-key (cons next-col flip))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (message "Sorted by %s%s" next-col (if flip " (descending)" ""))))

(defun beads-list-reverse-sort ()
  "Reverse the current sort direction."
  (interactive)
  (let ((current (car tabulated-list-sort-key))
        (flip (cdr tabulated-list-sort-key)))
    (setq tabulated-list-sort-key (cons current (not flip)))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (message "Sorted by %s%s" current (if (not flip) " (descending)" ""))))

(defun beads-list-goto-issue ()
  "Navigate to or display details for issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (condition-case err
          (let ((id (alist-get 'id issue)))
            (let ((full-issue (beads-rpc-show id)))
              (beads-detail-open full-issue)))
        (beads-rpc-error
         (message "Failed to fetch issue details: %s" (error-message-string err))))
    (message "No issue at point")))

(defun beads-list-edit-form ()
  "Open form editor for issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (condition-case err
          (let ((id (alist-get 'id issue)))
            (let ((full-issue (beads-rpc-show id)))
              (require 'beads-form)
              (beads-form-open full-issue)))
        (beads-rpc-error
         (message "Failed to fetch issue: %s" (error-message-string err))))
    (message "No issue at point")))

(defun beads-list-edit-title ()
  "Edit title of issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (let ((id (alist-get 'id issue))
            (title (alist-get 'title issue)))
        (require 'beads-edit)
        (when (beads-edit-field-minibuffer id :title title "Title: ")
          (beads-list-refresh)))
    (message "No issue at point")))

(defun beads-list-edit-status ()
  "Edit status of issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (let ((id (alist-get 'id issue))
            (status (alist-get 'status issue)))
        (require 'beads-edit)
        (when (beads-edit-field-completing
               id :status status "Status: "
               '("open" "in_progress" "blocked" "closed"))
          (beads-list-refresh)))
    (message "No issue at point")))

(defun beads-list-edit-priority ()
  "Edit priority of issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (let* ((id (alist-get 'id issue))
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
                    (beads-list-refresh))
                (beads-rpc-error
                 (message "Failed to update: %s" (error-message-string err))))))))
    (message "No issue at point")))

(defun beads-list-edit-type ()
  "Edit type of issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (let ((id (alist-get 'id issue))
            (type (alist-get 'issue_type issue)))
        (require 'beads-edit)
        (when (beads-edit-field-completing
               id :issue-type type "Type: "
               '("bug" "feature" "task" "epic" "chore"))
          (beads-list-refresh)))
    (message "No issue at point")))

(defun beads-list-edit-description ()
  "Edit description of issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (condition-case err
          (let* ((id (alist-get 'id issue))
                 (full-issue (beads-rpc-show id))
                 (description (alist-get 'description full-issue)))
            (require 'beads-edit)
            (beads-edit-field-markdown id :description description))
        (beads-rpc-error
         (message "Failed to fetch issue: %s" (error-message-string err))))
    (message "No issue at point")))

;;;###autoload
(defun beads-list ()
  "Open the Beads issue list buffer.
If beads-project.el is loaded and per-project buffers are enabled,
creates a project-specific buffer."
  (interactive)
  (let* ((buffer-name (if (featurep 'beads-project)
                          (beads-project-buffer-name)
                        "*Beads Issues*"))
         (buffer (get-buffer-create buffer-name))
         (project-root default-directory))
    (with-current-buffer buffer
      (unless (eq major-mode 'beads-list-mode)
        (beads-list-mode))
      (setq beads-list--project-root project-root)
      (beads-list-refresh))
    (switch-to-buffer buffer)))

(provide 'beads-list)
;;; beads-list.el ends here
