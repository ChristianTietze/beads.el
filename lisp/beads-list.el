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

(defcustom beads-list-columns
  '(id date status priority type title)
  "Columns to display in beads list view.
Available: id, date, status, priority, type, title, assignee, labels, deps."
  :type '(repeat (choice (const :tag "ID" id)
                         (const :tag "Date" date)
                         (const :tag "Status" status)
                         (const :tag "Priority" priority)
                         (const :tag "Type" type)
                         (const :tag "Title" title)
                         (const :tag "Assignee" assignee)
                         (const :tag "Labels" labels)
                         (const :tag "Dependencies" deps)))
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

(defface beads-list-deps-blocked
  '((t :foreground "red"))
  "Face for blocked dependency indicator.")

(defface beads-list-deps-parent
  '((t :foreground "yellow"))
  "Face for has-parent dependency indicator.")

(defface beads-list-deps-child
  '((t :foreground "green"))
  "Face for has-children dependency indicator.")

(defvar beads-list--column-defs
  '((id       . ("ID"       10 t              beads--format-id))
    (date     . ("Date"     10 beads-list--sort-by-date beads--format-date))
    (status   . ("Status"   12 t              beads--format-status))
    (priority . ("Pri"       4 t              beads--format-priority))
    (type     . ("Type"      8 t              beads--format-type))
    (title    . ("Title"    50 t              beads--format-title))
    (assignee . ("Assignee" 12 t              beads--format-assignee))
    (labels   . ("Labels"   15 t              beads--format-labels))
    (deps     . ("Dep"       3 t              beads--format-deps)))
  "Column definitions for beads list view.
Each entry is (SYMBOL . (HEADER WIDTH SORTABLE FORMATTER)).")

(defvar-local beads-list--marked nil
  "List of marked issue IDs in current buffer.")

(defvar-local beads-list--show-only-marked nil
  "When non-nil, only show marked issues in the list.")

(defun beads-list--build-format ()
  "Build `tabulated-list-format' from `beads-list-columns'.
Automatically prepends the mark column."
  (vconcat
   (cons (list " " 1 nil)
         (mapcar (lambda (col)
                   (let ((def (alist-get col beads-list--column-defs)))
                     (if def
                         (list (nth 0 def) (nth 1 def) (nth 2 def))
                       (error "Unknown column: %s" col))))
                 beads-list-columns))))

(defun beads-list--build-entry (issue)
  "Build entry vector for ISSUE based on `beads-list-columns'.
Automatically prepends the mark indicator."
  (let ((id (alist-get 'id issue)))
    (vconcat
     (cons (if (member id beads-list--marked) "*" " ")
           (mapcar (lambda (col)
                     (let ((def (alist-get col beads-list--column-defs)))
                       (if def
                           (funcall (nth 3 def) issue)
                         "")))
                   beads-list-columns)))))

(defun beads-list--column-names ()
  "Get list of column header names for current configuration."
  (mapcar (lambda (col)
            (let ((def (alist-get col beads-list--column-defs)))
              (if def (nth 0 def) "")))
          beads-list-columns))

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

(defvar beads-list-mark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'beads-list-mark-regexp)
    map)
  "Keymap for mark prefix commands in beads-list-mode.")

(defvar beads-list-bulk-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'beads-list-bulk-status)
    (define-key map (kbd "p") #'beads-list-bulk-priority)
    (define-key map (kbd "c") #'beads-list-bulk-close)
    (define-key map (kbd "D") #'beads-list-bulk-delete)
    map)
  "Keymap for bulk operation commands in beads-list-mode.")

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
    (define-key map (kbd "m") #'beads-list-mark)
    (define-key map (kbd "u") #'beads-list-unmark)
    (define-key map (kbd "U") #'beads-list-unmark-all)
    (define-key map (kbd "t") #'beads-list-toggle-marks)
    (define-key map (kbd "%") beads-list-mark-map)
    (define-key map (kbd "* m") #'beads-list-mark-regexp)
    (define-key map (kbd "* *") #'beads-list-toggle-marked-filter)
    (define-key map (kbd "B") beads-list-bulk-map)
    (define-key map (kbd "x") #'beads-list-bulk-close)
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
  (setq tabulated-list-format (beads-list--build-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (add-hook 'tabulated-list-revert-hook #'beads-list-refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1)
  (beads-show-hint))

(defun beads-list-refresh (&optional silent)
  "Fetch issues from daemon and refresh the display.
When SILENT is non-nil, don't show message.
Applies `beads-list--filter' if set, and `beads-list--show-only-marked' filter."
  (interactive)
  (let ((saved-id (tabulated-list-get-id))
        (saved-line (line-number-at-pos))
        (saved-start (window-start)))
    (condition-case err
        (let* ((all-issues (beads-rpc-list))
               (issues (if beads-list--filter
                           (beads-filter-apply beads-list--filter all-issues)
                         all-issues))
               (display-issues (if beads-list--show-only-marked
                                   (seq-filter (lambda (issue)
                                                 (member (alist-get 'id issue) beads-list--marked))
                                               issues)
                                 issues)))
          (setq beads-list--issues (append issues nil))
          (setq tabulated-list-entries (beads-list-entries display-issues))
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
              (list id (beads-list--build-entry issue))))
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

(defun beads--format-assignee (issue)
  "Format assignee column for ISSUE."
  (or (alist-get 'assignee issue) ""))

(defun beads--format-labels (issue)
  "Format labels column for ISSUE as comma-separated string."
  (let ((labels (alist-get 'labels issue)))
    (if (and labels (> (length labels) 0))
        (mapconcat #'identity labels ",")
      "")))

(defun beads--format-deps (issue)
  "Format dependency indicator for ISSUE.
Shows ↑ for has parents, ↓ for has children, ↕ for both."
  (let ((dep-count (alist-get 'dependency_count issue 0))
        (dependent-count (alist-get 'dependent_count issue 0)))
    (cond
     ((and (> dep-count 0) (> dependent-count 0))
      (propertize "↕" 'face 'beads-list-deps-parent))
     ((> dep-count 0)
      (propertize "↑" 'face 'beads-list-deps-blocked))
     ((> dependent-count 0)
      (propertize "↓" 'face 'beads-list-deps-child))
     (t ""))))

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
  (let* ((columns (beads-list--column-names))
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

(defun beads-list--update-mark-display ()
  "Update the display after marking changes."
  (setq tabulated-list-entries (beads-list-entries beads-list--issues))
  (tabulated-list-print t))

(defun beads-list-mark ()
  "Mark issue at point and move to next line."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (unless (member id beads-list--marked)
      (push id beads-list--marked))
    (beads-list--update-mark-display)
    (forward-line 1)
    (message "%d marked" (length beads-list--marked))))

(defun beads-list-unmark ()
  "Unmark issue at point and move to next line."
  (interactive)
  (when-let ((id (tabulated-list-get-id)))
    (setq beads-list--marked (delete id beads-list--marked))
    (beads-list--update-mark-display)
    (forward-line 1)
    (message "%d marked" (length beads-list--marked))))

(defun beads-list-unmark-all ()
  "Unmark all marked issues."
  (interactive)
  (let ((count (length beads-list--marked)))
    (setq beads-list--marked nil)
    (beads-list--update-mark-display)
    (message "Unmarked %d issue%s" count (if (= count 1) "" "s"))))

(defun beads-list-toggle-marks ()
  "Toggle marks: marked become unmarked and vice versa."
  (interactive)
  (let ((all-ids (mapcar (lambda (issue) (alist-get 'id issue)) beads-list--issues)))
    (setq beads-list--marked
          (seq-filter (lambda (id) (not (member id beads-list--marked))) all-ids))
    (beads-list--update-mark-display)
    (message "%d marked" (length beads-list--marked))))

(defun beads-list-mark-regexp (regexp)
  "Mark all issues whose title matches REGEXP."
  (interactive "sMark issues matching (title): ")
  (let ((count 0))
    (dolist (issue beads-list--issues)
      (let ((id (alist-get 'id issue))
            (title (alist-get 'title issue "")))
        (when (string-match-p regexp title)
          (unless (member id beads-list--marked)
            (push id beads-list--marked)
            (setq count (1+ count))))))
    (beads-list--update-mark-display)
    (message "Marked %d issue%s (%d total)" count (if (= count 1) "" "s") (length beads-list--marked))))

(defun beads-list-toggle-marked-filter ()
  "Toggle display between all issues and only marked issues."
  (interactive)
  (if (null beads-list--marked)
      (message "No marked issues")
    (setq beads-list--show-only-marked (not beads-list--show-only-marked))
    (beads-list-refresh t)
    (message "%s" (if beads-list--show-only-marked
                      (format "Showing %d marked issue(s)" (length beads-list--marked))
                    "Showing all issues"))))

(defun beads-list--get-marked-or-at-point ()
  "Return list of marked issue IDs, or ID at point if none marked."
  (if beads-list--marked
      beads-list--marked
    (when-let ((id (tabulated-list-get-id)))
      (list id))))

(defun beads-list-bulk-status (status)
  "Set STATUS for all marked issues (or issue at point if none marked)."
  (interactive
   (list (completing-read "Status: " '("open" "in_progress" "blocked" "closed") nil t)))
  (let ((ids (beads-list--get-marked-or-at-point)))
    (unless ids
      (user-error "No issues marked or at point"))
    (let ((count 0)
          (errors 0))
      (dolist (id ids)
        (condition-case nil
            (progn
              (beads-rpc-update id :status status)
              (setq count (1+ count)))
          (beads-rpc-error
           (setq errors (1+ errors)))))
      (beads-list-refresh t)
      (if (> errors 0)
          (message "Updated %d issue(s), %d error(s)" count errors)
        (message "Updated %d issue(s) to %s" count status)))))

(defun beads-list-bulk-priority (priority)
  "Set PRIORITY for all marked issues (or issue at point if none marked)."
  (interactive
   (let ((choice (completing-read "Priority: " '("P0" "P1" "P2" "P3" "P4") nil t)))
     (list (string-to-number (substring choice 1)))))
  (let ((ids (beads-list--get-marked-or-at-point)))
    (unless ids
      (user-error "No issues marked or at point"))
    (let ((count 0)
          (errors 0))
      (dolist (id ids)
        (condition-case nil
            (progn
              (beads-rpc-update id :priority priority)
              (setq count (1+ count)))
          (beads-rpc-error
           (setq errors (1+ errors)))))
      (beads-list-refresh t)
      (if (> errors 0)
          (message "Updated %d issue(s), %d error(s)" count errors)
        (message "Updated %d issue(s) to P%d" count priority)))))

(defun beads-list-bulk-close ()
  "Close all marked issues (or issue at point if none marked)."
  (interactive)
  (let ((ids (beads-list--get-marked-or-at-point)))
    (unless ids
      (user-error "No issues marked or at point"))
    (when (or (= (length ids) 1)
              (yes-or-no-p (format "Close %d issues? " (length ids))))
      (let ((count 0)
            (errors 0))
        (dolist (id ids)
          (condition-case nil
              (progn
                (beads-rpc-close id)
                (setq count (1+ count)))
            (beads-rpc-error
             (setq errors (1+ errors)))))
        (setq beads-list--marked nil)
        (beads-list-refresh t)
        (if (> errors 0)
            (message "Closed %d issue(s), %d error(s)" count errors)
          (message "Closed %d issue(s)" count))))))

(defun beads-list-bulk-delete ()
  "Delete all marked issues (or issue at point if none marked).
Prompts for confirmation."
  (interactive)
  (let ((ids (beads-list--get-marked-or-at-point)))
    (unless ids
      (user-error "No issues marked or at point"))
    (when (yes-or-no-p (format "DELETE %d issue(s)? This cannot be undone! " (length ids)))
      (let ((count 0)
            (errors 0))
        (dolist (id ids)
          (condition-case nil
              (progn
                (beads-rpc-delete id)
                (setq count (1+ count)))
            (beads-rpc-error
             (setq errors (1+ errors)))))
        (setq beads-list--marked nil)
        (beads-list-refresh t)
        (if (> errors 0)
            (message "Deleted %d issue(s), %d error(s)" count errors)
          (message "Deleted %d issue(s)" count))))))

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
