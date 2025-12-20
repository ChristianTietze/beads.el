;;; beads-transient.el --- Transient menus for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'transient)
(require 'beads-rpc)
(require 'beads-filter)

(defvar beads-list--filter)
(defvar beads-list--marked)

(declare-function beads-list-mark "beads-list")
(declare-function beads-list-unmark "beads-list")
(declare-function beads-list-unmark-all "beads-list")
(declare-function beads-list-toggle-marks "beads-list")
(declare-function beads-list-mark-regexp "beads-list")
(declare-function beads-list-toggle-marked-filter "beads-list")
(declare-function beads-list-bulk-status "beads-list")
(declare-function beads-list-bulk-priority "beads-list")
(declare-function beads-list-bulk-close "beads-list")
(declare-function beads-list-bulk-delete "beads-list")

(declare-function beads-filter-by-label "beads-filter")
(declare-function beads-filter-ready "beads-filter")
(declare-function beads-filter-blocked "beads-filter")
(declare-function beads-filter-by-search "beads-filter")

(declare-function beads-list "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-list-edit-form "beads-list")
(declare-function beads-list--build-format "beads-list")
(declare-function beads-list--column-names "beads-list")
(declare-function beads-preview-mode "beads-preview")
(declare-function beads-detail-refresh "beads-detail")
(declare-function beads-detail-edit-form "beads-detail")
(declare-function beads-hierarchy-show "beads-hierarchy")

(defgroup beads-transient nil
  "Transient menus for Beads issue tracker."
  :group 'beads)

(defun beads--truncate-middle (str max-len)
  "Truncate STR to MAX-LEN using middle ellipsis.
Shows beginning and end of string with … in the middle."
  (if (<= (length str) max-len)
      str
    (let* ((ellipsis "…")
           (available (- max-len (length ellipsis)))
           (head-len (/ (1+ available) 2))
           (tail-len (/ available 2)))
      (concat (substring str 0 head-len)
              ellipsis
              (substring str (- (length str) tail-len))))))

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
  (let* ((id (cond
              ((derived-mode-p 'beads-detail-mode)
               (bound-and-true-p beads-detail--current-issue-id))
              ((derived-mode-p 'beads-list-mode)
               (tabulated-list-get-id))
              (t nil)))
         (title (cond
                 ((derived-mode-p 'beads-detail-mode)
                  (alist-get 'title (bound-and-true-p beads-detail--current-issue)))
                 ((derived-mode-p 'beads-list-mode)
                  (when-let ((entry (tabulated-list-get-entry)))
                    (aref entry 5)))
                 (t nil)))
         (display-title (if title
                            (beads--truncate-middle title 30)
                          ""))
         (prompt (if (string-empty-p display-title)
                     (format "Permanently delete issue %s? " id)
                   (format "Permanently delete '%s' (%s)? " display-title id))))
    (if (not id)
        (message "No issue at point")
      (when (yes-or-no-p prompt)
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

(defun beads-stats ()
  "Display project statistics in a popup buffer.
Press `q' to close the stats window."
  (interactive)
  (condition-case err
      (let ((stats (beads-rpc-stats)))
        (with-current-buffer (get-buffer-create "*Beads Stats*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize "Beads Project Statistics\n" 'face 'bold))
            (insert (make-string 25 ?=) "\n\n")
            (insert (format "%-20s %d\n" "Total Issues:" (alist-get 'total_issues stats 0)))
            (insert (format "%-20s %d\n" "Open:" (alist-get 'open_issues stats 0)))
            (insert (format "%-20s %d\n" "In Progress:" (alist-get 'in_progress_issues stats 0)))
            (insert (format "%-20s %d\n" "Closed:" (alist-get 'closed_issues stats 0)))
            (insert (format "%-20s %d\n" "Blocked:" (alist-get 'blocked_issues stats 0)))
            (insert (format "%-20s %d\n" "Ready:" (alist-get 'ready_issues stats 0)))
            (let ((lead-time (alist-get 'average_lead_time_hours stats)))
              (when (and lead-time (> lead-time 0))
                (insert (format "\n%-20s %.1f hours\n" "Avg Lead Time:" lead-time)))))
          (special-mode)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)
                         '((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . fit-window-to-buffer)))))
    (beads-rpc-error
     (message "Failed to fetch stats: %s" (error-message-string err)))))

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

(defun beads-filter-type ()
  "Filter issues by type."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((choices '("all" "bug" "feature" "task" "epic" "chore"))
         (type (completing-read "Filter by type: " choices nil t)))
    (setq beads-list--filter
          (unless (string= type "all")
            (beads-filter-by-type type)))
    (beads-list-refresh)))

(defun beads-filter-assignee ()
  "Filter issues by assignee."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((issues (beads-rpc-list))
         (assignees (seq-uniq
                     (seq-filter #'identity
                                 (mapcar (lambda (i) (alist-get 'assignee i)) issues))))
         (choices (cons "all" (cons "unassigned" (sort assignees #'string<))))
         (assignee (completing-read "Filter by assignee: " choices nil t)))
    (setq beads-list--filter
          (cond
           ((string= assignee "all") nil)
           ((string= assignee "unassigned") (beads-filter-unassigned))
           (t (beads-filter-by-assignee assignee))))
    (beads-list-refresh)))

(defun beads-filter-label ()
  "Filter issues by label."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((issues (beads-rpc-list))
         (labels (seq-uniq
                  (apply #'append
                         (mapcar (lambda (i) (alist-get 'labels i)) issues))))
         (choices (cons "all" (sort labels #'string<)))
         (label (completing-read "Filter by label: " choices nil t)))
    (setq beads-list--filter
          (unless (string= label "all")
            (beads-filter-by-label label)))
    (beads-list-refresh)))

(defun beads-filter-ready-issues ()
  "Filter to show only ready issues (no blockers)."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (setq beads-list--filter (beads-filter-ready))
  (beads-list-refresh)
  (message "Showing ready issues only"))

(defun beads-filter-blocked-issues ()
  "Filter to show only blocked issues."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (setq beads-list--filter (beads-filter-blocked))
  (beads-list-refresh)
  (message "Showing blocked issues only"))

(defun beads-filter-clear ()
  "Clear all filters."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (setq beads-list--filter nil)
  (beads-list-refresh)
  (message "Filters cleared"))

(defun beads-search ()
  "Search issues by title or description.
Prompts for a search query and filters the list to matching issues."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let ((query (read-string "Search issues: ")))
    (if (string-empty-p query)
        (progn
          (setq beads-list--filter nil)
          (beads-list-refresh)
          (message "Search cleared"))
      (setq beads-list--filter (beads-filter-by-search query))
      (beads-list-refresh))))

;;; Column configuration

(defvar beads-list--column-order
  '(id date status priority type deps assignee labels title)
  "Canonical order of columns for insertion.")

(defvar beads-list-columns)
(defvar beads-list--column-defs)

(defun beads-list--column-enabled-p (col)
  "Return non-nil if column COL is currently enabled."
  (memq col beads-list-columns))

(defun beads-list--toggle-column (col)
  "Toggle column COL in the current buffer's column list.
Uses canonical order from `beads-list--column-order' for insertion."
  (require 'beads-list)
  (if (beads-list--column-enabled-p col)
      (setq-local beads-list-columns (remq col beads-list-columns))
    (let ((new-cols '()))
      (dolist (c beads-list--column-order)
        (when (or (eq c col) (memq c beads-list-columns))
          (push c new-cols)))
      (setq-local beads-list-columns (nreverse new-cols))))
  (setq tabulated-list-format (beads-list--build-format))
  (let* ((col-names (beads-list--column-names))
         (sort-col (car tabulated-list-sort-key)))
    (unless (member sort-col col-names)
      (setq tabulated-list-sort-key (cons (car col-names) nil))))
  (tabulated-list-init-header)
  (beads-list-refresh t))

(defun beads-list--column-description (col)
  "Get display name for column COL."
  (let ((def (alist-get col beads-list--column-defs)))
    (if def (nth 0 def) (symbol-name col))))

(defmacro beads-list--define-column-toggle (col key)
  "Define a transient suffix for toggling column COL with KEY."
  (let ((cmd-name (intern (format "beads-list-toggle-column-%s" col))))
    `(transient-define-suffix ,cmd-name ()
       ,(format "Toggle the %s column." col)
       :key ,key
       :description (lambda ()
                      (format "%s %s"
                              (if (beads-list--column-enabled-p ',col) "[x]" "[ ]")
                              (beads-list--column-description ',col)))
       (interactive)
       (beads-list--toggle-column ',col))))

(beads-list--define-column-toggle id "i")
(beads-list--define-column-toggle date "d")
(beads-list--define-column-toggle status "s")
(beads-list--define-column-toggle priority "p")
(beads-list--define-column-toggle type "t")
(beads-list--define-column-toggle title "T")
(beads-list--define-column-toggle assignee "a")
(beads-list--define-column-toggle labels "l")
(beads-list--define-column-toggle deps "D")

(defun beads-list-columns-reset ()
  "Reset columns to global default value."
  (interactive)
  (kill-local-variable 'beads-list-columns)
  (setq tabulated-list-format (beads-list--build-format))
  (tabulated-list-init-header)
  (beads-list-refresh t)
  (message "Columns reset to default"))

(defun beads-list-columns-customize ()
  "Open customize buffer for `beads-list-columns'."
  (interactive)
  (customize-variable 'beads-list-columns))

(defun beads-list-columns-edit ()
  "Edit column list directly in minibuffer."
  (interactive)
  (let* ((available (mapcar #'car beads-list--column-defs))
         (current (mapconcat #'symbol-name beads-list-columns " "))
         (input (read-string "Columns (space-separated): " current))
         (cols (mapcar #'intern (split-string input))))
    (dolist (c cols)
      (unless (memq c available)
        (user-error "Unknown column: %s (available: %s)"
                    c (mapconcat #'symbol-name available ", "))))
    (setq-local beads-list-columns cols)
    (setq tabulated-list-format (beads-list--build-format))
    (tabulated-list-init-header)
    (beads-list-refresh t)))

(transient-define-prefix beads-columns-menu ()
  "Configure list view columns."
  :transient-suffix 'transient--do-call
  ["Columns"
   [""
    :class transient-row
    (beads-list-toggle-column-id)
    (beads-list-toggle-column-date)
    (beads-list-toggle-column-status)
    (beads-list-toggle-column-priority)
    (beads-list-toggle-column-type)]
   [""
    :class transient-row
    (beads-list-toggle-column-deps)
    (beads-list-toggle-column-assignee)
    (beads-list-toggle-column-labels)
    (beads-list-toggle-column-title)]]
  ["Actions"
   ("e" "Edit list directly" beads-list-columns-edit :transient nil)
   ("r" "Reset to default" beads-list-columns-reset :transient nil)
   ("C" "Customize globally" beads-list-columns-customize :transient nil)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(transient-define-prefix beads-config-menu ()
  "Configure beads list view."
  ["Configuration"
   ("c" "Columns..." beads-columns-menu)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(defun beads--mark-menu-description ()
  "Return description for mark menu entry showing count."
  (let ((count (length beads-list--marked)))
    (if (> count 0)
        (format "Mark & Bulk (%d)..." count)
      "Mark & Bulk...")))

(transient-define-prefix beads-mark-menu ()
  "Mark issues and perform bulk operations."
  :transient-suffix 'transient--do-call
  [["Mark"
    ("m" "Mark" beads-list-mark :transient t)
    ("u" "Unmark" beads-list-unmark :transient t)
    ("U" "Unmark all" beads-list-unmark-all :transient t)
    ("t" "Toggle all" beads-list-toggle-marks :transient t)
    ("%" "Regexp (title)" beads-list-mark-regexp :transient t)
    ("*" "Show only marked" beads-list-toggle-marked-filter :transient t)]
   ["Bulk"
    ("s" "Set status" beads-list-bulk-status)
    ("p" "Set priority" beads-list-bulk-priority)
    ("x" "Close" beads-list-bulk-close)
    ("D" "Delete!" beads-list-bulk-delete)]]
  ["Navigation"
   ("q" "Back" transient-quit-one)]
  [""
   :hide always
   ("n" "Next" next-line :transient t)
   ("p" "Prev" previous-line :transient t)
   ("j" "Next" next-line :transient t)
   ("k" "Prev" previous-line :transient t)])

(transient-define-prefix beads-filter-menu ()
  "Beads filter menu."
  ["Filter by"
   ("s" "Status" beads-filter-status)
   ("p" "Priority" beads-filter-priority)
   ("t" "Type" beads-filter-type)
   ("a" "Assignee" beads-filter-assignee)
   ("l" "Label" beads-filter-label)]
  ["Quick filters"
   ("r" "Ready (no blockers)" beads-filter-ready-issues)
   ("b" "Blocked" beads-filter-blocked-issues)]
  ["Clear"
   ("c" "Clear all filters" beads-filter-clear)]
  ["Navigation"
   ("q" "Back" transient-quit-one)])

(transient-define-prefix beads-list-menu ()
  "Beads list mode menu."
  ["Navigation"
   ("g" "Refresh" beads-list-refresh)
   ("P" "Toggle preview" beads-preview-mode)
   ("H" "Dependency tree" beads-hierarchy-show)
   ("S" "Project stats" beads-stats)]
  ["Actions"
   ("c" "Create issue" beads-create-issue)
   ("E" "Edit issue" beads-list-edit-form)
   ("x" "Close issue" beads-close-issue)
   ("R" "Reopen issue" beads-reopen-issue)
   ("D" "Delete issue" beads-delete-issue)
   ("m" "Mark & Bulk..." beads-mark-menu
    :description beads--mark-menu-description)]
  ["Search & Filter"
   ("/" "Search..." beads-search)
   ("f" "Filter menu..." beads-filter-menu)]
  ["Configuration"
   ("C" "Configure..." beads-config-menu)]
  ["Help"
   ("?" "Describe mode" describe-mode)
   ("q" "Quit" transient-quit-one)])

(transient-define-prefix beads-detail-menu ()
  "Beads detail mode menu."
  ["Navigation"
   ("l" "List issues" beads-list)
   ("g" "Refresh" beads-detail-refresh)
   ("H" "Dependency tree" beads-hierarchy-show)
   ("S" "Project stats" beads-stats)]
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
