;;; beads-transient.el --- Transient menus for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, transient

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

;; Transient menus for Beads commands and navigation.

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
(declare-function beads-list-toggle-sort-mode "beads-list")
(declare-function beads-list-reverse-sort "beads-list")

(declare-function beads-filter-by-label "beads-filter")
(declare-function beads-filter-by-parent "beads-filter")
(declare-function beads-filter-ready "beads-filter")
(declare-function beads-filter-blocked "beads-filter")
(declare-function beads-filter-by-search "beads-filter")

(declare-function beads-list "beads-list")
(declare-function beads-list-refresh "beads-list")
(declare-function beads-list-edit-form "beads-list")
(declare-function beads-list--build-format "beads-list")
(declare-function beads-list--column-names "beads-list")
(declare-function beads-list-available-types "beads-list")
(declare-function beads-get-types "beads-list")
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
                                (beads-get-types)
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

(defvar-local beads-create-preview--params nil
  "Parameters for issue creation in preview buffer.")

(defvar beads-create-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-create-preview-confirm)
    (define-key map (kbd "C-c C-k") #'beads-create-preview-cancel)
    (define-key map (kbd "q") #'beads-create-preview-cancel)
    map)
  "Keymap for beads-create-preview-mode.")

(define-derived-mode beads-create-preview-mode special-mode "Beads-Preview"
  "Mode for previewing issue creation.

\\{beads-create-preview-mode-map}")

(defun beads-create-issue-preview ()
  "Preview a new issue before creating it.
Shows what the issue will look like, then press C-c C-c to create."
  (interactive)
  (let* ((title (read-string "Title: "))
         (type (completing-read "Type: "
                                (beads-get-types)
                                nil t "task"))
         (priority-str (completing-read "Priority: "
                                         '("P0" "P1" "P2" "P3" "P4")
                                         nil t "P2"))
         (priority (string-to-number (substring priority-str 1))))
    (if (string-empty-p title)
        (message "Title is required")
      (condition-case err
          (let ((preview (beads-rpc-create title
                                           :issue-type type
                                           :priority priority
                                           :dry-run t)))
            (beads-create-preview--show preview
                                        (list :title title
                                              :issue-type type
                                              :priority priority)))
        (beads-rpc-error
         (message "Failed to preview issue: %s" (error-message-string err)))))))

(defun beads-create-preview--show (preview params)
  "Show PREVIEW issue in buffer with PARAMS for creation."
  (let ((buffer (get-buffer-create "*Beads Create Preview*")))
    (with-current-buffer buffer
      (beads-create-preview-mode)
      (setq beads-create-preview--params params)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Issue Preview\n" 'face 'bold))
        (insert (make-string 40 ?─) "\n\n")
        (insert (propertize "Title: " 'face 'bold)
                (alist-get 'title preview "") "\n")
        (insert (propertize "Type:  " 'face 'bold)
                (alist-get 'issue_type preview "") "\n")
        (insert (propertize "Priority: " 'face 'bold)
                (format "P%d" (alist-get 'priority preview 2)) "\n")
        (insert (propertize "Status: " 'face 'bold)
                (alist-get 'status preview "open") "\n")
        (insert "\n" (make-string 40 ?─) "\n")
        (insert (propertize "C-c C-c" 'face 'help-key-binding)
                " to create, "
                (propertize "C-c C-k" 'face 'help-key-binding)
                " or "
                (propertize "q" 'face 'help-key-binding)
                " to cancel\n")
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun beads-create-preview-confirm ()
  "Create the previewed issue."
  (interactive)
  (let ((params beads-create-preview--params))
    (unless params
      (user-error "No issue to create"))
    (condition-case err
        (let ((issue (apply #'beads-rpc-create
                            (plist-get params :title)
                            (beads--plist-remove params :title))))
          (quit-window t)
          (message "Created issue %s" (alist-get 'id issue))
          (when (derived-mode-p 'beads-list-mode)
            (beads-list-refresh)))
      (beads-rpc-error
       (message "Failed to create issue: %s" (error-message-string err))))))

(defun beads-create-preview-cancel ()
  "Cancel issue creation preview."
  (interactive)
  (quit-window t)
  (message "Cancelled"))

(defun beads--plist-remove (plist key)
  "Return PLIST with KEY removed."
  (let ((result nil))
    (while plist
      (unless (eq (car plist) key)
        (setq result (cons (car plist) (cons (cadr plist) result))))
      (setq plist (cddr plist)))
    (nreverse result)))

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
           (let ((err-msg (error-message-string err)))
             (if (string-match-p "\\(blocker\\|blocked\\|open depend\\)" err-msg)
                 (message "Cannot close %s: has open blockers. Press H to view dependency tree." id)
               (message "Failed to close %s: %s" id err-msg)))))))))

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

(autoload 'beads-orphans "beads-orphans" nil t)
(autoload 'beads-stale "beads-stale" nil t)
(autoload 'beads-activity "beads-activity" nil t)
(autoload 'beads-duplicates "beads-duplicates" nil t)
(autoload 'beads-conflicts "beads-conflicts" nil t)
(autoload 'beads-lint "beads-lint" nil t)

(defun beads-filter-status ()
  "Filter issues by status.
Select a status to filter, or \"all\" to clear the filter."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((choices '("all" "open" "in_progress" "blocked" "hooked" "closed"))
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
  "Filter issues by type.
Includes built-in types and any custom types found in current issues."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((types (beads-list-available-types))
         (choices (cons "all" types))
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

(defun beads-filter-parent ()
  "Filter issues by parent (for epic-scoped views)."
  (interactive)
  (unless (derived-mode-p 'beads-list-mode)
    (user-error "Not in beads list mode"))
  (let* ((issues (beads-rpc-list '(:issue-type "epic")))
         (epics (mapcar (lambda (i)
                          (cons (format "%s: %s"
                                        (alist-get 'id i)
                                        (alist-get 'title i))
                                (alist-get 'id i)))
                        issues))
         (choices (cons '("all" . nil) epics))
         (selection (completing-read "Filter by parent epic: "
                                     (mapcar #'car choices) nil t))
         (parent-id (cdr (assoc selection choices))))
    (setq beads-list--filter
          (when parent-id
            (beads-filter-by-parent parent-id)))
    (beads-list-refresh)
    (if parent-id
        (message "Showing children of %s" parent-id)
      (message "Showing all issues"))))

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
    ("a" "Assign..." beads-list-quick-assign)
    ("A" "Assign to me" beads-list-assign-to-me)
    ("x" "Close" beads-list-bulk-close)
    ("D" "Delete!" beads-list-bulk-delete)]]
  ["Navigation"
   ("q" "Back" transient-quit-one)]
  [""
   :hide always
   ("n" "Next" next-line :transient t)
   ("p" "Prev" previous-line :transient t)
   ("j" "Next" next-line :transient t)
   ("k" "Prev" previous-line :transient t)
   ("<down>" "Next" next-line :transient t)
   ("<up>" "Prev" previous-line :transient t)
   ("C-n" "Next" next-line :transient t)
   ("C-p" "Prev" previous-line :transient t)])

(defvar-local beads-list--sort-mode-override nil)
(defvar tabulated-list-sort-key)

(defun beads-sort-by-column (column &optional descending)
  "Sort list by COLUMN.  When DESCENDING is non-nil, reverse order."
  (setq beads-list--sort-mode-override 'column)
  (setq tabulated-list-sort-key (cons column descending))
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Sorted by %s%s" column (if descending " (descending)" "")))

(defun beads-sort-by-id ()
  "Sort issues by ID."
  (interactive)
  (beads-sort-by-column "ID"))

(defun beads-sort-by-date ()
  "Sort issues by date (newest first)."
  (interactive)
  (beads-sort-by-column "Date" t))

(defun beads-sort-by-status ()
  "Sort issues by status."
  (interactive)
  (beads-sort-by-column "Status"))

(defun beads-sort-by-priority ()
  "Sort issues by priority (highest first)."
  (interactive)
  (beads-sort-by-column "Priority"))

(defun beads-sort-by-type ()
  "Sort issues by type."
  (interactive)
  (beads-sort-by-column "Type"))

(defun beads-sort-by-title ()
  "Sort issues by title."
  (interactive)
  (beads-sort-by-column "Title"))

(defun beads-sort-sectioned ()
  "Use sectioned sort (unblocked/blocked/closed groups)."
  (interactive)
  (setq beads-list--sort-mode-override 'sectioned)
  (beads-list-refresh t)
  (message "Sort mode: sectioned"))

(defun beads--sort-menu-description ()
  "Return description for sort menu showing current sort."
  (if (eq beads-list--sort-mode-override 'sectioned)
      "Sort: sectioned"
    (let ((key (car tabulated-list-sort-key))
          (desc (cdr tabulated-list-sort-key)))
      (format "Sort: %s%s" (or key "default") (if desc " ↓" " ↑")))))

(transient-define-prefix beads-sort-menu ()
  "Beads sort menu."
  [["Sort by Column"
    ("i" "ID" beads-sort-by-id)
    ("d" "Date" beads-sort-by-date)
    ("s" "Status" beads-sort-by-status)
    ("p" "Priority" beads-sort-by-priority)
    ("t" "Type" beads-sort-by-type)
    ("T" "Title" beads-sort-by-title)]
   ["Sort Mode"
    ("S" "Sectioned (default)" beads-sort-sectioned)
    ("r" "Reverse direction" beads-list-reverse-sort)
    ""
    ("q" "Back" transient-quit-one)]])

(transient-define-prefix beads-filter-menu ()
  "Beads filter menu."
  [["Filter by"
    ("s" "Status" beads-filter-status)
    ("p" "Priority" beads-filter-priority)
    ("t" "Type" beads-filter-type)
    ("a" "Assignee" beads-filter-assignee)
    ("l" "Label" beads-filter-label)
    ("e" "Parent epic" beads-filter-parent)]
   ["Quick Filters"
    ("r" "Ready (no blockers)" beads-filter-ready-issues)
    ("b" "Blocked" beads-filter-blocked-issues)
    ""
    ("c" "Clear all filters" beads-filter-clear)
    ""
    ("q" "Back" transient-quit-one)]])

(autoload 'beads-types-edit "beads-types" nil t)

(transient-define-prefix beads-list-menu ()
  "Beads list mode menu."
  [["Navigation"
    ("g" "Refresh" beads-list-refresh)
    ("RET" "View issue" beads-list-goto-issue)
    ("P" "Toggle preview" beads-preview-mode)
    ("H" "Dependency tree" beads-hierarchy-show)
    ("S" "Project stats" beads-stats)
    ("T" "Configure types" beads-types-edit)]
   ["Actions"
    ("c" "Create issue" beads-create-issue)
    ("C" "Create with preview" beads-create-issue-preview)
    ("E" "Edit issue" beads-list-edit-form)
    ("x" "Close issue" beads-close-issue)
    ("R" "Reopen issue" beads-reopen-issue)
    ("D" "Delete issue" beads-delete-issue)]
   ["Search & Filter"
    ("/" "Search..." beads-search)
    ("f" "Filter menu..." beads-filter-menu)
    ("o" "Sort menu..." beads-sort-menu
     :description beads--sort-menu-description)
    ("m" "Mark & Bulk..." beads-mark-menu
     :description beads--mark-menu-description)]]
  [["Views"
    ("O" "Orphaned issues" beads-orphans)
    ("A" "Stale issues" beads-stale)
    ("a" "Activity feed" beads-activity)
    ("d" "Duplicates" beads-duplicates)
    ("L" "Lint report" beads-lint)
    ("X" "Resolve conflicts" beads-conflicts)]
   [""
    ("?" "Describe mode" describe-mode)
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix beads-detail-menu ()
  "Beads detail mode menu."
  [["Navigation"
    ("l" "List issues" beads-list)
    ("g" "Refresh" beads-detail-refresh)
    ("P" "Go to parent" beads-detail-goto-parent)
    ("C" "View children" beads-detail-view-children)
    ("H" "Dependency tree" beads-hierarchy-show)
    ("S" "Project stats" beads-stats)]
   ["Edit"
    ("E" "Edit form" beads-detail-edit-form)
    ("e d" "Description" beads-detail-edit-description)
    ("e s" "Status" beads-detail-edit-status)
    ("e p" "Priority" beads-detail-edit-priority)
    ("e t" "Title" beads-detail-edit-title)]
   ["Actions"
    ("c" "Add comment" beads-detail-add-comment)
    ("x" "Close issue" beads-close-issue)
    ("R" "Reopen issue" beads-reopen-issue)
    ("D" "Delete issue" beads-delete-issue)
    ""
    ("?" "Describe mode" describe-mode)
    ("q" "Quit" transient-quit-one)]])

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
