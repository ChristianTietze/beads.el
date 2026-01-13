;;; beads-list.el --- Issue list mode for Beads -*- lexical-binding: t -*-

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

;; Tabulated issue list mode with sorting, filtering, and bulk operations.

;;; Code:

(require 'beads-rpc)
(require 'beads-detail)
(require 'beads-filter)
(require 'beads-preview)
(require 'beads-faces)
(require 'tabulated-list)
(require 'seq)

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
  "Whether to show statistics in the mode line.
When non-nil, displays issue counts (total, open, blocked, ready)
in the mode line of the list view."
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

(define-obsolete-variable-alias 'beads-list-type-style 'beads-type-style "0.47")
(define-obsolete-variable-alias 'beads-list-type-glyph 'beads-type-glyph "0.47")

(defcustom beads-list-sort-mode 'sectioned
  "How to sort issues in the list view.
When `sectioned', group issues into three sections:
  1. Unblocked (open/in_progress) - sorted by priority
  2. Blocked - sorted by priority
  3. Completed (closed) - sorted by completion date
When `column', use standard tabulated-list column sorting."
  :type '(choice (const :tag "Sectioned (unblocked/blocked/closed)" sectioned)
                 (const :tag "Column-based" column))
  :group 'beads-list)

(defcustom beads-list-section-separators t
  "Whether to show visual separators between sections.
Only applies when `beads-list-sort-mode' is `sectioned'."
  :type 'boolean
  :group 'beads-list)

(defcustom beads-list-id-column-max-width 15
  "Maximum width for the ID column in beads list view.
When nil, the column width is unlimited and adjusts to the longest ID.
When an integer, the column width will not exceed this value."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Maximum width"))
  :group 'beads-list)

(defconst beads-builtin-types
  '("bug" "feature" "task" "epic" "chore" "gate" "convoy" "agent" "role" "rig")
  "List of built-in issue types supported by beads.")

(define-obsolete-face-alias 'beads-list-status-open 'beads-status-open "0.47")
(define-obsolete-face-alias 'beads-list-status-in-progress 'beads-status-in-progress "0.47")
(define-obsolete-face-alias 'beads-list-status-closed 'beads-status-closed "0.47")
(define-obsolete-face-alias 'beads-list-status-blocked 'beads-status-blocked "0.47")
(define-obsolete-face-alias 'beads-list-status-hooked 'beads-status-hooked "0.47")
(define-obsolete-face-alias 'beads-list-priority-p0 'beads-priority-p0 "0.47")
(define-obsolete-face-alias 'beads-list-priority-p1 'beads-priority-p1 "0.47")

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

(define-obsolete-face-alias 'beads-list-type-gate 'beads-type-gate "0.47")
(define-obsolete-face-alias 'beads-list-type-convoy 'beads-type-convoy "0.47")
(define-obsolete-face-alias 'beads-list-type-agent 'beads-type-agent "0.47")
(define-obsolete-face-alias 'beads-list-type-role 'beads-type-role "0.47")
(define-obsolete-face-alias 'beads-list-type-rig 'beads-type-rig "0.47")

(defface beads-list-row-p0
  '((((class color) (background light))
     :background "#ffe0e0" :extend t)
    (((class color) (background dark))
     :background "#4a1a1a" :extend t))
  "Face for entire row of P0 priority issues.
Uses `:extend t' to highlight to end of line."
  :group 'beads-list)

(defcustom beads-list-highlight-p0-rows t
  "Whether to highlight entire rows for P0 priority issues.
When non-nil, P0 issues get a distinctive background color
across the entire row for maximum visibility."
  :type 'boolean
  :group 'beads-list)

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

(defun beads-list--build-format (&optional id-width)
  "Build `tabulated-list-format' from `beads-list-columns'.
Automatically prepends the mark column.
When ID-WIDTH is provided, use it instead of the default for the id column."
  (vconcat
   (cons (list " " 1 nil)
         (mapcar (lambda (col)
                   (let ((def (alist-get col beads-list--column-defs)))
                     (if def
                         (list (nth 0 def)
                               (if (and id-width (eq col 'id))
                                   id-width
                                 (nth 1 def))
                               (nth 2 def))
                       (error "Unknown column: %s" col))))
                 beads-list-columns))))

(defun beads-list--max-id-width (issues)
  "Return the ID column width for ISSUES.
Computes the maximum ID length with a minimum of 5.
Respects `beads-list-id-column-max-width' when set."
  (let ((max-len (seq-reduce (lambda (acc issue)
                               (max acc (length (alist-get 'id issue))))
                             issues 0)))
    (setq max-len (max 5 max-len))
    (if beads-list-id-column-max-width
        (min max-len beads-list-id-column-max-width)
      max-len)))

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

(defvar-local beads-list--sort-mode-override nil
  "Buffer-local override for `beads-list-sort-mode'.
When non-nil, overrides the global setting for this buffer.")

(defvar-local beads-list--section-overlays nil
  "List of overlays used for section separators.")

(defun beads-list--effective-sort-mode ()
  "Return the effective sort mode for this buffer."
  (or beads-list--sort-mode-override beads-list-sort-mode))

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
    (define-key map (kbd "a") #'beads-list-quick-assign)
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
    (define-key map (kbd "s") #'beads-list-toggle-sort-mode)
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
    (define-key map (kbd "a") #'beads-list-quick-assign)
    (define-key map (kbd "A") #'beads-list-assign-to-me)
    (define-key map (kbd "q") #'beads-list-quit)
    (define-key map (kbd "?") #'beads-menu)
    (define-key map (kbd "C-c m") #'beads-menu)
    (define-key map (kbd "M-n") #'beads-list-next-section)
    (define-key map (kbd "M-p") #'beads-list-previous-section)
    map)
  "Keymap for beads-list-mode.")

(defun beads-list--row-face-for-id (id)
  "Return row face for issue ID, or nil if no special styling needed."
  (when beads-list-highlight-p0-rows
    (when-let ((issue (seq-find (lambda (i) (equal (alist-get 'id i) id))
                                beads-list--issues)))
      (when (= 0 (alist-get 'priority issue 2))
        'beads-list-row-p0))))

(defun beads-list--print-entry (id cols)
  "Print entry ID with COLS, applying row-level styling for P0 issues."
  (let ((beg (point)))
    (tabulated-list-print-entry id cols)
    (when-let ((row-face (beads-list--row-face-for-id id)))
      (font-lock-prepend-text-property beg (point) 'face row-face))))

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

(defun beads-list--update-mode-line ()
  "Update the mode line with current stats.
Respects `beads-list-show-header-stats'."
  (if beads-list-show-header-stats
      (condition-case nil
          (let ((stats (beads-rpc-stats)))
            (setq mode-line-format
                  `(" "
                    mode-line-buffer-identification
                    "  "
                    ,(beads-list--format-header-line stats))))
        (beads-rpc-error
         (setq mode-line-format (default-value 'mode-line-format))))
    (setq mode-line-format (default-value 'mode-line-format))))

(define-derived-mode beads-list-mode tabulated-list-mode "Beads-List"
  "Major mode for displaying Beads issues in a table.

\\{beads-list-mode-map}"
  (setq tabulated-list-format (beads-list--build-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Date" t))
  (setq tabulated-list-printer #'beads-list--print-entry)
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
                                 issues))
               (effective-sort-mode (beads-list--effective-sort-mode))
               (sorted-issues (if (eq effective-sort-mode 'sectioned)
                                  (beads-list--sectioned-sort display-issues)
                                display-issues))
               (id-width (beads-list--max-id-width display-issues)))
          (setq beads-list--issues (append issues nil))
          (setq tabulated-list-format (beads-list--build-format id-width))
          (tabulated-list-init-header)
          (when (eq effective-sort-mode 'sectioned)
            (setq tabulated-list-sort-key nil))
          (setq tabulated-list-entries (beads-list-entries sorted-issues))
          (tabulated-list-print t)
          (beads-list--add-section-separators)
          (if saved-id
              (unless (beads-list-goto-id saved-id)
                (goto-char (point-min))
                (forward-line (1- (min saved-line (line-number-at-pos (point-max))))))
            (goto-char (point-min)))
          (when-let ((win (get-buffer-window (current-buffer))))
            (set-window-start win (min saved-start (point-max))))
          (beads-list--update-mode-line)
          (unless silent
            (let ((filter-msg (if beads-list--filter
                                  (format " [%s]" (beads-filter-name beads-list--filter))
                                ""))
                  (sort-msg (if (eq effective-sort-mode 'sectioned)
                                " (sectioned)"
                              "")))
              (message "Refreshed %d issues%s%s" (length beads-list--issues) filter-msg sort-msg))))
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

(defun beads-list--issue-section (issue)
  "Return section number for ISSUE: 0=unblocked, 1=blocked, 2=closed."
  (let ((status (alist-get 'status issue)))
    (cond
     ((string= status "closed") 2)
     ((string= status "blocked") 1)
     (t 0))))

(defun beads-list--sectioned-sort (issues)
  "Sort ISSUES into sections: unblocked, blocked, closed.
ISSUES can be a list or vector.
Within unblocked and blocked sections, sort by priority (ascending).
Within closed section, sort by closed_at date (most recent first)."
  (let ((unblocked nil)
        (blocked nil)
        (closed nil))
    (seq-doseq (issue issues)
      (pcase (beads-list--issue-section issue)
        (0 (push issue unblocked))
        (1 (push issue blocked))
        (2 (push issue closed))))
    (setq unblocked (sort unblocked
                          (lambda (a b)
                            (let ((status-a (alist-get 'status a))
                                  (status-b (alist-get 'status b))
                                  (prio-a (alist-get 'priority a 2))
                                  (prio-b (alist-get 'priority b 2)))
                              (cond
                               ((and (string= status-a "in_progress")
                                     (not (string= status-b "in_progress")))
                                t)
                               ((and (not (string= status-a "in_progress"))
                                     (string= status-b "in_progress"))
                                nil)
                               (t (< prio-a prio-b)))))))
    (setq blocked (sort blocked
                        (lambda (a b)
                          (< (alist-get 'priority a 2)
                             (alist-get 'priority b 2)))))
    (setq closed (sort closed
                       (lambda (a b)
                         (let ((date-a (or (alist-get 'closed_at a) ""))
                               (date-b (or (alist-get 'closed_at b) "")))
                           (string> date-a date-b)))))
    (append unblocked blocked closed)))

(defun beads-list--clear-section-overlays ()
  "Remove all section separator overlays."
  (mapc #'delete-overlay beads-list--section-overlays)
  (setq beads-list--section-overlays nil))

(defun beads-list--add-section-separators ()
  "Add visual separators between sections after printing.
Only adds separators when in sectioned sort mode and
`beads-list-section-separators' is non-nil."
  (beads-list--clear-section-overlays)
  (when (and beads-list-section-separators
             (eq (beads-list--effective-sort-mode) 'sectioned))
    (save-excursion
      (goto-char (point-min))
      (let ((prev-section nil))
        (while (not (eobp))
          (when-let* ((id (tabulated-list-get-id))
                      (issue (seq-find (lambda (i) (equal (alist-get 'id i) id))
                                       beads-list--issues))
                      (section (beads-list--issue-section issue)))
            (when (and prev-section (> section prev-section))
              (let* ((ov (make-overlay (line-beginning-position)
                                       (line-beginning-position)))
                     (label (pcase section
                              (1 "Blocked")
                              (2 "Completed")))
                     (separator (propertize (format "\n  ── %s ──\n" label)
                                            'face 'shadow)))
                (overlay-put ov 'before-string separator)
                (push ov beads-list--section-overlays)))
            (setq prev-section section))
          (forward-line 1))))))

(defun beads-list--section-positions ()
  "Return sorted list of section start positions.
Includes `point-min' for the first section and overlay positions
for subsequent sections."
  (let ((positions (list (point-min))))
    (dolist (ov beads-list--section-overlays)
      (when (overlay-buffer ov)
        (push (overlay-start ov) positions)))
    (sort positions #'<)))

(defun beads-list-next-section ()
  "Move point to the next section in the beads list."
  (interactive)
  (let* ((positions (beads-list--section-positions))
         (next (seq-find (lambda (pos) (> pos (point))) positions)))
    (when next
      (goto-char next))))

(defun beads-list-previous-section ()
  "Move point to the previous section in the beads list."
  (interactive)
  (let* ((positions (reverse (beads-list--section-positions)))
         (prev (seq-find (lambda (pos) (< pos (point))) positions)))
    (when prev
      (goto-char prev))))

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

(defun beads-list--has-active-filter ()
  "Return non-nil if any filter is currently active."
  (or beads-list--filter beads-list--show-only-marked))

(defun beads-list-quit ()
  "Quit beads list, clearing filters progressively.
First clears active filters, then closes preview, then quits window."
  (interactive)
  (cond
   ((beads-list--has-active-filter)
    (setq beads-list--filter nil)
    (setq beads-list--show-only-marked nil)
    (beads-list-refresh t)
    (message "Filter cleared"))
   (beads-preview-mode
    (beads-preview-mode -1))
   (t
    (quit-window))))

(defun beads-list-toggle-sort-mode ()
  "Toggle between sectioned and column sort modes."
  (interactive)
  (setq beads-list--sort-mode-override
        (if (eq (beads-list--effective-sort-mode) 'sectioned)
            'column
          'sectioned))
  (if (eq beads-list--sort-mode-override 'column)
      (setq tabulated-list-sort-key (cons "Date" t)))
  (beads-list-refresh t)
  (message "Sort mode: %s" beads-list--sort-mode-override))

(defun beads-list-cycle-sort ()
  "Cycle through sort columns.
If in sectioned mode, first switches to column mode."
  (interactive)
  (when (eq (beads-list--effective-sort-mode) 'sectioned)
    (setq beads-list--sort-mode-override 'column)
    (setq tabulated-list-sort-key (cons "Date" nil)))
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
  "Reverse the current sort direction.
If in sectioned mode, first switches to column mode."
  (interactive)
  (when (eq (beads-list--effective-sort-mode) 'sectioned)
    (setq beads-list--sort-mode-override 'column)
    (setq tabulated-list-sort-key (cons "Date" nil)))
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
   (list (completing-read "Status: " '("open" "in_progress" "blocked" "hooked" "closed") nil t)))
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
            (errors 0)
            (blocked-ids nil))
        (dolist (id ids)
          (condition-case err
              (progn
                (beads-rpc-close id)
                (setq count (1+ count)))
            (beads-rpc-error
             (setq errors (1+ errors))
             (when (string-match-p "\\(blocker\\|blocked\\|open depend\\)"
                                   (error-message-string err))
               (push id blocked-ids)))))
        (setq beads-list--marked nil)
        (beads-list-refresh t)
        (cond
         (blocked-ids
          (message "Closed %d issue(s), %d blocked (press H on issue to view blockers)"
                   count (length blocked-ids)))
         ((> errors 0)
          (message "Closed %d issue(s), %d error(s)" count errors))
         (t
          (message "Closed %d issue(s)" count)))))))

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

(defun beads-list--collect-assignees ()
  "Collect unique assignees from current issue list."
  (let ((assignees nil))
    (dolist (issue beads-list--issues)
      (when-let ((assignee (alist-get 'assignee issue)))
        (unless (or (string-empty-p assignee)
                    (member assignee assignees))
          (push assignee assignees))))
    (sort assignees #'string<)))

(defun beads-list--collect-types ()
  "Collect unique issue types from current issue list."
  (let ((types nil))
    (dolist (issue beads-list--issues)
      (when-let ((type (alist-get 'issue_type issue)))
        (unless (or (string-empty-p type)
                    (member type types))
          (push type types))))
    (sort types #'string<)))

(defun beads-list-available-types ()
  "Return list of available issue types.
Combines built-in types with any custom types found in current issues."
  (let ((custom-types (beads-list--collect-types)))
    (sort (seq-uniq (append beads-builtin-types custom-types)) #'string<)))

(defun beads-list-quick-assign (assignee)
  "Assign ASSIGNEE to marked issues or issue at point.
With completion for known assignees from current issues."
  (interactive
   (let* ((known (beads-list--collect-assignees))
          (user (or (getenv "USER") (getenv "USERNAME") "me"))
          (choices (delete-dups (cons user known))))
     (list (completing-read "Assign to: " choices nil nil))))
  (let ((ids (beads-list--get-marked-or-at-point)))
    (unless ids
      (user-error "No issues marked or at point"))
    (let ((count 0)
          (errors 0))
      (dolist (id ids)
        (condition-case nil
            (progn
              (beads-rpc-update id :assignee assignee)
              (setq count (1+ count)))
          (beads-rpc-error
           (setq errors (1+ errors)))))
      (setq beads-list--marked nil)
      (beads-list-refresh t)
      (if (> errors 0)
          (message "Assigned %d issue(s) to %s, %d error(s)" count assignee errors)
        (message "Assigned %d issue(s) to %s" count assignee)))))

(defun beads-list-assign-to-me ()
  "Assign marked issues or issue at point to current user."
  (interactive)
  (let ((user (or (getenv "USER") (getenv "USERNAME") "me")))
    (beads-list-quick-assign user)))

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
               '("open" "in_progress" "blocked" "hooked" "closed"))
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
               '("bug" "feature" "task" "epic" "chore" "gate" "convoy" "agent" "role"))
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
