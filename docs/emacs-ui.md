# Emacs UI Architecture Reference

This document summarizes the multi-pane UI architecture from vikunja-helper as a reference for implementing beads.el.

## Overview

The vikunja-helper implements a Finder-like three-pane layout:
- **Left pane**: Project hierarchy
- **Middle pane**: Task list (tabulated-list-mode)
- **Right pane**: Task detail preview

Key features to replicate for beads.el:
- Issue list with filtering
- Detail view in split pane
- "Browse mode" (quicklook) where selection changes auto-update the preview
- transient.el menus for user-friendly commands

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    Beads Daemon (RPC)                       │
│               Unix Socket (.beads/bd.sock)                  │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                    beads.el Client                          │
│              JSON-over-socket communication                 │
└────────────────────────┬────────────────────────────────────┘
                         │
        ┌────────────────┼────────────────┐
        │                │                │
        ▼                ▼                ▼
   ┌─────────┐    ┌──────────┐    ┌───────────────┐
   │ Filter  │    │ State    │    │ Issue List    │
   │ System  │    │ Manager  │    │ (table)       │
   └────┬────┘    └────┬─────┘    └────┬──────────┘
        │              │               │
        └──────────────┼───────────────┘
                       │
                       ▼
           ┌───────────────────────┐
           │   Preview Controller  │
           │   (post-command-hook) │
           └───────────┬───────────┘
                       │
           ┌───────────┴───────────┐
           │                       │
           ▼                       ▼
    ┌────────────┐         ┌────────────┐
    │ Issue List │         │ Issue      │
    │ Buffer     │         │ Detail     │
    └────────────┘         └────────────┘
```

## 1. Window/Buffer Management

Use `display-buffer-alist` for side-window management:

```elisp
(defun beads--configure-display-buffer ()
  "Configure display-buffer-alist for preview window."
  (add-to-list 'display-buffer-alist
               '((derived-mode . beads-detail-mode)
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.4)
                 (dedicated . t)
                 (window-parameters . ((no-delete-other-windows . t))))))
```

Pane detection by checking window properties and buffer major modes:

```elisp
(defun beads--get-current-pane ()
  "Determine which pane is currently active."
  (let ((mode major-mode))
    (cond
     ((eq mode 'beads-list-mode) 'list)
     ((eq mode 'beads-detail-mode) 'detail)
     (t nil))))
```

## 2. List Display with tabulated-list-mode

Define columns as data structures with pluggable formatters:

```elisp
(defconst beads--column-definitions
  '((id        . (:name "ID" :width 10 :sortable t :formatter beads--format-id))
    (title     . (:name "Title" :width 50 :sortable t :formatter beads--format-title))
    (status    . (:name "Status" :width 12 :sortable t :formatter beads--format-status))
    (priority  . (:name "Pri" :width 4 :sortable t :formatter beads--format-priority))
    (type      . (:name "Type" :width 8 :sortable t :formatter beads--format-type))
    (assignee  . (:name "Assignee" :width 12 :sortable t :formatter beads--format-assignee))))

(defun beads--build-tabulated-list-format ()
  "Build tabulated-list-format vector from column definitions."
  (vconcat
   (mapcar (lambda (col)
             (let ((def (cdr col)))
               (list (plist-get def :name)
                     (plist-get def :width)
                     (plist-get def :sortable))))
           beads--column-definitions)))

(defun beads--make-entry (issue)
  "Create a tabulated-list entry for ISSUE."
  (let ((id (alist-get 'id issue)))
    (list id
          (vconcat
           (mapcar (lambda (col)
                     (let* ((def (cdr col))
                            (formatter (plist-get def :formatter)))
                       (funcall formatter issue)))
                   beads--column-definitions)))))
```

## 3. Browse Mode / Quick Look (Automatic Preview)

The key pattern: use `post-command-hook` to trigger preview updates as cursor moves.

```elisp
(defvar beads-preview--timer nil)
(defvar beads-preview--current-issue-id nil)
(defvar beads-preview--buffers nil)
(defcustom beads-preview-delay 0.1 "Delay before showing preview.")
(defcustom beads-preview-max-buffers 3 "Max preview buffers to keep.")

(define-minor-mode beads-preview-mode
  "Enable automatic issue preview in issue list."
  :lighter " Preview"
  (if beads-preview-mode
      (progn
        (add-hook 'post-command-hook #'beads-preview-trigger nil t)
        (beads-preview-trigger))
    (beads-preview--cancel-timer)
    (beads-preview--cleanup)
    (remove-hook 'post-command-hook #'beads-preview-trigger t)))

(defun beads-preview-trigger ()
  "Trigger issue preview after cursor movement."
  (when (and beads-preview-mode
             (eq major-mode 'beads-list-mode))
    (if-let ((issue (beads--get-issue-at-point)))
        (beads-preview--start-timer issue)
      (beads-preview--cancel-timer))))

(defun beads-preview--start-timer (issue)
  "Start timer to preview ISSUE after delay (debouncing)."
  (beads-preview--cancel-timer)
  (setq beads-preview--timer
        (run-with-idle-timer beads-preview-delay nil
                             #'beads-preview--display-issue issue)))

(defun beads-preview--display-issue (issue)
  "Display preview for ISSUE in side window."
  (when issue
    (let ((issue-id (alist-get 'id issue)))
      (unless (equal issue-id beads-preview--current-issue-id)
        (setq beads-preview--current-issue-id issue-id)
        (when-let* ((buffer (beads-preview--get-detail-buffer issue))
                    (window (display-buffer buffer
                              '((display-buffer-in-side-window)
                                (side . right)
                                (window-width . 0.4)))))
          (beads-preview--cleanup-buffers))))))

(defun beads-preview--cleanup-buffers ()
  "Clean up old preview buffers, keeping only recent ones."
  (let ((live-buffers (seq-filter #'buffer-live-p beads-preview--buffers)))
    (when (> (length live-buffers) beads-preview-max-buffers)
      (dolist (buf (nthcdr beads-preview-max-buffers live-buffers))
        (when (buffer-live-p buf)
          (kill-buffer buf))))
    (setq beads-preview--buffers
          (seq-take live-buffers beads-preview-max-buffers))))
```

## 4. Detail View

Generate formatted content with faces for visual hierarchy:

```elisp
(defun beads-detail--generate-content (issue)
  "Generate formatted content for ISSUE."
  (with-temp-buffer
    (insert (propertize (alist-get 'title issue)
                        'face 'beads-header)
            "\n"
            (beads-detail--format-status issue)
            "\n\n")
    (beads-detail--print-field "ID" (alist-get 'id issue))
    (beads-detail--print-field "Type" (alist-get 'issue_type issue))
    (beads-detail--print-field "Priority" (beads--priority-name (alist-get 'priority issue)))
    (beads-detail--print-field "Status" (alist-get 'status issue))
    (beads-detail--print-field "Assignee" (or (alist-get 'assignee issue) "Unassigned"))
    (insert "\n")
    (when-let ((desc (alist-get 'description issue)))
      (insert (propertize "Description\n" 'face 'beads-section-header))
      (insert desc "\n"))
    (buffer-string)))

(defun beads-detail--print-field (label value)
  "Print LABEL: VALUE with right-aligned label."
  (insert (propertize (format "%12s: " label) 'face 'beads-field-label)
          (format "%s\n" (or value ""))))
```

## 5. Filter System (Higher-Order Composition)

Implement filters as composable functions (SICP pattern):

```elisp
(defun beads-filter-make (name predicate-fn &optional config)
  "Create a filter with NAME and PREDICATE-FN."
  (list :name name :predicate predicate-fn :config (or config '())))

(defun beads-filter-apply (filter issues)
  "Apply FILTER to ISSUES."
  (seq-filter (plist-get filter :predicate) issues))

(defun beads-filter-by-status (status)
  "Create filter for STATUS (open, in_progress, closed)."
  (beads-filter-make
   (format "status-%s" status)
   (lambda (issue)
     (string= (alist-get 'status issue) status))))

(defun beads-filter-by-priority (priority)
  "Create filter for PRIORITY (0-4)."
  (beads-filter-make
   (format "priority-%d" priority)
   (lambda (issue)
     (= (alist-get 'priority issue) priority))))

(defun beads-filter-by-type (type)
  "Create filter for TYPE (bug, feature, task, epic, chore)."
  (beads-filter-make
   (format "type-%s" type)
   (lambda (issue)
     (string= (alist-get 'issue_type issue) type))))

(defun beads-filter-compose (&rest filters)
  "Compose FILTERS with logical AND."
  (beads-filter-make
   (mapconcat (lambda (f) (plist-get f :name)) filters "-")
   (lambda (issue)
     (seq-every-p (lambda (f) (funcall (plist-get f :predicate) issue))
                  filters))))

(defun beads-filter-apply-pipeline (issues &rest filters)
  "Apply FILTERS to ISSUES in pipeline."
  (let ((result issues))
    (dolist (filter filters result)
      (setq result (beads-filter-apply filter result)))))
```

## 6. Transient Menus

```elisp
(require 'transient)

(transient-define-prefix beads-menu ()
  "Main menu for beads issue tracker."
  :refresh-suffixes t
  [["View"
    ("v o" "Open" beads--set-view-open
     :description (lambda ()
                    (if (eq beads--current-view 'open) "● Open" "○ Open"))
     :transient t)
    ("v p" "In Progress" beads--set-view-in-progress :transient t)
    ("v c" "Closed" beads--set-view-closed :transient t)
    ("v a" "All" beads--set-view-all :transient t)]

   ["Filter"
    ("f t" "By Type" beads--filter-by-type :transient t)
    ("f p" "By Priority" beads--filter-by-priority :transient t)
    ("f a" "By Assignee" beads--filter-by-assignee :transient t)
    ("f c" "Clear Filters" beads--clear-filters :transient t)]

   ["Actions"
    ("c" "Create Issue" beads-create-issue)
    ("e" "Edit Issue" beads-edit-issue)
    ("d" "Close Issue" beads-close-issue)
    ("r" "Refresh" beads-refresh :transient t)]]

  [["Navigation"
    ("RET" "View Details" beads-view-details)
    ("v" "Toggle Preview" beads-preview-toggle :transient t)
    ("g" "Refresh" beads-refresh :transient t)
    ("q" "Quit" quit-window)]])
```

## 7. State Management (Closure Pattern)

Encapsulate mutable state cleanly using closures:

```elisp
(defun beads-state-create ()
  "Create state manager using closure."
  (let ((view 'open)
        (type-filter nil)
        (priority-filter nil)
        (assignee-filter nil))
    (lambda (message &rest args)
      (pcase message
        ('get-view view)
        ('set-view (setq view (car args)))
        ('get-type-filter type-filter)
        ('set-type-filter (setq type-filter (car args)))
        ('get-priority-filter priority-filter)
        ('set-priority-filter (setq priority-filter (car args)))
        ('get-assignee-filter assignee-filter)
        ('set-assignee-filter (setq assignee-filter (car args)))
        ('clear-filters (setq type-filter nil
                              priority-filter nil
                              assignee-filter nil))))))

(defvar beads--state nil "Global state manager.")

(defun beads-state-get-view ()
  "Get current view state."
  (unless beads--state (setq beads--state (beads-state-create)))
  (funcall beads--state 'get-view))

(defun beads-state-set-view (view)
  "Set view state."
  (unless beads--state (setq beads--state (beads-state-create)))
  (funcall beads--state 'set-view view))
```

## 8. Window Helpers

Safe window operations with guaranteed cleanup:

```elisp
(defun beads--with-window-selection (window-or-fn &rest body)
  "Execute BODY with target window selected, restore original after."
  (when-let ((target (if (functionp window-or-fn)
                         (funcall window-or-fn)
                       window-or-fn)))
    (let ((original (selected-window)))
      (unwind-protect
          (progn
            (select-window target)
            (apply #'funcall body))
        (when (window-live-p original)
          (select-window original))))))
```

Mark preview windows for reliable identification:

```elisp
(defun beads-preview--set-window-parameters (window)
  "Mark WINDOW as a preview window."
  (set-window-parameter window 'beads-preview t)
  (set-window-parameter window 'dedicated t))

(defun beads-preview--get-windows ()
  "Return all preview windows."
  (seq-filter (lambda (w) (window-parameter w 'beads-preview))
              (window-list)))
```

## 9. Observer Pattern

Decouple cross-component communication:

```elisp
(defvar beads--filter-change-observers nil)

(defun beads--add-observer (observer)
  "Add OBSERVER for filter changes."
  (unless (memq observer beads--filter-change-observers)
    (push observer beads--filter-change-observers)))

(defun beads--notify-filter-changed ()
  "Notify all observers of filter change."
  (dolist (observer beads--filter-change-observers)
    (funcall observer)))
```

## 10. Mode Definition

```elisp
(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'beads-view-details)
    (define-key map (kbd "m") #'beads-menu)
    (define-key map (kbd "v") #'beads-preview-toggle)
    (define-key map (kbd "r") #'beads-refresh)
    (define-key map (kbd "g") #'beads-refresh)
    (define-key map (kbd "c") #'beads-create-issue)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "<right>") #'beads-move-to-detail)
    (define-key map (kbd "<left>") #'beads-move-to-list)
    map))

(define-derived-mode beads-list-mode tabulated-list-mode "Beads"
  "Major mode for viewing beads issues."
  (setq tabulated-list-format (beads--build-tabulated-list-format))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (hl-line-mode 1))
```

## 11. Key Patterns Summary

| Pattern | Purpose | Implementation |
|---------|---------|----------------|
| **post-command-hook** | Reactive preview updates | Hook triggers on every cursor move, debounced with idle timer |
| **display-buffer-alist** | Window layout control | Side windows with dedicated flag |
| **Column definitions as data** | DRY table setup | Plist with :name, :width, :formatter |
| **Filter composition** | Flexible filtering | Higher-order functions, pipeline application |
| **Buffer pool** | Memory management | Keep N most recent buffers, kill older ones |
| **Observer pattern** | Decoupled updates | Notify functions when state changes |
| **Closure state** | Encapsulate mutable state | Message-passing interface to closure |
| **Window parameters** | Identify special windows | Set/check parameters like `'beads-preview` |
| **unwind-protect** | Safe window operations | Guarantee cleanup after temporary selection |

## 12. Implementation Checklist

- [ ] **RPC Layer**: JSON-over-socket communication with daemon
- [ ] **List Mode**: `tabulated-list-mode` with column definitions
- [ ] **Detail Mode**: Formatted content generation with faces
- [ ] **Preview System**:
  - [ ] `post-command-hook` for selection tracking
  - [ ] Timer-based delayed preview (debouncing)
  - [ ] Deduplication (skip if same issue)
  - [ ] Buffer pooling
  - [ ] Window parameter marking
- [ ] **State Management**: Closure-based state with lazy init
- [ ] **Filter System**: Composable higher-order filters
- [ ] **Transient Menus**: View/filter/action commands
- [ ] **Observer Pattern**: Cross-component notifications
- [ ] **Window Helpers**: Safe selection with `unwind-protect`

## 13. File Structure Suggestion

```
beads.el/
├── beads.el              # Main entry point, autoloads
├── beads-rpc.el          # Daemon communication (JSON-over-socket)
├── beads-list.el         # Issue list mode (tabulated-list)
├── beads-detail.el       # Detail view mode
├── beads-preview.el      # Preview minor mode (quicklook)
├── beads-filter.el       # Filter system
├── beads-transient.el    # Transient menus
└── beads-faces.el        # Face definitions
```
