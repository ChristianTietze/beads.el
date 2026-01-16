;;; beads-vui.el --- VUI foundation for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, ui

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

;; Foundation module for vui.el-based UI components in beads.el.
;; Provides contexts, shared state, and common utilities for
;; declarative UI rendering.
;;
;; Contexts defined:
;; - `beads-issue': Current issue data, consumed via (use-beads-issue)
;; - `beads-callbacks': Refresh/navigate callbacks, consumed via (use-beads-callbacks)

;;; Code:

(require 'vui)
(require 'beads-faces)
(require 'beads-rpc)

(declare-function beads-detail-open "beads-detail")
(declare-function beads-edit-field-markdown "beads-edit")
(declare-function beads-edit-field-minibuffer "beads-edit")
(declare-function beads-edit-field-completing "beads-edit")

(defgroup beads-vui nil
  "VUI-based UI components for Beads."
  :group 'beads)

;;; Contexts

(vui-defcontext beads-issue nil
  "Context providing current issue data to nested components.
Access via (use-beads-issue) which returns the issue alist.")

(vui-defcontext beads-callbacks nil
  "Context providing callback functions to nested components.
Access via (use-beads-callbacks) which returns a plist with:
  :on-refresh - Function to call after edits
  :on-navigate - Function to navigate to an issue ID")

;;; Field value helpers

(defun beads-vui-issue-get (issue key &optional default)
  "Get KEY from ISSUE alist, or DEFAULT if not found."
  (or (alist-get key issue) default))

;;; Status face helper

(defun beads-vui-status-face (status)
  "Return face for STATUS string."
  (pcase status
    ("closed" 'beads-status-closed)
    ("in_progress" 'beads-status-in-progress)
    ("blocked" 'beads-status-blocked)
    ("hooked" 'beads-status-hooked)
    (_ 'beads-status-open)))

(defun beads-vui-priority-face (priority)
  "Return face for PRIORITY number."
  (pcase priority
    (0 'beads-priority-p0)
    (1 'beads-priority-p1)
    (_ nil)))

;;; Edit callbacks factory

(defun beads-vui-make-edit-handler (issue field-key on-refresh)
  "Create edit handler for FIELD-KEY on ISSUE.
Calls ON-REFRESH after successful edit."
  (lambda ()
    (let* ((id (alist-get 'id issue))
           (value (alist-get field-key issue)))
      (pcase field-key
        ((or 'description 'design 'acceptance_criteria 'notes)
         (beads-edit-field-markdown id (beads-vui--field-to-keyword field-key) value))
        ((or 'title 'assignee 'external_ref)
         (when (beads-edit-field-minibuffer
                id (beads-vui--field-to-keyword field-key) value
                (format "%s: " (capitalize (symbol-name field-key))))
           (when on-refresh (funcall on-refresh))))
        ('status
         (when (beads-edit-field-completing
                id :status value "Status: "
                '("open" "in_progress" "blocked" "hooked" "closed"))
           (when on-refresh (funcall on-refresh))))
        ('issue_type
         (when (beads-edit-field-completing
                id :issue-type value "Type: "
                '("bug" "feature" "task" "epic" "chore" "gate" "convoy" "agent" "role"))
           (when on-refresh (funcall on-refresh))))
        ('priority
         (let* ((priority-str (format "P%d" value))
                (new-value (completing-read "Priority: "
                                            '("P0" "P1" "P2" "P3" "P4")
                                            nil t priority-str)))
           (unless (string= new-value priority-str)
             (let ((new-priority (string-to-number (substring new-value 1))))
               (beads-rpc-update id :priority new-priority)
               (when on-refresh (funcall on-refresh))))))))))

(defun beads-vui--field-to-keyword (field-key)
  "Convert FIELD-KEY symbol to RPC keyword."
  (pcase field-key
    ('issue_type :issue-type)
    ('external_ref :external-ref)
    ('acceptance_criteria :acceptance-criteria)
    (_ (intern (concat ":" (symbol-name field-key))))))

;;; Navigation helpers

(defun beads-vui-navigate-to-issue (issue-id &optional navigate-fn)
  "Navigate to ISSUE-ID using NAVIGATE-FN or default behavior."
  (if navigate-fn
      (funcall navigate-fn issue-id)
    (when-let ((issue (beads-rpc-show issue-id)))
      (beads-detail-open issue))))

;;; Timestamp formatting

(defun beads-vui-format-timestamp (timestamp)
  "Format TIMESTAMP string for display (date only)."
  (if (stringp timestamp)
      (let ((parts (split-string timestamp "T")))
        (or (car parts) timestamp))
    (format "%s" timestamp)))

;;; Markdown rendering

(defcustom beads-vui-render-markdown t
  "Whether to render markdown in vui components.
When non-nil and `markdown-mode' is available, text content
will be fontified with markdown highlighting."
  :type 'boolean
  :group 'beads-vui)

(defun beads-vui-fontify-markdown (text)
  "Fontify TEXT with markdown-mode if available and enabled.
Returns the fontified string, or original text if disabled."
  (if (and beads-vui-render-markdown
           (fboundp 'markdown-mode)
           (stringp text)
           (not (string-empty-p text)))
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (markdown-mode))
        (font-lock-mode 1)
        (font-lock-ensure)
        (buffer-string))
    (or text "")))

;;; Common component definitions

(vui-defcomponent beads-vui-labeled-value (label value &key face)
  "Display a LABEL: VALUE pair with optional FACE for the value."
  :render
  (vui-hstack
   (vui-text (concat label ": ") :face 'bold)
   (vui-text (or value "") :face (or face 'default))))

(vui-defcomponent beads-vui-section-header (title)
  "Display a section header with TITLE."
  :render
  (vui-vstack
   (vui-text (make-string 60 ?─))
   (vui-text (concat title ":") :face '(:weight bold :underline t))
   (vui-newline)))

(vui-defcomponent beads-vui-clickable-id (issue-id &key on-click)
  "Display clickable ISSUE-ID button.
ON-CLICK is called when clicked, defaults to navigation."
  :render
  (vui-button issue-id
              :on-click (or on-click
                            (lambda ()
                              (beads-vui-navigate-to-issue issue-id)))))

;;; Detail view components

(vui-defcomponent beads-vui-editable-field (label value &key face on-edit)
  "Display a LABEL: VALUE pair with optional edit button.
FACE applies to value, ON-EDIT called when edit button clicked."
  :render
  (vui-hstack
   (vui-text (concat label ": ") :face 'bold)
   (vui-text (or value "") :face (or face 'default))
   (when on-edit
     (vui-fragment
      (vui-text " ")
      (vui-button "[edit]" :on-click on-edit :face 'link)))))

(vui-defcomponent beads-vui-metadata-row (issue &key editable on-refresh)
  "Display metadata row for ISSUE with status, priority, type, etc.
When EDITABLE is non-nil, show edit buttons. ON-REFRESH called after edits."
  :render
  (let ((status (alist-get 'status issue))
        (priority (alist-get 'priority issue))
        (assignee (alist-get 'assignee issue))
        (created-by (alist-get 'created_by issue))
        (created (alist-get 'created_at issue))
        (labels (alist-get 'labels issue))
        (parent-id (alist-get 'parent_id issue)))
    (vui-vstack
     (vui-hstack
      :spacing 5
      (vui-component 'beads-vui-editable-field
                     :label "Status"
                     :value status
                     :face (beads-vui-status-face status)
                     :on-edit (when editable
                                (beads-vui-make-edit-handler issue 'status on-refresh)))
      (vui-component 'beads-vui-editable-field
                     :label "Priority"
                     :value (format "P%d" priority)
                     :face (beads-vui-priority-face priority)
                     :on-edit (when editable
                                (beads-vui-make-edit-handler issue 'priority on-refresh)))
      (vui-component 'beads-vui-editable-field
                     :label "Type"
                     :value (beads--format-type issue)
                     :on-edit (when editable
                                (beads-vui-make-edit-handler issue 'issue_type on-refresh))))
     (vui-hstack
      :spacing 2
      (vui-component 'beads-vui-editable-field
                     :label "Assignee"
                     :value (or assignee "(none)")
                     :on-edit (when editable
                                (beads-vui-make-edit-handler issue 'assignee on-refresh)))
      (when created-by
        (vui-component 'beads-vui-labeled-value
                       :label "Created by" :value created-by))
      (when created
        (vui-component 'beads-vui-labeled-value
                       :label "Created"
                       :value (beads-vui-format-timestamp created))))
     (when parent-id
       (vui-hstack
        (vui-text "Parent: " :face 'bold)
        (vui-component 'beads-vui-clickable-id :issue-id parent-id)))
     (when (and labels (> (length labels) 0))
       (vui-component 'beads-vui-labeled-value
                      :label "Labels"
                      :value (mapconcat #'identity (append labels nil) ", "))))))

(vui-defcomponent beads-vui-content-section (title content &key on-edit)
  "Display a content section with TITLE and markdown CONTENT.
ON-EDIT called when edit button clicked (if provided)."
  :render
  (vui-vstack
   (vui-hstack
    (vui-text (make-string 60 ?─))
    (when on-edit
      (vui-fragment
       (vui-text " ")
       (vui-button "[edit]" :on-click on-edit :face 'link))))
   (vui-text (concat title ":") :face '(:weight bold :underline t))
   (vui-newline)
   (if (and content (not (string-empty-p content)))
       (vui-text (beads-vui-fontify-markdown content))
     (vui-text "(empty)" :face 'shadow))
   (vui-newline)))

(vui-defcomponent beads-vui-content-sections (issue &key editable on-refresh)
  "Display all content sections (description, design, acceptance) for ISSUE.
When EDITABLE is non-nil, show edit buttons. ON-REFRESH called after edits."
  :render
  (vui-fragment
   (vui-component 'beads-vui-content-section
                  :title "Description"
                  :content (alist-get 'description issue)
                  :on-edit (when editable
                             (beads-vui-make-edit-handler issue 'description on-refresh)))
   (vui-component 'beads-vui-content-section
                  :title "Design Notes"
                  :content (alist-get 'design issue)
                  :on-edit (when editable
                             (beads-vui-make-edit-handler issue 'design on-refresh)))
   (vui-component 'beads-vui-content-section
                  :title "Acceptance Criteria"
                  :content (alist-get 'acceptance_criteria issue)
                  :on-edit (when editable
                             (beads-vui-make-edit-handler issue 'acceptance_criteria on-refresh)))))

(vui-defcomponent beads-vui-dep-link (dep)
  "Display a single dependency/dependent DEP as clickable link."
  :render
  (let ((id (alist-get 'id dep))
        (title (alist-get 'title dep ""))
        (status (alist-get 'status dep))
        (dep-type (alist-get 'dependency_type dep)))
    (vui-hstack
     (vui-text "  ")
     (vui-component 'beads-vui-clickable-id :issue-id id)
     (vui-text " ")
     (vui-text (truncate-string-to-width title 40 nil nil "…"))
     (when status
       (vui-text (format " [%s]" status) :face (beads-vui-status-face status)))
     (when (and dep-type (not (string= dep-type "parent-child")))
       (vui-text (format " (%s)" dep-type) :face 'shadow)))))

(vui-defcomponent beads-vui-relationships (issue)
  "Display dependencies and dependents for ISSUE."
  :render
  (let ((deps (alist-get 'dependencies issue))
        (dependents (alist-get 'dependents issue)))
    (vui-fragment
     (when (and deps (> (length deps) 0))
       (vui-vstack
        (vui-text "Depends on:" :face 'bold)
        (vui-list deps
                  (lambda (d) (vui-component 'beads-vui-dep-link :dep d))
                  (lambda (d) (alist-get 'id d)))))
     (when (and dependents (> (length dependents) 0))
       (vui-vstack
        (vui-text "Dependents:" :face 'bold)
        (vui-list dependents
                  (lambda (d) (vui-component 'beads-vui-dep-link :dep d))
                  (lambda (d) (alist-get 'id d))))))))

(vui-defcomponent beads-vui-comment (comment)
  "Display a single COMMENT with author, timestamp, and text."
  :render
  (let ((author (alist-get 'author comment "unknown"))
        (text (alist-get 'text comment ""))
        (created (alist-get 'created_at comment)))
    (vui-vstack
     (vui-hstack
      (vui-text (format "[%s] " author) :face 'bold)
      (when created
        (vui-text (beads-vui-format-timestamp created) :face 'shadow)))
     (vui-text (beads-vui-fontify-markdown text))
     (vui-newline))))

(vui-defcomponent beads-vui-comments (issue)
  "Display comments section for ISSUE."
  :render
  (let ((comments (alist-get 'comments issue)))
    (when (and comments (> (length comments) 0))
      (vui-vstack
       (vui-component 'beads-vui-section-header
                      :title (format "Comments (%d)" (length comments)))
       (vui-list comments
                 (lambda (c) (vui-component 'beads-vui-comment :comment c))
                 (lambda (c) (or (alist-get 'id c)
                                 (alist-get 'created_at c))))))))

;;; Main detail view component

(vui-defcomponent beads-vui-detail-view (issue &key on-refresh on-navigate editable)
  "Complete detail view for ISSUE.
ON-REFRESH is called after edits, ON-NAVIGATE for issue navigation.
When EDITABLE is non-nil, show inline edit buttons."
  :render
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue "")))
    (beads-callbacks-provider
     (list :on-refresh on-refresh :on-navigate on-navigate)
     (beads-issue-provider
      issue
      (vui-vstack
       (vui-hstack
        (vui-text (format "[%s] " id) :face 'bold)
        (vui-text title :face '(:height 1.2 :weight bold))
        (when editable
          (vui-fragment
           (vui-text " ")
           (vui-button "[edit title]"
                       :on-click (beads-vui-make-edit-handler issue 'title on-refresh)
                       :face 'link))))
       (vui-newline)
       (vui-text (make-string 60 ?═))
       (vui-newline)
       (vui-component 'beads-vui-metadata-row
                      :issue issue
                      :editable editable
                      :on-refresh on-refresh)
       (vui-newline)
       (vui-text (make-string 60 ?─))
       (vui-newline)
       (vui-newline)
       (vui-component 'beads-vui-content-sections
                      :issue issue
                      :editable editable
                      :on-refresh on-refresh)
       (vui-component 'beads-vui-relationships :issue issue)
       (vui-component 'beads-vui-comments :issue issue))))))

;;; Form components

(vui-defcomponent beads-vui-form-field (label value &key on-change size label-width)
  "Single-line text input field with LABEL and VALUE.
ON-CHANGE called with new value. SIZE defaults to 40.
LABEL-WIDTH for aligned forms (right-aligned label in fixed box)."
  :render
  (vui-hstack
   (if label-width
       (vui-box (vui-text (concat label ":") :face 'bold)
                :width label-width :align :right)
     (vui-text (concat label ": ") :face 'bold))
   (vui-text " ")
   (vui-field :value (or value "")
              :size (or size 40)
              :on-change on-change)))

(vui-defcomponent beads-vui-form-select (label value choices &key on-change label-width)
  "Dropdown selection field with LABEL, current VALUE, and CHOICES list.
ON-CHANGE called with selected value.
LABEL-WIDTH for aligned forms (right-aligned label in fixed box)."
  :render
  (vui-hstack
   (if label-width
       (vui-box (vui-text (concat label ":") :face 'bold)
                :width label-width :align :right)
     (vui-text (concat label ": ") :face 'bold))
   (vui-text " ")
   (vui-select choices
               :value value
               :on-change on-change)))

(vui-defcomponent beads-vui-form-text-display (label value &key on-edit label-width)
  "Display multi-line text with LABEL, VALUE preview, and edit button.
ON-EDIT called when edit button clicked (opens dedicated edit buffer).
LABEL-WIDTH for aligned forms (right-aligned label in fixed box)."
  :render
  (vui-hstack
   (if label-width
       (vui-box (vui-text (concat label ":") :face 'bold)
                :width label-width :align :right)
     (vui-text (concat label ":") :face 'bold))
   (vui-text " ")
   (if (and value (not (string-empty-p value)))
       (vui-text (truncate-string-to-width value 50 nil nil "…") :face 'shadow)
     (vui-text "(empty)" :face 'shadow))
   (when on-edit
     (vui-fragment
      (vui-text " ")
      (vui-button "[edit]" :on-click on-edit :face 'link)))))

(vui-defcomponent beads-vui-form-buttons (on-save on-cancel)
  "Form action buttons with ON-SAVE and ON-CANCEL callbacks."
  :render
  (vui-hstack
   :spacing 2
   (vui-button "Save Changes" :on-click on-save)
   (vui-button "Cancel" :on-click on-cancel)))

;;; Complete form view component

(vui-defcomponent beads-vui-form-view (issue &key on-save on-cancel)
  "Form editor for ISSUE with all editable fields.
ON-SAVE called with changed fields plist, ON-CANCEL to abort.
Markdown fields (description, design, acceptance, notes) use dedicated
edit buffers and save directly - they are not part of the form save."
  :state ((title (alist-get 'title issue ""))
          (status (alist-get 'status issue "open"))
          (priority (format "P%d" (alist-get 'priority issue 2)))
          (issue-type (alist-get 'issue_type issue "task"))
          (assignee (or (alist-get 'assignee issue) ""))
          (external-ref (or (alist-get 'external_ref issue) "")))
  :render
  (let* ((id (alist-get 'id issue))
         (collect-changes
          (lambda ()
            (let ((changes nil))
              (unless (equal title (alist-get 'title issue ""))
                (setq changes (plist-put changes :title title)))
              (unless (equal status (alist-get 'status issue "open"))
                (setq changes (plist-put changes :status status)))
              (let ((new-pri (string-to-number (substring priority 1))))
                (unless (equal new-pri (alist-get 'priority issue 2))
                  (setq changes (plist-put changes :priority new-pri))))
              (unless (equal issue-type (alist-get 'issue_type issue "task"))
                (setq changes (plist-put changes :issue-type issue-type)))
              (let ((new-assignee (if (string-empty-p assignee) nil assignee)))
                (unless (equal new-assignee (alist-get 'assignee issue))
                  (setq changes (plist-put changes :assignee new-assignee))))
              (let ((new-ref (if (string-empty-p external-ref) nil external-ref)))
                (unless (equal new-ref (alist-get 'external_ref issue))
                  (setq changes (plist-put changes :external-ref new-ref))))
              changes))))
    (vui-vstack
     (vui-text (format "Edit Issue: %s" id) :face 'bold)
     (vui-text "C-c C-c to save, C-c C-k to cancel" :face 'shadow)
     (vui-newline)
     (vui-component 'beads-vui-form-field
                    :label "Title"
                    :label-width 14
                    :value title
                    :on-change (lambda (v) (vui-set-state :title v)))
     (vui-newline)
     (vui-hstack
      :spacing 2
      (vui-component 'beads-vui-form-select
                     :label "Status"
                     :value status
                     :choices '("open" "in_progress" "blocked" "hooked" "closed")
                     :on-change (lambda (v) (vui-set-state :status v)))
      (vui-component 'beads-vui-form-select
                     :label "Priority"
                     :value priority
                     :choices '("P0" "P1" "P2" "P3" "P4")
                     :on-change (lambda (v) (vui-set-state :priority v)))
      (vui-component 'beads-vui-form-select
                     :label "Type"
                     :value issue-type
                     :choices '("bug" "feature" "task" "epic" "chore"
                                "gate" "convoy" "agent" "role")
                     :on-change (lambda (v) (vui-set-state :issue-type v))))
     (vui-newline)
     (vui-component 'beads-vui-form-field
                    :label "Assignee"
                    :label-width 14
                    :value assignee
                    :on-change (lambda (v) (vui-set-state :assignee v)))
     (vui-component 'beads-vui-form-field
                    :label "External Ref"
                    :label-width 14
                    :value external-ref
                    :on-change (lambda (v) (vui-set-state :external-ref v)))
     (vui-newline)
     (vui-text "Markdown fields (click [edit] to open editor):" :face 'shadow)
     (vui-newline)
     (vui-component 'beads-vui-form-text-display
                    :label "Description"
                    :label-width 20
                    :value (alist-get 'description issue)
                    :on-edit (beads-vui-make-edit-handler issue 'description nil))
     (vui-component 'beads-vui-form-text-display
                    :label "Design Notes"
                    :label-width 20
                    :value (alist-get 'design issue)
                    :on-edit (beads-vui-make-edit-handler issue 'design nil))
     (vui-component 'beads-vui-form-text-display
                    :label "Acceptance Criteria"
                    :label-width 20
                    :value (alist-get 'acceptance_criteria issue)
                    :on-edit (beads-vui-make-edit-handler issue 'acceptance_criteria nil))
     (vui-component 'beads-vui-form-text-display
                    :label "Notes"
                    :label-width 20
                    :value (alist-get 'notes issue)
                    :on-edit (beads-vui-make-edit-handler issue 'notes nil))
     (vui-newline)
     (vui-newline)
     (vui-component 'beads-vui-form-buttons
                    :on-save (lambda ()
                               (when on-save
                                 (funcall on-save (funcall collect-changes))))
                    :on-cancel on-cancel))))

(provide 'beads-vui)
;;; beads-vui.el ends here
