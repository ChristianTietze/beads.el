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

(provide 'beads-vui)
;;; beads-vui.el ends here
