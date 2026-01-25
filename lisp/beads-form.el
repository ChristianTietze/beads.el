;;; beads-form.el --- Form-based metadata editor for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, ui, widget

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

;; Widget-based form editor for editing all issue fields at once.

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'beads-rpc)

(declare-function beads-show-hint "beads")
(declare-function vui-mount "vui")
(declare-function vui-component "vui")
(declare-function beads-vui-form-view "beads-vui")

(defgroup beads-form nil
  "Form-based editing for Beads issues."
  :group 'beads)

(defcustom beads-form-use-vui nil
  "Whether to use vui.el for the form editor.
When non-nil, uses declarative vui components.
When nil, uses traditional widget.el forms."
  :type 'boolean
  :group 'beads-form)

(declare-function beads-get-types "beads-list")

(defvar-local beads-form--issue-id nil
  "Issue ID being edited in this form buffer.")

(defvar-local beads-form--original-issue nil
  "Original issue data for comparison.")

(defvar-local beads-form--widgets nil
  "Alist of field name to widget.")

(defvar beads-form-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map (kbd "C-c C-c") #'beads-form-commit)
    (define-key map (kbd "C-c C-k") #'beads-form-cancel)
    map)
  "Keymap for beads-form-mode.")

(defvar beads-form-field-keymap
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-form-commit)
    (define-key map (kbd "C-c C-k") #'beads-form-cancel)
    map)
  "Keymap for editable fields inside beads form buffers.")

(defvar beads-form-text-keymap
  (let ((map (copy-keymap widget-text-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-form-commit)
    (define-key map (kbd "C-c C-k") #'beads-form-cancel)
    map)
  "Keymap for text widgets inside beads form buffers.")

(define-derived-mode beads-form-mode nil "Beads-Form"
  "Major mode for editing Beads issue metadata in a form.

\\{beads-form-mode-map}"
  (use-local-map beads-form-mode-map)
  (beads-show-hint))

(defun beads-form-open (issue)
  "Open form editor for ISSUE."
  (let* ((id (alist-get 'id issue))
         (buffer-name (format "*Beads Form: %s*" id))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq beads-form--issue-id id)
      (setq beads-form--original-issue issue)
      (if beads-form-use-vui
          (beads-form--render-vui buffer issue)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (remove-overlays)
        (beads-form-mode)
        (setq beads-form--widgets nil)
        (beads-form--render issue)
        (widget-setup)
        (goto-char (point-min))
        (widget-forward 1)))
    (pop-to-buffer buffer)))

(defun beads-form--render-vui (buffer issue)
  "Render form for ISSUE into BUFFER using vui.el components."
  (require 'beads-vui)
  (let ((issue-id (alist-get 'id issue)))
    (save-window-excursion
      (vui-mount (vui-component 'beads-vui-form-view
                                :issue issue
                                :on-save (lambda (changes)
                                           (if (null changes)
                                               (progn
                                                 (message "No changes to save")
                                                 (beads-form--close))
                                             (condition-case err
                                                 (progn
                                                   (apply #'beads-rpc-update issue-id changes)
                                                   (message "Updated %s" issue-id)
                                                   (beads-form--close)
                                                   (beads-form--refresh-views issue-id))
                                               (beads-rpc-error
                                                (message "Failed to update: %s"
                                                         (error-message-string err))))))
                                :on-cancel (lambda ()
                                             (beads-form--close)
                                             (message "Cancelled")))
                 (buffer-name buffer)))))

(defun beads-form--render (issue)
  "Render form widgets for ISSUE."
  (widget-insert (propertize (format "Edit Issue: %s\n" (alist-get 'id issue))
                             'face 'bold))
  (widget-insert (propertize "C-c C-c to save, C-c C-k to cancel\n\n"
                             'face 'shadow))

  (beads-form--add-field 'title "Title" 'editable-field
                         (alist-get 'title issue ""))

  (widget-insert "\n")
  (beads-form--add-field 'status "Status" 'menu-choice
                         (alist-get 'status issue "open")
                         :choices '("open" "in_progress" "blocked" "hooked" "closed"))

  (widget-insert "  ")
  (beads-form--add-field 'priority "Priority" 'menu-choice
                         (format "P%d" (alist-get 'priority issue 2))
                         :choices '("P0" "P1" "P2" "P3" "P4"))

  (widget-insert "  ")
  (let* ((current-type (alist-get 'issue_type issue "task"))
         (type-choices (sort (seq-uniq (cons current-type (beads-get-types))) #'string<)))
    (beads-form--add-field 'issue_type "Type" 'menu-choice
                           current-type
                           :choices type-choices))

  (widget-insert "\n\n")
  (beads-form--add-field 'assignee "Assignee" 'editable-field
                         (or (alist-get 'assignee issue) ""))

  (widget-insert "\n")
  (beads-form--add-field 'external_ref "External Ref" 'editable-field
                         (or (alist-get 'external_ref issue) ""))

  (widget-insert "\n\n")
  (beads-form--add-text-field 'description "Description"
                              (or (alist-get 'description issue) ""))

  (widget-insert "\n")
  (beads-form--add-text-field 'design "Design Notes"
                              (or (alist-get 'design issue) ""))

  (widget-insert "\n")
  (beads-form--add-text-field 'acceptance_criteria "Acceptance Criteria"
                              (or (alist-get 'acceptance_criteria issue) ""))

  (widget-insert "\n")
  (beads-form--add-text-field 'notes "Notes"
                              (or (alist-get 'notes issue) ""))

  (widget-insert "\n\n")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (beads-form-commit))
                 "Save Changes")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _) (beads-form-cancel))
                 "Cancel"))

(defun beads-form--add-field (name label type value &rest args)
  "Add a form field NAME with LABEL, TYPE, VALUE, and optional ARGS."
  (widget-insert (propertize (concat label ": ") 'face 'bold))
  (let ((widget
         (pcase type
           ('editable-field
            (widget-create 'editable-field
                           :size 40
                           :keymap beads-form-field-keymap
                           :value value))
           ('menu-choice
            (let ((choices (plist-get args :choices)))
              (apply #'widget-create 'menu-choice
                     :value value
                     (mapcar (lambda (c) (list 'item :tag c :value c))
                             choices)))))))
    (push (cons name widget) beads-form--widgets)))

(defun beads-form--add-text-field (name label value)
  "Add a multi-line text field NAME with LABEL and VALUE."
  (widget-insert (propertize (concat label ":\n") 'face 'bold))
  (let ((widget (widget-create 'text
                               :format "%v"
                               :keymap beads-form-text-keymap
                               :value value)))
    (push (cons name widget) beads-form--widgets)))

(defun beads-form--get-value (name)
  "Get current value of widget NAME."
  (when-let ((widget (alist-get name beads-form--widgets)))
    (widget-value widget)))

(defun beads-form--collect-changes ()
  "Collect changed fields as a plist for RPC update."
  (let ((changes nil)
        (original beads-form--original-issue))
    (cl-flet ((check-field (name rpc-key &optional transform)
                (let* ((new-val (beads-form--get-value name))
                       (old-val (or (alist-get name original) ""))
                       (new-val-transformed (if transform (funcall transform new-val) new-val)))
                  (unless (equal new-val-transformed old-val)
                    (setq changes (plist-put changes rpc-key new-val-transformed))))))
      (check-field 'title :title)
      (check-field 'status :status)
      (check-field 'priority :priority
                   (lambda (v) (string-to-number (substring v 1))))
      (check-field 'issue_type :issue-type)
      (check-field 'assignee :assignee
                   (lambda (v) (if (string-empty-p v) nil v)))
      (check-field 'external_ref :external-ref
                   (lambda (v) (if (string-empty-p v) nil v)))
      (check-field 'description :description)
      (check-field 'design :design)
      (check-field 'acceptance_criteria :acceptance-criteria)
      (check-field 'notes :notes))
    changes))

(defun beads-form-commit ()
  "Save all changes and close the form."
  (interactive)
  (let ((changes (beads-form--collect-changes))
        (issue-id beads-form--issue-id))
    (if (null changes)
        (progn
          (message "No changes to save")
          (beads-form--close))
      (condition-case err
          (progn
            (apply #'beads-rpc-update issue-id changes)
            (message "Updated %s" issue-id)
            (beads-form--close)
            (beads-form--refresh-views issue-id))
        (beads-rpc-error
         (message "Failed to update: %s" (error-message-string err)))))))

(defun beads-form-cancel ()
  "Discard changes and close the form."
  (interactive)
  (beads-form--close)
  (message "Cancelled"))

(defun beads-form--close ()
  "Close the form buffer."
  (quit-window t))

(defun beads-form--refresh-views (issue-id)
  "Refresh detail and list views after form edit for ISSUE-ID."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cond
         ((eq major-mode 'beads-list-mode)
          (when (fboundp 'beads-list-refresh)
            (beads-list-refresh)))
         ((and (eq major-mode 'beads-detail-mode)
               (boundp 'beads-detail--current-issue-id)
               (equal beads-detail--current-issue-id issue-id))
          (when (fboundp 'beads-detail-refresh)
            (beads-detail-refresh))))))))

(provide 'beads-form)
;;; beads-form.el ends here
