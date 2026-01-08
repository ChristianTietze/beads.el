;;; beads-hierarchy.el --- Dependency tree view for Beads -*- lexical-binding: t -*-

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

;; Tree view for visualizing issue dependencies using hierarchy.el.

;;; Code:

(require 'hierarchy)
(require 'wid-edit)
(require 'beads-rpc)
(require 'beads-faces)

(declare-function beads-detail-open "beads-detail")

(defgroup beads-hierarchy nil
  "Dependency tree display for Beads."
  :group 'beads)

(defvar-local beads-hierarchy--root-id nil
  "Root issue ID for current hierarchy buffer.")

(defvar-local beads-hierarchy--hierarchy nil
  "The hierarchy object for current buffer.")

(defvar-local beads-hierarchy--by-id nil
  "Hash table mapping issue IDs to issue data.")

(defvar beads-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'beads-hierarchy-refresh)
    (define-key map (kbd "RET") #'beads-hierarchy-goto-issue)
    (define-key map (kbd "TAB") #'beads-hierarchy-next)
    (define-key map (kbd "<backtab>") #'beads-hierarchy-previous)
    map)
  "Keymap for beads-hierarchy-mode.")

(defun beads-hierarchy--collect-positions ()
  "Collect positions of all interactable elements in buffer.
Returns sorted list of positions for widgets and buttons."
  (let ((positions nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (widget-at (point))
          (push (point) positions))
        (forward-char 1)))
    (save-excursion
      (goto-char (point-min))
      (let ((button (next-button (point))))
        (while button
          (push (button-start button) positions)
          (setq button (next-button (button-end button))))))
    (sort (delete-dups positions) #'<)))

(defun beads-hierarchy-next ()
  "Move to next interactable element (widget or button)."
  (interactive)
  (let* ((positions (beads-hierarchy--collect-positions))
         (pos (point))
         (next (seq-find (lambda (p) (> p pos)) positions)))
    (if next
        (goto-char next)
      (when positions
        (goto-char (car positions))))))

(defun beads-hierarchy-previous ()
  "Move to previous interactable element (widget or button)."
  (interactive)
  (let* ((positions (beads-hierarchy--collect-positions))
         (pos (point))
         (prev (seq-find (lambda (p) (< p pos)) (reverse positions))))
    (if prev
        (goto-char prev)
      (when positions
        (goto-char (car (last positions)))))))

(define-derived-mode beads-hierarchy-mode special-mode "Beads-Deps"
  "Major mode for displaying issue dependency trees.

\\{beads-hierarchy-mode-map}")

(defun beads-hierarchy--labelfn (issue indent)
  "Insert formatted ISSUE at INDENT level.
Format: ID [type] title [status]"
  (let* ((id (alist-get 'id issue))
         (title (alist-get 'title issue ""))
         (type-str (beads--format-type issue))
         (status-str (beads--format-status issue))
         (tree-indent (* indent 3))
         (fixed-width (+ (length id) (length type-str) (length status-str) 8))
         (available (- (window-body-width) tree-indent fixed-width)))
    (insert " ")
    (insert-text-button id
                        'face 'beads-detail-id-face
                        'action #'beads-hierarchy--button-action
                        'beads-issue issue
                        'follow-link t)
    (insert " ")
    (insert type-str)
    (insert " ")
    (insert (truncate-string-to-width title (max 10 available) nil nil "…"))
    (insert " [")
    (insert status-str)
    (insert "]")))

(defun beads-hierarchy--button-action (button)
  "Handle activation of issue BUTTON."
  (let ((issue (button-get button 'beads-issue)))
    (when issue
      (condition-case err
          (let* ((id (alist-get 'id issue))
                 (full-issue (beads-rpc-show id)))
            (beads-detail-open full-issue))
        (beads-rpc-error
         (message "Failed to fetch issue: %s" (error-message-string err)))))))

(defun beads-hierarchy--collect-ancestors (issue by-id)
  "Recursively collect ancestors (dependencies) of ISSUE into BY-ID.
Each ancestor is marked with beads--child-id pointing to its dependent."
  (let ((dependencies (alist-get 'dependencies issue)))
    (when (and dependencies (> (length dependencies) 0))
      (seq-doseq (dep (append dependencies nil))
        (let ((dep-id (alist-get 'id dep)))
          (unless (gethash dep-id by-id)
            (let ((dep-with-child (cons (cons 'beads--child-id (alist-get 'id issue)) dep)))
              (puthash dep-id dep-with-child by-id))
            (condition-case nil
                (let ((full-dep (beads-rpc-show dep-id)))
                  (puthash dep-id
                           (cons (cons 'beads--child-id (alist-get 'id issue)) full-dep)
                           by-id)
                  (beads-hierarchy--collect-ancestors full-dep by-id))
              (beads-rpc-error nil))))))))

(defun beads-hierarchy--collect-descendants (issue by-id)
  "Recursively collect descendants (dependents) of ISSUE into BY-ID.
Each descendant is marked with beads--parent-id pointing to its blocker."
  (let ((dependents (alist-get 'dependents issue)))
    (when (and dependents (> (length dependents) 0))
      (seq-doseq (dep (append dependents nil))
        (let ((dep-id (alist-get 'id dep)))
          (unless (gethash dep-id by-id)
            (let ((dep-with-parent (cons (cons 'beads--parent-id (alist-get 'id issue)) dep)))
              (puthash dep-id dep-with-parent by-id))
            (condition-case nil
                (let ((full-dep (beads-rpc-show dep-id)))
                  (puthash dep-id
                           (cons (cons 'beads--parent-id (alist-get 'id issue)) full-dep)
                           by-id)
                  (beads-hierarchy--collect-descendants full-dep by-id))
              (beads-rpc-error nil))))))))

(defun beads-hierarchy--build (issue-id)
  "Build hierarchy for ISSUE-ID showing full bidirectional tree.
Returns a cons of (hierarchy . by-id-hash).
Shows ancestors (blockers) above and descendants (dependents) below."
  (condition-case err
      (let* ((root-issue (beads-rpc-show issue-id))
             (h (hierarchy-new))
             (by-id (make-hash-table :test 'equal)))

        (puthash (alist-get 'id root-issue) root-issue by-id)

        (beads-hierarchy--collect-ancestors root-issue by-id)
        (beads-hierarchy--collect-descendants root-issue by-id)

        (maphash (lambda (_id issue)
                   (hierarchy-add-tree h issue
                     (lambda (i)
                       (beads-hierarchy--find-parent i by-id))))
                 by-id)

        (cons h by-id))
    (beads-rpc-error
     (message "Failed to fetch dependency tree: %s" (error-message-string err))
     nil)))

(defun beads-hierarchy--find-parent (issue by-id)
  "Find parent of ISSUE in BY-ID hash table.
For descendants, parent is beads--parent-id (the blocker).
For ancestors, we look for issues that have this as beads--child-id."
  (let ((parent-id (alist-get 'beads--parent-id issue))
        (own-id (alist-get 'id issue)))
    (if parent-id
        (gethash parent-id by-id)
      (let ((found nil))
        (maphash (lambda (_id other)
                   (when (equal (alist-get 'beads--child-id other) own-id)
                     (setq found other)))
                 by-id)
        found))))

(defun beads-hierarchy-goto-issue ()
  "Open detail view for issue at point."
  (interactive)
  (let ((issue nil))
    (when-let ((widget (widget-at (point))))
      (setq issue (widget-get widget :node)))
    (unless issue
      (when beads-hierarchy--by-id
        (save-excursion
          (beginning-of-line)
          (when (re-search-forward "\\([a-z]+-[a-z0-9]+\\)" (line-end-position) t)
            (let ((id (match-string 1)))
              (setq issue (gethash id beads-hierarchy--by-id)))))))
    (if issue
        (condition-case err
            (let* ((id (alist-get 'id issue))
                   (full-issue (beads-rpc-show id)))
              (beads-detail-open full-issue))
          (beads-rpc-error
           (message "Failed to fetch issue: %s" (error-message-string err))))
      (message "No issue at point"))))

(defun beads-hierarchy--expand-all ()
  "Expand all tree nodes in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (when-let ((widget (widget-at (point))))
        (when (and (widget-get widget :tag)
                   (not (widget-get widget :open)))
          (widget-apply-action widget)))
      (forward-line 1))))

(defun beads-hierarchy--goto-issue (issue-id)
  "Move point to ISSUE-ID in the hierarchy buffer."
  (goto-char (point-min))
  (when (re-search-forward (regexp-quote issue-id) nil t)
    (goto-char (match-beginning 0))))

(defun beads-hierarchy-refresh ()
  "Refresh the dependency tree display."
  (interactive)
  (unless beads-hierarchy--root-id
    (user-error "No root issue set"))
  (let ((saved-point (point))
        (saved-start (window-start)))
    (beads-hierarchy-show beads-hierarchy--root-id)
    (goto-char (min saved-point (point-max)))
    (when-let ((win (get-buffer-window (current-buffer))))
      (set-window-start win (min saved-start (point-max))))))

;;;###autoload
(defun beads-hierarchy-show (issue-id)
  "Display dependency tree for ISSUE-ID in a side window."
  (interactive
   (list (cond
          ((and (boundp 'beads-detail--current-issue-id)
                beads-detail--current-issue-id)
           beads-detail--current-issue-id)
          ((and (derived-mode-p 'beads-list-mode)
                (tabulated-list-get-id))
           (tabulated-list-get-id))
          (t (read-string "Issue ID: ")))))
  (let ((result (beads-hierarchy--build issue-id)))
    (unless result
      (user-error "Could not build dependency tree"))
    (let* ((h (car result))
           (by-id (cdr result))
           (buffer-name (format "*Beads Deps: %s*" issue-id))
           (buffer (get-buffer-create buffer-name)))
      (when (zerop (hierarchy-length h))
        (message "No dependencies found for %s" issue-id))
      (hierarchy-tree-display h #'beads-hierarchy--labelfn buffer)
      (with-current-buffer buffer
        (beads-hierarchy-mode)
        (setq beads-hierarchy--root-id issue-id)
        (setq beads-hierarchy--hierarchy h)
        (setq beads-hierarchy--by-id by-id)
        (beads-hierarchy--expand-all)
        (beads-hierarchy--goto-issue issue-id))
      (let ((window (display-buffer buffer
                                    '((display-buffer-in-side-window)
                                      (side . right)
                                      (window-width . 0.4)))))
        (when window
          (select-window window))))))

(provide 'beads-hierarchy)
;;; beads-hierarchy.el ends here
