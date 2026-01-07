;;; beads-duplicates.el --- Duplicate detection for Beads -*- lexical-binding: t -*-

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

;; Interactive buffer for finding and merging duplicate issues.
;; Duplicates are issues with identical content (title, description, etc.).

;;; Code:

(require 'json)

(declare-function beads-detail-open "beads-detail")
(declare-function beads-rpc-show "beads-rpc")

(defgroup beads-duplicates nil
  "Duplicate detection for Beads."
  :group 'beads)

(defface beads-duplicates-target
  '((t :inherit success :weight bold))
  "Face for merge target (canonical) issue.")

(defface beads-duplicates-source
  '((t :inherit warning))
  "Face for source issues (will be closed on merge).")

(defface beads-duplicates-group-header
  '((t :inherit bold :underline t))
  "Face for duplicate group headers.")

(defvar-local beads-duplicates--data nil
  "Parsed duplicates data in current buffer.")

(defvar beads-duplicates-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'beads-duplicates-goto-issue)
    (define-key map (kbd "m") #'beads-duplicates-merge-at-point)
    (define-key map (kbd "M") #'beads-duplicates-merge-group)
    (define-key map (kbd "g") #'beads-duplicates-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for beads-duplicates-mode.")

(define-derived-mode beads-duplicates-mode special-mode "Beads-Duplicates"
  "Major mode for viewing and merging duplicate Beads issues.

Duplicates are issues with identical content grouped by content hash.

\\{beads-duplicates-mode-map}"
  (setq buffer-read-only t))

(defun beads-duplicates--fetch ()
  "Fetch duplicate issues via CLI.
Uses --no-daemon flag as duplicates command isn't supported in daemon mode."
  (let* ((bd-program (or (executable-find "bd") "bd"))
         (output (with-temp-buffer
                   (let ((exit-code (call-process bd-program nil t nil
                                                  "--no-daemon" "duplicates" "--json")))
                     (unless (zerop exit-code)
                       (signal 'error
                               (list (format "bd duplicates failed: %s"
                                             (buffer-string)))))
                     (goto-char (point-min))
                     (condition-case nil
                         (json-read)
                       (json-error nil))))))
    output))

(defun beads-duplicates--render (data)
  "Render duplicate groups DATA into current buffer."
  (let ((inhibit-read-only t)
        (groups (alist-get 'groups data))
        (group-count (alist-get 'duplicate_groups data 0)))
    (erase-buffer)
    (insert (propertize "Duplicate Issues\n" 'face 'bold))
    (insert (propertize
             "Issues with identical content grouped for merging\n"
             'face 'shadow))
    (insert (propertize
             "RET=view  m=merge issue  M=merge group  g=refresh  q=quit\n"
             'face 'shadow))
    (insert (make-string 60 ?=) "\n\n")
    (if (or (null groups) (= group-count 0))
        (insert "No duplicate issues found.\n")
      (let ((group-num 0))
        (dolist (group (if (vectorp groups) (append groups nil) groups))
          (setq group-num (1+ group-num))
          (let* ((title (alist-get 'title group))
                 (issues (alist-get 'issues group))
                 (target (alist-get 'suggested_target group))
                 (sources (alist-get 'suggested_sources group))
                 (issues-list (if (vectorp issues) (append issues nil) issues))
                 (sources-list (if (vectorp sources) (append sources nil) sources))
                 (group-start (point)))
            (insert (propertize (format "Group %d: %s\n" group-num title)
                                'face 'beads-duplicates-group-header))
            (dolist (issue issues-list)
              (let* ((id (alist-get 'id issue))
                     (status (alist-get 'status issue))
                     (refs (alist-get 'references issue 0))
                     (is-target (string= id target))
                     (is-source (member id sources-list))
                     (face (cond (is-target 'beads-duplicates-target)
                                 (is-source 'beads-duplicates-source)
                                 (t 'default)))
                     (marker (cond (is-target "→")
                                   (is-source " ")
                                   (t " ")))
                     (start (point)))
                (insert (format " %s " marker))
                (insert (propertize (format "%-12s" id) 'face face))
                (insert (format " [%s] refs:%d" status refs))
                (when is-target
                  (insert (propertize " (keep)" 'face 'beads-duplicates-target)))
                (when is-source
                  (insert (propertize " (merge)" 'face 'beads-duplicates-source)))
                (insert "\n")
                (put-text-property start (point) 'beads-duplicate-id id)
                (put-text-property start (point) 'beads-duplicate-group group)
                (put-text-property start (point) 'beads-duplicate-target target)
                (put-text-property start (point) 'beads-duplicate-is-source is-source)))
            (insert "\n")
            (put-text-property group-start (point) 'beads-duplicate-group-data group)))))))

;;;###autoload
(defun beads-duplicates ()
  "Display duplicate issues with merge actions.
Duplicates are issues with identical content that can be merged."
  (interactive)
  (condition-case err
      (let ((data (beads-duplicates--fetch)))
        (with-current-buffer (get-buffer-create "*Beads Duplicates*")
          (beads-duplicates-mode)
          (setq beads-duplicates--data data)
          (beads-duplicates--render data)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)
                         '((display-buffer-reuse-window
                            display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . fit-window-to-buffer)))))
    (error
     (message "Failed to fetch duplicates: %s" (error-message-string err)))))

(defun beads-duplicates--id-at-point ()
  "Return issue ID at point, or nil."
  (get-text-property (point) 'beads-duplicate-id))

(defun beads-duplicates--group-at-point ()
  "Return group data at point, or nil."
  (get-text-property (point) 'beads-duplicate-group))

(defun beads-duplicates-goto-issue ()
  "Open the issue at point in detail view."
  (interactive)
  (let ((id (beads-duplicates--id-at-point)))
    (unless id
      (user-error "No issue at point"))
    (condition-case err
        (let ((issue (beads-rpc-show id)))
          (beads-detail-open issue))
      (beads-rpc-error
       (user-error "Failed to load issue: %s" (error-message-string err))))))

(defun beads-duplicates--merge (source-id target-id)
  "Merge SOURCE-ID into TARGET-ID using CLI."
  (let* ((bd-program (or (executable-find "bd") "bd"))
         (exit-code (call-process bd-program nil nil nil
                                  "duplicate" source-id "--of" target-id)))
    (unless (zerop exit-code)
      (signal 'error (list (format "Failed to merge %s into %s" source-id target-id))))))

(defun beads-duplicates-merge-at-point ()
  "Merge the issue at point into the canonical target."
  (interactive)
  (let ((id (beads-duplicates--id-at-point))
        (target (get-text-property (point) 'beads-duplicate-target))
        (is-source (get-text-property (point) 'beads-duplicate-is-source)))
    (unless id
      (user-error "No issue at point"))
    (unless target
      (user-error "No merge target found"))
    (when (string= id target)
      (user-error "Cannot merge target into itself"))
    (unless is-source
      (user-error "This issue is not marked for merge"))
    (when (yes-or-no-p (format "Merge %s into %s? " id target))
      (condition-case err
          (progn
            (beads-duplicates--merge id target)
            (message "Merged %s into %s" id target)
            (beads-duplicates-refresh))
        (error
         (message "Failed to merge: %s" (error-message-string err)))))))

(defun beads-duplicates-merge-group ()
  "Merge all source issues in the current group into the target."
  (interactive)
  (let ((group (beads-duplicates--group-at-point)))
    (unless group
      (user-error "No duplicate group at point"))
    (let* ((target (alist-get 'suggested_target group))
           (sources (alist-get 'suggested_sources group))
           (sources-list (if (vectorp sources) (append sources nil) sources))
           (count (length sources-list)))
      (unless sources-list
        (user-error "No sources to merge"))
      (when (yes-or-no-p (format "Merge %d issue(s) into %s? " count target))
        (let ((merged 0)
              (errors 0))
          (dolist (source sources-list)
            (condition-case nil
                (progn
                  (beads-duplicates--merge source target)
                  (setq merged (1+ merged)))
              (error (setq errors (1+ errors)))))
          (if (> errors 0)
              (message "Merged %d issue(s), %d error(s)" merged errors)
            (message "Merged %d issue(s) into %s" merged target))
          (beads-duplicates-refresh))))))

(defun beads-duplicates-refresh ()
  "Refresh the duplicates list."
  (interactive)
  (unless (derived-mode-p 'beads-duplicates-mode)
    (user-error "Not in beads-duplicates-mode"))
  (condition-case err
      (let ((data (beads-duplicates--fetch)))
        (setq beads-duplicates--data data)
        (let ((saved-point (point)))
          (beads-duplicates--render data)
          (goto-char (min saved-point (point-max))))
        (message "Refreshed duplicates"))
    (error
     (message "Failed to refresh: %s" (error-message-string err)))))

(provide 'beads-duplicates)
;;; beads-duplicates.el ends here
