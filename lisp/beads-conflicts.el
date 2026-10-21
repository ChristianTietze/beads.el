;;; beads-conflicts.el --- Merge conflict resolution for Beads -*- lexical-binding: t -*-

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

;; View and resolve Dolt merge conflicts, with legacy JSONL support.

;;; Code:

(require 'json)
(require 'beads-backend)
(require 'beads-core)

(defvar-local beads-conflicts--data nil
  "Conflict status data in current buffer.")

(defvar beads-conflicts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'beads-conflicts-resolve-all)
    (define-key map (kbd "g") #'beads-conflicts-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for beads-conflicts-mode.")

(define-derived-mode beads-conflicts-mode special-mode "Beads-Conflicts"
  "Major mode for viewing and resolving Beads merge conflicts.

\\{beads-conflicts-mode-map}"
  (setq buffer-read-only t))

(defun beads-conflicts--fetch ()
  "Fetch conflict status without modifying the workspace."
  (condition-case err
      (beads-backend-cli-execute
       "resolve-conflicts" '((dry_run . t)))
    (beads-backend-error
     (user-error "Cannot check conflicts: %s" (error-message-string err)))))

(defun beads-conflicts--render-dolt (result)
  "Render Dolt conflict RESULT with each side's field values."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (beads-core-render-header
     "Dolt Merge Conflicts"
     "Compare base, local (ours), and incoming (theirs) values"
     "r=resolve all  g=refresh  q=quit")
    (if (zerop (alist-get 'conflicts result 0))
        (insert "No row conflicts detected.\n")
      (seq-doseq (row (alist-get 'rows result))
        (insert (propertize (format "%s %s\n" (alist-get 'table row)
                                    (alist-get 'key row)) 'face 'bold))
        (insert (format "  Ours: %s; theirs: %s\n"
                        (if (eq t (alist-get 'our_exists row)) "present" "deleted")
                        (if (eq t (alist-get 'their_exists row)) "present" "deleted")))
        (seq-doseq (field (alist-get 'fields row))
          (insert (format "  %s\n" (alist-get 'name field)))
          (dolist (side '(base ours theirs))
            (insert (format "    %s: %s\n" side (or (alist-get side field) "(null)")))))
        (insert "\n")))))

(defun beads-conflicts--render (result)
  "Render conflict status RESULT into current buffer."
  (if (assq 'rows result)
      (beads-conflicts--render-dolt result)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (beads-core-render-header
       "JSONL Merge Conflicts"
       "Resolve git merge conflict markers in beads JSONL files"
       "r=resolve all  g=refresh  q=quit")
      (if (null result)
          (insert "Could not check for conflicts.\n")
        (let ((file-path (alist-get 'file_path result))
              (conflicts-found (alist-get 'conflicts_found result 0))
              (status (alist-get 'status result))
              (error-msg (alist-get 'error result)))
          (insert (propertize "File: " 'face 'bold)
                  (or file-path "unknown") "\n\n")
          (cond
           ((and error-msg (string-match-p "not found" error-msg))
            (insert (propertize "No JSONL file found.\n" 'face 'shadow))
            (insert "This is normal if no conflicts exist or sync hasn't run.\n"))
           ((and (equal status "error") error-msg)
            (insert (propertize "Error: " 'face 'error)
                    error-msg "\n"))
           ((zerop conflicts-found)
            (insert (propertize "No conflicts detected.\n" 'face 'success)))
           (t
            (insert (propertize (format "%d conflict(s) found!\n\n"
                                        conflicts-found)
                                'face 'warning))
            (insert "Press ")
            (insert (propertize "r" 'face 'help-key-binding))
            (insert " to resolve using mechanical merge rules.\n")
            (insert "(Updated timestamps win, field-level merging)\n"))))))))

;;;###autoload
(defun beads-conflicts ()
  "Display Beads merge conflicts with resolution options.
For Dolt workspaces, inspect field values and choose ours or theirs.
Older bd versions retain their mechanical JSONL merge behavior."
  (interactive)
  (let ((result (beads-conflicts--fetch)))
    (with-current-buffer (get-buffer-create "*Beads Conflicts*")
      (beads-conflicts-mode)
      (setq beads-conflicts--data result)
      (beads-conflicts--render result)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)
                     '((display-buffer-in-side-window)
                       (side . bottom)
                       (window-height . fit-window-to-buffer))))))

(defun beads-conflicts-resolve-all ()
  "Resolve all displayed conflicts after choosing and confirming a strategy."
  (interactive)
  (unless (derived-mode-p 'beads-conflicts-mode)
    (user-error "Not in beads-conflicts-mode"))
  (let* ((dolt (assq 'rows beads-conflicts--data))
         (strategy (when dolt
                     (when (zerop (alist-get 'conflicts beads-conflicts--data 0))
                       (user-error "There are no row conflicts to resolve"))
                     (completing-read "Keep which side? " '("ours" "theirs") nil t)))
         (prompt (if dolt
                     (format "Resolve ALL conflicts keeping %s? " strategy)
                   "Resolve all conflicts using mechanical merge? ")))
    (when (yes-or-no-p prompt)
      (condition-case err
          (let ((output (beads-backend-cli-execute
                         "resolve-conflicts"
                         (when strategy `((strategy . ,strategy))))))
            (beads-conflicts-refresh)
            (if dolt
                (message "Resolved %d conflict(s); %d remain"
                         (alist-get 'resolved output 0) (alist-get 'remaining output 0))
              (unless (equal (alist-get 'status output) "success")
                (user-error "Conflict resolution failed: %s" (alist-get 'error output)))
              (message "Resolved %d conflict(s)" (alist-get 'conflicts_resolved output 0))))
        (beads-backend-error
         (user-error "Conflict resolution failed: %s" (error-message-string err)))))))

(defun beads-conflicts-refresh ()
  "Refresh the conflict status."
  (interactive)
  (unless (derived-mode-p 'beads-conflicts-mode)
    (user-error "Not in beads-conflicts-mode"))
  (let ((result (beads-conflicts--fetch)))
    (setq beads-conflicts--data result)
    (let ((saved-point (point)))
      (beads-conflicts--render result)
      (goto-char (min saved-point (point-max))))
    (message "Refreshed conflict status")))

(provide 'beads-conflicts)
;;; beads-conflicts.el ends here
