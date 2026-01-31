;;; beads-conflicts.el --- JSONL merge conflict resolution for Beads -*- lexical-binding: t -*-

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

;; Interactive buffer for viewing and resolving JSONL merge conflicts.
;; When git merges fail to auto-resolve, JSONL files can end up with
;; conflict markers.  This mode shows the conflict status and allows
;; resolving them using beads' mechanical merge rules.

;;; Code:

(require 'json)
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
  "Major mode for viewing and resolving JSONL merge conflicts.

When git merges fail to auto-resolve, JSONL files can have conflict
markers (<<<<<<, =======, >>>>>>).  This mode detects such conflicts
and resolves them using beads' mechanical merge rules.

\\{beads-conflicts-mode-map}"
  (setq buffer-read-only t))

(defun beads-conflicts--fetch ()
  "Fetch conflict status via CLI with dry-run."
  (let* ((bd-program (or (executable-find "bd") "bd"))
         (output (with-temp-buffer
                   (call-process bd-program nil t nil
                                 "resolve-conflicts" "--dry-run" "--json")
                   (goto-char (point-min))
                   (condition-case nil
                       (json-read)
                     (json-error nil)))))
    output))

(defun beads-conflicts--render (result)
  "Render conflict status RESULT into current buffer."
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
          (insert "(Updated timestamps win, field-level merging)\n")))))))

;;;###autoload
(defun beads-conflicts ()
  "Display JSONL merge conflict status with resolution options.
Detects git merge conflict markers in beads JSONL files and offers
to resolve them using beads' mechanical merge rules."
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
  "Resolve all JSONL merge conflicts using mechanical merge."
  (interactive)
  (unless (derived-mode-p 'beads-conflicts-mode)
    (user-error "Not in beads-conflicts-mode"))
  (when (yes-or-no-p "Resolve all conflicts using mechanical merge? ")
    (let* ((bd-program (or (executable-find "bd") "bd"))
           (output (with-temp-buffer
                     (call-process bd-program nil t nil
                                   "resolve-conflicts" "--json")
                     (goto-char (point-min))
                     (condition-case nil
                         (json-read)
                       (json-error nil)))))
      (if (and output (equal (alist-get 'status output) "success"))
          (progn
            (message "Resolved %d conflict(s)"
                     (alist-get 'conflicts_resolved output 0))
            (beads-conflicts-refresh))
        (message "Failed to resolve conflicts: %s"
                 (or (alist-get 'error output) "unknown error"))))))

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
