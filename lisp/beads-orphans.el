;;; beads-orphans.el --- Orphan detection for Beads -*- lexical-binding: t -*-

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

;; Interactive buffer for viewing and acting on orphaned issues.
;; Orphans are issues referenced in commits but not marked as closed.

;;; Code:

(require 'beads-core)

(declare-function beads-rpc-close "beads-rpc")

(defvar-local beads-orphans--data nil
  "List of orphan data in current buffer.")

(defvar beads-orphans-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'beads-orphans-goto-issue)
    (define-key map (kbd "c") #'beads-orphans-close)
    (define-key map (kbd "g") #'beads-orphans-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for beads-orphans-mode.")

(define-derived-mode beads-orphans-mode special-mode "Beads-Orphans"
  "Major mode for viewing and acting on orphaned Beads issues.

Orphans are issues referenced in commits but not marked as closed.

\\{beads-orphans-mode-map}"
  (setq buffer-read-only t))

(defun beads-orphans--fetch ()
  "Fetch orphaned issues via CLI."
  (beads-core-cli-request "orphans"))

(defun beads-orphans--render (orphans)
  "Render ORPHANS list into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (beads-core-render-header
     "Orphaned Issues"
     "Issues referenced in commits but not closed"
     "RET=view  c=close  g=refresh  q=quit")
    (if (null orphans)
        (insert "No orphaned issues found.\n")
      (dolist (orphan orphans)
        (let ((id (alist-get 'issue_id orphan))
              (title (alist-get 'title orphan))
              (status (alist-get 'status orphan))
              (commit (alist-get 'latest_commit orphan))
              (msg (alist-get 'latest_commit_message orphan))
              (start (point)))
          (insert (propertize id 'face 'bold))
          (insert (format " [%s]\n" status))
          (insert (format "  %s\n" title))
          (insert (propertize (format "  Commit: %s\n" commit) 'face 'shadow))
          (insert (propertize (format "  Message: %s\n" msg) 'face 'shadow))
          (insert "\n")
          (put-text-property start (point) 'beads-orphan-id id)
          (put-text-property start (point) 'beads-orphan-data orphan))))))

;;;###autoload
(defun beads-orphans ()
  "Display orphaned issues with interactive actions.
Orphans are issues referenced in commits but not marked as closed."
  (interactive)
  (condition-case err
      (let ((orphans (beads-orphans--fetch)))
        (with-current-buffer (get-buffer-create "*Beads Orphans*")
          (beads-orphans-mode)
          (setq beads-orphans--data orphans)
          (beads-orphans--render orphans)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)
                         '((display-buffer-in-side-window)
                           (side . bottom)
                           (window-height . fit-window-to-buffer)))))
    (error
     (message "Failed to fetch orphans: %s" (error-message-string err)))))

(defun beads-orphans--id-at-point ()
  "Return orphan issue ID at point, or nil."
  (beads-core-id-at-point 'beads-orphan-id))

(defun beads-orphans-goto-issue ()
  "Open the orphan issue at point in detail view."
  (interactive)
  (beads-core-goto-issue-at-point 'beads-orphan-id))

(defun beads-orphans-close ()
  "Close the orphan issue at point."
  (interactive)
  (let ((id (beads-orphans--id-at-point)))
    (unless id
      (user-error "No orphan at point"))
    (let ((reason (read-string (format "Close %s reason: " id))))
      (condition-case err
          (progn
            (beads-rpc-close id reason)
            (message "Closed %s" id)
            (beads-orphans-refresh))
        (beads-rpc-error
         (user-error "Failed to close issue: %s" (error-message-string err)))))))

(defun beads-orphans-refresh ()
  "Refresh the orphans list."
  (interactive)
  (unless (derived-mode-p 'beads-orphans-mode)
    (user-error "Not in beads-orphans-mode"))
  (condition-case err
      (let ((orphans (beads-orphans--fetch)))
        (setq beads-orphans--data orphans)
        (let ((saved-point (point)))
          (beads-orphans--render orphans)
          (goto-char (min saved-point (point-max))))
        (message "Refreshed orphans"))
    (error
     (message "Failed to refresh orphans: %s" (error-message-string err)))))

(provide 'beads-orphans)
;;; beads-orphans.el ends here
