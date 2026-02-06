;;; beads-preview.el --- Quicklook preview mode for Beads -*- lexical-binding: t -*-

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

;; Live preview mode for browsing issues with automatic detail display.

;;; Code:

(require 'beads-detail)
(require 'beads-client)

(declare-function beads-list--get-issue-at-point "beads-list")

(defgroup beads-preview nil
  "Automatic issue preview for Beads."
  :group 'beads)

(defcustom beads-preview-delay 0.1
  "Delay in seconds before showing preview after cursor movement."
  :type 'number
  :group 'beads-preview)

(defvar-local beads-preview--timer nil
  "Idle timer for debouncing preview updates.")

(defvar-local beads-preview--current-issue-id nil
  "Issue ID currently displayed in preview (deduplication).")

(define-minor-mode beads-preview-mode
  "Enable automatic issue preview as cursor moves in issue list."
  :lighter " Preview"
  :group 'beads-preview
  (if beads-preview-mode
      (progn
        (add-hook 'post-command-hook #'beads-preview-trigger nil t)
        (beads-preview-trigger))
    (beads-preview--cancel-timer)
    (beads-preview--cleanup)
    (remove-hook 'post-command-hook #'beads-preview-trigger t)))

(defun beads-preview-trigger ()
  "Trigger issue preview after cursor movement.
Only active when in beads-list-mode with preview mode enabled."
  (when (and beads-preview-mode
             (derived-mode-p 'tabulated-list-mode)
             (eq major-mode 'beads-list-mode))
    (if-let ((issue (beads-list--get-issue-at-point)))
        (beads-preview--start-timer issue)
      (beads-preview--cancel-timer))))

(defun beads-preview--start-timer (issue)
  "Start idle timer to preview ISSUE after delay (debouncing)."
  (beads-preview--cancel-timer)
  (setq beads-preview--timer
        (run-with-idle-timer beads-preview-delay nil
                             #'beads-preview--display-issue issue)))

(defun beads-preview--cancel-timer ()
  "Cancel the preview timer if it is running."
  (when beads-preview--timer
    (cancel-timer beads-preview--timer)
    (setq beads-preview--timer nil)))

(defun beads-preview--display-issue (issue)
  "Fetch full issue data and display preview for ISSUE in side window."
  (when issue
    (let ((issue-id (alist-get 'id issue)))
      (unless (equal issue-id beads-preview--current-issue-id)
        (setq beads-preview--current-issue-id issue-id)
        (condition-case err
            (let ((full-issue (beads-client-show issue-id)))
              (beads-detail-show full-issue))
          (beads-client-error
           (message "Preview failed: %s" (error-message-string err))))))))

(defun beads-preview--cleanup ()
  "Full cleanup when preview mode is disabled."
  (beads-preview--cancel-timer)
  (setq beads-preview--current-issue-id nil)
  (when-let ((buf (get-buffer "*Beads Preview*")))
    (when-let ((window (get-buffer-window buf)))
      (delete-window window))
    (kill-buffer buf)))

(provide 'beads-preview)
;;; beads-preview.el ends here
