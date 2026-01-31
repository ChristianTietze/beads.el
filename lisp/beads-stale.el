;;; beads-stale.el --- Stale issue detection for Beads -*- lexical-binding: t -*-

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

;; Interactive buffer for viewing and acting on stale issues.
;; Stale issues are those not updated within a configurable number of days.

;;; Code:

(require 'beads-core)

(declare-function beads-rpc-update "beads-rpc")

(defgroup beads-stale nil
  "Stale issue detection for Beads."
  :group 'beads)

(defcustom beads-stale-days 30
  "Number of days after which an issue is considered stale."
  :type 'integer
  :group 'beads-stale)

(defcustom beads-stale-status nil
  "Status filter for stale issues.
When nil, show all statuses.  Otherwise, filter to this status."
  :type '(choice (const :tag "All" nil)
                 (const :tag "Open" "open")
                 (const :tag "In Progress" "in_progress")
                 (const :tag "Blocked" "blocked"))
  :group 'beads-stale)

(defvar-local beads-stale--data nil
  "List of stale issue data in current buffer.")

(defvar-local beads-stale--days nil
  "Current days threshold for this buffer.")

(defvar-local beads-stale--status nil
  "Current status filter for this buffer.")

(defvar beads-stale-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'beads-stale-goto-issue)
    (define-key map (kbd "c") #'beads-stale-claim)
    (define-key map (kbd "g") #'beads-stale-refresh)
    (define-key map (kbd "d") #'beads-stale-set-days)
    (define-key map (kbd "f") #'beads-stale-set-filter)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for beads-stale-mode.")

(define-derived-mode beads-stale-mode special-mode "Beads-Stale"
  "Major mode for viewing and acting on stale Beads issues.

Stale issues are those not updated within a configurable number of days.

\\{beads-stale-mode-map}"
  (setq buffer-read-only t))

(defun beads-stale--fetch (days &optional status)
  "Fetch stale issues via CLI with DAYS threshold.
Optional STATUS filters by issue status."
  (let ((args (list "--days" (number-to-string days))))
    (when status
      (setq args (append args (list "--status" status))))
    (apply #'beads-core-cli-request "stale" args)))

(defun beads-stale--days-ago (updated-at)
  "Calculate days since UPDATED-AT timestamp."
  (if (and updated-at (stringp updated-at))
      (let* ((time (date-to-time updated-at))
             (now (current-time))
             (diff (time-subtract now time))
             (days (/ (float-time diff) 86400)))
        (floor days))
    0))

(defun beads-stale--render (issues)
  "Render stale ISSUES list into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (beads-core-render-header
     "Stale Issues"
     (format "Issues not updated in %d+ days" (or beads-stale--days beads-stale-days))
     "RET=view  c=claim  d=days  f=filter  g=refresh  q=quit"
     60)
    (if (null issues)
        (insert "No stale issues found.\n")
      (dolist (issue issues)
        (let* ((id (alist-get 'id issue))
               (title (alist-get 'title issue ""))
               (status (alist-get 'status issue))
               (updated (alist-get 'updated_at issue))
               (days-ago (beads-stale--days-ago updated))
               (updated-display (if (and updated (stringp updated))
                                    (substring updated 0 10)
                                  "unknown"))
               (title-truncated (if (> (length title) 40)
                                    (concat (substring title 0 37) "...")
                                  title))
               (start (point)))
          (insert (propertize (format "%-12s" id) 'face 'bold))
          (insert (format " %-12s " status))
          (insert (propertize (format "%3dd ago" days-ago)
                              'face (if (> days-ago 60) 'error 'warning)))
          (insert (format "  %s" updated-display))
          (insert "\n")
          (insert (format "  %s\n" title-truncated))
          (insert "\n")
          (put-text-property start (point) 'beads-stale-id id)
          (put-text-property start (point) 'beads-stale-data issue))))))

;;;###autoload
(defun beads-stale (&optional days status)
  "Display stale issues with interactive actions.
DAYS is the staleness threshold (default `beads-stale-days').
STATUS optionally filters by issue status."
  (interactive)
  (let ((days (or days beads-stale-days))
        (status (or status beads-stale-status)))
    (condition-case err
        (let ((issues (beads-stale--fetch days status)))
          (with-current-buffer (get-buffer-create "*Beads Stale*")
            (beads-stale-mode)
            (setq beads-stale--data issues)
            (setq beads-stale--days days)
            (setq beads-stale--status status)
            (beads-stale--render issues)
            (goto-char (point-min))
            (pop-to-buffer (current-buffer)
                           '((display-buffer-reuse-window
                              display-buffer-in-side-window)
                             (side . bottom)
                             (window-height . fit-window-to-buffer)))))
      (error
       (message "Failed to fetch stale issues: %s" (error-message-string err))))))

(defun beads-stale--id-at-point ()
  "Return stale issue ID at point, or nil."
  (beads-core-id-at-point 'beads-stale-id))

(defun beads-stale-goto-issue ()
  "Open the stale issue at point in detail view."
  (interactive)
  (beads-core-goto-issue-at-point 'beads-stale-id))

(defun beads-stale-claim ()
  "Claim the stale issue at point by setting status to in_progress."
  (interactive)
  (let ((id (beads-stale--id-at-point)))
    (unless id
      (user-error "No issue at point"))
    (condition-case err
        (progn
          (beads-rpc-update id :status "in_progress")
          (message "Claimed %s" id)
          (beads-stale-refresh))
      (beads-rpc-error
       (user-error "Failed to claim issue: %s" (error-message-string err))))))

(defun beads-stale-refresh ()
  "Refresh the stale issues list."
  (interactive)
  (unless (derived-mode-p 'beads-stale-mode)
    (user-error "Not in beads-stale-mode"))
  (condition-case err
      (let ((issues (beads-stale--fetch
                     (or beads-stale--days beads-stale-days)
                     beads-stale--status)))
        (setq beads-stale--data issues)
        (let ((saved-point (point)))
          (beads-stale--render issues)
          (goto-char (min saved-point (point-max))))
        (message "Refreshed stale issues"))
    (error
     (message "Failed to refresh: %s" (error-message-string err)))))

(defun beads-stale-set-days (days)
  "Set the staleness threshold to DAYS and refresh."
  (interactive "nDays threshold: ")
  (unless (derived-mode-p 'beads-stale-mode)
    (user-error "Not in beads-stale-mode"))
  (setq beads-stale--days days)
  (beads-stale-refresh))

(defun beads-stale-set-filter (status)
  "Set the status filter to STATUS and refresh."
  (interactive
   (list (let ((choice (completing-read
                        "Filter by status: "
                        '("all" "open" "in_progress" "blocked")
                        nil t)))
           (if (string= choice "all") nil choice))))
  (unless (derived-mode-p 'beads-stale-mode)
    (user-error "Not in beads-stale-mode"))
  (setq beads-stale--status status)
  (beads-stale-refresh))

(provide 'beads-stale)
;;; beads-stale.el ends here
