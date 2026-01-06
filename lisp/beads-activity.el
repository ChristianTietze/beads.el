;;; beads-activity.el --- Activity feed for Beads -*- lexical-binding: t -*-

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

;; Real-time activity feed showing issue creates, updates, deletes, and comments.
;; Supports both historical view and live polling mode.

;;; Code:

(require 'json)

(declare-function beads-detail-open "beads-detail")
(declare-function beads-rpc-show "beads-rpc")

(defgroup beads-activity nil
  "Activity feed for Beads."
  :group 'beads)

(defcustom beads-activity-limit 50
  "Default number of events to show in activity feed."
  :type 'integer
  :group 'beads-activity)

(defcustom beads-activity-poll-interval 5
  "Polling interval in seconds for follow mode."
  :type 'integer
  :group 'beads-activity)

(defcustom beads-activity-glyphs
  '((create . "+")
    (update . "→")
    (status-in_progress . "→")
    (status-closed . "✓")
    (status-blocked . "✗")
    (status-open . "○")
    (delete . "⊘")
    (comment . "💬"))
  "Alist mapping event types to display glyphs.
Keys are symbols: create, update, status-STATUS, delete, comment.
Values are strings to display."
  :type '(alist :key-type symbol :value-type string)
  :group 'beads-activity)

(defcustom beads-activity-glyphs-ascii
  '((create . "+")
    (update . ">")
    (status-in_progress . ">")
    (status-closed . "x")
    (status-blocked . "!")
    (status-open . "o")
    (delete . "-")
    (comment . "#"))
  "ASCII-only glyphs for activity feed.
Use this as value for `beads-activity-glyphs' in terminals without Unicode."
  :type '(alist :key-type symbol :value-type string)
  :group 'beads-activity)

(defface beads-activity-create
  '((t :foreground "dodger blue"))
  "Face for create events.")

(defface beads-activity-update
  '((t :foreground "yellow"))
  "Face for update events.")

(defface beads-activity-completed
  '((t :foreground "green"))
  "Face for completed/closed events.")

(defface beads-activity-blocked
  '((t :foreground "red"))
  "Face for blocked/failed events.")

(defface beads-activity-delete
  '((t :foreground "red"))
  "Face for delete events.")

(defface beads-activity-comment
  '((t :foreground "gray"))
  "Face for comment events.")

(defface beads-activity-timestamp
  '((t :inherit shadow))
  "Face for timestamps.")

(defface beads-activity-actor
  '((t :inherit font-lock-keyword-face))
  "Face for actor names.")

(defvar-local beads-activity--data nil
  "List of activity events in current buffer.")

(defvar-local beads-activity--follow-mode nil
  "Whether follow mode is active (auto-refresh with polling).")

(defvar-local beads-activity--timer nil
  "Timer for polling in follow mode.")

(defvar-local beads-activity--filter-prefix nil
  "Filter events to this issue prefix.")

(defvar-local beads-activity--filter-type nil
  "Filter events to this type.")

(defvar beads-activity-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'beads-activity-goto-issue)
    (define-key map (kbd "f") #'beads-activity-toggle-follow)
    (define-key map (kbd "F") #'beads-activity-set-filter)
    (define-key map (kbd "g") #'beads-activity-refresh)
    (define-key map (kbd "l") #'beads-activity-set-limit)
    (define-key map (kbd "q") #'beads-activity-quit)
    map)
  "Keymap for beads-activity-mode.")

(define-derived-mode beads-activity-mode special-mode "Beads-Activity"
  "Major mode for viewing Beads activity feed.

Shows issue creates, updates, deletes, and comments in real-time.

\\{beads-activity-mode-map}"
  (setq buffer-read-only t))

(defun beads-activity--fetch (&optional limit prefix type)
  "Fetch activity events via CLI.
LIMIT is the max number of events (default `beads-activity-limit').
PREFIX filters to issues starting with this string.
TYPE filters to this event type."
  (let* ((bd-program (or (executable-find "bd") "bd"))
         (limit (or limit beads-activity-limit))
         (args (list "activity" "--limit" (number-to-string limit) "--json"))
         (args (if prefix (append args (list "--mol" prefix)) args))
         (args (if type (append args (list "--type" type)) args))
         (output (with-temp-buffer
                   (let ((exit-code (apply #'call-process bd-program nil t nil args)))
                     (unless (zerop exit-code)
                       (signal 'error
                               (list (format "bd activity failed: %s"
                                             (buffer-string)))))
                     (goto-char (point-min))
                     (condition-case nil
                         (json-read)
                       (json-error nil))))))
    (if (vectorp output) (append output nil) output)))

(defun beads-activity--glyph-for-event (event)
  "Get display glyph for EVENT based on type and status."
  (let* ((type (alist-get 'type event))
         (new-status (alist-get 'new_status event))
         (key (cond
               ((and (string= type "status") new-status)
                (intern (format "status-%s" new-status)))
               (t (intern type)))))
    (or (alist-get key beads-activity-glyphs)
        (alist-get 'update beads-activity-glyphs)
        "·")))

(defun beads-activity--face-for-event (event)
  "Get face for EVENT based on type and status."
  (let ((type (alist-get 'type event))
        (new-status (alist-get 'new_status event)))
    (cond
     ((string= type "create") 'beads-activity-create)
     ((string= type "delete") 'beads-activity-delete)
     ((string= type "comment") 'beads-activity-comment)
     ((string= new-status "closed") 'beads-activity-completed)
     ((string= new-status "blocked") 'beads-activity-blocked)
     ((string= new-status "in_progress") 'beads-activity-update)
     ((string= type "update") 'beads-activity-update)
     ((string= type "status") 'beads-activity-update)
     (t 'default))))

(defun beads-activity--format-timestamp (timestamp)
  "Format TIMESTAMP for display as HH:MM."
  (if (and timestamp (stringp timestamp))
      (let ((time (date-to-time timestamp)))
        (format-time-string "%H:%M" time))
    "??:??"))

(defun beads-activity--render (events)
  "Render activity EVENTS into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Activity Feed\n" 'face 'bold))
    (insert (propertize
             (format "Showing %d events%s\n"
                     (length events)
                     (if beads-activity--follow-mode " [LIVE]" ""))
             'face 'shadow))
    (insert (propertize
             "RET=view  f=follow  F=filter  g=refresh  l=limit  q=quit\n"
             'face 'shadow))
    (insert (make-string 60 ?=) "\n\n")
    (if (null events)
        (insert "No activity events found.\n")
      (dolist (event (reverse events))
        (let* ((timestamp (alist-get 'timestamp event))
               (issue-id (alist-get 'issue_id event))
               (message (alist-get 'message event))
               (actor (alist-get 'actor event))
               (glyph (beads-activity--glyph-for-event event))
               (face (beads-activity--face-for-event event))
               (time-str (beads-activity--format-timestamp timestamp))
               (start (point)))
          (insert (propertize time-str 'face 'beads-activity-timestamp))
          (insert " ")
          (insert (propertize glyph 'face face))
          (insert " ")
          (insert (propertize (or issue-id "???") 'face 'bold))
          (when actor
            (insert " ")
            (insert (propertize (format "@%s" actor) 'face 'beads-activity-actor)))
          (insert "\n")
          (when message
            (insert (format "   %s\n" (truncate-string-to-width message 55))))
          (put-text-property start (point) 'beads-activity-id issue-id)
          (put-text-property start (point) 'beads-activity-event event))))
    (when beads-activity--follow-mode
      (goto-char (point-max)))))

;;;###autoload
(defun beads-activity (&optional limit)
  "Display activity feed with interactive actions.
LIMIT is the max number of events to show."
  (interactive "P")
  (let ((limit (or limit beads-activity-limit)))
    (condition-case err
        (let ((events (beads-activity--fetch limit
                                             beads-activity--filter-prefix
                                             beads-activity--filter-type)))
          (with-current-buffer (get-buffer-create "*Beads Activity*")
            (beads-activity-mode)
            (setq beads-activity--data events)
            (beads-activity--render events)
            (goto-char (point-min))
            (pop-to-buffer (current-buffer)
                           '((display-buffer-reuse-window
                              display-buffer-in-side-window)
                             (side . bottom)
                             (window-height . fit-window-to-buffer)))))
      (error
       (message "Failed to fetch activity: %s" (error-message-string err))))))

(defun beads-activity--id-at-point ()
  "Return issue ID at point, or nil."
  (get-text-property (point) 'beads-activity-id))

(defun beads-activity-goto-issue ()
  "Open the issue at point in detail view."
  (interactive)
  (let ((id (beads-activity--id-at-point)))
    (unless id
      (user-error "No issue at point"))
    (condition-case err
        (let ((issue (beads-rpc-show id)))
          (beads-detail-open issue))
      (beads-rpc-error
       (user-error "Failed to load issue: %s" (error-message-string err))))))

(defun beads-activity-refresh ()
  "Refresh the activity feed."
  (interactive)
  (unless (derived-mode-p 'beads-activity-mode)
    (user-error "Not in beads-activity-mode"))
  (condition-case err
      (let ((events (beads-activity--fetch beads-activity-limit
                                           beads-activity--filter-prefix
                                           beads-activity--filter-type)))
        (setq beads-activity--data events)
        (let ((at-end (eobp)))
          (beads-activity--render events)
          (if (and beads-activity--follow-mode at-end)
              (goto-char (point-max))
            (goto-char (point-min))))
        (unless beads-activity--follow-mode
          (message "Refreshed activity feed")))
    (error
     (message "Failed to refresh: %s" (error-message-string err)))))

(defun beads-activity--start-polling ()
  "Start polling timer for follow mode."
  (beads-activity--stop-polling)
  (setq beads-activity--timer
        (run-with-timer beads-activity-poll-interval
                        beads-activity-poll-interval
                        (lambda ()
                          (when (buffer-live-p (get-buffer "*Beads Activity*"))
                            (with-current-buffer "*Beads Activity*"
                              (when beads-activity--follow-mode
                                (beads-activity-refresh))))))))

(defun beads-activity--stop-polling ()
  "Stop polling timer."
  (when beads-activity--timer
    (cancel-timer beads-activity--timer)
    (setq beads-activity--timer nil)))

(defun beads-activity-toggle-follow ()
  "Toggle follow mode (live updates)."
  (interactive)
  (unless (derived-mode-p 'beads-activity-mode)
    (user-error "Not in beads-activity-mode"))
  (setq beads-activity--follow-mode (not beads-activity--follow-mode))
  (if beads-activity--follow-mode
      (progn
        (beads-activity--start-polling)
        (beads-activity-refresh)
        (message "Follow mode ON (polling every %ds)" beads-activity-poll-interval))
    (beads-activity--stop-polling)
    (beads-activity-refresh)
    (message "Follow mode OFF")))

(defun beads-activity-set-filter ()
  "Set filter for activity feed."
  (interactive)
  (unless (derived-mode-p 'beads-activity-mode)
    (user-error "Not in beads-activity-mode"))
  (let* ((prefix (read-string "Filter by issue prefix (empty=all): "
                              beads-activity--filter-prefix))
         (type-choice (completing-read "Filter by type: "
                                       '("all" "create" "update" "status" "delete" "comment")
                                       nil t))
         (type (unless (string= type-choice "all") type-choice)))
    (setq beads-activity--filter-prefix (unless (string-empty-p prefix) prefix))
    (setq beads-activity--filter-type type)
    (beads-activity-refresh)))

(defun beads-activity-set-limit (limit)
  "Set the event LIMIT and refresh."
  (interactive "nEvent limit: ")
  (unless (derived-mode-p 'beads-activity-mode)
    (user-error "Not in beads-activity-mode"))
  (setq beads-activity-limit limit)
  (beads-activity-refresh))

(defun beads-activity-quit ()
  "Quit activity buffer, stopping follow mode if active."
  (interactive)
  (beads-activity--stop-polling)
  (quit-window))

(provide 'beads-activity)
;;; beads-activity.el ends here
