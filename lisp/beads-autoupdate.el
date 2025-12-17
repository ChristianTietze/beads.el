;;; beads-autoupdate.el --- Auto-refresh for Beads list buffers -*- lexical-binding: t -*-

;;; Code:

(declare-function beads-list-refresh "beads-list")

(defgroup beads-autoupdate nil
  "Auto-refresh settings for Beads list buffers."
  :group 'beads)

(defcustom beads-autoupdate-interval 30
  "Seconds between auto-refresh of beads list buffers.
Set to nil to disable auto-refresh."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'beads-autoupdate)

(defcustom beads-autoupdate-enable t
  "Whether to enable auto-update mode by default in beads-list-mode."
  :type 'boolean
  :group 'beads-autoupdate)

(defvar-local beads-autoupdate-message t
  "Whether to show message on auto-refresh.
Can be set in .dir-locals.el to reduce noise:
  ((beads-list-mode . ((beads-autoupdate-message . nil))))")
(put 'beads-autoupdate-message 'safe-local-variable #'booleanp)

(defvar-local beads-autoupdate--timer nil
  "Timer for auto-refresh in this buffer.")

(defun beads-autoupdate--refresh ()
  "Perform auto-refresh if buffer is still valid."
  (when (and (buffer-live-p (current-buffer))
             (derived-mode-p 'beads-list-mode))
    (beads-list-refresh (not beads-autoupdate-message))))

(defun beads-autoupdate--start-timer ()
  "Start the auto-refresh timer for current buffer."
  (beads-autoupdate--stop-timer)
  (when (and beads-autoupdate-interval
             (> beads-autoupdate-interval 0))
    (let ((buffer (current-buffer)))
      (setq beads-autoupdate--timer
            (run-with-timer
             beads-autoupdate-interval
             beads-autoupdate-interval
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (beads-autoupdate--refresh)))))))))

(defun beads-autoupdate--stop-timer ()
  "Stop the auto-refresh timer for current buffer."
  (when beads-autoupdate--timer
    (cancel-timer beads-autoupdate--timer)
    (setq beads-autoupdate--timer nil)))

(defun beads-autoupdate-start ()
  "Start auto-refresh for current beads list buffer."
  (interactive)
  (beads-autoupdate--start-timer)
  (message "Auto-refresh started (interval: %ds)" beads-autoupdate-interval))

(defun beads-autoupdate-stop ()
  "Stop auto-refresh for current beads list buffer."
  (interactive)
  (beads-autoupdate--stop-timer)
  (message "Auto-refresh stopped"))

(define-minor-mode beads-autoupdate-mode
  "Minor mode for auto-refreshing beads list buffers.
When enabled, the buffer refreshes automatically at intervals
defined by `beads-autoupdate-interval'."
  :lighter " AutoUpd"
  :group 'beads-autoupdate
  (if beads-autoupdate-mode
      (beads-autoupdate--start-timer)
    (beads-autoupdate--stop-timer)))

(defun beads-autoupdate--maybe-enable ()
  "Enable autoupdate mode if configured to do so."
  (when (and beads-autoupdate-enable
             beads-autoupdate-interval)
    (beads-autoupdate-mode 1)))

(defun beads-autoupdate--cleanup ()
  "Clean up timer when buffer is killed."
  (beads-autoupdate--stop-timer))

(add-hook 'beads-list-mode-hook #'beads-autoupdate--maybe-enable)
(add-hook 'kill-buffer-hook #'beads-autoupdate--cleanup)

(provide 'beads-autoupdate)
;;; beads-autoupdate.el ends here
