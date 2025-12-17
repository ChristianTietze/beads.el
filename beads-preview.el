;;; beads-preview.el --- Quicklook preview mode for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'beads-detail)
(require 'beads-rpc)

(declare-function beads--get-issue-at-point "beads-list")

(defgroup beads-preview nil
  "Automatic issue preview for Beads."
  :group 'beads)

(defcustom beads-preview-delay 0.1
  "Delay in seconds before showing preview after cursor movement."
  :type 'number
  :group 'beads-preview)

(defcustom beads-preview-max-buffers 3
  "Maximum number of preview buffers to keep in the pool."
  :type 'integer
  :group 'beads-preview)

(defvar-local beads-preview--timer nil
  "Idle timer for debouncing preview updates.")

(defvar-local beads-preview--current-issue-id nil
  "Issue ID currently displayed in preview (deduplication).")

(defvar beads-preview--buffers nil
  "List of preview buffers for pooling and cleanup.")

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
    (if-let ((issue (beads--get-issue-at-point)))
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
            (let ((full-issue (beads-rpc-show issue-id)))
              (beads-detail-show full-issue)
              (let ((detail-buffer (get-buffer "*beads-detail*")))
                (when detail-buffer
                  (beads-preview--track-buffer detail-buffer)
                  (beads-preview--cleanup-buffers))))
          (beads-rpc-error
           (message "Preview failed: %s" (error-message-string err))))))))

(defun beads-preview--track-buffer (buffer)
  "Add BUFFER to the pool for tracking."
  (setq beads-preview--buffers
        (cons buffer (delq buffer beads-preview--buffers))))

(defun beads-preview--cleanup-buffers ()
  "Clean up old preview buffers, keeping only the N most recent."
  (let ((live-buffers (seq-filter #'buffer-live-p beads-preview--buffers)))
    (when (> (length live-buffers) beads-preview-max-buffers)
      (dolist (buf (nthcdr beads-preview-max-buffers live-buffers))
        (when (buffer-live-p buf)
          (kill-buffer buf))))
    (setq beads-preview--buffers
          (seq-take live-buffers beads-preview-max-buffers))))

(defun beads-preview--cleanup ()
  "Full cleanup when preview mode is disabled."
  (beads-preview--cancel-timer)
  (setq beads-preview--current-issue-id nil)
  (dolist (buf beads-preview--buffers)
    (when (buffer-live-p buf)
      (let ((window (get-buffer-window buf)))
        (when window
          (delete-window window)))))
  (setq beads-preview--buffers nil))

(provide 'beads-preview)
;;; beads-preview.el ends here
