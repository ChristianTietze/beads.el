;;; beads-edit.el --- Dedicated editing buffers for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'beads-rpc)

(defvar beads-verbose)
(declare-function beads-show-hint "beads")

(defgroup beads-edit nil
  "Dedicated editing buffers for Beads issues."
  :group 'beads)

(defvar-local beads-edit--issue-id nil
  "Issue ID being edited in this buffer.")

(defvar-local beads-edit--field nil
  "Field name being edited (e.g., :description, :design).")

(defvar-local beads-edit--original-content nil
  "Original content before editing, for comparison.")

(defvar-local beads-edit--allow-write-back t
  "Gate flag for commit vs abort. Set to nil to discard changes.")

(defvar-local beads-edit--source-buffer nil
  "Buffer that initiated the edit (for refresh after commit).")

(defvar-local beads-edit--saved-window-config nil
  "Window configuration to restore on exit.")

(defvar beads-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-edit-commit)
    (define-key map (kbd "C-c C-k") #'beads-edit-abort)
    map)
  "Keymap for beads-edit-mode.")

(define-minor-mode beads-edit-mode
  "Minor mode for editing Beads issue fields in dedicated buffers.

\\{beads-edit-mode-map}"
  :lighter " BeadsEdit"
  :keymap beads-edit-mode-map
  (if beads-edit-mode
      (progn
        (setq header-line-format
              (substitute-command-keys
               "Edit: \\[beads-edit-commit] to save, \\[beads-edit-abort] to discard"))
        (when (bound-and-true-p beads-verbose)
          (run-at-time 0.1 nil #'message "C-c C-c save | C-c C-k discard")))
    (setq header-line-format nil)))

(defun beads-edit--exit ()
  "Common exit path for both commit and abort."
  (let ((write-back beads-edit--allow-write-back)
        (issue-id beads-edit--issue-id)
        (field beads-edit--field)
        (content (buffer-substring-no-properties (point-min) (point-max)))
        (original beads-edit--original-content)
        (source-buffer beads-edit--source-buffer)
        (window-config beads-edit--saved-window-config)
        (edit-buffer (current-buffer)))

    (when (and write-back
               issue-id
               field
               (not (string= content original)))
      (condition-case err
          (progn
            (beads-rpc-update issue-id field content)
            (message "Updated %s for %s" (substring (symbol-name field) 1) issue-id)
            (when (and source-buffer (buffer-live-p source-buffer))
              (with-current-buffer source-buffer
                (when (fboundp 'beads-detail-refresh)
                  (beads-detail-refresh)))))
        (beads-rpc-error
         (message "Failed to update: %s" (error-message-string err)))))

    (when (and (not write-back) (not (string= content original)))
      (message "Changes discarded"))

    (kill-buffer edit-buffer)

    (when window-config
      (set-window-configuration window-config))))

(defun beads-edit-commit ()
  "Save changes and close the edit buffer."
  (interactive)
  (unless beads-edit-mode
    (user-error "Not in a beads edit buffer"))
  (setq beads-edit--allow-write-back t)
  (beads-edit--exit))

(defun beads-edit-abort ()
  "Discard changes and close the edit buffer."
  (interactive)
  (unless beads-edit-mode
    (user-error "Not in a beads edit buffer"))
  (setq beads-edit--allow-write-back nil)
  (beads-edit--exit))

(defun beads-edit-field (issue-id field content &optional major-mode-fn)
  "Open dedicated buffer to edit FIELD of ISSUE-ID with CONTENT.
MAJOR-MODE-FN is the major mode to use (defaults to `text-mode').
Returns the edit buffer."
  (let* ((field-name (substring (symbol-name field) 1))
         (buffer-name (format "*Beads Edit: %s %s*" issue-id field-name))
         (buffer (get-buffer-create buffer-name))
         (source-buffer (current-buffer))
         (window-config (current-window-configuration)))

    (with-current-buffer buffer
      (erase-buffer)
      (funcall (or major-mode-fn #'text-mode))
      (insert (or content ""))
      (goto-char (point-min))
      (set-buffer-modified-p nil)

      (setq beads-edit--issue-id issue-id)
      (setq beads-edit--field field)
      (setq beads-edit--original-content (or content ""))
      (setq beads-edit--allow-write-back t)
      (setq beads-edit--source-buffer source-buffer)
      (setq beads-edit--saved-window-config window-config)

      (beads-edit-mode 1))

    (pop-to-buffer buffer)
    buffer))

(defun beads-edit-field-markdown (issue-id field content)
  "Open dedicated markdown buffer to edit FIELD of ISSUE-ID with CONTENT."
  (beads-edit-field issue-id field content
                    (if (fboundp 'markdown-mode)
                        #'markdown-mode
                      #'text-mode)))

(defun beads-edit-field-minibuffer (issue-id field current-value prompt)
  "Edit FIELD of ISSUE-ID via minibuffer with CURRENT-VALUE and PROMPT.
Returns the new value, or nil if unchanged."
  (let ((new-value (read-string prompt (or current-value ""))))
    (unless (string= new-value (or current-value ""))
      (condition-case err
          (progn
            (beads-rpc-update issue-id field new-value)
            (message "Updated %s for %s" (substring (symbol-name field) 1) issue-id)
            new-value)
        (beads-rpc-error
         (message "Failed to update: %s" (error-message-string err))
         nil)))))

(defun beads-edit-field-completing (issue-id field current-value prompt choices)
  "Edit FIELD of ISSUE-ID via completing-read.
CURRENT-VALUE is pre-filled in minibuffer, PROMPT is shown, CHOICES is
the list of options.  Returns the new value, or nil if unchanged."
  (let ((new-value (completing-read prompt choices nil t current-value)))
    (unless (string= new-value (or current-value ""))
      (condition-case err
          (progn
            (beads-rpc-update issue-id field new-value)
            (message "Updated %s for %s" (substring (symbol-name field) 1) issue-id)
            new-value)
        (beads-rpc-error
         (message "Failed to update: %s" (error-message-string err))
         nil)))))

(provide 'beads-edit)
;;; beads-edit.el ends here
