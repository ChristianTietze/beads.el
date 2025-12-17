;;; beads-list.el --- Issue list mode for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'beads-rpc)
(require 'beads-detail)
(require 'beads-preview)
(require 'tabulated-list)

(declare-function beads-menu "beads-transient")

(defgroup beads-list nil
  "Issue list display for Beads."
  :group 'beads)

(defface beads-list-status-open
  '((t :inherit default))
  "Face for open status.")

(defface beads-list-status-in-progress
  '((t :foreground "yellow"))
  "Face for in_progress status.")

(defface beads-list-status-closed
  '((t :foreground "green"))
  "Face for closed status.")

(defface beads-list-priority-p0
  '((t :foreground "red" :weight bold))
  "Face for P0 priority.")

(defface beads-list-priority-p1
  '((t :foreground "orange"))
  "Face for P1 priority.")

(defvar beads-list--issues nil
  "Cached list of issues for current buffer.")

(defvar beads-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'beads-list-refresh)
    (define-key map (kbd "RET") #'beads-list-goto-issue)
    (define-key map (kbd "P") #'beads-preview-mode)
    (define-key map (kbd "q") #'beads-list-quit)
    (define-key map (kbd "?") #'beads-menu)
    (define-key map (kbd "C-c m") #'beads-menu)
    map)
  "Keymap for beads-list-mode.")

(define-derived-mode beads-list-mode tabulated-list-mode "Beads-List"
  "Major mode for displaying Beads issues in a table.

\\{beads-list-mode-map}"
  (setq tabulated-list-format
        [("ID" 10 t)
         ("Status" 12 t)
         ("Pri" 4 t)
         ("Type" 8 t)
         ("Title" 50 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "ID" nil))
  (add-hook 'tabulated-list-revert-hook #'beads-list-refresh nil t)
  (tabulated-list-init-header)
  (hl-line-mode 1))

(defun beads-list-refresh ()
  "Fetch issues from daemon and refresh the display."
  (interactive)
  (condition-case err
      (let ((issues (beads-rpc-list)))
        (setq beads-list--issues (append issues nil))
        (setq tabulated-list-entries (beads-list-entries beads-list--issues))
        (tabulated-list-print t)
        (message "Refreshed %d issues" (length beads-list--issues)))
    (beads-rpc-error
     (message "Failed to fetch issues: %s" (error-message-string err)))))

(defun beads-list-entries (issues)
  "Convert ISSUES to tabulated-list entries."
  (mapcar (lambda (issue)
            (let ((id (alist-get 'id issue)))
              (list id
                    (vector
                     (beads--format-id issue)
                     (beads--format-status issue)
                     (beads--format-priority issue)
                     (beads--format-type issue)
                     (beads--format-title issue)))))
          issues))

(defun beads--format-id (issue)
  "Format ID column for ISSUE."
  (alist-get 'id issue))

(defun beads--format-status (issue)
  "Format status column for ISSUE with face."
  (let ((status (alist-get 'status issue)))
    (propertize status
                'face (pcase status
                        ("closed" 'beads-list-status-closed)
                        ("in_progress" 'beads-list-status-in-progress)
                        (_ 'beads-list-status-open)))))

(defun beads--format-priority (issue)
  "Format priority column for ISSUE as P0-P4 with face."
  (let* ((priority (alist-get 'priority issue))
         (priority-str (format "P%d" priority)))
    (propertize priority-str
                'face (pcase priority
                        (0 'beads-list-priority-p0)
                        (1 'beads-list-priority-p1)
                        (_ 'default)))))

(defun beads--format-type (issue)
  "Format type column for ISSUE with abbreviation."
  (let ((type (alist-get 'issue_type issue)))
    (pcase type
      ("bug" "bug")
      ("feature" "feat")
      ("task" "task")
      ("epic" "epic")
      ("chore" "chor")
      (_ type))))

(defun beads--format-title (issue)
  "Format title column for ISSUE, truncating if needed."
  (let ((title (alist-get 'title issue "")))
    (if (> (length title) 50)
        (concat (substring title 0 47) "...")
      title)))

(defun beads--get-issue-at-point ()
  "Get issue data at current line.
Returns the issue alist or nil if not found."
  (when-let ((id (tabulated-list-get-id)))
    (seq-find (lambda (issue)
                (string= (alist-get 'id issue) id))
              beads-list--issues)))

(defun beads-list-quit ()
  "Quit beads list, closing preview first if active."
  (interactive)
  (if beads-preview-mode
      (beads-preview-mode -1)
    (quit-window)))

(defun beads-list-goto-issue ()
  "Navigate to or display details for issue at point."
  (interactive)
  (if-let ((issue (beads--get-issue-at-point)))
      (condition-case err
          (let ((id (alist-get 'id issue)))
            (let ((full-issue (beads-rpc-show id)))
              (beads-detail-open full-issue)))
        (beads-rpc-error
         (message "Failed to fetch issue details: %s" (error-message-string err))))
    (message "No issue at point")))

;;;###autoload
(defun beads-list ()
  "Open the Beads issue list buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Beads Issues*")))
    (with-current-buffer buffer
      (beads-list-mode)
      (beads-list-refresh))
    (switch-to-buffer buffer)))

(provide 'beads-list)
;;; beads-list.el ends here
