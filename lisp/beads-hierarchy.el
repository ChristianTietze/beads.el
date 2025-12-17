;;; beads-hierarchy.el --- Dependency tree view for Beads -*- lexical-binding: t -*-

;;; Code:

(require 'hierarchy)
(require 'wid-edit)
(require 'beads-rpc)

(declare-function beads-detail-open "beads-detail")

(defgroup beads-hierarchy nil
  "Dependency tree display for Beads."
  :group 'beads)

(defvar-local beads-hierarchy--root-id nil
  "Root issue ID for current hierarchy buffer.")

(defvar-local beads-hierarchy--hierarchy nil
  "The hierarchy object for current buffer.")

(defvar-local beads-hierarchy--by-id nil
  "Hash table mapping issue IDs to issue data.")

(defvar beads-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'beads-hierarchy-refresh)
    (define-key map (kbd "RET") #'beads-hierarchy-goto-issue)
    map)
  "Keymap for beads-hierarchy-mode.")

(define-derived-mode beads-hierarchy-mode special-mode "Beads-Deps"
  "Major mode for displaying issue dependency trees.

\\{beads-hierarchy-mode-map}")

(defun beads-hierarchy--status-face (status)
  "Return face for STATUS."
  (pcase status
    ("closed" 'beads-list-status-closed)
    ("in_progress" 'beads-list-status-in-progress)
    (_ 'beads-list-status-open)))

(defun beads-hierarchy--labelfn (issue indent)
  "Insert formatted ISSUE at INDENT level.
INDENT is ignored as hierarchy.el handles indentation via tree widget."
  (ignore indent)
  (let ((id (alist-get 'id issue))
        (title (alist-get 'title issue ""))
        (status (alist-get 'status issue "open")))
    (insert (propertize id 'face 'beads-detail-id-face))
    (insert " ")
    (insert (truncate-string-to-width title 40 nil nil "…"))
    (insert " ")
    (insert (propertize (format "[%s]" status)
                        'face (beads-hierarchy--status-face status)))))

(defun beads-hierarchy--build (issue-id)
  "Build hierarchy for ISSUE-ID from dep_tree RPC.
Returns a cons of (hierarchy . by-id-hash)."
  (condition-case err
      (let* ((tree-data (beads-rpc-dep-tree issue-id))
             (h (hierarchy-new))
             (by-id (make-hash-table :test 'equal)))

        (seq-doseq (issue tree-data)
          (puthash (alist-get 'id issue) issue by-id))

        (seq-doseq (issue tree-data)
          (hierarchy-add-tree h issue
            (lambda (i)
              (let ((pid (alist-get 'parent_id i))
                    (id (alist-get 'id i)))
                (unless (equal pid id)
                  (gethash pid by-id))))))

        (cons h by-id))
    (beads-rpc-error
     (message "Failed to fetch dependency tree: %s" (error-message-string err))
     nil)))

(defun beads-hierarchy-goto-issue ()
  "Open detail view for issue at point."
  (interactive)
  (let ((issue nil))
    (when-let ((widget (widget-at (point))))
      (setq issue (widget-get widget :node)))
    (unless issue
      (when beads-hierarchy--by-id
        (save-excursion
          (beginning-of-line)
          (when (re-search-forward "\\([a-z]+-[a-z0-9]+\\)" (line-end-position) t)
            (let ((id (match-string 1)))
              (setq issue (gethash id beads-hierarchy--by-id)))))))
    (if issue
        (condition-case err
            (let* ((id (alist-get 'id issue))
                   (full-issue (beads-rpc-show id)))
              (beads-detail-open full-issue))
          (beads-rpc-error
           (message "Failed to fetch issue: %s" (error-message-string err))))
      (message "No issue at point"))))

(defun beads-hierarchy-refresh ()
  "Refresh the dependency tree display."
  (interactive)
  (unless beads-hierarchy--root-id
    (user-error "No root issue set"))
  (beads-hierarchy-show beads-hierarchy--root-id))

;;;###autoload
(defun beads-hierarchy-show (issue-id)
  "Display dependency tree for ISSUE-ID in a side window."
  (interactive
   (list (cond
          ((and (boundp 'beads-detail--current-issue-id)
                beads-detail--current-issue-id)
           beads-detail--current-issue-id)
          ((and (derived-mode-p 'beads-list-mode)
                (tabulated-list-get-id))
           (tabulated-list-get-id))
          (t (read-string "Issue ID: ")))))
  (let ((result (beads-hierarchy--build issue-id)))
    (unless result
      (user-error "Could not build dependency tree"))
    (let* ((h (car result))
           (by-id (cdr result))
           (buffer-name (format "*beads-deps: %s*" issue-id))
           (buffer (get-buffer-create buffer-name)))
      (when (zerop (hierarchy-length h))
        (message "No dependencies found for %s" issue-id))
      (hierarchy-tree-display h #'beads-hierarchy--labelfn buffer)
      (with-current-buffer buffer
        (beads-hierarchy-mode)
        (setq beads-hierarchy--root-id issue-id)
        (setq beads-hierarchy--hierarchy h)
        (setq beads-hierarchy--by-id by-id)
        (goto-char (point-min)))
      (display-buffer buffer
                      '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 0.4))))))

(provide 'beads-hierarchy)
;;; beads-hierarchy.el ends here
