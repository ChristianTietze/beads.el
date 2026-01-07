;;; beads-lint.el --- Issue quality linting for Beads -*- lexical-binding: t -*-

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

;; Issue quality validation.  Checks issues for missing recommended
;; template sections based on issue type (bugs need Steps to Reproduce,
;; tasks need Acceptance Criteria, etc.).

;;; Code:

(require 'json)

(declare-function beads-detail-open "beads-detail")
(declare-function beads-rpc-show "beads-rpc")

(defgroup beads-lint nil
  "Issue quality linting for Beads."
  :group 'beads)

(defface beads-lint-issue-id
  '((t :inherit link))
  "Face for issue IDs in lint report.")

(defface beads-lint-type-header
  '((t :inherit bold :underline t))
  "Face for type section headers.")

(defface beads-lint-missing
  '((t :inherit warning))
  "Face for missing sections.")

(defface beads-lint-warning-count
  '((t :inherit error :weight bold))
  "Face for warning counts.")

(defvar-local beads-lint--data nil
  "Parsed lint data in current buffer.")

(defvar-local beads-lint--type-filter nil
  "Current type filter (nil = all types).")

(defvar beads-lint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'beads-lint-goto-issue)
    (define-key map (kbd "g") #'beads-lint-refresh)
    (define-key map (kbd "f") #'beads-lint-filter-type)
    (define-key map (kbd "n") #'beads-lint-next-issue)
    (define-key map (kbd "p") #'beads-lint-prev-issue)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for beads-lint-mode.")

(define-derived-mode beads-lint-mode special-mode "Beads-Lint"
  "Major mode for viewing Beads issue lint report.

Checks issues for missing recommended template sections.

\\{beads-lint-mode-map}"
  (setq buffer-read-only t))

(defun beads-lint--fetch (&optional type-filter)
  "Fetch lint results via CLI.
Optional TYPE-FILTER limits to specific issue type."
  (let* ((bd-program (or (executable-find "bd") "bd"))
         (args (list "lint" "--json"))
         (args (if type-filter
                   (append args (list "--type" type-filter))
                 args))
         (output (with-temp-buffer
                   (let ((exit-code (apply #'call-process bd-program nil t nil args)))
                     (unless (zerop exit-code)
                       (signal 'error
                               (list (format "bd lint failed: %s"
                                             (buffer-string)))))
                     (goto-char (point-min))
                     (condition-case nil
                         (json-read)
                       (json-error nil))))))
    output))

(defun beads-lint--group-by-type (results)
  "Group RESULTS by issue type.
Returns alist of (type . issues)."
  (let ((groups nil))
    (dolist (issue (if (vectorp results) (append results nil) results))
      (let* ((type (alist-get 'type issue "unknown"))
             (existing (assoc type groups)))
        (if existing
            (setcdr existing (cons issue (cdr existing)))
          (push (cons type (list issue)) groups))))
    (dolist (group groups)
      (setcdr group (nreverse (cdr group))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

(defun beads-lint--render (data)
  "Render lint report DATA into current buffer."
  (let ((inhibit-read-only t)
        (results (alist-get 'results data))
        (total (alist-get 'total data 0)))
    (erase-buffer)
    (insert (propertize "Issue Lint Report\n" 'face 'bold))
    (insert (propertize
             "Check issues for missing recommended template sections\n"
             'face 'shadow))
    (insert (propertize
             "RET=view  f=filter type  n/p=next/prev  g=refresh  q=quit\n"
             'face 'shadow))
    (insert (make-string 60 ?=) "\n\n")
    (if (or (null results) (= total 0))
        (insert (propertize "All issues pass lint checks!\n" 'face 'success))
      (insert (format "%d issue(s) with missing sections"
                      total))
      (when beads-lint--type-filter
        (insert (format " (filtered: %s)" beads-lint--type-filter)))
      (insert "\n\n")
      (let ((grouped (beads-lint--group-by-type results)))
        (dolist (group grouped)
          (let ((type (car group))
                (issues (cdr group)))
            (insert (propertize (format "## %s (%d)\n"
                                        (capitalize type)
                                        (length issues))
                                'face 'beads-lint-type-header))
            (dolist (issue issues)
              (let* ((id (alist-get 'id issue))
                     (title (alist-get 'title issue))
                     (missing (alist-get 'missing issue))
                     (missing-list (if (vectorp missing)
                                       (append missing nil)
                                     missing))
                     (start (point)))
                (insert "  ")
                (insert (propertize (format "%-12s" id) 'face 'beads-lint-issue-id))
                (insert " ")
                (insert (truncate-string-to-width (or title "") 30 nil nil t))
                (insert "\n")
                (insert "    ")
                (insert (propertize "Missing: " 'face 'shadow))
                (insert (propertize
                         (mapconcat (lambda (s)
                                      (replace-regexp-in-string "^## " "" s))
                                    missing-list ", ")
                         'face 'beads-lint-missing))
                (insert "\n")
                (put-text-property start (point) 'beads-lint-id id)))
            (insert "\n")))))))

;;;###autoload
(defun beads-lint ()
  "Display issue lint report.
Shows issues missing required template sections based on their type."
  (interactive)
  (condition-case err
      (let ((data (beads-lint--fetch)))
        (with-current-buffer (get-buffer-create "*Beads Lint*")
          (beads-lint-mode)
          (setq beads-lint--data data)
          (setq beads-lint--type-filter nil)
          (beads-lint--render data)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer))))
    (error
     (message "Failed to run lint: %s" (error-message-string err)))))

(defun beads-lint--id-at-point ()
  "Return issue ID at point, or nil."
  (get-text-property (point) 'beads-lint-id))

(defun beads-lint-goto-issue ()
  "Open the issue at point in detail view."
  (interactive)
  (let ((id (beads-lint--id-at-point)))
    (unless id
      (user-error "No issue at point"))
    (condition-case err
        (let ((issue (beads-rpc-show id)))
          (beads-detail-open issue))
      (beads-rpc-error
       (user-error "Failed to load issue: %s" (error-message-string err))))))

(defun beads-lint-refresh ()
  "Refresh the lint report."
  (interactive)
  (unless (derived-mode-p 'beads-lint-mode)
    (user-error "Not in beads-lint-mode"))
  (condition-case err
      (let ((data (beads-lint--fetch beads-lint--type-filter)))
        (setq beads-lint--data data)
        (let ((saved-point (point)))
          (beads-lint--render data)
          (goto-char (min saved-point (point-max))))
        (message "Refreshed lint report"))
    (error
     (message "Failed to refresh: %s" (error-message-string err)))))

(defun beads-lint-filter-type ()
  "Filter lint report by issue type."
  (interactive)
  (unless (derived-mode-p 'beads-lint-mode)
    (user-error "Not in beads-lint-mode"))
  (let* ((types '("bug" "task" "feature" "epic" "chore"))
         (choices (cons "(all)" types))
         (choice (completing-read "Filter by type: " choices nil t)))
    (setq beads-lint--type-filter (if (string= choice "(all)") nil choice))
    (beads-lint-refresh)))

(defun beads-lint-next-issue ()
  "Move to next issue in lint report."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (beads-lint--id-at-point)))
      (forward-line 1))
    (unless (beads-lint--id-at-point)
      (goto-char start)
      (user-error "No more issues"))))

(defun beads-lint-prev-issue ()
  "Move to previous issue in lint report."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (beads-lint--id-at-point)))
      (forward-line -1))
    (unless (beads-lint--id-at-point)
      (goto-char start)
      (user-error "No previous issue"))))

(provide 'beads-lint)
;;; beads-lint.el ends here
