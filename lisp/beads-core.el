;;; beads-core.el --- Shared utilities for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools

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

;; Shared utilities for Beads report views and other modules.
;; Provides common patterns for:
;; - Buffer header rendering
;; - Issue navigation helpers
;; - CLI invocation wrappers
;; - Text property helpers

;;; Code:

(require 'json)

(declare-function beads-detail-open "beads-detail")
(declare-function beads-rpc-show "beads-rpc")

(defun beads-core-render-header (title description keybindings &optional separator-width)
  "Render a standard report header.
TITLE is displayed bold, DESCRIPTION and KEYBINDINGS in shadow face.
SEPARATOR-WIDTH defaults to 50."
  (let ((width (or separator-width 50)))
    (insert (propertize (concat title "\n") 'face 'bold))
    (insert (propertize (concat description "\n") 'face 'shadow))
    (insert (propertize (concat keybindings "\n") 'face 'shadow))
    (insert (make-string width ?=) "\n\n")))

(defun beads-core-id-at-point (property-name)
  "Return issue ID at point using PROPERTY-NAME.
PROPERTY-NAME should be a symbol like `beads-orphan-id'."
  (get-text-property (point) property-name))

(defun beads-core-goto-issue-at-point (property-name)
  "Open issue at point in detail view using PROPERTY-NAME for ID.
PROPERTY-NAME should be a symbol like `beads-orphan-id'."
  (let ((id (beads-core-id-at-point property-name)))
    (unless id
      (user-error "No issue at point"))
    (condition-case err
        (let ((issue (beads-rpc-show id)))
          (beads-detail-open issue))
      (beads-rpc-error
       (user-error "Failed to load issue: %s" (error-message-string err))))))

(defun beads-core-cli-request (subcommand &rest args)
  "Execute bd CLI with SUBCOMMAND and ARGS, returning parsed JSON.
Signals error if command fails or returns invalid JSON."
  (let* ((bd-program (or (executable-find "bd") "bd"))
         (full-args (append (list subcommand) args (list "--json"))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process bd-program nil t nil full-args)))
        (unless (zerop exit-code)
          (signal 'error
                  (list (format "bd %s failed: %s" subcommand (buffer-string)))))
        (goto-char (point-min))
        (condition-case nil
            (let ((output (json-read)))
              (if (vectorp output) (append output nil) output))
          (json-error nil))))))

(provide 'beads-core)
;;; beads-core.el ends here
