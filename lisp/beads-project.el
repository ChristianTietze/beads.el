;;; beads-project.el --- Project.el integration for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, project

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

;; Integration with project.el for per-project Beads buffers.

;;; Code:

(require 'project)

(declare-function beads-list "beads-list")

(defgroup beads-project nil
  "Project.el integration for Beads."
  :group 'beads)

(defcustom beads-project-per-project-buffers t
  "When non-nil, create separate beads list buffers per project.
Each project gets its own buffer named *Beads: project-name*."
  :type 'boolean
  :group 'beads-project)

(defcustom beads-project-buffer-name-function
  #'beads-project-default-buffer-name
  "Function to generate buffer name for project beads list.
Called with project root path, returns buffer name string."
  :type 'function
  :group 'beads-project)

(defcustom beads-project-add-switch-command t
  "When non-nil, add beads-project-list to project-switch-commands."
  :type 'boolean
  :group 'beads-project)

(defun beads-project-root ()
  "Get current project root, or nil if not in a project."
  (when-let ((proj (project-current)))
    (project-root proj)))

(defun beads-project-name (root)
  "Extract project name from ROOT path."
  (file-name-nondirectory (directory-file-name root)))

(defun beads-project-default-buffer-name (root)
  "Generate default buffer name for project at ROOT."
  (format "*Beads: %s*" (beads-project-name root)))

(defun beads-project-buffer-name ()
  "Get buffer name for current context.
Returns project-specific name if in a project and per-project buffers enabled,
otherwise returns the default buffer name."
  (if-let ((root (and beads-project-per-project-buffers
                      (beads-project-root))))
      (funcall beads-project-buffer-name-function root)
    "*Beads Issues*"))

;;;###autoload
(defun beads-project-list ()
  "Open beads list for current project.
If not in a project, opens the default beads list."
  (interactive)
  (if-let ((root (beads-project-root)))
      (let ((default-directory root))
        (beads-list))
    (beads-list)))

(defun beads-project--setup-switch-command ()
  "Add beads-project-list to project-switch-commands if configured."
  (when (and beads-project-add-switch-command
             (boundp 'project-switch-commands))
    (add-to-list 'project-switch-commands
                 '(beads-project-list "Beads issues") t)))

(add-hook 'after-init-hook #'beads-project--setup-switch-command)

(provide 'beads-project)
;;; beads-project.el ends here
