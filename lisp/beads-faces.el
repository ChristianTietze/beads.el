;;; beads-faces.el --- Shared faces and formatters for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, faces

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

;; Shared faces and formatting functions for displaying Beads issues
;; across different views (list, hierarchy, detail, etc.).

;;; Code:

(defgroup beads-faces nil
  "Faces for Beads issue display."
  :group 'beads)

(defface beads-status-open
  '((t :inherit default))
  "Face for open status."
  :group 'beads-faces)

(defface beads-status-in-progress
  '((t :foreground "yellow"))
  "Face for in_progress status."
  :group 'beads-faces)

(defface beads-status-closed
  '((t :foreground "green"))
  "Face for closed status."
  :group 'beads-faces)

(defface beads-status-blocked
  '((t :foreground "red"))
  "Face for blocked status."
  :group 'beads-faces)

(defface beads-status-hooked
  '((t :foreground "cyan"))
  "Face for hooked status (hook-based work assignment)."
  :group 'beads-faces)

(defface beads-priority-p0
  '((t :inherit error :inverse-video t))
  "Face for P0 priority.
Uses inverted error colors for maximum visibility."
  :group 'beads-faces)

(defface beads-priority-p1
  '((t :inherit error))
  "Face for P1 priority."
  :group 'beads-faces)

(defface beads-type-gate
  '((t :foreground "orange red"))
  "Face for gate type (checkpoint/approval gate)."
  :group 'beads-faces)

(defface beads-type-convoy
  '((t :foreground "deep sky blue"))
  "Face for convoy type (grouped work items)."
  :group 'beads-faces)

(defface beads-type-agent
  '((t :foreground "medium purple"))
  "Face for agent type (AI/automation tasks)."
  :group 'beads-faces)

(defface beads-type-role
  '((t :foreground "sea green"))
  "Face for role type (people/responsibility definitions)."
  :group 'beads-faces)

(defface beads-type-rig
  '((t :foreground "dark cyan"))
  "Face for rig type (Gas Town rig identity tracking)."
  :group 'beads-faces)

(defcustom beads-type-style 'full
  "How to display issue types.
When `full', display full type names (bug, feature, task, epic, chore).
When `short', display 4-character abbreviations (bug, feat, task, epic, chor)."
  :type '(choice (const :tag "Full names" full)
                 (const :tag "Short (4-char)" short))
  :group 'beads-faces)

(defcustom beads-type-glyph nil
  "Whether to show glyphs for special issue types.
When non-nil, display a unicode glyph prefix for gate, convoy,
agent, role, and rig types:
  gate   → ■
  convoy → ▶
  agent  → ◉
  role   → ●
  rig    → ⚙"
  :type 'boolean
  :group 'beads-faces)

(defun beads--format-status (issue)
  "Format status for ISSUE with face."
  (let ((status (alist-get 'status issue)))
    (propertize status
                'face (pcase status
                        ("closed" 'beads-status-closed)
                        ("in_progress" 'beads-status-in-progress)
                        ("blocked" 'beads-status-blocked)
                        ("hooked" 'beads-status-hooked)
                        (_ 'beads-status-open)))))

(defun beads--format-priority (issue)
  "Format priority for ISSUE as P0-P4 with face."
  (let* ((priority (alist-get 'priority issue))
         (priority-str (format "P%d" priority)))
    (propertize priority-str
                'face (pcase priority
                        (0 'beads-priority-p0)
                        (1 'beads-priority-p1)
                        (_ 'default)))))

(defun beads--format-type (issue)
  "Format type for ISSUE based on `beads-type-style'.
Applies faces for special types (gate, convoy, agent, role, rig).
When `beads-type-glyph' is non-nil, prepends a unicode glyph."
  (let* ((type (alist-get 'issue_type issue))
         (display-type (if (eq beads-type-style 'short)
                           (pcase type
                             ("feature" "feat")
                             ("chore" "chor")
                             ("convoy" "conv")
                             ("agent" "agnt")
                             (_ (or type "")))
                         (or type "")))
         (face (pcase type
                 ("gate" 'beads-type-gate)
                 ("convoy" 'beads-type-convoy)
                 ("agent" 'beads-type-agent)
                 ("role" 'beads-type-role)
                 ("rig" 'beads-type-rig)
                 (_ nil)))
         (glyph (when beads-type-glyph
                  (pcase type
                    ("gate" "■ ")
                    ("convoy" "▶ ")
                    ("agent" "◉ ")
                    ("role" "● ")
                    ("rig" "⚙ ")
                    (_ nil))))
         (result (concat glyph display-type)))
    (if face
        (propertize result 'face face)
      result)))

(provide 'beads-faces)
;;; beads-faces.el ends here
