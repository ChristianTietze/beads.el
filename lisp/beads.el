;;; beads.el --- Emacs client for Beads issue tracker -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (hierarchy "0.7.0"))
;; Keywords: tools, project
;; URL: https://github.com/ChristianTietze/beads.el

;; This file is NOT part of GNU Emacs.

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

;; beads.el provides an Emacs interface to the Beads issue tracking system.
;; Beads is a Git-backed, AI-native issue tracker that stores data in `.beads/`
;; and communicates with a daemon via Unix socket RPC.
;;
;; Usage:
;;   M-x beads  - Open the Beads issue list
;;
;; The client automatically discovers the Beads database by walking up from
;; `default-directory` looking for `.beads/beads.db`, then connects to the
;; daemon socket at `.beads/bd.sock`.

;;; Code:

(defgroup beads nil
  "Beads issue tracker."
  :group 'tools
  :prefix "beads-")

(defcustom beads-verbose t
  "When non-nil, show helpful hints about keybindings in the minibuffer.
Hints are shown when entering beads modes to help with discoverability."
  :type 'boolean
  :group 'beads)

(defvar beads-hints-alist
  '((beads-list-mode
     . "? menu | RET open | e <key> edit | f <key> filter | E form | P preview | q quit")
    (beads-list-mode-preview
     . "↑↓ browse | RET open | e/E edit | P/q exit preview | ? menu")
    (beads-detail-mode
     . "? menu | e <key> edit | E form | g refresh | q quit")
    (beads-form-mode
     . "TAB next | C-c C-c save | C-c C-k cancel"))
  "Alist of mode symbols to hint strings.")

(defun beads-show-hint ()
  "Show hint for current major mode if `beads-verbose' is enabled."
  (when beads-verbose
    (let* ((mode-key (if (and (eq major-mode 'beads-list-mode)
                              (bound-and-true-p beads-preview-mode))
                         'beads-list-mode-preview
                       major-mode))
           (hint (alist-get mode-key beads-hints-alist)))
      (when hint
        (run-at-time 0.1 nil (lambda (h) (message h)) hint)))))

(require 'beads-rpc)
(require 'beads-list)
(require 'beads-detail)
(require 'beads-transient)
(require 'beads-autoupdate)
(require 'beads-project)

(autoload 'beads-hierarchy-show "beads-hierarchy" "Display dependency tree." t)

;;;###autoload
(defun beads ()
  "Open the Beads issue tracker."
  (interactive)
  (beads-list))

(provide 'beads)
;;; beads.el ends here
