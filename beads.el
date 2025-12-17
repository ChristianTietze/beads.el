;;; beads.el --- Emacs client for Beads issue tracker -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Beads Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, project
;; URL: https://github.com/steveyegge/beads

;; This file is NOT part of GNU Emacs.

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

(require 'beads-rpc)
(require 'beads-list)
(require 'beads-detail)
(require 'beads-transient)

;;;###autoload
(defun beads ()
  "Open the Beads issue tracker."
  (interactive)
  (beads-list))

(provide 'beads)
;;; beads.el ends here
