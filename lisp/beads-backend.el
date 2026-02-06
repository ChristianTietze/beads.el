;;; beads-backend.el --- Backend abstraction for Beads CLI -*- lexical-binding: t -*-

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

;; Backend abstraction layer that decouples beads.el from any specific
;; CLI binary.  Each backend (bd, br, etc.) registers itself via
;; `beads-backend-register' and provides operation translation and
;; capability metadata.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(define-error 'beads-backend-error "Beads backend error")

(cl-defstruct beads-backend
  (name nil :type string)
  (cli-program nil :type string)
  (supports-daemon nil :type boolean)
  (socket-name nil :type (or string null))
  (daemon-start-args nil :type list)
  (supported-ops nil :type list)
  (op-to-cli-args nil :type function)
  (cli-extra-flags nil :type function))

(defcustom beads-cli-program nil
  "Override the CLI program used for this project.
When nil, auto-detect from available backends.
Can be set per-project via .dir-locals.el."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Program name"))
  :group 'beads
  :safe #'stringp)

(defvar beads-backend--registry nil
  "Alist of (name . backend) pairs.")

(defvar beads-backend--project-cache (make-hash-table :test 'equal)
  "Cache of project-root to backend mappings.")

(defun beads-backend-register (backend)
  "Register BACKEND in the backend registry."
  (let ((name (beads-backend-name backend)))
    (setf (alist-get name beads-backend--registry nil nil #'equal) backend)))

(defun beads-backend--lookup (name)
  "Look up a backend by NAME in the registry."
  (alist-get name beads-backend--registry nil nil #'equal))

(declare-function beads-client--find-database "beads-client")
(declare-function beads-client--project-root "beads-client")

(defun beads-backend-for-project ()
  "Return the backend for the current project.
Uses `beads-cli-program' if set, otherwise auto-detects."
  (if beads-cli-program
      (or (beads-backend--lookup beads-cli-program)
          (let ((found (seq-find
                        (lambda (pair)
                          (equal (beads-backend-cli-program (cdr pair))
                                 beads-cli-program))
                        beads-backend--registry)))
            (if found
                (cdr found)
              (signal 'beads-backend-error
                      (list (format "No backend registered for program: %s"
                                    beads-cli-program))))))
    (let ((root (beads-client--project-root)))
      (or (when root (gethash root beads-backend--project-cache))
          (let ((backend (beads-backend--auto-detect)))
            (when root
              (puthash root backend beads-backend--project-cache))
            backend)))))

(defun beads-backend--auto-detect ()
  "Auto-detect the best available backend."
  (cond
   ((executable-find "bd")
    (or (beads-backend--lookup "bd")
        (signal 'beads-backend-error '("bd found but backend not registered"))))
   ((executable-find "br")
    (or (beads-backend--lookup "br")
        (signal 'beads-backend-error '("br found but backend not registered"))))
   (t
    (signal 'beads-backend-error '("No beads CLI found (tried bd, br)")))))

(defun beads-backend-supports-p (backend operation)
  "Return non-nil if BACKEND supports OPERATION."
  (member operation (beads-backend-supported-ops backend)))

(defun beads-backend-require-operation (backend operation)
  "Signal error if BACKEND does not support OPERATION."
  (unless (beads-backend-supports-p backend operation)
    (signal 'beads-backend-error
            (list (format "Backend %s does not support operation: %s"
                          (beads-backend-name backend) operation)))))

(defun beads-backend-supports-daemon-p (&optional backend)
  "Return non-nil if BACKEND (or current project backend) supports daemon."
  (beads-backend-supports-daemon (or backend (beads-backend-for-project))))

(defun beads-backend-socket-name-for-project ()
  "Return the socket filename for the current project's backend."
  (beads-backend-socket-name (beads-backend-for-project)))

(defun beads-backend-daemon-start-command ()
  "Return the full daemon start command for the current project's backend."
  (let ((backend (beads-backend-for-project)))
    (when (beads-backend-supports-daemon backend)
      (let ((program (beads-backend-cli-program-path backend)))
        (cons program (beads-backend-daemon-start-args backend))))))

(defun beads-backend-cli-program-path (&optional backend)
  "Return the absolute path to the CLI program for BACKEND.
BACKEND defaults to the current project's backend."
  (let* ((be (or backend (beads-backend-for-project)))
         (program (beads-backend-cli-program be)))
    (or (executable-find program)
        (signal 'beads-backend-error
                (list (format "%s executable not found" program))))))

(defun beads-backend--build-cli-args (command args allowed-keys)
  "Build CLI args for COMMAND from ARGS, filtering by ALLOWED-KEYS."
  (append (list command)
          (beads-backend--alist-to-cli-flags
           (seq-filter (lambda (pair)
                         (memq (car pair) allowed-keys))
                       args))))

(defun beads-backend--alist-to-cli-flags (alist)
  "Convert ALIST to list of CLI flags.
Keys are converted from snake_case to --kebab-case."
  (let ((flags '()))
    (dolist (pair alist)
      (let* ((key (car pair))
             (value (cdr pair))
             (flag-name (concat "--" (replace-regexp-in-string
                                      "_" "-" (symbol-name key)))))
        (cond
         ((eq value t)
          (push flag-name flags))
         ((eq value nil)
          nil)
         ((listp value)
          (dolist (v value)
            (push flag-name flags)
            (push (format "%s" v) flags)))
         (t
          (push flag-name flags)
          (push (format "%s" value) flags)))))
    (nreverse flags)))

(defun beads-backend-cli-execute (operation args &optional project-root)
  "Execute CLI for OPERATION with ARGS, returning parsed JSON.
PROJECT-ROOT overrides the working directory.
Signals `beads-backend-error' on failure."
  (let* ((backend (beads-backend-for-project))
         (program (beads-backend-cli-program-path backend))
         (op-args (funcall (beads-backend-op-to-cli-args backend)
                           operation args))
         (extra (when-let ((fn (beads-backend-cli-extra-flags backend)))
                  (funcall fn operation)))
         (cmd-args (append extra op-args '("--json"))))
    (with-temp-buffer
      (let* ((default-directory (or project-root default-directory))
             (exit-code (apply #'call-process program nil t nil cmd-args)))
        (unless (zerop exit-code)
          (signal 'beads-backend-error
                  (list (format "CLI failed with exit code %d: %s"
                                exit-code
                                (string-trim (buffer-string))))))
        (goto-char (point-min))
        (condition-case nil
            (let ((output (json-read)))
              (if (vectorp output) (append output nil) output))
          (json-error
           (signal 'beads-backend-error
                   (list (format "CLI returned invalid JSON: %s"
                                 (buffer-string))))))))))

(defun beads-backend-cli-call-raw (args &optional project-root)
  "Execute CLI with ARGS without --json, returning exit code.
PROJECT-ROOT overrides the working directory."
  (let* ((backend (beads-backend-for-project))
         (program (beads-backend-cli-program-path backend)))
    (let ((default-directory (or project-root default-directory)))
      (apply #'call-process program nil nil nil args))))

(defun beads-backend-clear-cache ()
  "Clear the per-project backend cache."
  (interactive)
  (clrhash beads-backend--project-cache))

(provide 'beads-backend)

(require 'beads-backend-bd nil t)
(require 'beads-backend-br nil t)

;;; beads-backend.el ends here
