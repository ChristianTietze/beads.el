;;; init.el --- Development init file for beads.el -*- lexical-binding: t; no-byte-compile: t -*-

(load-theme 'modus-vivendi-tinted)  ;; Dark theme to match dark slides
(menu-bar-mode -1)                  ;; Disable topmost menu bar

(setq inhibit-startup-screen t)

(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" project-root)))

(require 'beads)

(defvar beads-reload--features
  '(beads beads-transient beads-autoupdate beads-project
    beads-list beads-preview beads-detail beads-hierarchy
    beads-form beads-edit beads-filter beads-faces beads-rpc
    beads-state beads-orphans beads-stale beads-activity
    beads-duplicates beads-lint)
  "Beads features in reverse dependency order for unloading.")

(defun beads-reload ()
  "Reload all beads.el modules from source.
Useful during development to pick up changes without restarting Emacs."
  (interactive)
  (let ((load-prefer-newer t))
    (dolist (feature beads-reload--features)
      (when (featurep feature)
        (unload-feature feature t)))
    (require 'beads)
    (message "Reloaded beads.el")))

(add-hook 'emacs-startup-hook #'beads)
