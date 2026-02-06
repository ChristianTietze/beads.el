;;; init.el --- Development init file for beads.el -*- lexical-binding: t; no-byte-compile: t -*-

(load-theme 'modus-vivendi-tinted)  ;; Dark theme to match dark slides
(menu-bar-mode -1)                  ;; Disable topmost menu bar

(setq inhibit-startup-screen t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'markdown-mode))

(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" project-root))
  (add-to-list 'load-path (expand-file-name "vendor/vui.el" project-root)))

(require 'beads)
(require 'markdown-mode)

;; Enable vui.el rendering for testing
(setq beads-detail-use-vui t)
(setq beads-detail-vui-editable t)
(setq beads-form-use-vui t)

(defvar beads-reload--features
  '(beads beads-transient beads-autoupdate beads-project
    beads-list beads-preview beads-detail beads-hierarchy
    beads-form beads-edit beads-filter beads-faces beads-client
    beads-state beads-orphans beads-stale beads-activity
    beads-duplicates beads-lint beads-vui)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
