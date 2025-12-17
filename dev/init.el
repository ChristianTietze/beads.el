;;; init.el --- Development init file for beads.el -*- lexical-binding: t; no-byte-compile: t -*-

(load-theme 'modus-vivendi-tinted)  ;; Dark theme to match dark slides
(menu-bar-mode -1)                  ;; Disable topmost menu bar

(setq inhibit-startup-screen t)

(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path (expand-file-name "lisp" project-root)))

(require 'beads)

(add-hook 'emacs-startup-hook #'beads)
