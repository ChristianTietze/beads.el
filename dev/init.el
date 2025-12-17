;;; init.el --- Development init file for beads.el -*- lexical-binding: t -*-

(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root))

(require 'beads)

(add-hook 'emacs-startup-hook #'beads)

(provide 'init)
;;; init.el ends here
