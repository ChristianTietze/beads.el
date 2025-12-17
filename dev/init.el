;;; init.el --- Development init file for beads.el -*- lexical-binding: t -*-

(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root))

(require 'beads)

(message "beads.el loaded from %s" (locate-library "beads"))
(message "Run M-x beads to open the issue tracker")

(provide 'init)
;;; init.el ends here
