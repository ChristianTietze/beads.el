;;; init.el --- Development init file for beads.el -*- lexical-binding: t -*-

(let ((project-root (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))
  (add-to-list 'load-path project-root))

(require 'beads-rpc)
(require 'beads-list)

(message "beads.el loaded from %s" (locate-library "beads-rpc"))
(message "Run M-x beads-list to open the issue list")

(provide 'init)
;;; init.el ends here
