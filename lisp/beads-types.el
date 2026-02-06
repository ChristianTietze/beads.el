;;; beads-types.el --- Issue type configuration for Beads -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools, ui

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

;; VUI-based editor for configuring custom issue types.
;; Core types (task, bug, feature, chore, epic) are read-only.
;; Custom types can be added, removed, and reordered.

;;; Code:

(require 'vui)
(require 'beads-client)

(defvar beads--types-cache)
(defvar beads--types-cache-time)

(defgroup beads-types nil
  "Issue type configuration for Beads."
  :group 'beads)

(defvar beads-types-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'beads-types-save)
    (define-key map (kbd "C-c C-k") #'beads-types-cancel)
    (define-key map (kbd "q") #'beads-types-cancel)
    map)
  "Keymap for beads-types-mode.")

(define-derived-mode beads-types-mode special-mode "Beads-Types"
  "Major mode for editing Beads issue types.

\\{beads-types-mode-map}")

(vui-defcomponent beads-types-core-item (type-info)
  "Display a single core type TYPE-INFO as read-only."
  :render
  (let ((name (alist-get 'name type-info))
        (desc (alist-get 'description type-info)))
    (vui-hstack
     (vui-text "  ")
     (vui-text name :face 'font-lock-constant-face)
     (vui-text " - " :face 'shadow)
     (vui-text desc :face 'shadow))))

(vui-defcomponent beads-types-custom-item (type-name idx &key on-remove on-move-up on-move-down can-move-up can-move-down)
  "Display an editable custom type TYPE-NAME at index IDX."
  :render
  (vui-hstack
   (vui-text "  ")
   (vui-text type-name :face 'font-lock-type-face)
   (vui-text "  ")
   (when can-move-up
     (vui-button "↑" :on-click (lambda () (funcall on-move-up idx)) :face 'link))
   (when can-move-down
     (vui-button "↓" :on-click (lambda () (funcall on-move-down idx)) :face 'link))
   (vui-text " ")
   (vui-button "×" :on-click (lambda () (funcall on-remove idx)) :face 'error)))

(vui-defcomponent beads-types-editor (initial-core-types initial-custom-types &key on-save on-cancel)
  "Editor for issue types with ON-SAVE and ON-CANCEL callbacks.
INITIAL-CORE-TYPES and INITIAL-CUSTOM-TYPES are loaded before mounting."
  :state ((custom-types initial-custom-types)
          (new-type-name ""))
  :render
  (let ((core-types initial-core-types))
    (vui-vstack
     (vui-text "Issue Types Configuration" :face '(:weight bold :height 1.2))
     (vui-text "C-c C-c to save, C-c C-k or q to cancel" :face 'shadow)
     (vui-newline)
     (vui-text (make-string 50 ?-))
     (vui-newline)
     (vui-text "Core Types" :face 'bold)
     (vui-text " (read-only)" :face 'shadow)
     (vui-newline)
     (if core-types
         (vui-fragment
          (vui-list core-types
                    (lambda (type-item)
                      (vui-component 'beads-types-core-item :type-info type-item))
                    (lambda (type-item)
                      (alist-get 'name type-item))))
       (vui-text "  (none)" :face 'shadow))
     (vui-newline)
     (vui-newline)
     (vui-text (make-string 50 ?-))
     (vui-newline)
     (vui-text "Custom Types" :face 'bold)
     (vui-newline)
     (if custom-types
         (let ((len (length custom-types)))
           (vui-fragment
            (vui-list custom-types
                      (lambda (type-name)
                        (let ((idx (seq-position custom-types type-name)))
                          (vui-component 'beads-types-custom-item
                                         :type-name type-name
                                         :idx idx
                                         :can-move-up (> idx 0)
                                         :can-move-down (< idx (1- len))
                                         :on-remove
                                         (lambda (i)
                                           (vui-set-state :custom-types
                                                          (beads-types--remove-at custom-types i)))
                                         :on-move-up
                                         (lambda (i)
                                           (vui-set-state :custom-types
                                                          (beads-types--swap custom-types i (1- i))))
                                         :on-move-down
                                         (lambda (i)
                                           (vui-set-state :custom-types
                                                          (beads-types--swap custom-types i (1+ i)))))))
                      #'identity)))
       (vui-text "  (none)" :face 'shadow))
     (vui-newline)
     (vui-newline)
     (vui-hstack
      (vui-text "Add type: ")
      (vui-field :value new-type-name
                 :size 20
                 :on-change (lambda (v) (vui-set-state :new-type-name v)))
      (vui-text " ")
      (vui-button "Add"
                  :on-click
                  (lambda ()
                    (when (and new-type-name
                               (not (string-empty-p new-type-name))
                               (not (member new-type-name custom-types))
                               (not (seq-find
                                     (lambda (ct)
                                       (equal new-type-name (alist-get 'name ct)))
                                     core-types)))
                      (vui-set-state :custom-types
                                     (append custom-types (list new-type-name)))
                      (vui-set-state :new-type-name "")))))
     (vui-newline)
     (vui-newline)
     (vui-text (make-string 50 ?-))
     (vui-newline)
     (vui-hstack
      :spacing 2
      (vui-button "Save"
                  :on-click (lambda () (funcall on-save custom-types)))
      (vui-button "Cancel" :on-click on-cancel)))))

(defun beads-types--remove-at (list idx)
  "Remove element at IDX from LIST."
  (append (seq-take list idx) (seq-drop list (1+ idx))))

(defun beads-types--swap (list i j)
  "Swap elements at positions I and J in LIST."
  (let ((vec (vconcat list)))
    (let ((tmp (aref vec i)))
      (aset vec i (aref vec j))
      (aset vec j tmp))
    (append vec nil)))

(defun beads-types--save (custom-types)
  "Save CUSTOM-TYPES to beads config."
  (condition-case err
      (let ((value (if custom-types
                       (mapconcat #'identity custom-types ",")
                     "")))
        (if (string-empty-p value)
            (beads-client-request "config_unset" '((key . "types.custom")))
          (beads-client-config-set "types.custom" value))
        (setq beads--types-cache nil
              beads--types-cache-time 0)
        (message "Custom types saved")
        (beads-types--close))
    (beads-client-error
     (message "Failed to save: %s" (error-message-string err)))))

(defun beads-types--close ()
  "Close the types editor buffer."
  (quit-window t))

(defun beads-types-save ()
  "Save changes from the types editor."
  (interactive)
  (message "Use the [Save] button in the editor to save changes"))

(defun beads-types-cancel ()
  "Cancel and close the types editor."
  (interactive)
  (beads-types--close)
  (message "Cancelled"))

;;;###autoload
(defun beads-types-edit ()
  "Open the issue types editor."
  (interactive)
  (condition-case err
      (let* ((response (beads-client-types-full))
             (core-types (append (alist-get 'core_types response) nil))
             (custom-types (append (alist-get 'custom_types response) nil))
             (buffer (get-buffer-create "*Beads Types*")))
        (with-current-buffer buffer
          (beads-types-mode)
          (vui-mount (vui-component 'beads-types-editor
                                    :initial-core-types core-types
                                    :initial-custom-types custom-types
                                    :on-save #'beads-types--save
                                    :on-cancel #'beads-types-cancel)
                     (buffer-name buffer)))
        (pop-to-buffer buffer))
    (beads-client-error
     (message "Failed to load types: %s" (error-message-string err)))))

(provide 'beads-types)
;;; beads-types.el ends here
