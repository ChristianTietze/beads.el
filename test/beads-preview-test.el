;;; beads-preview-test.el --- Tests for beads-preview.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads issue preview mode.
;;
;; Test categories:
;; 1. Mode tests - test minor mode lifecycle (no daemon)
;; 2. Timer tests - test idle timer management (mocked)
;; 3. Keybinding tests - test mode activation keybindings (no daemon)
;; 4. Customization tests - test defcustom defaults

;;; Code:

(require 'ert)
(require 'beads-preview)
(require 'beads-list)

;;; Mode tests (no daemon needed)

(ert-deftest beads-preview-test-mode-defined ()
  "Test that beads-preview-mode is defined as a minor mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (fboundp 'beads-preview-mode))
    (should (commandp 'beads-preview-mode))))

(ert-deftest beads-preview-test-mode-enable-adds-hook ()
  "Test that enabling beads-preview-mode adds post-command-hook."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (should beads-preview-mode)
    (should (member 'beads-preview-trigger
                    (if (local-variable-p 'post-command-hook)
                        post-command-hook
                      (default-value 'post-command-hook))))))

(ert-deftest beads-preview-test-mode-disable-removes-hook ()
  "Test that disabling beads-preview-mode removes post-command-hook."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (beads-preview-mode -1)
    (should-not beads-preview-mode)
    (should-not (member 'beads-preview-trigger
                        (if (local-variable-p 'post-command-hook)
                            post-command-hook
                          (default-value 'post-command-hook))))))

(ert-deftest beads-preview-test-mode-toggle ()
  "Test that beads-preview-mode can be toggled on and off."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (should beads-preview-mode)
    (beads-preview-mode -1)
    (should-not beads-preview-mode)
    (beads-preview-mode 1)
    (should beads-preview-mode)))

(ert-deftest beads-preview-test-mode-lighter ()
  "Test that beads-preview-mode has ' Preview' lighter."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (let ((mode-line (format-mode-line minor-mode-alist)))
      (should (string-match-p "Preview" mode-line)))))

(ert-deftest beads-preview-test-mode-disable-cancels-timer ()
  "Test that disabling mode cancels any pending timer."
  (with-temp-buffer
    (beads-list-mode)
    (let ((timer-created nil))
      (cl-letf (((symbol-function 'beads-preview--start-timer)
                 (lambda (_issue)
                   (setq timer-created t)
                   (run-with-idle-timer 0.1 nil #'ignore)))
                ((symbol-function 'beads-preview--cancel-timer)
                 (lambda ()
                   (setq timer-created nil))))
        (beads-preview-mode 1)
        (beads-preview-trigger)
        (beads-preview-mode -1)
        (should-not timer-created)))))

;;; Timer tests (mocked)

(ert-deftest beads-preview-test-start-timer-creates-timer ()
  "Test that beads-preview--start-timer creates an idle timer."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (let ((timer-created nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (secs _repeat _function &rest _args)
                   (setq timer-created secs)
                   (timer-create))))
        (beads-preview--start-timer '((id . "test-123")))
        (should (numberp timer-created))
        (should (>= timer-created 0))))))

(ert-deftest beads-preview-test-cancel-timer-cancels-existing ()
  "Test that beads-preview--cancel-timer cancels existing timer."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (let ((timer (timer-create)))
      (setq beads-preview--timer timer)
      (should beads-preview--timer)
      (beads-preview--cancel-timer)
      (should-not beads-preview--timer))))

(ert-deftest beads-preview-test-cancel-timer-no-timer ()
  "Test that beads-preview--cancel-timer handles nil timer gracefully."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (setq beads-preview--timer nil)
    (beads-preview--cancel-timer)
    (should-not beads-preview--timer)))

(ert-deftest beads-preview-test-timer-debouncing ()
  "Test that starting a new timer cancels the previous one."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (let ((cancel-count 0)
          (timer-count 0))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (_timer)
                   (setq cancel-count (1+ cancel-count))))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat _function &rest _args)
                   (setq timer-count (1+ timer-count))
                   (timer-create))))
        (setq beads-preview--timer (timer-create))
        (beads-preview--start-timer '((id . "test-1")))
        (should (= cancel-count 1))
        (should (= timer-count 1))
        (beads-preview--start-timer '((id . "test-2")))
        (should (= cancel-count 2))
        (should (= timer-count 2))))))

;;; Keybinding tests (no daemon)

(ert-deftest beads-preview-test-keybinding-in-list-mode ()
  "Test that P is bound to beads-preview-mode in beads-list-mode."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "P"))
                #'beads-preview-mode))))

(ert-deftest beads-preview-test-keybinding-uppercase-p ()
  "Test that uppercase P specifically is used (not lowercase p)."
  (with-temp-buffer
    (beads-list-mode)
    (should (eq (lookup-key beads-list-mode-map (kbd "P"))
                #'beads-preview-mode))
    (should-not (eq (lookup-key beads-list-mode-map (kbd "p"))
                    #'beads-preview-mode))))

(ert-deftest beads-preview-test-keybinding-callable ()
  "Test that keybinding P can actually activate the mode."
  (with-temp-buffer
    (beads-list-mode)
    (let ((command (lookup-key beads-list-mode-map (kbd "P"))))
      (should (commandp command))
      (call-interactively command)
      (should beads-preview-mode))))

;;; Customization tests

(ert-deftest beads-preview-test-delay-default ()
  "Test that beads-preview-delay defaults to 0.1."
  (should (= beads-preview-delay 0.1)))

(ert-deftest beads-preview-test-delay-customizable ()
  "Test that beads-preview-delay is a customizable variable."
  (should (custom-variable-p 'beads-preview-delay)))

(ert-deftest beads-preview-test-delay-positive ()
  "Test that beads-preview-delay is positive."
  (should (> beads-preview-delay 0)))

;;; Display tests (mocked, no daemon)

(ert-deftest beads-preview-test-display-issue-mocked ()
  "Test that beads-preview--display-issue creates preview buffer."
  (let ((buffer-created nil))
    (cl-letf (((symbol-function 'beads-rpc-show)
               (lambda (_issue-id)
                 '((id . "bd-test")
                   (title . "Test Issue")
                   (status . "open")
                   (priority . 2)
                   (issue_type . "task")
                   (description . "Test description"))))
              ((symbol-function 'display-buffer)
               (lambda (buffer _action)
                 (setq buffer-created buffer)
                 buffer)))
      (beads-preview--display-issue '((id . "bd-test")))
      (should buffer-created)
      (should (buffer-live-p buffer-created))
      (kill-buffer buffer-created))))

(ert-deftest beads-preview-test-display-issue-error-handling ()
  "Test that display-issue handles RPC errors gracefully."
  (cl-letf (((symbol-function 'beads-rpc-show)
             (lambda (_issue-id)
               (signal 'beads-rpc-error '("Test error"))))
            ((symbol-function 'message)
             (lambda (_format &rest _args) nil)))
    (beads-preview--display-issue '((id . "bd-test")))))

;;; Trigger tests (mocked)

(ert-deftest beads-preview-test-trigger-only-in-list-mode ()
  "Test that trigger only activates in beads-list-mode."
  (with-temp-buffer
    (let ((timer-started nil))
      (cl-letf (((symbol-function 'beads-preview--start-timer)
                 (lambda (_issue)
                   (setq timer-started t))))
        (beads-preview-trigger)
        (should-not timer-started)))))

(ert-deftest beads-preview-test-trigger-requires-mode-enabled ()
  "Test that trigger requires beads-preview-mode to be enabled."
  (with-temp-buffer
    (beads-list-mode)
    (let ((timer-started nil))
      (cl-letf (((symbol-function 'beads-preview--start-timer)
                 (lambda (_issue)
                   (setq timer-started t))))
        (beads-preview-trigger)
        (should-not timer-started)))))

(ert-deftest beads-preview-test-trigger-activates-when-enabled ()
  "Test that trigger activates when mode is enabled."
  (with-temp-buffer
    (beads-list-mode)
    (beads-preview-mode 1)
    (let ((timer-started nil))
      (cl-letf (((symbol-function 'beads-preview--cancel-timer)
                 (lambda () nil))
                ((symbol-function 'beads-preview--start-timer)
                 (lambda (_issue)
                   (setq timer-started t))))
        (beads-preview-trigger)
        (should timer-started)))))

;;; Cleanup tests

(ert-deftest beads-preview-test-cleanup-kills-preview-buffer ()
  "Test that cleanup kills the preview buffer."
  (let ((preview-buf (get-buffer-create "*beads-preview*")))
    (unwind-protect
        (progn
          (beads-preview--cleanup)
          (should-not (buffer-live-p preview-buf)))
      (when (buffer-live-p preview-buf)
        (kill-buffer preview-buf)))))

(provide 'beads-preview-test)
;;; beads-preview-test.el ends here
