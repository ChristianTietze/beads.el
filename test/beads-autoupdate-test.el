;;; beads-autoupdate-test.el --- Tests for beads-autoupdate.el -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'beads)
(require 'beads-autoupdate)
(require 'beads-list)

(ert-deftest beads-autoupdate-test-mode-defined ()
  "Test that beads-autoupdate-mode is defined."
  (should (fboundp 'beads-autoupdate-mode)))

(ert-deftest beads-autoupdate-test-start-defined ()
  "Test that beads-autoupdate-start is defined as a command."
  (should (fboundp 'beads-autoupdate-start))
  (should (commandp 'beads-autoupdate-start)))

(ert-deftest beads-autoupdate-test-stop-defined ()
  "Test that beads-autoupdate-stop is defined as a command."
  (should (fboundp 'beads-autoupdate-stop))
  (should (commandp 'beads-autoupdate-stop)))

(ert-deftest beads-autoupdate-test-interval-default ()
  "Test that beads-autoupdate-interval has sensible default."
  (should (integerp beads-autoupdate-interval))
  (should (> beads-autoupdate-interval 0)))

(ert-deftest beads-autoupdate-test-message-var-safe ()
  "Test that beads-autoupdate-message is safe for dir-locals."
  (should (get 'beads-autoupdate-message 'safe-local-variable)))

(ert-deftest beads-autoupdate-test-message-var-default ()
  "Test that beads-autoupdate-message defaults to t."
  (with-temp-buffer
    (should (eq beads-autoupdate-message t))))

(ert-deftest beads-autoupdate-test-timer-starts-with-mode ()
  "Test that timer starts when mode is enabled."
  (with-temp-buffer
    (let ((beads-autoupdate-interval 60))
      (beads-autoupdate-mode 1)
      (should beads-autoupdate--timer)
      (beads-autoupdate-mode -1)
      (should-not beads-autoupdate--timer))))

(ert-deftest beads-autoupdate-test-timer-stops-with-mode ()
  "Test that timer stops when mode is disabled."
  (with-temp-buffer
    (let ((beads-autoupdate-interval 60))
      (beads-autoupdate-mode 1)
      (should beads-autoupdate--timer)
      (beads-autoupdate-mode -1)
      (should-not beads-autoupdate--timer))))

(ert-deftest beads-autoupdate-test-no-timer-when-disabled ()
  "Test that no timer is created when interval is nil."
  (with-temp-buffer
    (let ((beads-autoupdate-interval nil))
      (beads-autoupdate-mode 1)
      (should-not beads-autoupdate--timer)
      (beads-autoupdate-mode -1))))

(ert-deftest beads-list-test-goto-id-defined ()
  "Test that beads-list-goto-id is defined."
  (should (fboundp 'beads-list-goto-id)))

(ert-deftest beads-list-test-refresh-has-silent-arg ()
  "Test that beads-list-refresh accepts silent argument."
  (should (equal (func-arity #'beads-list-refresh) '(0 . 1))))

(ert-deftest beads-autoupdate-test-fetch-failure-stops-local-timer ()
  "A failed fetch reports the error once and stops only its buffer's timer."
  (let ((other (generate-new-buffer " *beads-other*"))
        (beads-autoupdate-enable nil)
        (beads-verbose nil)
        messages)
    (unwind-protect
        (progn
          (with-current-buffer other
            (beads-list-mode)
            (beads-autoupdate-mode 1))
          (with-temp-buffer
            (beads-list-mode)
            (beads-autoupdate-mode 1)
            (cl-letf (((symbol-function 'beads-client-list)
                       (lambda (&rest _args)
                         (signal 'beads-client-error '("no beads database found"))))
                      ((symbol-function 'message)
                       (lambda (format-string &rest args)
                         (push (apply #'format format-string args) messages))))
              (beads-autoupdate--refresh)
              (should-not beads-autoupdate-mode)
              (should-not beads-autoupdate--timer)
              (should (= 1 (length messages)))
              (should (string-match-p "no beads database found" (car messages)))
              ;; Manual refresh still reports failures after the timer stops.
              (beads-list-refresh)
              (should (= 2 (length messages)))))
          (with-current-buffer other
            (should beads-autoupdate-mode)
            (should beads-autoupdate--timer)))
      (kill-buffer other))))

(ert-deftest beads-autoupdate-test-success-keeps-timer ()
  "A successful automatic refresh keeps running, including an empty list."
  (let ((beads-autoupdate-enable nil)
        (beads-verbose nil))
    (with-temp-buffer
      (beads-list-mode)
      (beads-autoupdate-mode 1)
      (cl-letf (((symbol-function 'beads-client-list) (lambda (&rest _args) nil)))
        (beads-autoupdate--refresh)
        (should beads-autoupdate-mode)
        (should beads-autoupdate--timer)))))

(provide 'beads-autoupdate-test)
;;; beads-autoupdate-test.el ends here
