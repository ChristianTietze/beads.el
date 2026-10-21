;;; beads-backend-test.el --- Tests for beads-backend.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the backend abstraction layer.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beads-backend)
(require 'beads-backend-bd)
(require 'beads-backend-br)
(require 'beads-client)

;;; Registry tests

(ert-deftest beads-backend-test-bd-registered ()
  "Test that bd backend is registered."
  (should (beads-backend--lookup "bd")))

(ert-deftest beads-backend-test-br-registered ()
  "Test that br backend is registered."
  (should (beads-backend--lookup "br")))

(ert-deftest beads-backend-test-lookup-unknown ()
  "Test that looking up unknown backend returns nil."
  (should-not (beads-backend--lookup "nonexistent")))

(ert-deftest beads-backend-test-register-custom ()
  "Test registering a custom backend."
  (let ((beads-backend--registry beads-backend--registry))
    (beads-backend-register
     (make-beads-backend
      :name "test-custom"
      :cli-program "test-cmd"
      :supports-daemon nil
      :supported-ops '("list" "show")
      :op-to-cli-args (lambda (_op _args) '("list"))))
    (should (beads-backend--lookup "test-custom"))
    (should (equal (beads-backend-cli-program
                    (beads-backend--lookup "test-custom"))
                   "test-cmd"))))

;;; Backend property tests

(ert-deftest beads-backend-test-bd-supports-daemon ()
  "Test that bd backend supports daemon."
  (should (beads-backend-supports-daemon (beads-backend--lookup "bd"))))

(ert-deftest beads-backend-test-br-no-daemon ()
  "Test that br backend does not support daemon."
  (should-not (beads-backend-supports-daemon (beads-backend--lookup "br"))))

(ert-deftest beads-backend-test-bd-socket-name ()
  "Test that bd backend has socket name."
  (should (equal (beads-backend-socket-name (beads-backend--lookup "bd"))
                 "bd.sock")))

(ert-deftest beads-backend-test-br-no-socket ()
  "Test that br backend has no socket name."
  (should-not (beads-backend-socket-name (beads-backend--lookup "br"))))

(ert-deftest beads-backend-test-bd-daemon-start-args ()
  "Test bd backend daemon start args."
  (should (equal (beads-backend-daemon-start-args (beads-backend--lookup "bd"))
                 '("daemon" "start" "--foreground"))))

;;; Capability tests

(ert-deftest beads-backend-test-bd-supports-all-ops ()
  "Test that bd backend supports standard operations."
  (let ((bd (beads-backend--lookup "bd")))
    (dolist (op '("list" "show" "ready" "create" "update" "close"
                  "delete" "stats" "health" "types" "duplicates"
                  "resolve-conflicts" "comments-add"))
      (should (beads-backend-supports-p bd op)))))

(ert-deftest beads-backend-test-br-supports-core-ops ()
  "Test that br backend supports core operations."
  (let ((br (beads-backend--lookup "br")))
    (dolist (op '("list" "show" "ready" "create" "update" "close"
                  "delete" "stats"))
      (should (beads-backend-supports-p br op)))))

(ert-deftest beads-backend-test-br-missing-ops ()
  "Test that br backend does not support bd-specific operations."
  (let ((br (beads-backend--lookup "br")))
    (dolist (op '("health" "types" "duplicates" "resolve-conflicts"
                  "comments-add" "get_mutations" "config_get"))
      (should-not (beads-backend-supports-p br op)))))

(ert-deftest beads-backend-test-require-operation-signals ()
  "Test that require-operation signals for unsupported ops."
  (let ((br (beads-backend--lookup "br")))
    (should-error (beads-backend-require-operation br "health")
                  :type 'beads-backend-error)))

(ert-deftest beads-backend-test-require-operation-passes ()
  "Test that require-operation passes for supported ops."
  (let ((bd (beads-backend--lookup "bd")))
    (should-not (beads-backend-require-operation bd "list"))))

;;; Auto-detection tests

(ert-deftest beads-backend-test-detect-bd ()
  "Test auto-detection prefers bd when available."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "bd") "/usr/bin/bd")))
            ((symbol-function 'beads-client--project-root)
             (lambda () nil)))
    (let ((beads-cli-program nil)
          (beads-backend--project-cache (make-hash-table :test 'equal)))
      (should (equal (beads-backend-name (beads-backend-for-project)) "bd")))))

(ert-deftest beads-backend-test-detect-br-fallback ()
  "Test auto-detection falls back to br when bd not available."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "br") "/usr/bin/br")))
            ((symbol-function 'beads-client--project-root)
             (lambda () nil)))
    (let ((beads-cli-program nil)
          (beads-backend--project-cache (make-hash-table :test 'equal)))
      (should (equal (beads-backend-name (beads-backend-for-project)) "br")))))

(ert-deftest beads-backend-test-detect-none-signals ()
  "Test auto-detection signals when no CLI found."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil))
            ((symbol-function 'beads-client--project-root)
             (lambda () nil)))
    (let ((beads-cli-program nil)
          (beads-backend--project-cache (make-hash-table :test 'equal)))
      (should-error (beads-backend-for-project)
                    :type 'beads-backend-error))))

(ert-deftest beads-backend-test-override-via-defcustom ()
  "Test that beads-cli-program overrides auto-detection."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (when (equal cmd "bd") "/usr/bin/bd")))
            ((symbol-function 'beads-client--project-root)
             (lambda () nil)))
    (let ((beads-cli-program "br")
          (beads-backend--project-cache (make-hash-table :test 'equal)))
      (should (equal (beads-backend-name (beads-backend-for-project)) "br")))))

(ert-deftest beads-backend-test-project-cache ()
  "Test that backends are cached per project root."
  (let ((detect-count 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd)
                 (when (equal cmd "bd")
                   (setq detect-count (1+ detect-count))
                   "/usr/bin/bd")))
              ((symbol-function 'beads-client--project-root)
               (lambda () "/fake/project/")))
      (let ((beads-cli-program nil)
            (beads-backend--project-cache (make-hash-table :test 'equal)))
        (beads-backend-for-project)
        (beads-backend-for-project)
        (should (= detect-count 1))))))

;;; CLI arg translation parity tests

(ert-deftest beads-backend-test-bd-list-args ()
  "Test bd backend list operation CLI args."
  (let ((args (beads-backend-bd--operation-to-cli-args
               "list" '((status . "open") (priority . 1)))))
    (should (equal (car args) "list"))
    (should (member "--status" args))
    (should (member "open" args))))

(ert-deftest beads-backend-test-bd-show-args ()
  "Test bd backend show operation CLI args."
  (should (equal (beads-backend-bd--operation-to-cli-args
                  "show" '((id . "bd-001")))
                 '("show" "bd-001"))))

(ert-deftest beads-backend-test-bd-create-args ()
  "Test bd backend create operation CLI args."
  (let ((args (beads-backend-bd--operation-to-cli-args
               "create" '((title . "Test") (priority . 1)))))
    (should (equal (car args) "create"))
    (should (equal (cadr args) "Test"))
    (should (member "--priority" args))))

(ert-deftest beads-backend-test-bd-close-with-reason ()
  "Test bd backend close with reason."
  (should (equal (beads-backend-bd--operation-to-cli-args
                  "close" '((id . "bd-001") (reason . "done")))
                 '("close" "bd-001" "--reason" "done"))))

(ert-deftest beads-backend-test-bd-close-without-reason ()
  "Test bd backend close without reason."
  (should (equal (beads-backend-bd--operation-to-cli-args
                  "close" '((id . "bd-001")))
                 '("close" "bd-001"))))

(ert-deftest beads-backend-test-bd-health-args ()
  "Test bd backend health operation CLI args."
  (cl-letf (((symbol-function 'beads-backend-bd--supports-daemon-p) (lambda () t)))
    (should (equal (beads-backend-bd--operation-to-cli-args "health" nil)
                   '("daemon" "status")))))

(ert-deftest beads-backend-test-bd-duplicates-extra-flags ()
  "Test bd backend adds --no-daemon for duplicates."
  (cl-letf (((symbol-function 'beads-backend-bd--supports-daemon-p) (lambda () t)))
    (should (equal (beads-backend-bd--cli-extra-flags "duplicates")
                   '("--no-daemon")))))

(ert-deftest beads-backend-test-bd-no-extra-flags-for-list ()
  "Test bd backend has no extra flags for list."
  (should-not (beads-backend-bd--cli-extra-flags "list")))

(ert-deftest beads-backend-test-bd-unknown-op-signals ()
  "Test bd backend signals on unknown operation."
  (should-error (beads-backend-bd--operation-to-cli-args "bogus" nil)
                :type 'beads-backend-error))

(ert-deftest beads-backend-test-br-unknown-op-signals ()
  "Test br backend signals on unknown operation."
  (should-error (beads-backend-br--operation-to-cli-args "health" nil)
                :type 'beads-backend-error))

;;; Shared utility tests

(ert-deftest beads-backend-test-alist-to-cli-flags ()
  "Test alist-to-cli-flags conversion."
  (should (equal (beads-backend--alist-to-cli-flags
                  '((status . "open") (priority . 1)))
                 '("--status" "open" "--priority" "1"))))

(ert-deftest beads-backend-test-alist-to-cli-flags-boolean ()
  "Test alist-to-cli-flags with boolean value."
  (should (equal (beads-backend--alist-to-cli-flags '((force . t)))
                 '("--force"))))

(ert-deftest beads-backend-test-alist-to-cli-flags-nil ()
  "Test alist-to-cli-flags skips nil values."
  (should (equal (beads-backend--alist-to-cli-flags '((force . nil)))
                 '())))

(ert-deftest beads-backend-test-build-cli-args ()
  "Test build-cli-args filters by allowed keys."
  (should (equal (beads-backend--build-cli-args
                  "list" '((status . "open") (bogus . "x")) '(status))
                 '("list" "--status" "open"))))

;;; CLI contract regressions

(ert-deftest beads-backend-test-client-cli-contract ()
  "Client keyword arguments reach the CLI with its actual flag names."
  (require 'beads-client)
  (let ((beads-client-connection-strategy 'cli)
        (beads-cli-program "bd")
        commands)
    (cl-letf (((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest args)
                 (push args commands)
                 (insert (if (equal (car args) "create")
                             "{\"id\":\"bd-1\"}"
                           "[{\"id\":\"bd-1\"}]"))
                 0)))
      (beads-client-list '(:status "closed" :issue-type "bug" :labels ("one" "two")))
      (should (equal (pop commands)
                     '("list" "--all" "--limit" "0" "--status" "closed"
                       "--type" "bug" "--label" "one" "--label" "two" "--json")))
      (should (equal (alist-get 'id (beads-client-create
                                    "Title" :issue-type "task"
                                    :acceptance-criteria "Done")) "bd-1"))
      (should (equal (pop commands)
                     '("create" "Title" "--type" "task" "--acceptance" "Done" "--json")))
      (dolist (op '("show" "update" "close"))
        (should (equal (alist-get 'id (beads-client-request op '((id . "bd-1"))))
                       "bd-1")))
      (beads-client-update "bd-1" :add-labels '("one") :remove-labels '("two"))
      (should (equal (pop commands)
                     '("update" "bd-1" "--add-label" "one" "--remove-label" "two" "--json")))
      (beads-client-ready '(:sort-policy "oldest" :limit 3))
      (should (equal (pop commands)
                     '("ready" "--sort" "oldest" "--limit" "3" "--json"))))))

(ert-deftest beads-backend-test-stderr-is-not-json ()
  "A successful CLI warning must not corrupt its JSON response."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               (should (equal (car destination) t))
               (write-region "warning: configuration missing" nil (cadr destination) nil 'silent)
               (insert "[{\"id\":\"bd-1\"}]")
               0)))
    (should (equal (beads-backend--call-json "bd" '("list" "--json"))
                   [((id . "bd-1"))]))))

(ert-deftest beads-backend-test-cli-failure-includes-stderr ()
  "Keep CLI diagnostics when a command fails."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               (write-region "unknown flag" nil (cadr destination) nil 'silent)
               1)))
    (let ((err (should-error (beads-backend--call-json "bd" '("list"))
                             :type 'beads-backend-error)))
      (should (string-match-p "unknown flag" (error-message-string err))))))

(ert-deftest beads-backend-test-cli-does-not-change-workspace ()
  "A stale SQLite cache must not redirect CLI requests to another project."
  (require 'beads-client)
  (let ((beads-client-connection-strategy 'cli)
        (default-directory temporary-file-directory)
        (beads-cli-program "bd"))
    (cl-letf (((symbol-function 'beads-client--find-database)
               (lambda () "/another/project/.beads/beads.db"))
              ((symbol-function 'call-process)
               (lambda (&rest _args)
                 (should (equal default-directory temporary-file-directory))
                 (insert "[]")
                 0)))
      (should-not (beads-client-list)))))

(ert-deftest beads-backend-test-cli-integration ()
  "Exercise real CLI workflows in an isolated, disposable workspace."
  :tags '(:integration :cli)
  (require 'beads-client)
  (skip-unless (executable-find "bd"))
  (let* ((root (make-temp-file "beads-cli-test-" t))
         (default-directory (file-name-as-directory root))
         (process-environment (copy-sequence process-environment))
         (beads-client-connection-strategy 'cli)
         (beads-cli-program "bd"))
    (setenv "BEADS_DIR" nil)
    (setenv "BEADS_DB" nil)
    (unwind-protect
        (progn
          (should (eq 0 (call-process "git" nil nil nil "init" "--quiet")))
          (should (eq 0 (call-process "bd" nil nil nil "init"
                                     "--prefix" "test" "--non-interactive")))
          (let* ((issue (beads-client-create "CLI contract test"
                                            :description "Isolated ERT issue"
                                            :issue-type "bug"
                                            :acceptance-criteria "Works"
                                            :labels '("regression")))
                 (id (alist-get 'id issue)))
            (should (stringp id))
            (should (equal (alist-get 'id (beads-client-show id)) id))
            (should (equal (alist-get 'acceptance_criteria (beads-client-show id)) "Works"))
            (should (= 1 (length (beads-client-list '(:issue-type "bug" :labels ("regression"))))))
            (should (= 1 (length (beads-client-ready '(:sort-policy "oldest")))))
            (should (equal (alist-get 'title (beads-client-update id :title "Edited" :priority 1))
                           "Edited"))
            (beads-client-update id :add-labels '("added") :remove-labels '("regression"))
            (should (= 1 (length (beads-client-list '(:labels ("added"))))))
            (should-not (beads-client-list '(:labels ("regression"))))
            (should (equal (alist-get 'status (beads-client-close id "Verified")) "closed"))
            ;; Closed issues remain available for the Emacs client-side filters.
            (should (= 1 (length (beads-client-list))))
            (should (= 1 (length (beads-client-list '(:status "closed")))))))
      (delete-directory root t))))

(provide 'beads-backend-test)
;;; beads-backend-test.el ends here
