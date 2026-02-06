;;; beads-client-test.el --- Tests for beads-client.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the Beads RPC client module.
;;
;; Test categories:
;; 1. Discovery tests - auto-discovery of .beads/beads.db (no daemon needed)
;; 2. Socket path tests - socket path construction
;; 3. Request serialization tests - JSON request structure (mocked)
;; 4. Integration tests - actual daemon communication (tagged :integration)
;;
;; Note on test isolation:
;; Integration tests connect to the actual beads daemon in this repo.
;; Tests that create issues are tagged :destructive and MUST delete
;; their test data via beads-client-delete to avoid polluting the
;; production database.  Read-only integration tests are safe to run.

;;; Code:

(require 'ert)
(require 'json)
(require 'beads-client)

;;; Discovery tests (no daemon needed)

(ert-deftest beads-client-test-find-database-current-dir ()
  "Test that beads-client--find-database finds .beads/beads.db in current directory."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let ((beads-dir (expand-file-name ".beads" temp-dir))
              (default-directory temp-dir))
          (make-directory beads-dir)
          (write-region "" nil (expand-file-name "beads.db" beads-dir))
          (should (equal (beads-client--find-database)
                        (expand-file-name ".beads/beads.db" temp-dir))))
      (delete-directory temp-dir t))))

(ert-deftest beads-client-test-find-database-parent-dir ()
  "Test that beads-client--find-database walks up parent directories."
  (let ((temp-root (make-temp-file "beads-test-root-" t)))
    (unwind-protect
        (let* ((beads-dir (expand-file-name ".beads" temp-root))
               (sub-dir (expand-file-name "sub/dir" temp-root))
               (default-directory sub-dir))
          (make-directory beads-dir)
          (make-directory sub-dir t)
          (write-region "" nil (expand-file-name "beads.db" beads-dir))
          (should (equal (beads-client--find-database)
                        (expand-file-name ".beads/beads.db" temp-root))))
      (delete-directory temp-root t))))

(ert-deftest beads-client-test-find-database-not-found ()
  "Test that beads-client--find-database returns nil when no database found."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (should (null (beads-client--find-database))))
      (delete-directory temp-dir t))))

(ert-deftest beads-client-test-find-database-beads-db-env ()
  "Test that BEADS_DB environment variable overrides auto-discovery."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((db-path (expand-file-name "custom/beads.db" temp-dir))
               (process-environment (cons (concat "BEADS_DB=" db-path)
                                         process-environment))
               (default-directory temp-dir))
          (make-directory (file-name-directory db-path) t)
          (write-region "" nil db-path)
          (should (equal (beads-client--find-database) db-path)))
      (delete-directory temp-dir t))))

(ert-deftest beads-client-test-find-database-beads-dir-env ()
  "Test that BEADS_DIR environment variable is used for discovery."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((beads-dir (expand-file-name "custom-beads" temp-dir))
               (db-path (expand-file-name "beads.db" beads-dir))
               (process-environment (cons (concat "BEADS_DIR=" beads-dir)
                                         process-environment))
               (default-directory temp-dir))
          (make-directory beads-dir t)
          (write-region "" nil db-path)
          (should (equal (beads-client--find-database) db-path)))
      (delete-directory temp-dir t))))

(ert-deftest beads-client-test-find-database-redirect ()
  "Test that .beads/redirect file is followed."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((real-beads-dir (expand-file-name "real-beads" temp-dir))
               (fake-beads-dir (expand-file-name ".beads" temp-dir))
               (redirect-file (expand-file-name "redirect" fake-beads-dir))
               (db-path (expand-file-name "beads.db" real-beads-dir))
               (default-directory temp-dir))
          (make-directory fake-beads-dir t)
          (make-directory real-beads-dir t)
          (write-region real-beads-dir nil redirect-file)
          (write-region "" nil db-path)
          (should (equal (beads-client--find-database) db-path)))
      (delete-directory temp-dir t))))

(ert-deftest beads-client-test-find-database-custom-db-name ()
  "Test that custom .db files are found (excluding vc.db and backups)."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((beads-dir (expand-file-name ".beads" temp-dir))
               (custom-db (expand-file-name "custom.db" beads-dir))
               (default-directory temp-dir))
          (make-directory beads-dir t)
          (write-region "" nil custom-db)
          (write-region "" nil (expand-file-name "vc.db" beads-dir))
          (write-region "" nil (expand-file-name "backup.db" beads-dir))
          (should (equal (beads-client--find-database) custom-db)))
      (delete-directory temp-dir t))))

;;; Socket path tests

(ert-deftest beads-client-test-socket-path ()
  "Test that beads-client--socket-path returns correct path."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let* ((beads-dir (expand-file-name ".beads" temp-dir))
               (db-path (expand-file-name "beads.db" beads-dir))
               (expected-socket (expand-file-name "bd.sock" beads-dir))
               (default-directory temp-dir))
          (make-directory beads-dir t)
          (write-region "" nil db-path)
          (should (equal (beads-client--socket-path) expected-socket)))
      (delete-directory temp-dir t))))

(ert-deftest beads-client-test-socket-path-no-database ()
  "Test that beads-client--socket-path returns nil when no database found."
  (let ((temp-dir (make-temp-file "beads-test-" t)))
    (unwind-protect
        (let ((default-directory temp-dir))
          (should (null (beads-client--socket-path))))
      (delete-directory temp-dir t))))

;;; Request serialization tests (mock the socket)

(ert-deftest beads-client-test-request-structure ()
  "Test that beads-client-request creates correct JSON structure."
  (let ((request-data nil))
    (cl-letf (((symbol-function 'beads-client--send-to-socket)
               (lambda (_socket-path request)
                 (setq request-data (json-read-from-string request))
                 '((success . t) (data . nil)))))
      (beads-client-request "test-operation" '((key . "value")))
      (should (equal (alist-get 'operation request-data) "test-operation"))
      (should (equal (alist-get 'key (alist-get 'args request-data)) "value"))
      (should (stringp (alist-get 'cwd request-data)))
      (should (stringp (alist-get 'client_version request-data)))
      (should (stringp (alist-get 'expected_db request-data))))))

(ert-deftest beads-client-test-request-null-args ()
  "Test that beads-client-request handles nil args correctly."
  (let ((request-data nil))
    (cl-letf (((symbol-function 'beads-client--send-to-socket)
               (lambda (_socket-path request)
                 (setq request-data (json-read-from-string request))
                 '((success . t) (data . nil)))))
      (beads-client-request "test-operation" nil)
      (should (eq (alist-get 'args request-data) :null)))))

(ert-deftest beads-client-test-request-filter-encoding ()
  "Test that list filters are properly encoded in requests."
  (let ((request-data nil))
    (cl-letf (((symbol-function 'beads-client--send-to-socket)
               (lambda (_socket-path request)
                 (setq request-data (json-read-from-string request))
                 '((success . t) (data . nil)))))
      (beads-client-request "list" '((status . "open")
                                  (priority . 1)
                                  (labels . ["backend" "urgent"])))
      (let ((args (alist-get 'args request-data)))
        (should (equal (alist-get 'status args) "open"))
        (should (equal (alist-get 'priority args) 1))
        (should (equal (alist-get 'labels args) ["backend" "urgent"]))))))

(ert-deftest beads-client-test-request-error-handling ()
  "Test that beads-client-request handles error responses."
  (cl-letf (((symbol-function 'beads-client--send-to-socket)
             (lambda (_socket-path _request)
               '((success . :json-false) (error . "Test error message")))))
    (should-error (beads-client-request "test-operation" nil)
                  :type 'beads-client-error)))

;;; Integration tests (require daemon)

(ert-deftest beads-client-test-health ()
  "Test that beads-client-health returns healthy status from daemon."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((result (beads-client-health)))
    (should result)))

(ert-deftest beads-client-test-list ()
  "Test that beads-client-list returns issues array."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((issues (beads-client-list)))
    (should (vectorp issues))))

(ert-deftest beads-client-test-list-with-filters ()
  "Test that beads-client-list accepts filter arguments."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((issues (beads-client-list '(:status "open" :priority 1))))
    (should (vectorp issues))))

(ert-deftest beads-client-test-ready ()
  "Test that beads-client-ready returns unblocked issues."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((issues (beads-client-ready)))
    (should (vectorp issues))))

(ert-deftest beads-client-test-stats ()
  "Test that beads-client-stats returns statistics."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((stats (beads-client-stats)))
    (should (numberp (alist-get 'total_issues stats)))
    (should (numberp (alist-get 'open_issues stats)))
    (should (numberp (alist-get 'closed_issues stats)))))

(ert-deftest beads-client-test-show ()
  "Test that beads-client-show returns issue details."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((issues (beads-client-list '(:limit 1))))
    (skip-unless (> (length issues) 0))
    (let* ((issue-id (alist-get 'id (aref issues 0)))
           (issue (beads-client-show issue-id)))
      (should (stringp (alist-get 'id issue)))
      (should (stringp (alist-get 'title issue)))
      (should (stringp (alist-get 'status issue))))))

(ert-deftest beads-client-test-create-and-close ()
  "Test creating and closing an issue via RPC."
  :tags '(:integration :destructive)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let* ((title "Test issue from ERT")
         (issue (beads-client-create
                 title
                 :description "Test description"
                 :priority 2
                 :issue-type "task"))
         (issue-id (alist-get 'id issue)))
    (should (stringp issue-id))
    (should (equal (alist-get 'title issue) title))
    (unwind-protect
        (progn
          (let ((show-issue (beads-client-show issue-id)))
            (should (stringp (alist-get 'id show-issue))))
          (let ((closed-issue (beads-client-close issue-id "Test cleanup")))
            (should (equal (alist-get 'status closed-issue) "closed"))))
      (beads-client-delete (list issue-id) :force t))))

(ert-deftest beads-client-test-update ()
  "Test updating an issue via RPC."
  :tags '(:integration :destructive)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let* ((issue (beads-client-create
                 "Test update issue"
                 :priority 2))
         (issue-id (alist-get 'id issue)))
    (unwind-protect
        (let* ((updated-issue (beads-client-update
                               issue-id
                               :status "in_progress"
                               :priority 1
                               :notes "Working on it"))
               (show-issue (beads-client-show issue-id)))
          (should (equal (alist-get 'status updated-issue) "in_progress"))
          (should (equal (alist-get 'priority show-issue) 1)))
      (beads-client-delete (list issue-id) :force t))))

(ert-deftest beads-client-test-count ()
  "Test that beads-client-count returns grouped counts."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let ((counts (beads-client-count '(:group-by "status"))))
    (should (listp counts))))

(ert-deftest beads-client-test-dep-add-remove ()
  "Test adding and removing dependencies via RPC."
  :tags '(:integration :destructive)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let* ((issue1 (beads-client-create "Dependency test 1"))
         (issue2 (beads-client-create "Dependency test 2"))
         (id1 (alist-get 'id issue1))
         (id2 (alist-get 'id issue2)))
    (unwind-protect
        (progn
          (let ((_add-result (beads-client-dep-add id1 id2 "blocks")))
            (should t))
          (let ((_show-issue (beads-client-show id1)))
            (should t))
          (let ((_remove-result (beads-client-dep-remove id1 id2)))
            (should t)))
      (beads-client-delete (list id1 id2) :force t))))

(ert-deftest beads-client-test-label-operations ()
  "Test adding and removing labels via RPC."
  :tags '(:integration :destructive)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (let* ((issue (beads-client-create "Label test"))
         (issue-id (alist-get 'id issue)))
    (unwind-protect
        (progn
          (let ((_add-result (beads-client-label-add issue-id "test-label")))
            (should t))
          (let* ((show-issue (beads-client-show issue-id))
                 (labels (alist-get 'labels show-issue)))
            (should (member "test-label" (append labels nil))))
          (let ((_remove-result (beads-client-label-remove issue-id "test-label")))
            (should t)))
      (beads-client-delete (list issue-id) :force t))))

(ert-deftest beads-client-test-connection-timeout ()
  "Test that connection timeout is handled gracefully."
  :tags '(:integration)
  (let ((_beads-client-timeout 0.1))
    (cl-letf (((symbol-function 'beads-client--socket-path)
               (lambda () "/tmp/nonexistent-socket.sock")))
      (should-error (beads-client-health)
                    :type 'beads-client-error))))

(ert-deftest beads-client-test-invalid-operation ()
  "Test that invalid operations are handled."
  :tags '(:integration)
  (skip-unless (beads-client--socket-path))
  (skip-unless (file-exists-p (beads-client--socket-path)))
  (should-error (beads-client-request "invalid-operation" nil)
                :type 'beads-client-error))

(provide 'beads-client-test)
;;; beads-client-test.el ends here
