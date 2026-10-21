;;; beads-compat-test.el --- Current CLI contract regressions -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'beads)
(require 'beads-form)
(require 'beads-activity)
(require 'beads-conflicts)
(require 'beads-duplicates)

(ert-deftest beads-compat-test-stats-response ()
  "Both current nested and legacy flat statistics reach the UI unchanged."
  (let ((beads-cli-program "bd"))
    (dolist (json '("{\"summary\":{\"total_issues\":7,\"open_issues\":3}}"
                    "{\"total_issues\":7,\"open_issues\":3}"))
      (cl-letf (((symbol-function 'beads-backend--call-json)
                 (lambda (&rest _) (json-read-from-string json))))
               (let ((stats (beads-client-stats)))
                 (should (= 7 (alist-get 'total_issues stats)))
                 (should (= 3 (alist-get 'open_issues stats))))))))

(ert-deftest beads-compat-test-count-and-modern-flags ()
  "Current bd uses grouping switches, ping, and no legacy daemon flags."
  (cl-letf (((symbol-function 'beads-backend-bd--supports-daemon-p) (lambda () nil)))
           (should (equal '("ping") (beads-backend-bd--operation-to-cli-args "health" nil)))
           (should-not (beads-backend-bd--cli-extra-flags "duplicates"))
           (should-error (beads-backend-bd--operation-to-cli-args "get_mutations" nil)
                         :type 'beads-backend-error))
  (dolist (group '("status" "priority" "type" "assignee" "label"))
    (should (equal (list "count" "--status" "open" (concat "--by-" group))
                   (beads-backend-bd--operation-to-cli-args
                    "count" `((status . "open") (group_by . ,group))))))
  (should-error (beads-backend-bd--operation-to-cli-args
                 "count" '((group_by . "invalid"))) :type 'beads-backend-error))

(ert-deftest beads-compat-test-label-json-array ()
  "Label filtering accepts the vectors returned by JSON, as well as lists."
  (dolist (labels '(["regression" "other"] ("regression" "other") nil))
    (let ((issues (list `((id . "test-1") (labels . ,labels)))))
      (should (= (if labels 1 0)
                 (length (beads-filter-apply (beads-filter-by-label "regression") issues)))))))

(ert-deftest beads-compat-test-ready-refetches-and-composes ()
  "Readiness comes from bd once per application, even in composed filters."
  (let* ((issues '(((id . "ready") (status . "open") (dependency_count . 2))
                   ((id . "working") (status . "in_progress") (dependency_count . 0))
                   ((id . "blocked") (status . "open") (dependency_count . 0))))
         (filter (beads-filter-compose (beads-filter-ready) (beads-filter-by-status "open")))
         (ready '(((id . "ready"))))
         (calls 0))
    (cl-letf (((symbol-function 'beads-client-ready)
               (lambda (filters)
                 (should (equal filters '(:limit 0)))
                 (cl-incf calls)
                 ready)))
             (should (equal '("ready") (mapcar (lambda (i) (alist-get 'id i))
                                               (beads-filter-apply filter issues))))
             (should (= 1 calls))
             (setq ready nil)
             (should-not (beads-filter-apply filter issues))
             (should (= 2 calls)))))

(ert-deftest beads-compat-test-confirmed-delete ()
  "Single and bulk UI deletes perform deletion only after confirmation."
  (let ((beads-verbose nil) (beads-autoupdate-enable nil) calls)
    (with-temp-buffer
      (beads-detail-mode)
      (setq-local beads-detail--current-issue-id "test-1")
      (setq-local beads-detail--current-issue '((title . "Fixture")))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'beads-client-delete)
                 (lambda (&rest args) (push args calls)))
                ((symbol-function 'quit-window) #'ignore))
               (beads-delete-issue)
               (should-not calls)
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                        (beads-delete-issue))
               (should (equal '(("test-1") :force t) (pop calls)))))
    (with-temp-buffer
      (beads-list-mode)
      (setq-local beads-list--marked '("test-2" "test-3"))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'beads-client-delete)
                 (lambda (&rest args) (push args calls)))
                ((symbol-function 'beads-list-refresh) #'ignore))
               (beads-list-bulk-delete)
               (should (equal '(("test-3" :force t) ("test-2" :force t)) calls))))))

(ert-deftest beads-compat-test-no-daemon-auto-falls-back ()
  "Auto/managed strategy uses CLI without trying an unavailable socket."
  (dolist (beads-client-connection-strategy '(auto managed))
    (cl-letf (((symbol-function 'beads-backend-supports-daemon-p) (lambda (&rest _) nil))
              ((symbol-function 'beads-client--request-socket)
               (lambda (&rest _) (ert-fail "Must not try a socket")))
              ((symbol-function 'beads-client--cli-fallback)
               (lambda (op args) (list op args))))
             (should (equal '("list" nil) (beads-client-request "list" nil))))))

(ert-deftest beads-compat-test-unavailable-commands-explain ()
  "Removed commands report actionable limitations without starting timers."
  (cl-letf (((symbol-function 'beads-backend-cli-supports-command-p) (lambda (&rest _) nil)))
           (should-error (beads-activity) :type 'user-error)
           (should-error (beads-client-daemon-status) :type 'user-error)
           (should-error (beads-client-start-daemon) :type 'beads-client-error)))

(defconst beads-compat-test--conflicts
  '((conflicts . 1)
    (rows . [((table . "issues") (key . "test-1")
              (our_exists . t) (their_exists . t)
              (fields . [((name . "title") (base . "Before")
                          (ours . "Local") (theirs . "Incoming"))]))])))

(ert-deftest beads-compat-test-dolt-conflicts ()
  "Conflict UI shows both sides and requires an explicit confirmed choice."
  (with-temp-buffer
    (beads-conflicts-mode)
    (beads-conflicts--render beads-compat-test--conflicts)
    (dolist (text '("test-1" "Before" "Local" "Incoming"))
      (should (string-match-p text (buffer-string))))
    (setq-local beads-conflicts--data beads-compat-test--conflicts)
    (let (calls)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "theirs"))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'beads-backend-cli-execute)
                 (lambda (&rest args) (push args calls) '((resolved . 1) (remaining . 0))))
                ((symbol-function 'beads-conflicts-refresh) #'ignore))
               (beads-conflicts-resolve-all)
               (should-not calls)
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                        (beads-conflicts-resolve-all))
               (should (equal '(("resolve-conflicts" ((strategy . "theirs")))) calls))))))

(ert-deftest beads-compat-test-conflict-cli-contract ()
  "Dolt conflicts use show/resolve; legacy JSONL remains available."
  (cl-letf (((symbol-function 'beads-backend-cli-supports-command-p) (lambda (&rest _) t)))
           (should (equal '("conflicts" "show")
                          (beads-backend-bd--operation-to-cli-args
                           "resolve-conflicts" '((dry_run . t)))))
           (should-error (beads-backend-bd--operation-to-cli-args "resolve-conflicts" nil)
                         :type 'beads-backend-error)
           (should (equal '("conflicts" "resolve" "--all" "--strategy" "ours")
                          (beads-backend-bd--operation-to-cli-args
                           "resolve-conflicts" '((strategy . "ours"))))))
  (cl-letf (((symbol-function 'beads-backend-cli-supports-command-p) (lambda (&rest _) nil)))
           (should (equal '("resolve-conflicts" "--dry-run")
                          (beads-backend-bd--operation-to-cli-args
                           "resolve-conflicts" '((dry_run . t)))))))

(ert-deftest beads-compat-test-view-state-survives-mode-setup ()
  "New detail/form modes must retain their issue after clearing locals."
  (let ((issue '((id . "state-fixture") (title . "State fixture") (priority . 2)
                 (status . "open") (issue_type . "task")))
        (beads-verbose nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil))
                  ((symbol-function 'pop-to-buffer) #'ignore))
                 (dolist (vui '(nil t))
                   (let ((beads-detail-use-vui vui) (beads-form-use-vui vui))
                     (beads-detail-open issue)
                     (with-current-buffer "*Beads Detail: state-fixture*"
                       (should (equal (beads-detail--require-issue) issue))
                       (cl-letf (((symbol-function 'beads-client-show) (lambda (_id) issue))
                                 ((symbol-function 'beads-detail--refresh-list-buffers) #'ignore))
                                (dotimes (_ 2) (beads-detail-refresh)))
                       (should (equal beads-detail--current-issue-id "state-fixture")))
                     (beads-form-open issue)
                     (with-current-buffer "*Beads Form: state-fixture*"
                       (should (equal beads-form--issue-id "state-fixture"))
                       (should (equal beads-form--original-issue issue))))))
      (dolist (name '("*Beads Detail: state-fixture*" "*Beads Form: state-fixture*"))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest beads-compat-test-preview-retains-project-context ()
  "An idle preview uses its originating project even after switching buffers."
  (let ((source (generate-new-buffer " *beads-source*")) callback seen)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq default-directory "/tmp/")
            (cl-letf (((symbol-function 'run-with-idle-timer)
                       (lambda (_seconds _repeat fn &rest _args)
                         (setq callback fn) nil)))
                     (beads-preview--start-timer '((id . "test-1")))))
          (with-temp-buffer
            (cl-letf (((symbol-function 'beads-preview--display-issue)
                       (lambda (_issue) (setq seen (list (current-buffer) default-directory)))))
                     (funcall callback)))
          (should (equal seen (list source "/tmp/"))))
      (kill-buffer source))))

(ert-deftest beads-compat-test-clearing-form-fields ()
  "Clearing assignment and external reference sends empty strings, not omitted nils."
  (let ((beads-form-use-vui nil) (beads-verbose nil))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
                 (beads-form-open '((id . "clear-fixture") (title . "Clear fields")
                                    (status . "open") (priority . 2) (issue_type . "task")
                                    (assignee . "owner") (external_ref . "link")))
                 (with-current-buffer "*Beads Form: clear-fixture*"
                   (dolist (field '(assignee external_ref))
                     (widget-value-set (alist-get field beads-form--widgets) ""))
                   (should (equal (beads-client--plist-to-alist (beads-form--collect-changes))
                                  '(("assignee" . "") ("external_ref" . ""))))))
      (when (get-buffer "*Beads Form: clear-fixture*")
        (kill-buffer "*Beads Form: clear-fixture*")))))

(defmacro beads-compat-test--with-workspace (&rest body)
  "Run BODY in a disposable bd workspace, never the user's project database."
  (declare (indent 0))
  `(progn
     (skip-unless (executable-find "bd"))
     (let* ((root (make-temp-file "beads-compat-test-" t))
            (default-directory (file-name-as-directory root))
            (process-environment (copy-sequence process-environment))
            (beads-cli-program "bd")
            (beads-client-connection-strategy 'cli)
            (beads-backend--command-cache (make-hash-table :test 'equal))
            (beads-verbose nil)
            (beads-autoupdate-enable nil))
       (setenv "BEADS_DIR" nil)
       (setenv "BEADS_DB" nil)
       (unwind-protect
           (progn
             (should (eq 0 (call-process "git" nil nil nil "init" "--quiet")))
             (should (eq 0 (call-process "bd" nil nil nil "init" "--prefix" "test"
                                         "--non-interactive")))
             ,@body)
         (delete-directory root t)))))

(ert-deftest beads-compat-test-cli-integration ()
  "Verify reports, readiness, labels, confirmed deletes, and duplicate merging."
  :tags '(:integration :cli)
  (beads-compat-test--with-workspace
   (let* ((blocker (beads-client-create "Blocker" :description "Blocking work"))
          (dependent (beads-client-create "Dependent" :description "Dependent work"))
          (blocker-id (alist-get 'id blocker))
          (dependent-id (alist-get 'id dependent))
          (ready-filter (beads-filter-ready)))
     (beads-client-dep-add dependent-id blocker-id)
     (should-not (seq-find (lambda (i) (equal dependent-id (alist-get 'id i)))
                           (beads-filter-apply ready-filter (beads-client-list))))
     (beads-client-close blocker-id "Unblock the dependent")
     ;; Closed blockers leave a dependency edge, but no longer block readiness.
     (should (seq-find (lambda (i) (equal dependent-id (alist-get 'id i)))
                       (beads-filter-apply ready-filter (beads-client-list))))
     (beads-client-update dependent-id :status "in_progress" :add-labels '("regression"))
     (should-not (beads-filter-apply ready-filter (beads-client-list)))
     (should (= 1 (length (beads-filter-apply (beads-filter-by-label "regression")
                                              (beads-client-list)))))
     (should (= 2 (alist-get 'total_issues (beads-client-stats))))
     (should (= 1 (alist-get 'closed_issues (beads-client-stats))))
     (should (= 2 (alist-get 'total (beads-client-count '(:group-by "status")))))
     (should (beads-client-health)))
   (let* ((single (beads-client-create "Delete single" :description "Disposable"))
          (bulk (beads-client-create "Delete bulk" :description "Disposable"))
          (single-id (alist-get 'id single))
          (bulk-id (alist-get 'id bulk)))
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'quit-window) #'ignore))
              (with-temp-buffer
                (beads-detail-mode)
                (setq-local beads-detail--current-issue-id single-id)
                (setq-local beads-detail--current-issue single)
                (beads-delete-issue))
              (with-temp-buffer
                (beads-list-mode)
                (setq-local beads-list--marked (list bulk-id))
                (beads-list-bulk-delete)))
     (dolist (issue (beads-client-list))
       (should-not (member (alist-get 'id issue) (list single-id bulk-id)))))
   (dotimes (_ 2)
     (beads-client-create "Duplicate fixture" :description "Identical description"))
   (let* ((data (beads-duplicates--fetch))
          (group (aref (alist-get 'groups data) 0))
          (target (alist-get 'suggested_target group))
          (source (aref (alist-get 'suggested_sources group) 0)))
     (should (= 1 (alist-get 'duplicate_groups data)))
     (with-temp-buffer
       (beads-duplicates-mode)
       (beads-duplicates--render data)
       (should (string-match-p "Duplicate fixture" (buffer-string))))
     (beads-duplicates--merge source target)
     (should (equal "closed" (alist-get 'status (beads-client-show source))))
     (should (equal "open" (alist-get 'status (beads-client-show target)))))))

(ert-deftest beads-compat-test-dolt-conflict-integration ()
  "Render and resolve a genuine merge conflict in an isolated Dolt database."
  :tags '(:integration :cli)
  (skip-unless (executable-find "dolt"))
  (beads-compat-test--with-workspace
   (let* ((issue (beads-client-create "Base title" :description "Conflict fixture"))
          (id (alist-get 'id issue))
          (data-dir (expand-file-name ".beads/embeddeddolt/test/" root)))
     (skip-unless (file-directory-p data-dir))
     (let ((default-directory data-dir))
       (dolist (args '(("config" "--local" "--add" "user.name" "Beads Test")
                       ("config" "--local" "--add" "user.email" "beads-test@example.invalid")
                       ("add" ".") ("commit" "--allow-empty" "-m" "Fixture base")
                       ("checkout" "-b" "incoming")))
         (should (eq 0 (apply #'call-process "dolt" nil nil nil args))))
       (should (eq 0 (call-process "dolt" nil nil nil "sql" "-q"
                                   (format "UPDATE issues SET title='Incoming title' WHERE id='%s'" id))))
       (should (eq 0 (call-process "dolt" nil nil nil "add" ".")))
       (should (eq 0 (call-process "dolt" nil nil nil "commit" "-m" "Incoming change")))
       (should (eq 0 (call-process "dolt" nil nil nil "checkout" "main")))
       (should (eq 0 (call-process "dolt" nil nil nil "sql" "-q"
                                   (format "UPDATE issues SET title='Local title' WHERE id='%s'" id))))
       (should (eq 0 (call-process "dolt" nil nil nil "add" ".")))
       (should (eq 0 (call-process "dolt" nil nil nil "commit" "-m" "Local change")))
       (should (eq 1 (call-process "dolt" nil nil nil "merge" "--no-commit" "incoming"))))
     (with-temp-buffer
       (beads-conflicts-mode)
       (setq-local beads-conflicts--data (beads-conflicts--fetch))
       (should (= 1 (alist-get 'conflicts beads-conflicts--data)))
       (beads-conflicts--render beads-conflicts--data)
       (dolist (title '("Base title" "Local title" "Incoming title"))
         (should (string-match-p title (buffer-string))))
       (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "theirs"))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (beads-conflicts-resolve-all))
       (should (= 0 (alist-get 'conflicts beads-conflicts--data))))
     (should (equal "Incoming title" (alist-get 'title (beads-client-show id)))))))

(provide 'beads-compat-test)
;;; beads-compat-test.el ends here
