;;; beads-rpc.el --- RPC layer for Beads issue tracker -*- lexical-binding: t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze
;; Keywords: tools

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

;; RPC layer for communicating with the Beads daemon via Unix socket.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)

(defconst beads-rpc-client-version "0.1.0")

(define-error 'beads-rpc-error "Beads RPC error")

(defcustom beads-rpc-connection-strategy 'auto
  "Strategy for connecting to the Beads daemon.
- `auto': Try daemon, auto-start if not running, fall back to CLI if that fails.
- `daemon': Only use daemon (no auto-start), fail if not available.
- `managed': Start and manage daemon from Emacs, fall back to CLI if that fails.
- `cli': Only use CLI commands (for environments where daemon doesn't work)."
  :type '(choice (const :tag "Auto (start daemon, fallback to CLI)" auto)
                 (const :tag "Daemon only (no auto-start)" daemon)
                 (const :tag "Managed daemon (Emacs controls lifecycle)" managed)
                 (const :tag "CLI only" cli))
  :group 'beads)

(defcustom beads-rpc-daemon-startup-timeout 10
  "Seconds to wait for daemon to become ready after starting."
  :type 'integer
  :group 'beads)

(defvar beads-rpc--project-daemons (make-hash-table :test 'equal)
  "Hash table mapping project root paths to daemon processes.")

(defvar beads-rpc--daemon-start-in-progress (make-hash-table :test 'equal)
  "Hash table tracking which projects have daemon start in progress.")

(defvar beads-rpc--cached-db-path nil)
(defvar beads-rpc--cache-time nil)
(defconst beads-rpc--cache-ttl 10)

(cl-defun beads-rpc--find-database ()
  "Find the Beads database path using auto-discovery.
Checks BEADS_DIR env, BEADS_DB env, then walks up from default-directory."
  (when (and beads-rpc--cached-db-path
             beads-rpc--cache-time
             (< (float-time (time-since beads-rpc--cache-time))
                beads-rpc--cache-ttl))
    (when (file-exists-p beads-rpc--cached-db-path)
      (cl-return-from beads-rpc--find-database beads-rpc--cached-db-path)))

  (let ((db-path
         (or
          (let ((beads-dir (getenv "BEADS_DIR")))
            (when beads-dir
              (setq beads-dir (expand-file-name beads-dir))
              (setq beads-dir (beads-rpc--follow-redirect beads-dir))
              (beads-rpc--find-db-in-dir beads-dir)))

          (let ((beads-db (getenv "BEADS_DB")))
            (when beads-db
              (expand-file-name beads-db)))

          (let ((dir (expand-file-name default-directory)))
            (while (and dir
                        (not (string= dir "/"))
                        (not (string= dir (expand-file-name "~/.."))))
              (let* ((beads-dir (expand-file-name ".beads" dir))
                     (redirected-dir (beads-rpc--follow-redirect beads-dir))
                     (db (beads-rpc--find-db-in-dir redirected-dir)))
                (when db
                  (cl-return-from beads-rpc--find-database
                                  (progn
                                    (setq beads-rpc--cached-db-path db)
                                    (setq beads-rpc--cache-time (current-time))
                                    db)))
                (setq dir (file-name-directory (directory-file-name dir)))))
            nil))))

    (when db-path
      (setq beads-rpc--cached-db-path db-path)
      (setq beads-rpc--cache-time (current-time)))

    db-path))

(defun beads-rpc--follow-redirect (beads-dir)
  "Follow redirect file if present in BEADS-DIR."
  (let ((redirect-file (expand-file-name "redirect" beads-dir)))
    (if (file-exists-p redirect-file)
        (with-temp-buffer
          (insert-file-contents redirect-file)
          (string-trim (buffer-string)))
      beads-dir)))

(defun beads-rpc--find-db-in-dir (beads-dir)
  "Find database file in BEADS-DIR."
  (when (file-directory-p beads-dir)
    (let ((default-db (expand-file-name "beads.db" beads-dir)))
      (if (file-exists-p default-db)
          default-db
        (let ((db-files (directory-files beads-dir t "\\.db\\'")))
          (cl-find-if (lambda (f)
                        (and (not (string-match-p "\\.backup" f))
                             (not (string-match-p "vc\\.db\\'" f))))
                      db-files))))))

(defun beads-rpc--socket-path ()
  "Get the Unix socket path for the Beads daemon."
  (let ((db-path (beads-rpc--find-database)))
    (unless db-path
      (signal 'beads-rpc-error '("No Beads database found")))
    (expand-file-name "bd.sock" (file-name-directory db-path))))

(defun beads-rpc--project-root ()
  "Get the project root directory for the current Beads workspace.
This is the parent directory of .beads/."
  (when-let ((db-path (beads-rpc--find-database)))
    (file-name-directory
     (directory-file-name
      (file-name-directory db-path)))))

(defun beads-rpc--get-managed-daemon ()
  "Get the managed daemon process for the current project, if any."
  (when-let ((root (beads-rpc--project-root)))
    (let ((proc (gethash root beads-rpc--project-daemons)))
      (when (and proc (process-live-p proc))
        proc))))

(defun beads-rpc--daemon-sentinel (proc event)
  "Handle daemon PROC status change EVENT."
  (let ((root (process-get proc 'beads-project-root)))
    (when (and root (memq (process-status proc) '(exit signal)))
      (remhash root beads-rpc--project-daemons)
      (message "Beads daemon for %s terminated: %s"
               (abbreviate-file-name root)
               (string-trim event)))))

(defun beads-rpc--start-managed-daemon ()
  "Start a managed daemon for the current project.
Returns the process, or nil if starting fails."
  (let* ((root (beads-rpc--project-root))
         (bd-program (executable-find "bd")))
    (unless root
      (signal 'beads-rpc-error '("No Beads project found")))
    (unless bd-program
      (signal 'beads-rpc-error '("bd executable not found")))
    (when (gethash root beads-rpc--daemon-start-in-progress)
      (signal 'beads-rpc-error '("Daemon start already in progress")))
    (when-let ((existing (gethash root beads-rpc--project-daemons)))
      (when (process-live-p existing)
        (cl-return-from beads-rpc--start-managed-daemon existing)))
    (puthash root t beads-rpc--daemon-start-in-progress)
    (unwind-protect
        (let* ((default-directory root)
               (buf-name (format " *beads-daemon:%s*"
                                 (file-name-nondirectory
                                  (directory-file-name root))))
               (proc (make-process
                      :name "beads-daemon"
                      :buffer (get-buffer-create buf-name)
                      :command (list bd-program "daemon" "start" "--foreground")
                      :sentinel #'beads-rpc--daemon-sentinel
                      :noquery t)))
          (process-put proc 'beads-project-root root)
          (set-process-query-on-exit-flag proc nil)
          (puthash root proc beads-rpc--project-daemons)
          (message "Starting beads daemon for %s..." (abbreviate-file-name root))
          proc)
      (remhash root beads-rpc--daemon-start-in-progress))))

(defun beads-rpc--wait-for-socket (timeout)
  "Wait up to TIMEOUT seconds for the daemon socket to become available.
Returns non-nil if socket is ready, nil if timeout."
  (let ((socket-path (beads-rpc--socket-path))
        (start-time (float-time)))
    (while (and (< (- (float-time) start-time) timeout)
                (not (file-exists-p socket-path)))
      (sleep-for 0.1))
    (file-exists-p socket-path)))

(defun beads-rpc--ensure-daemon ()
  "Ensure the daemon is running, starting it if necessary.
Returns non-nil if daemon is available, nil otherwise."
  (let ((socket-path (beads-rpc--socket-path)))
    (cond
     ((file-exists-p socket-path)
      t)
     ((memq beads-rpc-connection-strategy '(auto managed))
      (beads-rpc--start-managed-daemon)
      (beads-rpc--wait-for-socket beads-rpc-daemon-startup-timeout))
     (t
      nil))))

(defun beads-rpc-start-daemon ()
  "Start the beads daemon for the current project.
Interactive command to manually start the daemon."
  (interactive)
  (if (beads-rpc--get-managed-daemon)
      (message "Beads daemon already running for this project")
    (beads-rpc--start-managed-daemon)
    (if (beads-rpc--wait-for-socket beads-rpc-daemon-startup-timeout)
        (message "Beads daemon started successfully")
      (message "Beads daemon started but socket not yet available"))))

(defun beads-rpc-stop-daemon ()
  "Stop the managed beads daemon for the current project."
  (interactive)
  (if-let ((proc (beads-rpc--get-managed-daemon)))
      (progn
        (delete-process proc)
        (message "Beads daemon stopped"))
    (message "No managed beads daemon running for this project")))

(defun beads-rpc-daemon-status ()
  "Show the status of the beads daemon for the current project."
  (interactive)
  (let ((socket-path (condition-case nil
                         (beads-rpc--socket-path)
                       (beads-rpc-error nil)))
        (managed-proc (beads-rpc--get-managed-daemon)))
    (cond
     ((and managed-proc (file-exists-p socket-path))
      (message "Beads daemon: running (managed by Emacs, PID %d)"
               (process-id managed-proc)))
     ((file-exists-p socket-path)
      (message "Beads daemon: running (external)"))
     (managed-proc
      (message "Beads daemon: starting (PID %d, socket not ready)"
               (process-id managed-proc)))
     (t
      (message "Beads daemon: not running")))))

(defun beads-rpc--cleanup-project-daemons ()
  "Stop all managed daemons.  Called on Emacs exit."
  (maphash (lambda (_root proc)
             (when (process-live-p proc)
               (delete-process proc)))
           beads-rpc--project-daemons)
  (clrhash beads-rpc--project-daemons))

(add-hook 'kill-emacs-hook #'beads-rpc--cleanup-project-daemons)

(defun beads-rpc--make-request-alist (operation args)
  "Build request alist for OPERATION with ARGS."
  (let ((db-path (beads-rpc--find-database)))
    `((operation . ,operation)
      (args . ,args)
      (cwd . ,(expand-file-name default-directory))
      (client_version . ,beads-rpc-client-version)
      (expected_db . ,db-path))))

(defun beads-rpc-request (operation args)
  "Send RPC request with OPERATION and ARGS to Beads daemon.
Returns the data field on success, signals beads-rpc-error on failure.

Connection behavior depends on `beads-rpc-connection-strategy':
- `auto'/`managed': Try daemon, auto-start if needed, fall back to CLI.
- `daemon': Only use daemon, no auto-start.
- `cli': Only use CLI commands."
  (pcase beads-rpc-connection-strategy
    ('cli
     (beads-rpc--cli-fallback operation args))
    ('daemon
     (beads-rpc--request-socket operation args))
    ((or 'auto 'managed)
     (condition-case err
         (progn
           (beads-rpc--ensure-daemon)
           (beads-rpc--request-socket operation args))
       (beads-rpc-error
        (let ((err-msg (cadr err)))
          (if (beads-rpc--connection-error-p err-msg)
              (beads-rpc--cli-fallback operation args)
            (signal 'beads-rpc-error (cdr err)))))))))

(defun beads-rpc--connection-error-p (err-msg)
  "Return non-nil if ERR-MSG indicates a connection problem.
These are errors where CLI fallback is appropriate."
  (and (stringp err-msg)
       (or (string-match-p "Socket not found" err-msg)
           (string-match-p "Failed to connect" err-msg)
           (string-match-p "Connection refused" err-msg)
           (string-match-p "No such file or directory" err-msg)
           (string-match-p "Timeout waiting for response" err-msg))))

(defun beads-rpc--request-socket (operation args)
  "Send RPC request with OPERATION and ARGS via socket.
This is the internal function that does the actual socket communication."
  (let* ((socket-path (beads-rpc--socket-path))
         (request-alist (beads-rpc--make-request-alist operation args))
         (request-json (json-encode request-alist)))

    (unless (file-exists-p socket-path)
      (signal 'beads-rpc-error
              (list (format "Socket not found: %s. Is the daemon running?" socket-path))))

    (with-temp-buffer
      (let* ((coding-system-for-read 'utf-8)
             (coding-system-for-write 'utf-8)
             (proc (condition-case nil
                       (make-network-process
                        :name "beads-rpc"
                        :buffer (current-buffer)
                        :family 'local
                        :service socket-path
                        :coding 'utf-8
                        :noquery t)
                     (file-error
                      (signal 'beads-rpc-error
                              (list "Failed to connect to daemon"))))))

        (unless proc
          (signal 'beads-rpc-error (list "Failed to connect to daemon")))

        (unwind-protect
            (progn
              (process-send-string proc (concat request-json "\n"))

              (let ((timeout 30)
                    (start-time (float-time)))
                (while (and (eq (process-status proc) 'open)
                            (not (progn
                                   (goto-char (point-min))
                                   (search-forward "\n" nil t)))
                            (< (- (float-time) start-time) timeout))
                  (accept-process-output proc 0.1)))

              (unless (progn (goto-char (point-min))
                             (search-forward "\n" nil t))
                (signal 'beads-rpc-error
                        (list (if (not (eq (process-status proc) 'open))
                                  "Daemon connection lost"
                                "Timeout waiting for response"))))

              (goto-char (point-min))
              (let* ((response (json-read))
                     (success (alist-get 'success response))
                     (data (alist-get 'data response))
                     (error-msg (alist-get 'error response)))

                (if (eq success t)
                    data
                  (signal 'beads-rpc-error
                          (list (or error-msg "Unknown error"))))))

          (when (process-live-p proc)
            (delete-process proc)))))))

(defun beads-rpc--cli-fallback (operation args)
  "Execute bd CLI as fallback for RPC OPERATION with ARGS.
Converts RPC operation and args to CLI command and flags.
Returns parsed JSON output."
  (let* ((bd-program (or (executable-find "bd")
                         (error "bd executable not found")))
         (cli-args (beads-rpc--operation-to-cli-args operation args))
         (cmd-args (append cli-args '("--json"))))
    (with-temp-buffer
      (let* ((default-directory (or (when-let ((db (beads-rpc--find-database)))
                                      (file-name-directory
                                       (directory-file-name
                                        (file-name-directory db))))
                                    default-directory))
             (exit-code (apply #'call-process bd-program nil t nil cmd-args)))
        (unless (zerop exit-code)
          (signal 'beads-rpc-error
                  (list (format "CLI failed with exit code %d: %s"
                                exit-code
                                (string-trim (buffer-string))))))
        (goto-char (point-min))
        (condition-case nil
            (json-read)
          (json-error
           (signal 'beads-rpc-error
                   (list (format "CLI returned invalid JSON: %s"
                                 (buffer-string))))))))))

(defun beads-rpc--operation-to-cli-args (operation args)
  "Convert RPC OPERATION and ARGS to CLI command and arguments.
Returns a list of strings suitable for `call-process'."
  (pcase operation
    ("health"
     '("daemon" "status"))
    ("list"
     (beads-rpc--build-cli-args "list" args
                                '(status priority issue_type assignee
                                  labels limit title_contains parent)))
    ("show"
     (let ((id (alist-get 'id args)))
       (list "show" id)))
    ("ready"
     (beads-rpc--build-cli-args "ready" args
                                '(assignee priority limit sort_policy parent)))
    ("create"
     (let ((title (alist-get 'title args))
           (other-args (assq-delete-all 'title (copy-alist args))))
       (append (list "create" title)
               (beads-rpc--alist-to-cli-flags other-args))))
    ("update"
     (let ((id (alist-get 'id args))
           (other-args (assq-delete-all 'id (copy-alist args))))
       (append (list "update" id)
               (beads-rpc--alist-to-cli-flags other-args))))
    ("close"
     (let ((id (alist-get 'id args))
           (reason (alist-get 'reason args)))
       (if reason
           (list "close" id "--reason" reason)
         (list "close" id))))
    ("delete"
     (let ((ids (alist-get 'ids args))
           (force (alist-get 'force args)))
       (append (list "delete")
               (if (listp ids) ids (list ids))
               (when force '("--force")))))
    ("stats"
     '("stats"))
    ("count"
     (beads-rpc--build-cli-args "count" args '(status group_by)))
    ("dep_add"
     (let ((from-id (alist-get 'from_id args))
           (to-id (alist-get 'to_id args))
           (dep-type (alist-get 'dep_type args)))
       (if dep-type
           (list "dep" "add" from-id to-id "--type" dep-type)
         (list "dep" "add" from-id to-id))))
    ("dep_remove"
     (let ((from-id (alist-get 'from_id args))
           (to-id (alist-get 'to_id args)))
       (list "dep" "remove" from-id to-id)))
    ("dep_tree"
     (let ((id (alist-get 'id args))
           (max-depth (alist-get 'max_depth args)))
       (if max-depth
           (list "dep" "tree" id "--max-depth" (number-to-string max-depth))
         (list "dep" "tree" id))))
    ("label_add"
     (let ((id (alist-get 'id args))
           (label (alist-get 'label args)))
       (list "label" "add" id label)))
    ("label_remove"
     (let ((id (alist-get 'id args))
           (label (alist-get 'label args)))
       (list "label" "remove" id label)))
    ("get_mutations"
     (let ((since-id (alist-get 'since_id args)))
       (if since-id
           (list "mutations" "--since" since-id)
         '("mutations"))))
    (_
     (signal 'beads-rpc-error
             (list (format "Unknown operation for CLI fallback: %s" operation))))))

(defun beads-rpc--build-cli-args (command args allowed-keys)
  "Build CLI args for COMMAND from ARGS, filtering by ALLOWED-KEYS."
  (append (list command)
          (beads-rpc--alist-to-cli-flags
           (seq-filter (lambda (pair)
                         (memq (car pair) allowed-keys))
                       args))))

(defun beads-rpc--alist-to-cli-flags (alist)
  "Convert ALIST to list of CLI flags.
Keys are converted from snake_case to --kebab-case."
  (let ((flags '()))
    (dolist (pair alist)
      (let* ((key (car pair))
             (value (cdr pair))
             (flag-name (concat "--" (replace-regexp-in-string
                                      "_" "-" (symbol-name key)))))
        (cond
         ((eq value t)
          (push flag-name flags))
         ((eq value nil)
          nil)
         ((listp value)
          (dolist (v value)
            (push flag-name flags)
            (push (format "%s" v) flags)))
         (t
          (push flag-name flags)
          (push (format "%s" value) flags)))))
    (nreverse flags)))

(defun beads-rpc-health ()
  "Check daemon health.
Returns t if healthy, signals error otherwise."
  (condition-case err
      (let ((response (beads-rpc-request "health" nil)))
        (equal (alist-get 'status response) "healthy"))
    (beads-rpc-error
     (signal 'beads-rpc-error (list "Daemon unhealthy" err)))))

(defun beads-rpc-list (&optional filters)
  "List issues with optional FILTERS.
FILTERS is a plist with keys like :status, :priority, :issue-type, :assignee,
:labels, :limit, :title-contains, :parent (for epic-scoped views), etc.
Returns array of issue objects."
  (let ((args (beads-rpc--plist-to-alist filters)))
    (beads-rpc-request "list" args)))

(defun beads-rpc-show (id)
  "Get single issue by ID.
Returns issue object."
  (unless id
    (signal 'beads-rpc-error (list "Issue ID required")))
  (beads-rpc-request "show" `((id . ,id))))

(defun beads-rpc-ready (&optional filters)
  "Get unblocked issues with optional FILTERS.
FILTERS is a plist with keys like :assignee, :priority, :limit, :sort-policy,
:parent (for epic-scoped views).
Returns array of ready issue objects."
  (let ((args (beads-rpc--plist-to-alist filters)))
    (beads-rpc-request "ready" args)))

(defun beads-rpc-create (title &rest args)
  "Create new issue with TITLE and additional ARGS.
ARGS is a plist with keys like :description, :issue-type, :priority,
:assignee, :labels, :design, :acceptance-criteria, :dependencies, :parent,
and :dry-run.  When :dry-run is non-nil, returns a preview without creating.
Returns created (or previewed) issue object."
  (unless title
    (signal 'beads-rpc-error (list "Title required")))
  (let ((request-args (beads-rpc--plist-to-alist
                       (plist-put args :title title))))
    (beads-rpc-request "create" request-args)))

(defun beads-rpc-update (id &rest args)
  "Update issue ID with ARGS.
ARGS is a plist with keys like :title, :description, :status,
:priority, :assignee, :issue-type, :design, :notes, :add-labels,
:remove-labels, :set-labels.  Returns updated issue object."
  (unless id
    (signal 'beads-rpc-error (list "Issue ID required")))
  (let ((request-args (beads-rpc--plist-to-alist
                       (plist-put args :id id))))
    (beads-rpc-request "update" request-args)))

(defun beads-rpc-close (id &optional reason)
  "Close issue ID with optional REASON.
Returns closed issue object."
  (unless id
    (signal 'beads-rpc-error (list "Issue ID required")))
  (let ((args `((id . ,id))))
    (when reason
      (push `(reason . ,reason) args))
    (beads-rpc-request "close" args)))

(defun beads-rpc-delete (ids &rest args)
  "Delete issues by IDS (list of issue IDs) with optional ARGS.
ARGS is a plist with keys like :force, :cascade, :reason.
Returns deletion result."
  (unless ids
    (signal 'beads-rpc-error (list "Issue IDs required")))
  (let ((request-args (beads-rpc--plist-to-alist
                       (plist-put args :ids ids))))
    (beads-rpc-request "delete" request-args)))

(defun beads-rpc-stats ()
  "Get issue statistics.
Returns stats object with counts and breakdowns."
  (beads-rpc-request "stats" nil))

(defun beads-rpc-count (&optional filters)
  "Count issues with optional FILTERS.
FILTERS is a plist with keys like :status, :group-by.
Returns count data."
  (let ((args (beads-rpc--plist-to-alist filters)))
    (beads-rpc-request "count" args)))

(defun beads-rpc-dep-add (from-id to-id &optional dep-type)
  "Add dependency FROM-ID to TO-ID with optional DEP-TYPE.
DEP-TYPE can be \"blocks\", \"related\", \"parent-child\", or \"discovered-from\".
Defaults to \"blocks\"."
  (unless (and from-id to-id)
    (signal 'beads-rpc-error (list "Both from-id and to-id required")))
  (let ((args `((from_id . ,from-id)
                (to_id . ,to-id))))
    (when dep-type
      (push `(dep_type . ,dep-type) args))
    (beads-rpc-request "dep_add" args)))

(defun beads-rpc-dep-remove (from-id to-id)
  "Remove dependency FROM-ID to TO-ID."
  (unless (and from-id to-id)
    (signal 'beads-rpc-error (list "Both from-id and to-id required")))
  (beads-rpc-request "dep_remove" `((from_id . ,from-id)
                                     (to_id . ,to-id))))

(defun beads-rpc-dep-tree (id &optional max-depth)
  "Get dependency tree for issue ID with optional MAX-DEPTH."
  (unless id
    (signal 'beads-rpc-error (list "Issue ID required")))
  (let ((args `((id . ,id))))
    (when max-depth
      (push `(max_depth . ,max-depth) args))
    (beads-rpc-request "dep_tree" args)))

(defun beads-rpc-label-add (id label)
  "Add LABEL to issue ID."
  (unless (and id label)
    (signal 'beads-rpc-error (list "Issue ID and label required")))
  (beads-rpc-request "label_add" `((id . ,id)
                                    (label . ,label))))

(defun beads-rpc-label-remove (id label)
  "Remove LABEL from issue ID."
  (unless (and id label)
    (signal 'beads-rpc-error (list "Issue ID and label required")))
  (beads-rpc-request "label_remove" `((id . ,id)
                                       (label . ,label))))

(defun beads-rpc-get-mutations (&optional since-id)
  "Get mutations since SINCE-ID for real-time updates.
Returns array of mutation objects."
  (let ((args (when since-id
                `((since_id . ,since-id)))))
    (beads-rpc-request "get_mutations" args)))

(defun beads-rpc--plist-to-alist (plist)
  "Convert PLIST with keyword keys to alist with string keys.
Converts :kebab-case to snake_case for JSON."
  (when plist
    (let ((alist '())
          (key nil))
      (while plist
        (setq key (pop plist))
        (unless (keywordp key)
          (signal 'beads-rpc-error (list "Expected keyword in plist" key)))
        (let* ((key-name (substring (symbol-name key) 1))
               (json-key (replace-regexp-in-string "-" "_" key-name))
               (value (pop plist)))
          (when value
            (push (cons json-key value) alist))))
      (nreverse alist))))

(defun beads-rpc-clear-cache ()
  "Clear the cached database path.
Useful when switching between projects."
  (interactive)
  (setq beads-rpc--cached-db-path nil)
  (setq beads-rpc--cache-time nil))

(provide 'beads-rpc)
;;; beads-rpc.el ends here
