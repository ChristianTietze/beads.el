;;; beads-rpc.el --- RPC layer for Beads issue tracker -*- lexical-binding: t -*-

;;; Code:

(require 'json)
(require 'cl-lib)

(defconst beads-rpc-client-version "0.1.0")

(define-error 'beads-rpc-error "Beads RPC error")

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
Returns the data field on success, signals beads-rpc-error on failure."
  (let* ((socket-path (beads-rpc--socket-path))
         (request-alist (beads-rpc--make-request-alist operation args))
         (request-json (json-encode request-alist)))

    (unless (file-exists-p socket-path)
      (signal 'beads-rpc-error
              (list (format "Socket not found: %s. Is the daemon running?" socket-path))))

    (with-temp-buffer
      (let* ((coding-system-for-read 'utf-8)
             (coding-system-for-write 'utf-8)
             (proc (make-network-process
                    :name "beads-rpc"
                    :buffer (current-buffer)
                    :family 'local
                    :service socket-path
                    :coding 'utf-8
                    :noquery t)))

        (unless proc
          (signal 'beads-rpc-error (list "Failed to connect to daemon")))

        (unwind-protect
            (progn
              (process-send-string proc (concat request-json "\n"))

              (let ((timeout 30)
                    (start-time (float-time)))
                (while (and (process-live-p proc)
                            (not (progn
                                   (goto-char (point-min))
                                   (search-forward "\n" nil t)))
                            (< (- (float-time) start-time) timeout))
                  (accept-process-output proc 0.1)))

              (unless (progn (goto-char (point-min))
                             (search-forward "\n" nil t))
                (signal 'beads-rpc-error (list "Timeout waiting for response")))

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

(defun beads-rpc--cli-fallback (command &rest args)
  "Execute bd COMMAND with ARGS as CLI fallback.
Returns parsed JSON output."
  (let* ((bd-program (or (executable-find "bd")
                         (expand-file-name "bd" (file-name-directory
                                                 (beads-rpc--find-database)))))
         (cmd-args (append (list command "--json") args))
         (output (with-temp-buffer
                   (let ((exit-code (apply #'call-process bd-program nil t nil cmd-args)))
                     (unless (zerop exit-code)
                       (signal 'beads-rpc-error
                               (list (format "CLI failed with exit code %d: %s"
                                           exit-code
                                           (buffer-string)))))
                     (goto-char (point-min))
                     (json-read)))))
    output))

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
:labels, :limit, :title-contains, etc.
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
FILTERS is a plist with keys like :assignee, :priority, :limit, :sort-policy.
Returns array of ready issue objects."
  (let ((args (beads-rpc--plist-to-alist filters)))
    (beads-rpc-request "ready" args)))

(defun beads-rpc-create (title &rest args)
  "Create new issue with TITLE and additional ARGS.
ARGS is a plist with keys like :description, :issue-type, :priority,
:assignee, :labels, :design, :acceptance-criteria, :dependencies, :parent.
Returns created issue object."
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
