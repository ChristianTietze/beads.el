# How to Write a Beads Client

## Architecture

```
┌─────────────────────────────────┐
│    Client Application           │
└───────────────┬─────────────────┘
                │ RPC over Unix Socket
                ▼
┌─────────────────────────────────┐
│      Beads Daemon               │
│      .beads/bd.sock             │
│                                 │
│  • Single-owner pattern         │
│  • Event-driven FlushManager    │
│  • Hash-based collision avoid   │
└───────────────┬─────────────────┘
                │
                ▼
┌─────────────────────────────────┐
│      SQLite + JSONL Sync        │
│      .beads/beads.db            │
└─────────────────────────────────┘
```

All database operations go through the daemon, which serializes writes and handles concurrency. Your client never touches SQLite directly.

## Auto-Discovery

Your client must find the database with zero user configuration.

### Discovery Algorithm

```
function discoverDatabase():
    if env.BEADS_DIR exists:
        beadsDir = canonicalize(env.BEADS_DIR)
        beadsDir = followRedirect(beadsDir)
        return findDBInDir(beadsDir)

    if env.BEADS_DB exists:
        return canonicalize(env.BEADS_DB)

    current = cwd()
    while current != root():
        beadsDir = current + "/.beads"
        if exists(beadsDir):
            beadsDir = followRedirect(beadsDir)
            if db = findDBInDir(beadsDir):
                return db
        current = parent(current)

    return null

function followRedirect(beadsDir):
    redirectFile = beadsDir + "/redirect"
    if exists(redirectFile):
        return readFile(redirectFile).trim()
    return beadsDir

function findDBInDir(beadsDir):
    if exists(beadsDir + "/beads.db"):
        return beadsDir + "/beads.db"

    for file in glob(beadsDir + "/*.db"):
        if not file.contains(".backup") and file != "vc.db":
            return file
    return null
```

### Socket Path

```
socketPath = dirname(databasePath) + "/bd.sock"
```

## RPC Protocol

### Wire Format

- JSON + newline (`\n`) delimiter
- UTF-8 encoding
- Request → Response pattern

### Request Structure

```json
{
  "operation": "string",
  "args": {...},
  "cwd": "/absolute/path",
  "client_version": "0.21.0",
  "expected_db": "/abs/path/to/beads.db"
}
```

### Response Structure

```json
{
  "success": boolean,
  "data": {...},
  "error": "string"
}
```

### Connection Flow

1. Check if socket file exists
2. Connect to Unix socket (200ms timeout)
3. Send health check: `{"operation":"health","args":null}`
4. Verify response: `{"success":true,"data":{"status":"healthy"}}`

## Operations

### Read Operations

#### list

```json
{
  "operation": "list",
  "args": {
    "status": "open",
    "priority": 1,
    "issue_type": "bug",
    "assignee": "alice",
    "labels": ["backend", "urgent"],
    "labels_any": ["frontend", "backend"],
    "limit": 50,
    "title_contains": "auth",
    "description_contains": "token",
    "created_after": "2024-01-01",
    "created_before": "2024-12-31",
    "priority_min": 0,
    "priority_max": 2,
    "no_assignee": true,
    "empty_description": false
  }
}
```

All args optional. Response `data`: array of issue objects.

#### show

```json
{
  "operation": "show",
  "args": {"id": "bd-a1b2"}
}
```

Response `data`: single issue object.

#### ready

Get unblocked issues (no open blockers).

```json
{
  "operation": "ready",
  "args": {
    "assignee": "alice",
    "unassigned": false,
    "priority": 1,
    "limit": 20,
    "sort_policy": "hybrid",
    "labels": ["backend"]
  }
}
```

Sort policies: `"priority"`, `"oldest"`, `"hybrid"` (default)

#### stats

```json
{"operation": "stats", "args": null}
```

Response:

```json
{
  "total_issues": 150,
  "open_issues": 45,
  "in_progress_issues": 12,
  "closed_issues": 93,
  "priority_breakdown": {"0": 5, "1": 15, "2": 80},
  "type_breakdown": {"bug": 25, "feature": 40, "task": 70}
}
```

#### count

```json
{
  "operation": "count",
  "args": {
    "status": "open",
    "group_by": "priority"
  }
}
```

Group by: `"status"`, `"priority"`, `"type"`, `"assignee"`, `"label"`

#### dep_tree

```json
{
  "operation": "dep_tree",
  "args": {"id": "bd-a1b2", "max_depth": 5}
}
```

### Write Operations

#### create

```json
{
  "operation": "create",
  "args": {
    "title": "Fix authentication bug",
    "description": "Users cannot login",
    "issue_type": "bug",
    "priority": 1,
    "assignee": "alice",
    "labels": ["backend", "auth"],
    "design": "Design notes",
    "acceptance_criteria": "Criteria here",
    "dependencies": ["bd-x1y2"],
    "parent": "bd-epic1"
  }
}
```

Issue types: `"bug"`, `"feature"`, `"task"`, `"epic"`, `"chore"`
Priority: 0 (highest) to 4 (lowest), default 2

Response `data`: created issue with generated hash-based ID.

#### update

```json
{
  "operation": "update",
  "args": {
    "id": "bd-a1b2",
    "title": "New title",
    "description": "New description",
    "status": "in_progress",
    "priority": 0,
    "assignee": "bob",
    "issue_type": "feature",
    "design": "Updated design",
    "notes": "Working on this now",
    "add_labels": ["urgent"],
    "remove_labels": ["backlog"],
    "set_labels": ["frontend", "urgent"]
  }
}
```

Use `add_labels`/`remove_labels` for incremental changes, `set_labels` to replace all.

Status values: `"open"`, `"in_progress"`, `"closed"`

#### close

```json
{
  "operation": "close",
  "args": {
    "id": "bd-a1b2",
    "reason": "Implemented in commit abc123"
  }
}
```

#### delete

```json
{
  "operation": "delete",
  "args": {
    "ids": ["bd-a1b2", "bd-c3d4"],
    "force": true,
    "cascade": false,
    "reason": "Duplicate of bd-x1y2"
  }
}
```

### Dependency Operations

#### dep_add

```json
{
  "operation": "dep_add",
  "args": {
    "from_id": "bd-f14c",
    "to_id": "bd-a1b2",
    "dep_type": "blocks"
  }
}
```

Dependency types:
- `"blocks"` - Hard blocker (affects ready work)
- `"related"` - Soft link (informational)
- `"parent-child"` - Hierarchical
- `"discovered-from"` - Found during work on parent

#### dep_remove

```json
{
  "operation": "dep_remove",
  "args": {
    "from_id": "bd-f14c",
    "to_id": "bd-a1b2"
  }
}
```

### Label Operations

#### label_add / label_remove

```json
{
  "operation": "label_add",
  "args": {"id": "bd-a1b2", "label": "urgent"}
}
```

### Real-Time Updates

#### get_mutations

Poll for changes to implement live UI updates.

```json
{
  "operation": "get_mutations",
  "args": {"since_id": "mut-12345"}
}
```

Response: array of mutation objects with `id`, `type`, `issue_id`, `data`, `timestamp`.

Track `since_id` to get only new mutations. Poll every 1-2 seconds.

## Issue Object Schema

```json
{
  "id": "bd-a1b2",
  "title": "Fix authentication bug",
  "description": "Users cannot login with SSO",
  "status": "open",
  "priority": 1,
  "issue_type": "bug",
  "assignee": "alice",
  "labels": ["backend", "auth"],
  "design": "Design notes",
  "acceptance_criteria": "- Users can login",
  "notes": "Working notes",
  "external_ref": "JIRA-123",
  "estimated_minutes": 120,
  "created_at": "2024-01-15T10:30:00Z",
  "updated_at": "2024-01-16T14:20:00Z",
  "closed_at": null,
  "close_reason": null,
  "parent_id": "bd-x1y2"
}
```

## Concurrency Guarantees

### Hash-Based IDs

Beads uses hash-based IDs (`bd-a1b2`) instead of sequential numbers. Concurrent creates won't collide.

### Daemon Serialization

- Single owner pattern: one goroutine owns all flush state
- Channel communication: no shared mutable state
- 5-second debounce: rapid writes batched automatically
- Git sync: JSONL merges cleanly across machines

**No client-side locking needed** - the daemon handles it.

## CLI Fallback

If daemon unavailable, use CLI with `--json`:

```bash
bd list --json
bd create "Title" -p 1 --json
bd update bd-a1b2 --status in_progress --json
bd close bd-a1b2 --reason "Done" --json
bd ready --json
```

The CLI auto-connects to daemon when available, falls back to direct DB.

## Implementation Examples

### Python

```python
import json
import os
import socket
from pathlib import Path

class BeadsClient:
    def __init__(self):
        self.db_path = self._find_database()
        self.socket_path = os.path.join(os.path.dirname(self.db_path), "bd.sock")

    def _find_database(self) -> str:
        if 'BEADS_DB' in os.environ:
            return os.path.abspath(os.environ['BEADS_DB'])

        current = Path.cwd()
        while current != current.parent:
            db_path = current / '.beads' / 'beads.db'
            if db_path.exists():
                return str(db_path.absolute())
            current = current.parent

        raise FileNotFoundError("No beads database found")

    def request(self, operation: str, args=None):
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.settimeout(30)
        sock.connect(self.socket_path)

        try:
            request = {
                'operation': operation,
                'args': args,
                'cwd': os.getcwd(),
                'client_version': '0.21.0',
                'expected_db': self.db_path
            }
            sock.sendall((json.dumps(request) + '\n').encode())

            response_data = b''
            while b'\n' not in response_data:
                response_data += sock.recv(4096)

            return json.loads(response_data.decode().strip())
        finally:
            sock.close()

    def list_issues(self, status=None, priority=None):
        args = {}
        if status: args['status'] = status
        if priority is not None: args['priority'] = priority

        response = self.request('list', args)
        if not response['success']:
            raise Exception(response['error'])
        return response['data']

    def create_issue(self, title, description="", priority=2, issue_type="task"):
        args = {
            'title': title,
            'description': description,
            'priority': priority,
            'issue_type': issue_type
        }
        response = self.request('create', args)
        if not response['success']:
            raise Exception(response['error'])
        return response['data']

    def close_issue(self, id, reason="Completed"):
        response = self.request('close', {'id': id, 'reason': reason})
        if not response['success']:
            raise Exception(response['error'])
        return response['data']
```

### Emacs Lisp

```elisp
(require 'json)

(defun beads--find-database ()
  (or (getenv "BEADS_DB")
      (let ((dir default-directory))
        (while (and dir (not (file-exists-p (expand-file-name ".beads/beads.db" dir))))
          (setq dir (file-name-directory (directory-file-name dir))))
        (when dir
          (expand-file-name ".beads/beads.db" dir)))))

(defun beads--socket-path ()
  (let ((db (beads--find-database)))
    (when db
      (expand-file-name "bd.sock" (file-name-directory db)))))

(defun beads--request (operation args)
  (let* ((socket (beads--socket-path))
         (request (json-encode
                   `((operation . ,operation)
                     (args . ,args)
                     (cwd . ,default-directory)))))
    (with-temp-buffer
      (let ((proc (make-network-process
                   :name "beads"
                   :buffer (current-buffer)
                   :family 'local
                   :service socket)))
        (process-send-string proc (concat request "\n"))
        (accept-process-output proc 5)
        (goto-char (point-min))
        (json-read)))))

(defun beads-list-issues (&optional status)
  (interactive)
  (let* ((args (when status `((status . ,status))))
         (response (beads--request "list" args)))
    (if (eq (alist-get 'success response) t)
        (alist-get 'data response)
      (error "Failed: %s" (alist-get 'error response)))))

(defun beads-create-issue (title)
  (interactive "sTitle: ")
  (let* ((args `((title . ,title) (priority . 2) (issue_type . "task")))
         (response (beads--request "create" args)))
    (if (eq (alist-get 'success response) t)
        (message "Created: %s" (alist-get 'id (alist-get 'data response)))
      (error "Failed: %s" (alist-get 'error response)))))
```

### Swift

```swift
import Foundation

class BeadsClient {
    private let socketPath: String
    private let dbPath: String

    init() throws {
        guard let db = Self.findDatabase() else {
            throw BeadsError.noDatabaseFound
        }
        self.dbPath = db
        self.socketPath = (db as NSString).deletingLastPathComponent + "/bd.sock"
    }

    static func findDatabase() -> String? {
        if let beadsDB = ProcessInfo.processInfo.environment["BEADS_DB"] {
            return beadsDB
        }

        var url = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
        while url.path != "/" {
            let dbPath = url.appendingPathComponent(".beads/beads.db").path
            if FileManager.default.fileExists(atPath: dbPath) {
                return dbPath
            }
            url = url.deletingLastPathComponent()
        }
        return nil
    }

    func request(_ operation: String, args: [String: Any]? = nil) throws -> [String: Any] {
        let fd = socket(AF_UNIX, SOCK_STREAM, 0)
        guard fd >= 0 else { throw BeadsError.connectionFailed }
        defer { close(fd) }

        var addr = sockaddr_un()
        addr.sun_family = sa_family_t(AF_UNIX)
        socketPath.withCString { ptr in
            withUnsafeMutablePointer(to: &addr.sun_path.0) { dest in
                _ = strcpy(dest, ptr)
            }
        }

        let connectResult = withUnsafePointer(to: &addr) { ptr in
            ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sockaddrPtr in
                Darwin.connect(fd, sockaddrPtr, socklen_t(MemoryLayout<sockaddr_un>.size))
            }
        }
        guard connectResult == 0 else { throw BeadsError.connectionFailed }

        var request: [String: Any] = [
            "operation": operation,
            "cwd": FileManager.default.currentDirectoryPath,
            "expected_db": dbPath
        ]
        if let args = args { request["args"] = args }

        let jsonData = try JSONSerialization.data(withJSONObject: request)
        var requestString = String(data: jsonData, encoding: .utf8)! + "\n"
        requestString.withCString { ptr in _ = write(fd, ptr, strlen(ptr)) }

        var buffer = [CChar](repeating: 0, count: 65536)
        let bytesRead = read(fd, &buffer, buffer.count - 1)
        guard bytesRead > 0 else { throw BeadsError.readFailed }

        let responseString = String(cString: buffer)
        guard let responseData = responseString.data(using: .utf8),
              let response = try JSONSerialization.jsonObject(with: responseData) as? [String: Any] else {
            throw BeadsError.invalidResponse
        }
        return response
    }
}

enum BeadsError: Error {
    case noDatabaseFound, connectionFailed, readFailed, invalidResponse
}
```

### Shell (for testing)

```bash
#!/bin/bash

find_beads_db() {
    if [[ -n "$BEADS_DB" ]]; then echo "$BEADS_DB"; return; fi

    local dir="$PWD"
    while [[ "$dir" != "/" ]]; do
        if [[ -f "$dir/.beads/beads.db" ]]; then
            echo "$dir/.beads/beads.db"
            return
        fi
        dir="$(dirname "$dir")"
    done
}

beads_rpc() {
    local operation="$1" args="$2"
    local db_path=$(find_beads_db)
    local socket_path="$(dirname "$db_path")/bd.sock"

    jq -n --arg op "$operation" --arg cwd "$PWD" --arg db "$db_path" \
        --argjson args "${args:-null}" \
        '{operation: $op, args: $args, cwd: $cwd, expected_db: $db}' | \
    nc -U "$socket_path"
}

# Examples:
# beads_rpc "list" '{"status": "open"}'
# beads_rpc "create" '{"title": "New task", "priority": 2}'
# beads_rpc "ready" 'null'
```

## Error Handling

Common errors:

- `"issue not found: bd-xxxx"` - Invalid issue ID
- `"daemon unhealthy"` - Daemon needs restart
- `"database locked"` - Another process has exclusive lock
- `"dependency cycle detected"` - Circular dependency

Retry strategy: exponential backoff for connection errors, no retry for validation errors.

## Summary

1. Auto-discover database and socket path
2. Connect to daemon via Unix socket
3. Send JSON-newline requests
4. Parse JSON-newline responses
5. Check `success` field, handle errors

The daemon handles all concurrency. Your client just sends requests and processes responses.
