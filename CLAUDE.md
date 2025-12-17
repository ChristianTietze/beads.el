# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads)
for issue tracking. Use `bd` commands instead of markdown TODOs.
See AGENTS.md for workflow details.

## Project Overview

This is **beads.el** - an Emacs Lisp client for the Beads issue tracking system. Beads is a Git-backed, AI-native issue tracker that stores data in `.beads/` and communicates with a daemon via Unix socket RPC.

## Architecture

The client communicates with the Beads daemon via JSON-over-Unix-socket:

```
Emacs (beads.el) → Unix Socket (.beads/bd.sock) → Beads Daemon → SQLite DB
```

Key concepts:
- **Auto-discovery**: Walk up from `default-directory` looking for `.beads/beads.db`
- **Socket path**: Always `bd.sock` in same directory as `beads.db`
- **Wire format**: JSON + newline delimiter, request/response pattern
- **No client-side locking**: Daemon handles all concurrency

## RPC Protocol Reference

See `docs/beads-client-howto.md` for the complete protocol specification including:
- Request/response structures
- All operations (list, show, ready, create, update, close, dep_add, etc.)
- Issue object schema
- Example implementations in multiple languages including Emacs Lisp

## Development

**Tools**: Go, Python (managed by mise, see `mise.toml`)

**Quality checks** (run before committing):
```bash
mise run check   # Run lint + test
mise run lint    # Check syntax/parens, byte-compile
mise run test    # Run ERT tests
mise run build   # Byte-compile all .el files
```

**Testing the daemon connection**:
```bash
bd daemon --status
```

**CLI fallback** (when daemon unavailable):
```bash
bd list --json
bd ready --json
bd create "Title" --json
```

## Issue Tracking

This project uses **bd (beads)** for issue tracking. Do NOT use markdown TODOs.

```bash
bd ready              # Find available work
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Session Completion

Work is NOT complete until `git push` succeeds:
```bash
git pull --rebase && bd sync && git push
```
