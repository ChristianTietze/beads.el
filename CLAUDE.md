# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads)
for issue tracking. Use `bd` commands instead of markdown TODOs.
See AGENTS.md for workflow details.

## Project Overview

This is **beads.el** - an Emacs Lisp client for the Beads issue tracking system. Beads is a Git-backed, AI-native issue tracker that stores data in `.beads/` and communicates with a daemon via Unix socket RPC.

The canonical upstream is https://codeberg.org/ctietze/beads.el

## Beads Version Compatibility

Tested with **beads CLI 0.44.0**. Version info maintained in `.claude/skills/beads-compat/references/version-info.md`.

- Changelog: https://github.com/steveyegge/beads/blob/main/CHANGELOG.md
- Run `/beads-compat` to check installed version
- beads.el versioning mirrors beads CLI version (e.g., beads.el 0.44.0 = tested with beads 0.44.0)

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

**Interactive testing**:
```bash
mise run interactive   # Launch Emacs with beads.el loaded
```
This starts Emacs with `--init-directory=dev`, loading dev/init.el which sets up load-path and requires beads modules. Run `M-x beads-list` to test.

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

## Commit Strategy

**Atomic commits as you go** - Create logical commits during development, not after:

1. **Tests must pass** - Never commit breaking changes. Run `mise run check` before every commit.
2. **Fix code, not tests** - If tests fail, fix the implementation first. Only modify tests if they are genuinely wrong.
3. **Commit at logical points**:
   - When a beads task is complete
   - When a meaningful milestone is reached during an in-progress task
   - After fixing a bug or completing a feature unit
4. **No reconstructed history** - Don't batch changes then create artificial commits from a working state. Commits must represent actual development order so checking out any commit yields a working state.
5. **Branches and rollbacks are fine** - Use feature branches, rollback broken changes, experiment freely.

## Documentation

User-facing feature changes must be documented in README.md:
- Add new commands to the Usage section
- Add keybinding tables for new modes
- Add customization options with examples

For visual changes (new UI, modified display):
1. Create a beads task to capture an appropriate screenshot
2. Add an HTML comment in README.md where the screenshot should go:
   ```markdown
   <!-- TODO: Add screenshot for X (see bdel-xxx) -->
   ```

## Session Completion

Work is NOT complete until `git push` succeeds:
```bash
git pull --rebase && bd sync && git push
```
