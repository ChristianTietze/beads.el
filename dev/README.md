# Development Environment

This directory contains a minimal Emacs configuration for interactive testing of beads.el.

## Usage

```bash
mise run interactive
```

This launches Emacs with `--init-directory=dev`, which:
- Loads only `dev/init.el` (ignores your personal Emacs config)
- Adds the project root to `load-path`
- Requires `beads-client` and `beads-list` modules

## Testing

Once Emacs starts:

1. **Open the issue list**: `M-x beads-list`
2. **Navigate**: Use standard Emacs movement keys
3. **Refresh**: Press `g` to reload from daemon
4. **Quit**: Press `q` to close the list

## Requirements

- The beads daemon must be running: `bd daemon --status`
- A `.beads/` directory must exist in the project or a parent directory

## Customizing

Edit `dev/init.el` to load additional modules or configure settings for testing.
