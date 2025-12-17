# beads.el - Emacs Client for Beads Issue Tracker

[![Codeberg Repository](https://img.shields.io/badge/Codeberg-Repository-2185D0?logo=codeberg&logoColor=white)](https://codeberg.org/ctietze/beads.el)

## Overview

beads.el provides an Emacs interface to the [Beads](https://github.com/steveyegge/beads) issue tracking system.
Beads is a lightweight, Git-backed issue tracker that stores data locally in
`.beads/` alongside your code. It works well with AI coding assistants but
doesn't require them—you can use it entirely from Emacs or the command line.

## Screenshots

*(Screenshots use [modus-themes](https://protesilaos.com/emacs/modus-themes))*

### Issue List

Browse issues in a sortable, filterable table. Press `P` to toggle preview mode.

![Issue List](screenshots/beads-list.png)

### Preview Mode

Navigate issues with live preview in a side window:

![Preview Mode](screenshots/beads-preview-mode-demo.gif)

([View full quality video](screenshots/beads-preview-mode-demo.mp4))

### Issue Details

View full issue details including description, design notes, and metadata:

![Issue Details](screenshots/beads-details.png)

### Editing

Edit long-form fields (description, design, notes) in dedicated buffers with
`markdown-mode` support:

![Edit Description](screenshots/beads-edit-description.png)

Or use the form editor (`E`) to edit all fields at once:

![Form Editor](screenshots/beads-edit-widgets.png)

## Installation

```elisp
(add-to-list 'load-path "/path/to/beads.el/lisp")
(require 'beads)
```

## Usage

- `M-x beads` or `M-x beads-list` - Open the issue list
- `M-x beads-project-list` - Open issue list for current project

## Keybindings

### List Mode (`beads-list-mode`)

| Key | Command | Description |
|-----|---------|-------------|
| `RET` | `beads-list-goto-issue` | Open issue in detail view |
| `E` | `beads-list-edit-form` | Open form editor for issue |
| `e t` | `beads-list-edit-title` | Edit title |
| `e s` | `beads-list-edit-status` | Edit status |
| `e p` | `beads-list-edit-priority` | Edit priority |
| `e T` | `beads-list-edit-type` | Edit type |
| `e d` | `beads-list-edit-description` | Edit description |
| `f s` | `beads-filter-status` | Filter by status |
| `f p` | `beads-filter-priority` | Filter by priority |
| `P` | `beads-preview-mode` | Toggle preview mode |
| `D` | `beads-delete-issue` | Delete issue (with confirmation) |
| `R` | `beads-reopen-issue` | Reopen closed issue |
| `g` | `beads-list-refresh` | Refresh issue list |
| `?` | `beads-menu` | Show transient menu |
| `q` | `beads-list-quit` | Quit |

### Detail Mode (`beads-detail-mode`)

| Key | Command | Description |
|-----|---------|-------------|
| `E` | `beads-detail-edit-form` | Open form editor |
| `e d` | `beads-detail-edit-description` | Edit description |
| `e D` | `beads-detail-edit-design` | Edit design notes |
| `e a` | `beads-detail-edit-acceptance` | Edit acceptance criteria |
| `e n` | `beads-detail-edit-notes` | Edit notes |
| `e t` | `beads-detail-edit-title` | Edit title |
| `e s` | `beads-detail-edit-status` | Edit status |
| `e p` | `beads-detail-edit-priority` | Edit priority |
| `e T` | `beads-detail-edit-type` | Edit type |
| `e A` | `beads-detail-edit-assignee` | Edit assignee |
| `e x` | `beads-detail-edit-external-ref` | Edit external reference |
| `e l a` | `beads-detail-edit-label-add` | Add label |
| `e l r` | `beads-detail-edit-label-remove` | Remove label |
| `D` | `beads-delete-issue` | Delete issue (with confirmation) |
| `R` | `beads-reopen-issue` | Reopen closed issue |
| `g` | `beads-detail-refresh` | Refresh detail view |
| `?` | `beads-menu` | Show transient menu |
| `q` | `quit-window` | Quit |

### Form Editor (`beads-form-mode`)

| Key | Command | Description |
|-----|---------|-------------|
| `TAB` | `widget-forward` | Next field |
| `S-TAB` | `widget-backward` | Previous field |
| `C-c C-c` | `beads-form-commit` | Save all changes |
| `C-c C-k` | `beads-form-cancel` | Discard and close |

### Edit Buffer (`beads-edit-mode`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-c` | `beads-edit-commit` | Save changes |
| `C-c C-k` | `beads-edit-abort` | Discard changes |

## Customization

### General

```elisp
;; Disable keybinding hints in minibuffer
(setq beads-verbose nil)
```

### Auto-Refresh (`beads-autoupdate`)

```elisp
;; Disable auto-refresh entirely
(setq beads-autoupdate-enable nil)

;; Change refresh interval (default: 30 seconds)
(setq beads-autoupdate-interval 60)

;; Disable auto-refresh per-project via .dir-locals.el:
((beads-list-mode . ((beads-autoupdate-message . nil))))
```

| Variable | Default | Description |
|----------|---------|-------------|
| `beads-autoupdate-enable` | `t` | Enable auto-refresh in list mode |
| `beads-autoupdate-interval` | `30` | Seconds between refreshes (nil=disabled) |
| `beads-autoupdate-message` | `t` | Show message on auto-refresh (dir-local) |

### Project Integration (`beads-project`)

```elisp
;; Disable per-project buffers (use single global buffer)
(setq beads-project-per-project-buffers nil)

;; Disable adding to project-switch-commands
(setq beads-project-add-switch-command nil)

;; Custom buffer name function
(setq beads-project-buffer-name-function
      (lambda (root)
        (format "*Issues: %s*" (beads-project-name root))))
```

| Variable | Default | Description |
|----------|---------|-------------|
| `beads-project-per-project-buffers` | `t` | Create separate buffer per project |
| `beads-project-add-switch-command` | `t` | Add to project-switch-commands |
| `beads-project-buffer-name-function` | `beads-project-default-buffer-name` | Function to generate buffer name |

## Requirements

- Emacs 28.1+
- Running Beads daemon (`bd daemon`)
- `transient` package (for menus)
- `markdown-mode` (optional, for editing long text fields)

## Development

This project uses [Beads](https://github.com/steveyegge/beads) itself for issue tracking.
You can check out the repository and view the project's issues:

```bash
# Start the daemon
bd daemon

# Launch Emacs with beads.el loaded
mise run interactive
```

Then run `M-x beads-list` to see the beads.el development issues.
