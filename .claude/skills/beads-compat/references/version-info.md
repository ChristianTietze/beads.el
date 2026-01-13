# Beads Version Compatibility

Tested CLI version: 0.47.1
Minimum DB version: 0.35.0

## Changelog

https://github.com/steveyegge/beads/blob/main/CHANGELOG.md

## Version History

| beads.el | beads CLI | Notes |
|----------|-----------|-------|
| 0.47.1   | 0.47.1    | Pull-first sync, `--ready` flag, dry-run create |
| 0.46.0   | 0.46.0    | Custom types, rig type, hierarchy view, dependencies in detail |
| 0.44.0   | 0.44.0    | Initial version tracking |

## New Features by Version

### v0.47.1
- `bd list --ready` flag to display issues with no blockers
- Markdown rendering support in comments
- Various bug fixes

### v0.47.0
- Pull-first sync with 3-way merge for conflict reconciliation
- `bd resolve-conflicts` command for JSONL merge conflict markers
- `bd create --dry-run` flag to preview issue creation
- `bd ready --gated` flag for gated molecules
- Prevention of closing issues with open blockers
- Daemon CLI refactored to subcommands

### v0.46.0
- Custom issue types (project-configurable beyond built-in types)

### v0.45.0
- New `rig` issue type for Gas Town rig tracking
- `--filter-parent` alias for `--parent` in `bd list`

## Known Breaking Changes

### v0.33.1
- Field rename: `ephemeral` -> `wisp` in JSON

### v0.30.0
- Removed `--resolve-collisions` flag

### v0.21.6
- Default to hash-based IDs

### v0.20.0
- Per-project daemon socket (`.beads/bd.sock` instead of global)
