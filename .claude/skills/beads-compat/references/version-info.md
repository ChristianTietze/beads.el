# Beads Version Compatibility

Tested CLI version: 0.44.0
Minimum DB version: 0.35.0

## Changelog

https://github.com/steveyegge/beads/blob/main/CHANGELOG.md

## Version History

| beads.el | beads CLI | Notes |
|----------|-----------|-------|
| 0.44.0   | 0.44.0    | Initial version tracking |

## Known Breaking Changes

### v0.33.1
- Field rename: `ephemeral` -> `wisp` in JSON

### v0.30.0
- Removed `--resolve-collisions` flag

### v0.21.6
- Default to hash-based IDs

### v0.20.0
- Per-project daemon socket (`.beads/bd.sock` instead of global)
