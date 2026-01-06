---
name: beads-compat
description: Check and update beads version compatibility for beads.el. Use when upgrading beads, checking if installed beads version is compatible, or updating version documentation after testing with a new beads release.
---

# Beads Compatibility

This skill helps track beads CLI version compatibility for beads.el.

## Version Checking

Run `scripts/check-version.sh` to compare installed beads version against documented compatible version.

## Upgrade Workflow

When upgrading beads:

1. Run `bd --version` to get new version
2. Review changelog for breaking changes: https://github.com/steveyegge/beads/blob/main/CHANGELOG.md
3. Test beads.el functionality against new version
4. Update version in `references/version-info.md`
5. Update version in `README.md` and `AGENTS.md`
6. Create git tag matching beads version (e.g., `v0.44.0`)

## Breaking Change Patterns

Watch for these in the changelog:
- RPC protocol changes (affects socket communication)
- Field renames in JSON responses
- Command/flag deprecations
- New required fields in requests

## Files to Update

When bumping version:
- `references/version-info.md` - source of truth
- `README.md` - Requirements section
- `AGENTS.md` - compatibility header
