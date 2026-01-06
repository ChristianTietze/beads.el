#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VERSION_FILE="$SCRIPT_DIR/../references/version-info.md"

if ! command -v bd &> /dev/null; then
    echo "ERROR: bd command not found"
    exit 1
fi

installed=$(bd --version | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -1)
documented=$(grep -oE 'Tested CLI version: [0-9]+\.[0-9]+\.[0-9]+' "$VERSION_FILE" | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')

echo "Installed beads: $installed"
echo "Documented:      $documented"

if [[ "$installed" == "$documented" ]]; then
    echo "OK: Versions match"
elif [[ "$installed" > "$documented" ]]; then
    echo "NOTICE: Installed version is newer. Review changelog and test before updating documentation."
    echo "Changelog: https://github.com/steveyegge/beads/blob/main/CHANGELOG.md"
else
    echo "WARNING: Installed version is older than documented compatible version."
fi
