---
name: elisp-unused-vars
description: Find unused defvar/defvar-local/defcustom variables in Emacs Lisp projects. Use when cleaning up elisp code, reviewing for dead code, or when asked to find unused variables in .el files.
---

# Emacs Lisp Unused Variable Finder

Find `defvar`, `defvar-local`, and `defcustom` declarations that are defined but never referenced elsewhere.

## Usage

Launch an Explore subagent to analyze the codebase:

```
Task(subagent_type="Explore", prompt="""
Find unused defvar/defvar-local/defcustom variables in the elisp files.

For each variable definition found:
1. Search for references to that variable name across all .el files
2. A variable is "unused" if it only appears in its own definition

Report format:
| File | Variable | Type | Status |
|------|----------|------|--------|
| path | name | defvar/defvar-local/defcustom | unused/used |

Focus on: <target files or directories>
""")
```

## Detection Logic

A variable is **unused** if:
- Defined with `defvar`, `defvar-local`, or `defcustom`
- Only occurrence in codebase is the definition itself
- Not referenced in any other expression

A variable is **effectively dead** if:
- Defined and checked (e.g., in a `when` clause)
- But never set/assigned anywhere
- Common pattern: callback variables that were never wired up

## Common Patterns

**Vestigial callbacks**: Variables like `foo--save-callback` that are checked but never set—indicates abandoned refactor.

**Orphaned state**: Buffer-local variables from removed features.

**Unused customization**: `defcustom` options for features that were removed.

## Example Output

```
Found 3 potentially unused variables:

1. beads-types--original-custom (defvar-local)
   - lisp/beads-types.el:52 - definition only
   - No other references found
   - Recommendation: REMOVE

2. beads-types--save-callback (defvar-local)
   - lisp/beads-types.el:55 - definition
   - lisp/beads-types.el:207 - checked but never set
   - Recommendation: REMOVE (dead code path)

3. beads-types--cancel-callback (defvar-local)
   - lisp/beads-types.el:58 - definition only
   - No other references found
   - Recommendation: REMOVE
```
