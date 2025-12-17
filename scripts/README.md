# Scripts Directory

Development automation scripts for beads.el.

## Available Scripts

### lint.py

Check Emacs Lisp syntax and style:
- Validates balanced parentheses using `check-parens`
- Byte-compiles files to catch warnings and errors
- Reports issues with clear output

Run via: `mise run lint`

### test.py

Run ERT (Emacs Lisp Regression Testing) tests:
- Discovers all `*-test.el` files in `test/`
- Loads them in Emacs batch mode
- Runs all tests and reports results

Run via: `mise run test`

### build.py

Byte-compile Emacs Lisp files:
- Compiles all `.el` files (except tests)
- Generates `.elc` bytecode files
- Reports compilation warnings/errors

Run via: `mise run build`

### new-test.py

Create new test file with boilerplate:

```bash
python3 scripts/new-test.py beads-socket
```

This creates `test/beads-socket-test.el` with ERT template.

## Why Python?

These scripts use Python instead of bash for:
- Better readability and maintainability
- Cross-platform compatibility
- Easier error handling
- More structured output formatting

## Direct Usage

Scripts can be run directly:

```bash
./scripts/lint.py
./scripts/test.py
./scripts/build.py
./scripts/new-test.py feature-name
```

Or via mise tasks (preferred):

```bash
mise run lint
mise run test
mise run build
```
