# Development Guide

This document describes the development workflow for beads.el.

## Prerequisites

- Emacs 30.2+ (installed at `/opt/homebrew/bin/emacs`)
- Python 3 (for build scripts)
- mise (for task running)

## Development Tasks

### Linting

Check syntax, parentheses, and byte-compile warnings:

```bash
mise run lint
```

This will:
- Check for balanced parentheses using `check-parens`
- Byte-compile files to catch warnings and errors
- Report any syntax issues

### Testing

Run ERT (Emacs Lisp Regression Testing) tests:

```bash
mise run test
```

Test files should:
- Be placed in `test/` directory
- End with `-test.el` suffix
- Use `(require 'ert)` and define tests with `ert-deftest`

See `test/README.md` for more details on writing tests.

### Building

Byte-compile all `.el` files:

```bash
mise run build
```

This creates `.elc` files alongside the source files. Compiled files are ignored by git.

### Combined Check

Run both lint and test:

```bash
mise run check
```

## File Structure

```
beads.el/
├── scripts/          # Python build/test scripts
│   ├── lint.py      # Syntax and style checker
│   ├── test.py      # ERT test runner
│   └── build.py     # Byte-compiler
├── test/            # ERT test files (*-test.el)
│   ├── README.md
│   └── example-test.el
├── mise.toml        # Task definitions
└── .gitignore       # Excludes *.elc, etc.
```

## Workflow

1. Write code in `.el` files
2. Write tests in `test/*-test.el`
3. Run `mise run check` to verify
4. Fix any issues reported
5. Commit when all checks pass

## ERT Testing Quick Reference

```elisp
(require 'ert)

(ert-deftest my-feature-test ()
  "Test description."
  (should (equal (my-function) expected-value))
  (should-not (null something))
  (should-error (broken-function) :type 'wrong-type-argument))
```

Run interactively in Emacs:
- `M-x ert RET t RET` - Run all tests
- `M-x ert RET my-feature-test RET` - Run specific test

## Common Issues

### Parenthesis Mismatch

If `mise run lint` reports unbalanced parens:
- Open the file in Emacs
- Run `M-x check-parens` to find the location
- Use `M-x show-paren-mode` for highlighting

### Byte-Compilation Warnings

Common warnings:
- `reference to free variable` - Missing `defvar` or `require`
- `function X not known to be defined` - Missing `require` or `declare-function`
- `obsolete function` - Update to modern API

### Test Failures

If tests fail:
- Run `M-x ert RET t RET` in Emacs for interactive debugging
- Use `edebug-defun` (C-u C-M-x) on test to step through
- Check test expectations match actual behavior

## Adding New Tests

1. Create `test/feature-test.el`
2. Add `(require 'ert)` at top
3. Write tests using `ert-deftest`
4. Run `mise run test` to verify
5. Tests automatically discovered by test runner

## Continuous Integration

The `mise run check` task is designed for CI:
- Exit code 0 on success
- Exit code 1 on failure
- Clear output indicating pass/fail

Example GitHub Actions workflow:

```yaml
- name: Check code
  run: |
    mise install
    mise run check
```
