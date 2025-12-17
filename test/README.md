# Test Directory

This directory contains ERT (Emacs Lisp Regression Testing) tests for beads.el.

## Running Tests

```bash
mise run test
```

## Writing Tests

Test files should:
- End with `-test.el` suffix
- Use `(require 'ert)` at the top
- Define tests using `ert-deftest`

Example:

```elisp
(require 'ert)

(ert-deftest beads-feature-test ()
  "Test that feature works correctly."
  (should (equal (some-function) expected-result)))
```

## ERT Assertions

- `(should CONDITION)` - Assert that condition is true
- `(should-not CONDITION)` - Assert that condition is false
- `(should-error BODY)` - Assert that body signals an error
- `(should-error BODY :type 'error-type)` - Assert specific error type

## Test Organization

- Unit tests: Test individual functions in isolation
- Integration tests: Test component interactions
- Socket tests: Test daemon communication (mock the socket)

See `example-test.el` for basic examples.
