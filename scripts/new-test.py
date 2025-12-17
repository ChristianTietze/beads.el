#!/usr/bin/env python3
"""
Create a new ERT test file with boilerplate.
"""
import sys
from pathlib import Path


def create_test_file(feature_name):
    """Create a new test file for the given feature."""
    root_dir = Path(__file__).parent.parent
    test_dir = root_dir / "test"
    test_dir.mkdir(exist_ok=True)

    test_file = test_dir / f"{feature_name}-test.el"

    if test_file.exists():
        print(f"Error: {test_file} already exists")
        return 1

    content = f''';;; {feature_name}-test.el --- Tests for {feature_name} -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for {feature_name}.el

;;; Code:

(require 'ert)
(require '{feature_name})

(ert-deftest {feature_name}-test-example ()
  "Example test for {feature_name}."
  (should t))

(provide '{feature_name}-test)
;;; {feature_name}-test.el ends here
'''

    test_file.write_text(content)
    print(f"Created {test_file.relative_to(root_dir)}")
    print()
    print("Next steps:")
    print(f"1. Edit {test_file.relative_to(root_dir)}")
    print("2. Add your test cases")
    print("3. Run: mise run test")

    return 0


def main():
    if len(sys.argv) != 2:
        print("Usage: new-test.py FEATURE_NAME")
        print()
        print("Example:")
        print("  new-test.py beads-socket")
        print()
        print("This will create test/beads-socket-test.el")
        return 1

    feature_name = sys.argv[1].replace(".el", "").replace("-test", "")
    return create_test_file(feature_name)


if __name__ == "__main__":
    sys.exit(main())
