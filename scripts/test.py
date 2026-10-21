#!/usr/bin/env python3
"""
Run ERT (Emacs Lisp Regression Testing) tests.
"""
import argparse
import subprocess
import sys
from pathlib import Path


def find_test_files(test_dir):
    """Find all test files in the test directory."""
    test_path = Path(test_dir)
    if not test_path.exists():
        return []
    return sorted(test_path.rglob("*-test.el"))


def run_ert_tests(test_files, integration=False):
    """Run ERT tests using Emacs batch mode."""
    if not test_files:
        print("No test files found")
        return 0

    root_dir = Path(__file__).parent.parent

    load_dirs = sorted({
        str(el_file.parent)
        for el_file in root_dir.rglob("*.el")
        if not any(part.startswith('.') for part in el_file.relative_to(root_dir).parts)
        and not el_file.name.endswith("-test.el")
    })
    load_path_args = [arg for directory in load_dirs for arg in ("-L", directory)]
    selector = "(tag :cli)" if integration else "(not (tag :integration))"

    elisp_code = f"""
(progn
(setq load-prefer-newer t)
(require 'ert)
(require 'beads)

;; Load test files
{chr(10).join(f'(load-file "{test_file}")' for test_file in test_files)}

;; Run all tests
(ert-run-tests-batch-and-exit '{selector}))
"""

    cmd = ["emacs", "-Q", "--batch"] + load_path_args + ["--eval", elisp_code]

    print(f"Running {len(test_files)} test file(s)...")
    print()

    result = subprocess.run(cmd, capture_output=True, text=True)

    output = result.stderr + result.stdout
    print(output)

    if result.returncode == 0:
        print()
        print("✓ All tests passed")
        return 0
    else:
        print()
        print("✗ Some tests failed")
        return 1


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--integration", action="store_true",
                        help="Run isolated CLI integration tests instead of unit tests")
    args = parser.parse_args()
    root_dir = Path(__file__).parent.parent
    test_dir = root_dir / "test"

    test_files = find_test_files(test_dir)

    return run_ert_tests(test_files, args.integration)


if __name__ == "__main__":
    sys.exit(main())
