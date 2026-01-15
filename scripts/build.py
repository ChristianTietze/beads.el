#!/usr/bin/env python3
"""
Byte-compile all Emacs Lisp files in the project.
"""
import subprocess
import sys
from pathlib import Path


def find_el_files(root_dir):
    """Find all .el files in the project, excluding test and vendor files."""
    root = Path(root_dir)
    return [
        f for f in root.rglob("*.el")
        if not any(part.startswith('.') for part in f.parts)
        and not f.name.endswith("-test.el")
        and 'vendor' not in f.parts
    ]


def find_vendor_paths(root_dir):
    """Find all vendor package directories."""
    vendor_dir = root_dir / "vendor"
    if not vendor_dir.exists():
        return []
    paths = []
    for item in vendor_dir.iterdir():
        if item.is_dir():
            paths.append(str(item))
    return paths


def byte_compile_files(el_files):
    """Byte-compile all .el files."""
    if not el_files:
        print("No .el files to compile")
        return 0

    print(f"Byte-compiling {len(el_files)} file(s)...")
    print()

    root_dir = Path(__file__).parent.parent

    load_path_args = []
    for el_file in el_files:
        load_path_args.extend(["-L", str(el_file.parent)])

    for vendor_path in find_vendor_paths(root_dir):
        load_path_args.extend(["-L", vendor_path])

    load_path_args = list(dict.fromkeys(load_path_args))

    all_passed = True

    for el_file in sorted(el_files):
        rel_path = el_file.relative_to(root_dir)
        print(f"Compiling {rel_path}...")

        result = subprocess.run(
            ["emacs", "-Q", "--batch"] + load_path_args +
            ["-f", "batch-byte-compile", str(el_file)],
            capture_output=True,
            text=True
        )

        if result.returncode != 0:
            print(f"  ✗ Failed")
            print(result.stderr + result.stdout)
            all_passed = False
        else:
            output = result.stderr + result.stdout
            if "Warning:" in output:
                print(f"  ⚠ Compiled with warnings")
                print(output)
            else:
                print(f"  ✓ Compiled")

        elc_file = el_file.with_suffix(".elc")
        if elc_file.exists():
            print(f"    → {elc_file.relative_to(root_dir)}")

        print()

    if all_passed:
        print("✓ Build completed successfully")
        return 0
    else:
        print("✗ Build failed")
        return 1


def main():
    root_dir = Path(__file__).parent.parent
    el_files = find_el_files(root_dir)

    return byte_compile_files(el_files)


if __name__ == "__main__":
    sys.exit(main())
