#!/usr/bin/env python3

from __future__ import annotations

import re
import sys
from pathlib import Path


def rewrite(path: Path, pattern: str, replacement: str, *, count: int = 1) -> None:
    content = path.read_text()
    updated, matches = re.subn(pattern, replacement, content, count=count, flags=re.MULTILINE)
    if matches == 0:
        raise SystemExit(f"failed to stamp version in {path}")
    path.write_text(updated)


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: stamp-release-version.py <version>")

    version = sys.argv[1]
    root = Path(__file__).resolve().parents[1]

    rewrite(root / "flake.nix", r'version = "[^"]+";', f'version = "{version}";')
    rewrite(root / "compositor" / "Cargo.toml", r'^version = "[^"]+"$', f'version = "{version}"')
    rewrite(
        root / "compositor" / "Cargo.lock",
        r'(name = "ewwm-compositor"\nversion = ")[^"]+(")',
        rf'\g<1>{version}\2',
    )


if __name__ == "__main__":
    main()
