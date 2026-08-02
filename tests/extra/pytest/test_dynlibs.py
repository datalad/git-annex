"""
Guard against regressions in git-annex's dynamic-library lookup behaviour.

Older git-annex builds probed hundreds of directories for libpcre before
finding it, causing measurable startup slowdowns on some filesystems.
The check is a strace over `git-annex version` / `git-annex init`,
counting ENOENT lookups whose path matches `libpcre.*so`, and asserting
the count stays below a known-reasonable ceiling.

Linux-only: strace has no cross-platform equivalent that is trivial to
substitute here.
"""

from __future__ import annotations

import re
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

pytestmark = pytest.mark.skipif(
    not sys.platform.startswith("linux") or shutil.which("strace") is None,
    reason="strace is Linux-only",
)


def _count_enoent(subcommand: str, pattern: str, cwd: Path) -> int:
    """Return the number of ENOENT lines matching `pattern` under strace."""
    result = subprocess.run(
        ["strace", "-f", "git-annex", subcommand],
        cwd=cwd,
        capture_output=True,
        text=True,
    )
    regex = re.compile(rf"{pattern}.*ENOENT")
    matches = [line for line in result.stderr.splitlines() if regex.search(line)]
    for m in matches:
        print(m, file=sys.stderr)
    return len(matches)


def test_libpcre_lookups_on_version(tmp_path: Path) -> None:
    subprocess.run(["git", "init", "-q"], cwd=tmp_path, check=True)
    n = _count_enoent("version", r"libpcre.*so", tmp_path)
    assert n < 7, f"too many libpcre ENOENT lookups on `git-annex version`: {n}"


def test_libpcre_lookups_on_init(tmp_path: Path) -> None:
    subprocess.run(["git", "init", "-q"], cwd=tmp_path, check=True)
    n = _count_enoent("init", r"libpcre.*so", tmp_path)
    assert n < 260, f"too many libpcre ENOENT lookups on `git-annex init`: {n}"
