"""
Shared helpers for the tests/ tree.

Kept as a plain module (not a conftest.py) so both the pytest report
hook in tests/conftest.py and individual test modules can import via
`from _helpers import ...` without relying on pytest's conftest
import-name magic (which broke under `--import-mode=importlib` and
similar).

pytest puts tests/ on sys.path via the conftest.py at that level, so
`from _helpers import ...` resolves for both callers.
"""

from __future__ import annotations

import shutil
import subprocess
from functools import lru_cache


# Fix version for the URL-encoded-key bug (upstream 8fd9b67ed8,
# 2026-02-16; first shipped in release 10.20260420).  Single source of
# truth for both the xfail marker in test_url_backend.py and the
# version reporting in tests/conftest.py.
URL_BACKEND_FIX_VERSION = "10.20260420"


@lru_cache(maxsize=1)
def git_annex_version_output() -> str | None:
    """Raw stdout of `git annex version`, cached for the session."""
    if shutil.which("git-annex") is None:
        return None
    try:
        return subprocess.run(
            ["git", "annex", "version"],
            capture_output=True, text=True, timeout=15, check=True,
        ).stdout
    except (subprocess.SubprocessError, OSError):
        return None


def git_annex_version() -> str | None:
    """
    Bare version string reported by git-annex, e.g. "10.20260421"
    (`-g<sha>` build suffix stripped).  None if git-annex is not
    installed / not runnable.
    """
    out = git_annex_version_output()
    if not out:
        return None
    for line in out.splitlines():
        if line.startswith("git-annex version:"):
            return line.split(":", 1)[1].strip().split("-", 1)[0]
    return None


def _version_key(s: str) -> tuple[int, ...]:
    return tuple(int(p) for p in s.split(".") if p.isdigit())


def git_annex_version_below(threshold: str) -> bool:
    """
    True if the installed git-annex version is *strictly* below `threshold`.
    Compares as tuples of ints on the "." separator ("10.20220615" <
    "10.20260421").  False if git-annex is missing.
    """
    v = git_annex_version()
    if v is None:
        return False
    return _version_key(v) < _version_key(threshold)


@lru_cache(maxsize=32)
def git_annex_releases_since(threshold: str) -> int | None:
    """
    Count git-annex release tags in the current repository that are
    strictly newer than `threshold`.  Returns None if not inside a git
    repo or if there are no matching tags (e.g. a shallow checkout).
    """
    if shutil.which("git") is None:
        return None
    try:
        out = subprocess.run(
            ["git", "tag", "--list", "10.*"],
            capture_output=True, text=True, timeout=10, check=True,
        ).stdout
    except (subprocess.SubprocessError, OSError):
        return None
    key = _version_key(threshold)
    n = 0
    for tag in out.splitlines():
        tag = tag.strip()
        if not tag:
            continue
        try:
            if _version_key(tag) > key:
                n += 1
        except ValueError:
            continue
    return n if n or out.strip() else None
