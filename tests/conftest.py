"""
Shared pytest configuration for the tests/ tree.

Lives at tests/ (rather than tests/extra/pytest/) so that
`pytest_report_header` fires regardless of whether the caller runs
`pytest tests`, `pytest tests/extra/pytest`, or a single test file.
pytest loads conftest.py files eagerly along the ancestor chain from
each argument path down to rootdir; a conftest below the given path is
loaded lazily during collection, which is too late for the header.
"""

from __future__ import annotations

import platform
import shutil
import subprocess
from functools import lru_cache

import pytest


# --- git-annex version helpers -----------------------------------------------


@lru_cache(maxsize=1)
def _git_annex_version_output() -> str | None:
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
    out = _git_annex_version_output()
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


def _git_annex_summary() -> list[str]:
    """Critical fields from `git annex version` (no --json upstream)."""
    out = _git_annex_version_output()
    if out is None:
        return ["git-annex: NOT INSTALLED"]
    wanted = (
        "git-annex version",
        "build flags",
        "dependency versions",
        "operating system",
        "supported repository versions",
    )
    lines = []
    for line in out.splitlines():
        key = line.split(":", 1)[0].strip().lower()
        if key in wanted:
            lines.append(f"  {line.strip()}")
    return lines


def _first_line(cmd: list[str]) -> str | None:
    if shutil.which(cmd[0]) is None:
        return None
    try:
        out = subprocess.run(
            cmd, capture_output=True, text=True, timeout=15, check=True,
        ).stdout
    except (subprocess.SubprocessError, OSError):
        return None
    return out.splitlines()[0].strip() if out.strip() else None


# Fix version for the URL-encoded-key bug (upstream 8fd9b67ed8,
# 2026-02-16; first shipped in release 10.20260420).  Kept here as a
# single source of truth so tests and the header agree.
URL_BACKEND_FIX_VERSION = "10.20260420"


def pytest_report_header(config: pytest.Config) -> list[str]:
    """Version + tool info at the top of pytest's session banner."""
    tools = [
        ("git",        ["git", "--version"]),
        ("yt-dlp",     ["yt-dlp", "--version"]),
        ("youtube-dl", ["youtube-dl", "--version"]),
        ("strace",     ["strace", "--version"]),
    ]
    tool_versions = []
    for name, cmd in tools:
        first = _first_line(cmd)
        tool_versions.append(f"{name}={first if first is not None else '(missing)'}")

    installed = git_annex_version()
    if installed is None:
        since_line = "git-annex releases newer than installed: (git-annex not installed)"
    else:
        n_since = git_annex_releases_since(installed)
        if n_since is None:
            since_line = (
                f"git-annex releases in this repo newer than installed "
                f"({installed}): (no tags found)"
            )
        else:
            since_line = (
                f"git-annex releases in this repo newer than installed "
                f"({installed}): {n_since}"
            )

    header = [
        "extra-tests tools: " + ", ".join(tool_versions),
        f"platform: {platform.platform()}",
        "git-annex:",
        *_git_annex_summary(),
        since_line,
    ]
    return header
