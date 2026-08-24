"""
Shared pytest configuration for the tests/ tree.

Lives at tests/ (rather than tests/extra/pytest/) so that
`pytest_report_header` fires regardless of whether the caller runs
`pytest tests`, `pytest tests/extra/pytest`, or a single test file.
pytest loads conftest.py files eagerly along the ancestor chain from
each argument path down to rootdir; a conftest below the given path is
loaded lazily during collection, which is too late for the header.

Version / release helpers live in tests/_helpers.py so they are
importable both from here and from individual test modules without
relying on pytest's conftest-import magic.
"""

from __future__ import annotations

import platform
import shutil
import subprocess

import pytest

from _helpers import (
    git_annex_releases_since,
    git_annex_version,
    git_annex_version_output,
)


def _git_annex_summary() -> list[str]:
    """Critical fields from `git annex version` (no --json upstream)."""
    out = git_annex_version_output()
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
