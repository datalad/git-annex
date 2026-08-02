"""
Regression: older git-annex failed on URL-backend keys whose encoded name
contained characters like `&c`, `%%`, `,63v` (from URL-encoded scheme,
`://`, `?v=`, etc.).  The reproducer is a real DataLad dataset that
stores YouTube videos with `yt:<url>` keys.

Two levels of check on a URL-backend key that decodes to a `yt:` URL:
  1. `git annex whereis` — parses the key and lists its recorded URLs.
                           This code path was NOT affected by the bug,
                           and passes on all git-annex versions.
  2. `git annex get`      — retrieves the ~18 MB video file, falling
                            back through the recorded remotes.  This
                            was the affected code path; xfail on
                            git-annex versions below the fix.

The DataLad dataset's `origin` remote serves the annex content over
HTTPS, so the retrieval does not require yt-dlp or YouTube access.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from conftest import (
    URL_BACKEND_FIX_VERSION,
    git_annex_version,
    git_annex_version_below,
)

# URL_BACKEND_FIX_VERSION is the git-annex release that first shipped
# the fix (upstream commit 8fd9b67ed8 "factor out extendUrlWithPath …",
# 2026-02-16).  Older versions xfail so we do not block CI on a known
# regression while still guaranteeing that once a build is on a fixed
# version, the test acts as a permanent regression guard.

_xfail_broken_url_backend = pytest.mark.xfail(
    condition=git_annex_version_below(URL_BACKEND_FIX_VERSION),
    reason=(
        f"URL-encoded-key retrieval bug present in git-annex "
        f"< {URL_BACKEND_FIX_VERSION} "
        f"(installed: {git_annex_version() or 'unknown'})"
    ),
    strict=False,
)

REPRO_URL = "https://datasets.datalad.org/repronim/ReproTube/DataLad/.git/"
TARGET = (
    "videos/2021/07/"
    "2021-07-11_Demo-Fully-recomputing-a-real-scientific-paper-DIY/"
    "video.mkv"
)


@pytest.fixture(scope="module")
def cloned_repo(tmp_path_factory: pytest.TempPathFactory) -> Path:
    workdir = tmp_path_factory.mktemp("ReproTube")
    repo = workdir / "DataLad"
    # --no-single-branch so we also fetch the git-annex branch, which is
    # where URL-backend metadata lives.
    subprocess.run(
        ["git", "clone", "--depth=1", "--no-single-branch", REPRO_URL, str(repo)],
        check=True,
    )
    subprocess.run(
        ["git", "config", "user.email", "test@github.land"],
        cwd=repo, check=True,
    )
    subprocess.run(
        ["git", "config", "user.name", "GitHub Almighty"],
        cwd=repo, check=True,
    )
    subprocess.run(["git", "annex", "init", "-q"], cwd=repo, check=True)
    return repo


def test_whereis_parses_url_backend_key(cloned_repo: Path) -> None:
    """`git annex whereis` on a URL-backend key must list the decoded URL."""
    result = subprocess.run(
        ["git", "annex", "whereis", TARGET],
        cwd=cloned_repo,
        capture_output=True,
        text=True,
        check=True,
    )
    out = result.stdout
    # The `,63v,61` chars in the key are the URL-encoded `?v=`;
    # git-annex must decode them back to the original YouTube URL.
    assert "youtube.com/watch?v=" in out, (
        f"expected decoded youtube URL in `whereis` output; got:\n{out}"
    )


@_xfail_broken_url_backend
def test_get_url_backend_key(cloned_repo: Path) -> None:
    """Full reproducer: retrieve the URL-backend file."""
    subprocess.run(
        ["git", "annex", "get", TARGET],
        cwd=cloned_repo,
        check=True,
        timeout=600,
    )
    target = cloned_repo / TARGET
    assert target.exists(), f"{TARGET} was not retrieved"
    assert target.stat().st_size > 0, f"{TARGET} is empty after get"
