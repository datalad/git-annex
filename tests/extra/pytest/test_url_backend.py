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

import os
import shutil
import stat
import subprocess
import sys
from pathlib import Path
from typing import Any, Callable

import pytest

from _helpers import git_annex_version, git_annex_version_below

# URL_BACKEND_FIX_VERSION is the git-annex release that first shipped
# the fix (upstream commit 8fd9b67ed8 "factor out extendUrlWithPath …",
# 2026-02-16).  Older versions xfail so we do not block local dev on
# a known regression while still guaranteeing that once a build is on
# a fixed version, the test acts as a permanent regression guard.
URL_BACKEND_FIX_VERSION = "10.20260420"

# On CI, forbid xfails: CI runs against a specific build of git-annex,
# and we want every failure — including "known-broken old-version"
# failures — to be loud rather than silently swallowed by an xfail
# marker.  Setting condition=False disables the xfail entirely (so a
# failure surfaces as a normal FAIL), independent of the installed
# version.  Locally, the version check keeps the marker useful for
# interactive dev on older branches.
_ON_CI = bool(os.environ.get("CI"))

_xfail_broken_url_backend = pytest.mark.xfail(
    condition=(not _ON_CI) and git_annex_version_below(URL_BACKEND_FIX_VERSION),
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


def _make_tree_writable(root: Path) -> None:
    """
    git-annex sets the key file *and* its containing directory to mode
    0500, which makes both `os.unlink(file)` and `os.rmdir(dir)` fail.
    Walk the tree bottom-up and add owner-write to every dir and file
    so a subsequent rmtree succeeds.  Mirrors what `chmod -R u+w` did
    in the dropped bats teardown.
    """
    for dirpath, dirnames, filenames in os.walk(root):
        for name in (*dirnames, *filenames):
            p = os.path.join(dirpath, name)
            try:
                os.chmod(p, os.stat(p).st_mode | stat.S_IWUSR | stat.S_IRUSR | stat.S_IXUSR)
            except OSError:
                pass
    try:
        os.chmod(root, os.stat(root).st_mode | stat.S_IWUSR | stat.S_IRUSR | stat.S_IXUSR)
    except OSError:
        pass


def _chmod_and_retry(func: Callable[..., Any], path: str, _exc: BaseException) -> None:
    """rmtree onexc fallback: chmod the file *and its parent dir* writable, retry."""
    for target in (path, os.path.dirname(path)):
        try:
            os.chmod(target, os.stat(target).st_mode | stat.S_IWUSR | stat.S_IRUSR | stat.S_IXUSR)
        except OSError:
            pass
    func(path)


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
    yield repo
    # Explicit teardown so pytest's later `tmp_path_factory` cleanup
    # doesn't trip over git-annex's read-only object files (Windows,
    # and also POSIX where the containing key-directory is 0500).
    _make_tree_writable(workdir)
    # Belt-and-braces: even after the walk, if a race added new
    # read-only entries, the onexc handler chmods and retries.  Python
    # < 3.12 spells the kwarg `onerror`; 3.12+ prefers `onexc`.
    if sys.version_info >= (3, 12):
        shutil.rmtree(workdir, onexc=_chmod_and_retry)
    else:
        shutil.rmtree(
            workdir,
            onerror=lambda f, p, e: _chmod_and_retry(f, p, e[1]),
        )


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


def _run(cmd: list[str], cwd: Path) -> subprocess.CompletedProcess[str]:
    """Run a diagnostic command; never raise, capture text."""
    try:
        return subprocess.run(
            cmd, cwd=cwd, capture_output=True, text=True, timeout=30,
        )
    except (subprocess.SubprocessError, OSError) as exc:
        return subprocess.CompletedProcess(cmd, returncode=-1, stdout="", stderr=f"{type(exc).__name__}: {exc}")


def _collect_diagnostics(cloned_repo: Path, target: Path) -> str:
    """
    Gather everything an upstream bug report would want when a
    `git annex get` claims success but the working-tree file isn't
    visible.  Kept as a plain-text dump so it appears verbatim in
    the pytest assertion message.
    """
    lines: list[str] = ["", "--- diagnostics ---"]
    lines.append(f"platform: {sys.platform}")
    lines.append(f"cwd: {cloned_repo}")
    lines.append(f"target (rel): {TARGET}")
    lines.append(f"target (abs): {target}")

    # Working-tree entry: does anything exist there at all?
    lines.append(f"os.path.lexists(target): {os.path.lexists(target)}")
    lines.append(f"target.exists(): {target.exists()}")
    lines.append(f"target.is_symlink(): {target.is_symlink()}")
    try:
        st = os.lstat(target)
        lines.append(
            f"os.lstat: mode=0o{st.st_mode:o} size={st.st_size} "
            f"mtime={st.st_mtime}"
        )
    except OSError as exc:
        lines.append(f"os.lstat: {type(exc).__name__}: {exc}")

    if target.is_symlink():
        try:
            link_target = os.readlink(target)
            lines.append(f"readlink(target): {link_target!r}")
            resolved = (target.parent / link_target).resolve(strict=False)
            lines.append(f"resolved: {resolved}")
            lines.append(f"resolved.exists(): {resolved.exists()}")
            if resolved.exists():
                lines.append(f"resolved.stat().st_size: {resolved.stat().st_size}")
        except OSError as exc:
            lines.append(f"readlink/resolve: {type(exc).__name__}: {exc}")

    # git-annex's own view: is content locally available?
    for cmd in (
        ["git", "annex", "find", "--in=here", TARGET],
        ["git", "annex", "whereis", TARGET],
        ["git", "annex", "info", TARGET, "--bytes"],
        ["git", "annex", "lookupkey", TARGET],
        ["git", "annex", "version"],
        ["git", "annex", "config", "--get", "annex.crippledfilesystem"],
        ["git", "config", "--get", "core.symlinks"],
        ["git", "config", "--get", "core.longpaths"],
        ["git", "status", "--porcelain"],
        ["git", "log", "-1", "--pretty=%H %s", "--", TARGET],
    ):
        r = _run(cmd, cloned_repo)
        lines.append(f"$ {' '.join(cmd)}  (rc={r.returncode})")
        if r.stdout.strip():
            lines.append(f"  stdout: {r.stdout.strip()}")
        if r.stderr.strip():
            lines.append(f"  stderr: {r.stderr.strip()}")

    # If we got a key, try to inspect the annex object file directly.
    key_out = _run(["git", "annex", "lookupkey", TARGET], cloned_repo).stdout.strip()
    if key_out:
        # Compute annex object path via `git annex examinekey --format`.
        r = _run(
            ["git", "annex", "examinekey", key_out, "--format=${objectpath}\\n"],
            cloned_repo,
        )
        obj_rel = r.stdout.strip()
        if obj_rel:
            obj_abs = cloned_repo / obj_rel
            lines.append(f"annex object path (rel): {obj_rel}")
            lines.append(f"annex object exists: {obj_abs.exists()}")
            if obj_abs.exists():
                lines.append(f"annex object size: {obj_abs.stat().st_size}")

    # Parent directory listing — did the intermediate dirs get created?
    parent = target.parent
    lines.append(f"parent dir exists: {parent.exists()}")
    if parent.exists():
        try:
            names = sorted(os.listdir(parent))
            lines.append(f"parent listing ({len(names)} entries): {names[:20]}")
        except OSError as exc:
            lines.append(f"listdir(parent): {type(exc).__name__}: {exc}")

    lines.append("--- end diagnostics ---")
    return "\n".join(lines)


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
    # Cross-check via git-annex first — content should be recorded as
    # locally available.  If this fails, `get` didn't actually work.
    found = _run(["git", "annex", "find", "--in=here", TARGET], cloned_repo)
    if not found.stdout.strip():
        pytest.fail(
            f"`git annex find --in=here {TARGET}` returned empty after get; "
            f"content not locally available per git-annex's own view."
            + _collect_diagnostics(cloned_repo, target)
        )
    # And the working-tree entry should be present + non-empty.  On
    # 2026-08-13 Windows we saw `find --in=here` pass while
    # `target.exists()` returned False — capture full state so an
    # upstream report has enough to reproduce.
    if not target.exists():
        pytest.fail(
            f"{TARGET} not visible via Path.exists() despite `find --in=here` "
            f"reporting content present."
            + _collect_diagnostics(cloned_repo, target)
        )
    if target.stat().st_size == 0:
        pytest.fail(
            f"{TARGET} exists but is empty after get."
            + _collect_diagnostics(cloned_repo, target)
        )
