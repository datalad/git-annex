#!/usr/bin/env bats
#
# Regression: older git-annex failed on URL-backend keys whose encoded name
# contained characters like `&c`, `%%`, `,63v`.  See
# tests/extra/pytest/test_url_backend.py for the equivalent pytest version.

load helpers

REPRO_URL='https://datasets.datalad.org/repronim/ReproTube/DataLad/.git/'
TARGET='videos/2021/07/2021-07-11_Demo-Fully-recomputing-a-real-scientific-paper-DIY/video.mkv'

# git-annex releases strictly older than this have the URL-encoded-key
# retrieval bug (upstream commit 8fd9b67ed8, first shipped in
# 10.20260420).  bats has no direct xfail; we `skip` on older versions,
# so a regression on a fixed version fails loudly and older versions
# don't red the run.  Keep in sync with URL_BACKEND_FIX_VERSION in
# tests/extra/pytest/test_url_backend.py.
URL_BACKEND_FIX_VERSION='10.20260420'

setup_file() {
    REPO_PARENT="$(mktemp -d "${BATS_FILE_TMPDIR:-/tmp}/ReproTube.XXXXXX")"
    REPO="$REPO_PARENT/DataLad"
    export REPO REPO_PARENT
    # --no-single-branch so we also fetch the git-annex branch, which is
    # where URL-backend metadata lives.
    git clone --depth=1 --no-single-branch "$REPRO_URL" "$REPO"
    (
        cd "$REPO"
        git config user.email "test@github.land"
        git config user.name "GitHub Almighty"
        git annex init -q
    )
}

teardown_file() {
    if [[ -n "${REPO_PARENT:-}" && -d "$REPO_PARENT" ]]; then
        # git-annex objects are read-only; make them writable before rm.
        chmod -R u+w "$REPO_PARENT" 2>/dev/null || true
        rm -rf "$REPO_PARENT"
    fi
    return 0
}

@test "whereis decodes URL-backend key back to youtube.com/watch?v=" {
    cd "$REPO"
    run git annex whereis "$TARGET"
    [ "$status" -eq 0 ]
    echo "$output" | grep -q 'youtube.com/watch?v='
}

@test "get retrieves URL-backend file" {
    if git_annex_version_below "$URL_BACKEND_FIX_VERSION"; then
        skip "URL-encoded-key retrieval bug in git-annex < $URL_BACKEND_FIX_VERSION (installed: $(git_annex_version))"
    fi
    cd "$REPO"
    timeout 600 git annex get "$TARGET"
    [ -s "$TARGET" ]
}
