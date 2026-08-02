# Shared helpers for the bats extra-tests suite.

# make_tmp_repo: create and cd into a fresh git-annex repo under $BATS_TEST_TMPDIR.
# Sets $REPO to the created path.
make_tmp_repo() {
    REPO="$(mktemp -d "${BATS_TEST_TMPDIR:-/tmp}/annex.XXXXXX")"
    (
        cd "$REPO"
        git init -q
        git config user.email "test@github.land"
        git config user.name "GitHub Almighty"
        git annex init -q
    )
}

# require_cmd <cmd> [reason]: skip the test if <cmd> is not on PATH.
require_cmd() {
    local cmd=$1
    local reason=${2:-"$cmd is not installed"}
    command -v "$cmd" >/dev/null 2>&1 || skip "$reason"
}

# require_linux: skip if not running on Linux.
require_linux() {
    [[ "$(uname -s)" == "Linux" ]] || skip "test is Linux-only"
}

# git_annex_version: prints just the numeric version, e.g. "10.20260421".
# Strips the "-g<sha>" build suffix.  Empty output on error.
git_annex_version() {
    command -v git-annex >/dev/null 2>&1 || return 0
    git annex version 2>/dev/null \
        | awk -F': ' '/^git-annex version:/ { split($2, a, "-"); print a[1] }'
}

# git_annex_version_below <threshold>: exit 0 (true) if the installed
# git-annex version is strictly older than <threshold>, else exit 1.
# Uses `sort -V` for version-aware comparison.
git_annex_version_below() {
    local threshold=$1
    local v
    v=$(git_annex_version)
    [[ -z "$v" ]] && return 1
    # If sorted-V-ascending puts $v first and $threshold second AND they
    # differ, then $v < $threshold.
    local first
    first=$(printf '%s\n%s\n' "$v" "$threshold" | sort -V | head -n1)
    [[ "$first" == "$v" && "$v" != "$threshold" ]]
}

# git_annex_releases_since <threshold>: count git-annex release tags
# in the current repository that are strictly newer than <threshold>.
# Prints the count on stdout; empty on error / no tags found.
git_annex_releases_since() {
    local threshold=$1
    command -v git >/dev/null 2>&1 || return 0
    local tags
    tags=$(git tag --list '10.*' 2>/dev/null) || return 0
    [[ -z "$tags" ]] && return 0
    # Newer-than test with sort -V: keep tags strictly > threshold.
    local newer
    newer=$(printf '%s\n' "$tags" \
        | awk -v t="$threshold" '$0 > t')  # lex compare is fine for 10.YYYYMMDD
    # Refine with sort -V to be safe for oddly-shaped tags.
    newer=$(printf '%s\n' "$newer" \
        | while read -r tag; do
              [[ -z "$tag" ]] && continue
              first=$(printf '%s\n%s\n' "$tag" "$threshold" | sort -V | head -n1)
              [[ "$first" == "$threshold" && "$tag" != "$threshold" ]] && echo "$tag"
          done)
    printf '%s\n' "$newer" | grep -c .
}

# print_versions: emit the same "extra-tests" header the pytest
# conftest prints, so bats runs are self-describing too.  Written to
# BATS's own fd (3) so it's visible without --show-output-of-passing-tests.
print_versions() {
    local tools_line=""
    local sep=""
    for entry in "git|git --version" "bats|bats --version" \
                 "yt-dlp|yt-dlp --version" "youtube-dl|youtube-dl --version" \
                 "strace|strace --version"
    do
        local name=${entry%%|*}
        local cmd=${entry#*|}
        local v
        if command -v "${cmd%% *}" >/dev/null 2>&1; then
            v=$($cmd 2>/dev/null | head -n1)
        else
            v="(missing)"
        fi
        tools_line+="${sep}${name}=${v}"
        sep=", "
    done

    {
        echo "# extra-tests tools: $tools_line"
        echo "# platform: $(uname -sr) $(uname -m)"
        echo "# git-annex:"
        git annex version 2>/dev/null | awk '
            /^git-annex version:|^build flags:|^dependency versions:|^operating system:|^supported repository versions:/ {
                print "#   " $0
            }
        '
        local installed
        installed=$(git_annex_version)
        if [[ -z "$installed" ]]; then
            echo "# git-annex releases newer than installed: (git-annex not installed)"
        else
            local n
            n=$(git_annex_releases_since "$installed")
            if [[ -n "$n" ]]; then
                echo "# git-annex releases in this repo newer than installed (${installed}): ${n}"
            else
                echo "# git-annex releases in this repo newer than installed (${installed}): (no tags found)"
            fi
        fi
    } >&3
}
