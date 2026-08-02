#!/usr/bin/env bats
#
# Guard against regressions in git-annex's dynamic-library lookup behaviour.
# See tests/extra/pytest/test_dynlibs.py for the equivalent pytest version.

load helpers

setup() {
    require_linux
    require_cmd strace
    TESTREPO="$(mktemp -d "${BATS_TEST_TMPDIR:-/tmp}/dynlibs.XXXXXX")"
    ( cd "$TESTREPO" && git init -q )
}

teardown() {
    [[ -n "${TESTREPO:-}" && -d "$TESTREPO" ]] && rm -rf "$TESTREPO"
    return 0
}

nfailed() {
    local subcommand=$1
    local pattern=$2
    strace -f git-annex "$subcommand" 2>&1 \
        | awk "/${pattern}.*ENOENT/{print}" \
        | tee /dev/fd/2 \
        | wc -l
}

@test "libpcre ENOENT lookups on 'git-annex version' < 7" {
    cd "$TESTREPO"
    n=$(nfailed version "libpcre.*so")
    [ "$n" -lt 7 ]
}

@test "libpcre ENOENT lookups on 'git-annex init' < 260" {
    cd "$TESTREPO"
    n=$(nfailed init "libpcre.*so")
    [ "$n" -lt 260 ]
}
