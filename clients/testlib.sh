function workdir_base {
    WORKDIR_BASE="$1"
    mkdir -p "$WORKDIR_BASE/$BUILDNO"
    trap 'cd "$WORKDIR_BASE" && chmod -R +w "$BUILDNO" && rm -rf "$BUILDNO"' EXIT
    cd "$WORKDIR_BASE/$BUILDNO"
}
