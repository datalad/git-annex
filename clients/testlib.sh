function workdir_base {
    WORKDIR_BASE="$1"
    mkdir -p "$WORKDIR_BASE/$BUILDNO"
    SECONDS=0
    trap 'echo "Elapsed $SECONDS seconds" && cd "$WORKDIR_BASE" && chmod -R +w "$BUILDNO" && rm -rf "$BUILDNO"' EXIT
    cd "$WORKDIR_BASE/$BUILDNO"
}
