This is a mirror of the official [git-annex](https://git-annex.branchable.com)
repository at <https://git.kitenet.net/index.cgi/git-annex.git/>.  The remote
`master` branch is mirrored here as
[`upstream/master`](https://github.com/datalad/git-annex/tree/upstream/master).
The local `master` branch in this repository is only used to store CI
configuration and associated scripts.

## Submitting Patches

Patches to the git-annex source code can be added by opening a pull request
against `master` that adds a Git patch file with a `.patch` file extension to
the `patches/` directory; the CI will then build git-annex with this patch (and
any others currently in `patches/`) applied.  If the patch fails to apply or
appears to have already been applied upstream, the builds will fail.

It is recommended that patches be given names of the form
`YYYYMMDD-{commit}-{brief_description}.patch`, where `{commit}` is the short
hash of the git-annex source code commit against which the patch was made.

Once a patch PR is merged into `master`, the patch will be applied to all
git-annex builds on all platforms, including release builds.  Patches in
`patches/` are applied in lexicographic filename order.  If a patch in
`patches/` gets applied upstream, a PR will automatically be created to delete
the file from `patches/`, and the patch will be skipped.  If a patch fails to
apply, an issue will be automatically created in this repository, and the build
will fail.

## Status

![Update mirror](https://github.com/datalad/git-annex/workflows/Update%20mirror/badge.svg)

## Build git-annex & test DataLad against it

[![Ubuntu](https://github.com/datalad/git-annex/workflows/Build%20git-annex%20on%20Ubuntu/badge.svg)](https://github.com/datalad/git-annex/actions?query=workflow%3A%22Build+git-annex+on+Ubuntu%22)
[![macOS](https://github.com/datalad/git-annex/workflows/Build%20git-annex%20on%20macOS/badge.svg)](https://github.com/datalad/git-annex/actions?query=workflow%3A%22Build+git-annex+on+macOS%22)
[![Windows](https://github.com/datalad/git-annex/workflows/Build%20git-annex%20on%20Windows/badge.svg)](https://github.com/datalad/git-annex/actions?query=workflow%3A%22Build+git-annex+on+Windows%22)

## Client Tests

| Client | Test Status |
| --- | --- |
| ndoli | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli.svg) ![git-annex-home test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli/git-annex-home.svg) ![git-annex-tmp test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli/git-annex-tmp.svg) |
| smaug | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/smaug.svg) ![git-annex test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/smaug/git-annex.svg) |
