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

[![Update mirror](https://github.com/datalad/git-annex/actions/workflows/update-mirror.yml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/update-mirror.yml)

## Build git-annex & test DataLad against it

[![Ubuntu](https://github.com/datalad/git-annex/actions/workflows/build-ubuntu.yaml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/build-ubuntu.yaml)
[![macOS](https://github.com/datalad/git-annex/actions/workflows/build-macos.yaml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/build-macos.yaml)
[![Windows](https://github.com/datalad/git-annex/actions/workflows/build-windows.yaml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/build-windows.yaml)

## Client Tests

| Client | Test Status |
| --- | --- |
| [openmind7](https://datalad.github.io/git-annex-ci-reports/#openmind7) | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/openmind7.svg) ![git-annex-home test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/openmind7/git-annex-home.svg) ![git-annex-om2 test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/openmind7/git-annex-om2.svg) |
| [ndoli](https://datalad.github.io/git-annex-ci-reports/#ndoli) | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli.svg) ![git-annex-home test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli/git-annex-home.svg) ![git-annex-tmp test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli/git-annex-tmp.svg) |
| [smaug](https://datalad.github.io/git-annex-ci-reports/#smaug) | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/smaug.svg) ![git-annex test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/smaug/git-annex.svg) |

## Builds of whls for PyPI (by @psychoinformatics-de)

[![Linux](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-linux.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-linux.yaml)
[![MacOS (13, Intel)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-intel.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-intel.yaml)
[![MacOS (14, M1)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-m1.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-m1.yaml)
[![Windows](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-windows.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-windows.yaml)

[![Test git-annex wheel from PyPi](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/test-pypi-wheel.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/test-pypi-wheel.yaml)

