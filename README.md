# datalad/git-annex

## The purpose

### Downstream testing

The primary goal for the repository is to provide extensive automated testing
of fresh builds of [git-annex](https://git-annex.branchable.com) using
its built-in test battery across various base operating systems and
environments, and against our downstream [DataLad](https://datalad.org/)
project.

### Distribution

As a side-artifact of the build during the testing process, we provide non-official
automated builds of `git-annex`.  They are attached as artifacts to the CI
runs, and, for the `git-annex` releases, also to the
[Releases](https://github.com/datalad/git-annex/releases) page.  All build logs
and builds are archived "privately" on an internal server using
[con/tinuous](https://github.com/con/tinuous), and release builds are then
re-shared from the
[///datalad/packages](https://datasets.datalad.org/?dir=/datalad/packages)
DataLad package.

These builds are **non-official** builds of
[git-annex](https://git-annex.branchable.com) and their functioning might
differ from the official builds.

## AI Disclaimer

Agentic AI tools might be used for various aspects of the
troubleshooting, development, and maintenance in this repository. We
strive to provide our best effort for adequate provenance and attribution on
AI-assisted work in commit messages, patch headers, and issues.

## Structure and workflow

### Branches

This repository contains two primary branches:

- [`upstream/master`](https://github.com/datalad/git-annex/tree/upstream/master) -
  an unmodified mirror of the `master` branch from the original
  [git-annex](https://git-annex.branchable.com) [repository](https://git.kitenet.net/index.cgi/git-annex.git/), and
- [`master`](https://github.com/datalad/git-annex/tree/master) -
  CI configuration, associated scripts, and potential patches on top of `upstream/master`,
  which are then applied at build time.

As a result, the builds provided here may function differently
than the official original builds of git-annex.  This repository also contains
issues which might be specific to these builds and not necessarily relevant
to the original git-annex.

### Submitting Patches

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

### Build git-annex & test DataLad against it

[![Ubuntu](https://github.com/datalad/git-annex/actions/workflows/build-ubuntu.yaml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/build-ubuntu.yaml)
[![macOS](https://github.com/datalad/git-annex/actions/workflows/build-macos.yaml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/build-macos.yaml)
[![Windows](https://github.com/datalad/git-annex/actions/workflows/build-windows.yaml/badge.svg)](https://github.com/datalad/git-annex/actions/workflows/build-windows.yaml)

### Client Tests

| Client | Test Status |
| --- | --- |
| [openmind7](https://datalad.github.io/git-annex-ci-reports/#openmind7) | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/openmind7.svg) ![git-annex-home test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/openmind7/git-annex-home.svg) ![git-annex-om2 test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/openmind7/git-annex-om2.svg) |
| [ndoli](https://datalad.github.io/git-annex-ci-reports/#ndoli) | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli.svg) ![git-annex-home test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli/git-annex-home.svg) ![git-annex-tmp test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/ndoli/git-annex-tmp.svg) |
| [smaug](https://datalad.github.io/git-annex-ci-reports/#smaug) | ![Overall test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/smaug.svg) ![git-annex test status](https://github.com/datalad/git-annex-ci-client-jobs/raw/master/badges/smaug/git-annex.svg) |

### Builds of whls for PyPI (by @psychoinformatics-de)

[![Linux](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-linux.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-linux.yaml)
[![MacOS (13, Intel)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-intel.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-intel.yaml)
[![MacOS (14, M1)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-m1.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-macos-m1.yaml)
[![Windows](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-windows.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/build-windows.yaml)

[![Test git-annex wheel from PyPi](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/test-pypi-wheel.yaml/badge.svg)](https://github.com/psychoinformatics-de/git-annex-wheel/actions/workflows/test-pypi-wheel.yaml)

