#!/bin/sh
# Script to build git-annex on windows. Run by build.bat

set -e
set -x

PATH="/c/Program Files/Git/cmd:/c/Program Files/NSIS:$PATH"

stack setup
stack build --dependencies-only

# Build git-annex
stack build

# Build the installer
stack runghc --package nsis Build/NullSoftInstaller.hs
