#!/bin/sh
# Script to build git-annex on windows. Run by build.bat

set -e
set -x

PATH="/c/Program Files/Git/cmd:/c/Program Files/NSIS:$PATH"

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}

# Prefer programs from cygwin.
withcygpreferred () {
	PATH="/c/cygwin/bin:$PATH" "$@"
}

# Deps are not built with cygwin environment, because we don't want
# configure scripts for haskell libraries to link them with the cygwin
# libraries.
stack setup
stack build --dependencies-only

# Build git-annex
withcyg stack build

# Build the installer
withcygpreferred stack runghc --package nsis Build/NullSoftInstaller.hs
