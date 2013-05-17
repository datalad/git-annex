#!/bin/sh
# 
# This script is run by the jenkins autobuilder, in a mingw environment,
# to build git-annex for Windows.

set -x
set -e

HP="/c/Program Files (x86)/Haskell Platform/2012.4.0.0"

PATH="$HP/bin:$HP/lib/extralibs/bin:$PATH"

# Run a command in the cygwin environment.
incygwin () {
	PATH="/c/cygwin/bin:$PATH" "$@"
}

# Build git-annex
cabal update
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
FLAGS="-Webapp -Assistant"
cabal install --only-dependencies -f"$FLAGS"
incygwin cabal configure -f"$FLAGS"
incygwin cabal build

# Build the installer
cabal install nsis
ghc --make Build/NullSoftInstaller.hs
Build/NullSoftInstaller.exe
