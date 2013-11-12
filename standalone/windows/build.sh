#!/bin/sh
# 
# This script is run by the jenkins autobuilder, in a mingw environment,
# to build git-annex for Windows.

set -x
set -e

HP="/c/Program Files (x86)/Haskell Platform/2012.4.0.0"
FLAGS="-Webapp -Assistant -XMPP"

PATH="$HP/bin:$HP/lib/extralibs/bin:/c/Program Files (x86)/NSIS:$PATH"

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}

# Don't allow build artifact from a past successful build to be extracted
# if we fail.
rm -f git-annex-installer.exe

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal update || true

cabal install --only-dependencies -f"$FLAGS"

# Detect when the last build was an incremental build and failed, 
# and try a full build. Done this way because this shell seems a bit
# broken.
if [ -e last-incremental-failed ]; then
	cabal clean || true
	# windows breakage..
	rm -rf dist
fi
touch last-incremental-failed

# Build git-annex
withcyg cabal configure -f"$FLAGS"
withcyg cabal build
	
# Build the installer
cabal install nsis
ghc --make Build/NullSoftInstaller.hs
withcyg Build/NullSoftInstaller.exe

rm -f last-incremental-failed

# Test git-annex
rm -rf .t
withcyg dist/build/git-annex/git-annex.exe test || true
