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

# Don't allow build artifact from a past successfuly build to be extracted
# if we fail.
rm -f git-annex-installer.exe

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal update

rm -rf MissingH-1.2.0.0
cabal unpack MissingH
cd MissingH-1.2.0.0
withcyg patch -p1 <../standalone/windows/haskell-patches/MissingH_1.2.0.0-0001-hack-around-strange-build-problem-in-jenkins-autobui.patch
cabal install || true
cd ..

cabal install --only-dependencies -f"$FLAGS"

# Build git-annex
withcyg cabal configure -f"$FLAGS"
withcyg cabal build

# Build the installer
cabal install nsis
ghc --make Build/NullSoftInstaller.hs
withcyg Build/NullSoftInstaller.exe

# Test git-annex
rm -rf .t
withcyg dist/build/git-annex/git-annex.exe test
