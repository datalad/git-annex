#!/bin/sh
# 
# This script is run by the jenkins autobuilder, in a mingw environment,
# to build git-annex for Windows.

set -x
set -e

# Path to the Haskell Platform.
HP="/c/Program Files (x86)/Haskell Platform/2013.2.0.0"

PATH="$HP/bin:$HP/lib/extralibs/bin:/c/Program Files (x86)/NSIS:$PATH"

# This tells git-annex where to upgrade itself from.
UPGRADE_LOCATION=http://downloads.kitenet.net/git-annex/windows/current/git-annex-installer.exe

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}

# Uncomment to get rid of cabal installed libraries.
rm -rf /c/Users/jenkins/AppData/Roaming/cabal /c/Users/jenkins/AppData/Roaming/ghc

# Don't allow build artifact from a past successful build to be extracted
# if we fail.
#rm -f git-annex-installer.exe

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal update || true

cabal install --only-dependencies

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
withcyg cabal configure
withcyg cabal build || true 

# Works around link failure https://ghc.haskell.org/trac/ghc/ticket/8596
# using a response file.
rm -f build.log gcc.opt
withcyg cabal build --ghc-options='-v -keep-tmp-files' > build.log 2>&1 || true
grep '"dist\\build\\git-annex\\git-annex.exe"' build.log | sed -e 's/^"[^"]*" //' -e 's/\\/\//g' > gcc.opt
"$HP/mingw/bin/gcc.exe" @gcc.opt

# Build the installer
cabal install nsis
ghc --make Build/NullSoftInstaller.hs
withcyg Build/NullSoftInstaller.exe

rm -f last-incremental-failed

# Test git-annex
# (doesn't currently work well on autobuilder, reason unknown)
rm -rf .t
withcyg dist/build/git-annex/git-annex.exe test || true
