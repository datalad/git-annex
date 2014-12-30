#!/bin/sh
# 
# This script is run by the jenkins autobuilder, in a mingw environment,
# to build git-annex for Windows.

set -x
set -e

# Path to the Haskell Platform.
#HP="/c/haskell/2014.2.0.0" # now in the default PATH

PATH="/c/Program Files (x86)/NSIS:/c/msysgit/cmd:$PATH"

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}
withcygpreferred () {
	PATH="/c/cygwin/bin:$PATH" "$@"
}

# This tells git-annex where to upgrade itself from.
UPGRADE_LOCATION=http://downloads.kitenet.net/git-annex/windows/current/git-annex-installer.exe

# Uncomment to get rid of cabal installed libraries.
#rm -rf /c/Users/jenkins/AppData/Roaming/cabal /c/Users/jenkins/AppData/Roaming/ghc

# Don't allow build artifact from a past successful build to be extracted
# if we fail.
rm -f git-annex-installer.exe

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal update || true

# This workaround is still needed, it seems.
#cabal install transformers-compat -fthree
#cabal install DAV-1.0

cabal install --only-dependencies || true

# Detect when the last build was an incremental build and failed, 
# and try a full build. Done this way because this shell seems a bit
# broken.
if [ -e last-incremental-failed ]; then
	cabal clean || true
	# windows breakage..
	rm -rf dist dist.old || mv -v dist dist.old
fi
touch last-incremental-failed

# Build git-annex
withcyg cabal configure
if ! withcyg cabal build; then
	rm -f Build/EvilLinker.exe
	ghc --make Build/EvilLinker
	Build/EvilLinker
fi

# Build the installer
cabal install nsis
ghc -fforce-recomp --make Build/NullSoftInstaller.hs
# Want to include cygwin programs in bundle, not others, since
# it includes the cygwin libs that go with them.
withcygpreferred Build/NullSoftInstaller.exe

rm -f last-incremental-failed

rm -f dist/build-version
ghc --make Build/BuildVersion.hs
Build/BuildVersion > dist/build-version

# Test git-annex
# (doesn't currently work well on autobuilder, reason unknown)
PATH="$(pwd)/dist/build/git-annex/:$PATH"
export PATH
mkdir -p c:/WINDOWS/Temp/git-annex-test/
cd c:/WINDOWS/Temp/git-annex-test/
if withcyg git-annex.exe test; then
	rm -rf .t
else
	rm -rf .t
	echo "Test suite failure; failing build!"
	false
fi
