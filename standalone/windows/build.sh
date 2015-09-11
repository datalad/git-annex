#!/bin/sh
# 
# This script is run by the jenkins autobuilder, in a mingw environment,
# to build git-annex for Windows.

set -x
set -e

# Path to the Haskell Platform.
#HP="/c/haskell/2014.2.0.0" # now in the default PATH

PATH="/c/Program Files (x86)/NSIS:/c/msysgit/cmd:/c/msysgit/bin:$PATH"

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}

# This tells git-annex where to upgrade itself from.
UPGRADE_LOCATION=http://downloads.kitenet.net/git-annex/windows/current/git-annex-installer.exe
export UPGRADE_LOCATION

# This can be used to force git-annex to build supporting a particular
# version of git, instead of the version installed at build time.
#FORCE_GIT_VERSION=1.9.5
#export FORCE_GIT_VERSION

# Uncomment to get rid of cabal installed libraries.
#rm -rf /c/Users/jenkins/AppData/Roaming/cabal /c/Users/jenkins/AppData/Roaming/ghc

# Don't allow build artifact from a past successful build to be extracted
# if we fail.
rm -f git-annex-installer.exe

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal update || true

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
	ghc --make Build/EvilLinker -fno-warn-tabs
	Build/EvilLinker
fi

# Get extra programs to bundle with git-annex.
# These are msys2 programs, from https://msys2.github.io/.
# Since git for windows uses msys2, and includes its libraries,
# these programs will work well with it.
if [ ! -e rsync.exe ] || [ "$(sha1sum rsync.exe)" != "85cb7a4d16d274fcf8069b39042965ad26abd6aa" ]; then
	rm -f rsync.exe || true
	withcyg wget https://downloads.kitenet.net/git-annex/windows/assets/rsync.exe
	withcyg chmod +x rsync.exe
fi
if [ ! -e wget.exe ] || [ "$(sha1sum wget.exe)" != "044380729200d5762965b10123a4f134806b01cf" ]; then
	rm -f wget.exe || true
	withcyg wget https://downloads.kitenet.net/git-annex/windows/assets/wget.exe
	withcyg chmod +x wget.exe
fi

# Build the installer
cabal install nsis
ghc -fforce-recomp --make Build/NullSoftInstaller.hs -fno-warn-tabs
PATH=".:/c/cygwin/bin:$PATH" Build/NullSoftInstaller.exe

rm -f last-incremental-failed

rm -f dist/build-version
ghc --make Build/BuildVersion.hs
Build/BuildVersion > dist/build-version

# Test git-annex
# The test is run in c:/WINDOWS/Temp, because running it in the autobuilder
# directory runs afoul of Windows's short PATH_MAX.
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
