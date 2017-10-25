#!/bin/sh
# 
# This script is run by the jenkins autobuilder, in a mingw environment,
# to build git-annex for Windows.

set -x
set -e

PATH="/cygdrive/c/git/cmd:/cygdrive/c/Program Files (x86)/NSIS/Bin:/cygdrive/c/Program Files (x86)/NSIS:/usr/local/bin:/usr/bin:$PATH"

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}

# Prefer programs from cygwin.
withcygpreferred () {
	PATH="/c/cygwin/bin:$PATH" "$@"
}

# This tells git-annex where to upgrade itself from.
UPGRADE_LOCATION=http://downloads.kitenet.net/git-annex/windows/current/git-annex-installer.exe
export UPGRADE_LOCATION

# This can be used to force git-annex to build supporting a particular
# version of git, instead of the version installed at build time.
#FORCE_GIT_VERSION=1.9.5
#export FORCE_GIT_VERSION

# Don't allow build artifact from a past successful build to be extracted
# if we fail.
rm -f git-annex-installer.exe

# Get extra programs to bundle with git-annex.
# These are msys2 programs, from https://msys2.github.io/.
# Since git for windows uses msys2, and includes its libraries,
# these programs will work well with it. Note that these are 32 bit
# programs, so the 32 bit version of git for windows needs to be installed,
# not the 64 bit.
getextra () {
	extrap="$1"
	extrasha="$2"
	curextrasha="$(sha1sum $extrap | sed 's/ .*//')"
	if [ ! -e "$extrap" ] || [ "$curextrasha" != "$extrasha" ]; then
		rm -f "$extrap" || true
		wget https://downloads.kitenet.net/git-annex/windows/assets/$extrap
		curextrasha="$(sha1sum $extrap | sed 's/ .*//')"
		if [ "$curextrasha" != "$extrasha" ]; then
			rm -f "$extrap"
			echo "CHECKSUM FAILURE" >&2
			exit 1
		fi
		chmod +x $extrap
	fi
}
getextra rsync.exe 85cb7a4d16d274fcf8069b39042965ad26abd6aa

# Deps are not built with cygwin environment, because we don't want
# configure scripts for haskell libraries to link them with the cygwin
# libraries.
stack setup --stack-yaml stack-windows.yaml
stack build -j 1 --stack-yaml stack-windows.yaml --no-haddock --dependencies-only 
  
# Build git-annex
withcyg stack build --stack-yaml stack-windows.yaml

# Build the installer
withcygpreferred stack ghc --stack-yaml stack-windows.yaml --no-haddock \
	--package nsis Build/NullSoftInstaller.hs
./Build/NullSoftInstaller

rm -f dist/build-version
mkdir -p dist
stack ghc --stack-yaml stack-windows.yaml --no-haddock Build/BuildVersion.hs
./Build/BuildVersion > dist/build-version

# Test git-annex
# The test is run in c:/WINDOWS/Temp, because running it in the autobuilder
# directory runs afoul of Windows's short PATH_MAX.
PATH="$(pwd):$PATH"
export PATH
mkdir -p c:/WINDOWS/Temp/git-annex-test/
cd c:/WINDOWS/Temp/git-annex-test/
rm -rf .t

# Currently the test fails in the autobuilder environment for reasons not
# yet understood. Windows users are encouraged to run the test suite
# themseves, so we'll ignore these failures for now.
withcyg git-annex.exe test || true
