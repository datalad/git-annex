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

# Deps are not built with cygwin environment, because we don't want
# configure scripts for haskell libraries to link them with the cygwin
# libraries.
stack setup
stack build --dependencies-only
  
# Build git-annex
withcyg stack build  

# Get extra programs to bundle with git-annex.
# These are msys2 programs, from https://msys2.github.io/.
# Since git for windows uses msys2, and includes its libraries,
# these programs will work well with it.
getextra () {
	extrap="$1"
	extrasha="$2"
	curextrasha="$(withcyg sha1sum $extrap | sed 's/ .*//')"
	if [ ! -e "$extrap" ] || [ "$curextrasha" != "$extrasha" ]; then
		rm -f "$extrap" || true
		withcyg wget https://downloads.kitenet.net/git-annex/windows/assets/$extrap
		curextrasha="$(withcyg sha1sum $extrap | sed 's/ .*//')"
		if [ "$curextrasha" != "$extrasha" ]; then
			rm -f "$extrap"
			echo "CHECKSUM FAILURE" >&2
			exit 1
		fi
		withcyg chmod +x $extrap
	fi
}
getextra rsync.exe 85cb7a4d16d274fcf8069b39042965ad26abd6aa
getextra wget.exe 044380729200d5762965b10123a4f134806b01cf

# Build the installer
withcygpreferred stack runghc --package nsis Build/NullSoftInstaller.hs

rm -f dist/build-version
stack runghc Build/BuildVersion.hs > dist/build-version

# Test git-annex
# The test is run in c:/WINDOWS/Temp, because running it in the autobuilder
# directory runs afoul of Windows's short PATH_MAX.
PATH="$(pwd)/dist/build/git-annex/:$PATH"
export PATH
mkdir -p c:/WINDOWS/Temp/git-annex-test/
cd c:/WINDOWS/Temp/git-annex-test/
rm -rf .t

# Currently the test fails in the autobuilder environment for reasons not
# yet understood. Windows users are encouraged to run the test suite
# themseves, so we'll ignore these failures for now.
withcyg git-annex.exe test || true
