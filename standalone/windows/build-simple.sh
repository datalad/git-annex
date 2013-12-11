#!/bin/sh
# Script to build git-annex on windows. Run by build.bat
# Does not currently build the NSIS installer. See build.sh for how to do
# that.

set -e
set -x

# Path to the Haskell Platform.
HP="/c/Program Files/Haskell Platform/2013.2.0.0"

PATH="$HP/bin:$HP/lib/extralibs/bin:$PATH"

# Run a command with the cygwin environment available.
# However, programs not from cygwin are preferred.
withcyg () {
	PATH="$PATH:/c/cygwin/bin" "$@"
}
withcygpreferred () {
	PATH="/c/cygwin/bin:$PATH" "$@"
}

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal install --only-dependencies || true

# Build git-annex
if [ ! -e "dist/setup-config" ]; then
	withcyg cabal configure
fi
if ! withcyg cabal build; then
	ghc --make Build/EvilLinker
	withcyg Build/EvilLinker
fi

# Build the installer
cabal install nsis
ghc --make Build/NullSoftInstaller.hs
PATH="$PATH:/cygdrive/c/Program Files/NSIS"
# Want to include cygwin programs in bundle, not others, since
# it includes the cygwin libs that go with them.
withcygpreferred Build/NullSoftInstaller.exe
