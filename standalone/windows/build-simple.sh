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
withcygpreferred () {
	PATH="/c/cygwin/bin:$PATH" "$@"
}

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
if ! cabal install --only-dependencies; then
	cabal update || true
	cabal install --only-dependencies
fi

# Build git-annex
if [ ! -e "dist/setup-config" ]; then
	withcyg cabal configure
fi
if ! withcyg cabal build; then
	ghc --make Build/EvilLinker -fno-warn-tabs
	withcyg Build/EvilLinker
fi

# Build the installer
cabal install nsis
ghc --make Build/NullSoftInstaller.hs -fno-warn-tabs
PATH="$PATH:/cygdrive/c/Program Files/NSIS" Build/NullSoftInstaller.exe
