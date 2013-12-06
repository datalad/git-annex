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

# Install haskell dependencies.
# cabal install is not run in cygwin, because we don't want configure scripts
# for haskell libraries to link them with the cygwin library.
cabal install --only-dependencies || true

# Build git-annex
withcyg cabal configure
withcyg cabal build || true

# Works around link failure https://ghc.haskell.org/trac/ghc/ticket/8596
# using a response file.
withcyg rm -f build.log gcc.opt
withcyg cabal build --ghc-options='-v -keep-tmp-files' > build.log 2>&1
withcyg grep '"dist\\build\\git-annex\\git-annex.exe"' build.log | sed -e 's/^"[^"]*" //' -e 's/\\/\//g' > gcc.opt
"$HP/mingw/bin/gcc.exe" @gcc.opt
