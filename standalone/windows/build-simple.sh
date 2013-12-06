#!/bin/sh
# Script to build git-annex on windows. Run by build.bat

set -e

# Path to the Haskell Platform.
HP="/c/Program Files (x86)/Haskell Platform/2012.4.0.0"

PATH="$HP/bin:$HP/lib/extralibs/bin:/c/Program Files (x86)/NSIS:$PATH"

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
rm -f build.log gcc.opt
withcyg cabal build --ghc-options='-v -keep-tmp-files' > build.log 2>&1
grep '"dist\\build\\git-annex\\git-annex.exe"' build.log | sed -e 's/^"[^"]*" //' -e 's/\\/\//g' > gcc.opt
"$HP/mingw/bin/gcc.exe" @gcc.opt
