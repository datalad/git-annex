#!/bin/sh
#
# Workaround for `cabal sdist` requiring all included files to be listed
# in .cabal.

# Create target directory
sdist_dir=git-annex-$(grep '^Version:' git-annex.cabal | sed -re 's/Version: *//')
mkdir --parents dist/$sdist_dir

find . \( -name .git -or -name dist -or -name cabal-dev \) -prune \
	-or -not -name \\*.orig -not -type d -print \
	| perl -ne "print unless length >= 100 - length q{$sdist_dir}" \
	| grep -v ':' \
	| xargs cp --parents --target-directory dist/$sdist_dir

cd dist
tar --format=ustar -caf $sdist_dir.tar.gz $sdist_dir

# Check that tarball can be unpacked by cabal.
# It's picky about tar longlinks etc.
rm -rf $sdist_dir
cabal unpack $sdist_dir.tar.gz
