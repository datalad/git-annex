#!/bin/bash

# Create target directory
sdist_dir=git-annex-$(grep '^Version:' git-annex.cabal | sed -re 's/Version: *//')
mkdir --parents dist/$sdist_dir

find . \( -name .git -or -name dist -or -name cabal-dev \) -prune \
	-or -not -name \\*.orig -not -type d -print \
| perl -ne 'print unless length >= 100' \
| xargs cp --parents --target-directory dist/$sdist_dir

cd dist
tar -caf $sdist_dir.tar.gz $sdist_dir
