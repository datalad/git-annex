#!/bin/bash

# Create target directory
cabal sdist
sdist_dir=$(basename dist/*.tar.gz .tar.gz)
rm -f dist/*.tar.gz
mkdir dist/$sdist_dir

find . \( -name .git -or -name dist -or -name cabal-dev \) -prune \
	-or -not -name \\*.orig -not -type d -print \
| perl -ne 'print unless length >= 100' \
| xargs cp --parents --target-directory dist/$sdist_dir

cd dist
tar -caf $sdist_dir.tar.gz $sdist_dir
