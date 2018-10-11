#!/bin/sh
# 
# Installs git-annex in termux on android.
# 
# This is only a convenience script to avoid the user needing to type much
# in termux. The standalone tarball automatically adapts itself to the termux
# environment, so this script should stay as mininal as possible.

set -e

case $(uname -m) in
	aarch64)
		arch=arm64
		;;
	arm)
		arch=armel
		;;
	x86_64)
		arch=amd64
		;;
	x86_32)
		arch=i386
		;;
	*) 
		echo "unknown architecture $(uname -m), cannot install" >&2
		exit 1
		;;
esac

url=https://downloads.kitenet.net/git-annex/linux/current/git-annex-standalone-"$arch".tar.gz
# uncomment to use latest autobuild
#url=https://downloads.kitenet.net/git-annex/autobuild/"$arch"/git-annex-standalone-"$arch".tar.gz

echo "Installing dependencies with termux pkg manager..."
pkg install git wget tar coreutils proot

echo "Downloading git-annex..."
cd
wget -O- "$url" | tar zx

# This lets runshell finish the installation.
git-annex.linux/git-annex version
echo "git-annex is successfully installed."

echo "Now running termux-setup-storage, to let git-annex access system storage."
termux-setup-storage

echo "Installation complete."
