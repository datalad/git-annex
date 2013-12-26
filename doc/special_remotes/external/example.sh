#!/bin/sh
# git-annex external special remote program
# 
# This is basically the same as git-annex's built-in directory special remote.
# 
# Install in PATH as git-annex-remote-directorya
#
# Copyright 2013 Joey Hess; licenced under the GNU GPL version 3 or higher.

set -e

# This program speaks a line-based protocol on stdin and stdout.
# When running any commands, their stdout should be redirected to stderr
# (or /dev/null) to avoid messing up the protocol.
runcmd () {
	"$@" >&2
}

# Gets a value from the remote's configuration, and stores it in RET
getconfig () {
	echo GETCONFIG "$1"
	read resp
	set -- $resp
	case "$1" in
		VALUE)
			RET="$2"
		;;
		*)
			RET=""
		;;
	esac
}

# Sets LOC to the location to use to store a key.
mylocation () {
	echo HASHDIR "$1"
	read resp
	set -- $resp
	case "$1" in
		VALUE)
			LOC="$hashdir/$1"
		;;
		*)
			LOC=
		;;
	esac
}

echo VERSION 1

while read line; do
	set -- $line
	case "$1" in
		INITREMOTE)
			# XXX do anything necessary to create resources
			# used by the remote. Try to be idempotent.
			# Use GETCONFIG to get any needed configuration
			# settings, and SETCONFIG to set any persistent
			# configuration settings.
			getconfig directory
			mydirectory="$RET"
			if [ -z "$mydirectory" ]; then
				echo INITREMOTE-FAILURE "You need to set directory="
			else
				mkdir -p "$mydirectory"
				echo INITREMOTE-SUCCESS
			fi
		;;
		GETCOST)
			echo COST-UNKNOWN
		;;
		PREPARE)
			# XXX Use GETCONFIG to get configuration settings,
			# and do anything needed to get ready for using the
			# special remote here.
			getconfig directory
			mydirectory="$RET"
		;;
		TRANSFER)
			key="$3"
			file="$4"
			case "$2" in
				STORE)
					# XXX upload file here
					# XXX when possible, send PROGRESS
					calclocation "$key"
					mkdir -p "$(dirname "$LOC")"
					runcmd cp -v "$file" "$LOC"
					echo TRANSFER-SUCCESS STORE "$key"
				;;
				RETRIEVE)
					# XXX download file here
					calclocation "$key"
					runcmd cp -v "$LOC" "$file"
					echo TRANSFER-SUCCESS RETRIEVE "$key"
				;;
			esac
		;;
		CHECKPRESENT)
			key="$2"
			calclocation "$key"
			if [ -e "$LOC" ]; then
				echo CHECKPRESENT-SUCCESS "$key"
			else
				if [ -d "$mydirectory" ]; then
					echo CHECKPRESENT-FAILURE "$key"
				else
					# If the directory does not exist,
					# the remote is not available.
					# (A network remote would similarly
					# fail with CHECKPRESENT-UNKNOWN
					# if it couldn't be contacted).
					echo CHECKPRESENT-UNKNOWN "$key" "this remote is not currently available"
				fi
			fi
		;;
		REMOVE)
			key="$2"
			calclocation "$key"
			# Note that it's not a failure to remove a
			# key that is not present, so -f is used.
			runcmd rm -f "$LOC"
			echo REMOVE-SUCCESS "$key"
		;;
		*)
			echo UNKNOWN-REQUEST
		;;
	esac	
done

# XXX anything that needs to be done at shutdown can be done here
