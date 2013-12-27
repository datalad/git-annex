#!/bin/sh
# git-annex external special remote program
# 
# This is basically the same as git-annex's built-in directory special remote.
# 
# Install in PATH as git-annex-remote-directory
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
	ask GETCONFIG "$1"
}

# Sets LOC to the location to use to store a key.
calclocation () {
	ask DIRHASH "$1"
	LOC="$mydirectory/$RET/$1"
}

# Asks for some value, and stores it in RET
ask () {
	echo "$1" "$2"
	read resp
	# Tricky POSIX shell code to split first word of the resp,
	# preserving all other whitespace
	case "${resp%% *}" in
		VALUE)
			RET="$(echo "$resp" | sed 's/^VALUE \?//')"
		;;
		*)
			RET=""
		;;
	esac
}

# This has to come first, to get the protocol started.
echo VERSION 1

while read line; do
	set -- $line
	case "$1" in
		INITREMOTE)
			# Do anything necessary to create resources
			# used by the remote. Try to be idempotent.
			# 
			# Use GETCONFIG to get any needed configuration
			# settings, and SETCONFIG to set any persistent
			# configuration settings.
			# 
			# (Note that this is not run every time, only when
			# git annex initremote or git annex enableremote is
			# run.)
			getconfig directory
			mydirectory="$RET"
			if [ -z "$mydirectory" ]; then
				echo INITREMOTE-FAILURE "You need to set directory="
			else
				mkdir -p "$mydirectory"
				echo INITREMOTE-SUCCESS
			fi
		;;
		PREPARE)
			# Use GETCONFIG to get configuration settings,
			# and do anything needed to get ready for using the
			# special remote here.
			getconfig directory
			mydirectory="$RET"
			echo PREPARE-SUCCESS
		;;
		TRANSFER)
			key="$3"
			file="$4"
			case "$2" in
				STORE)
					# Store the file to a location
					# based on the key.
					# XXX when possible, send PROGRESS
					calclocation "$key"
					mkdir -p "$(dirname "$LOC")"
					if runcmd cp "$file" "$LOC"; then
						echo TRANSFER-SUCCESS STORE "$key"
					else
						echo TRANSFER-FAILURE STORE "$key"
					fi
				;;
				RETRIEVE)
					# Retrieve from a location based on
					# the key, outputting to the file.
					# XXX when easy to do, send PROGRESS
					calclocation "$key"
					if runcmd cp "$LOC" "$file"; then
						echo TRANSFER-SUCCESS RETRIEVE "$key"
					else
						echo TRANSFER-FAILURE RETRIEVE "$key"
					fi
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
					# When the directory does not exist,
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
			# key that is not present.
			if [ -e "$LOC" ]; then
				if runcmd rm -f "$LOC"; then
					echo REMOVE-SUCCESS "$key"
				else
					echo REMOVE-FAILURE "$key"
				fi
			else
				echo REMOVE-SUCCESS "$key"
			fi
		;;
		*)
			# The requests listed above are all the ones
			# that are required to be supported, so it's fine
			# to say that any other request is unsupported.
			echo UNSUPPORTED-REQUEST
		;;
	esac	
done

# XXX anything that needs to be done at shutdown can be done here
