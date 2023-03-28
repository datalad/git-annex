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

# Stores a value in the remote's configuration.
setconfig () {
	echo SETCONFIG "$1" "$2"
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

# This remote doesn't need credentials to access it,
# but many of them will. Here's how to handle requiring the user
# set MYPASSWORD and MYLOGIN when running initremote. The creds
# will be stored securely for later use, so the user only needs
# to provide them once.
setupcreds () {
	if [ -z "$MYPASSWORD" ] || [ -z "$MYLOGIN" ]; then
		echo INITREMOTE-FAILURE "You need to set MYPASSWORD and MYLOGIN environment variables when running initremote."
	else
		echo SETCREDS mycreds "$MYLOGIN" "$MYPASSWORD"	
		echo INITREMOTE-SUCCESS
	fi
}

getcreds () {
	echo GETCREDS mycreds
	read resp
	case "${resp%% *}" in
		CREDS)
			MYLOGIN="$(echo "$resp" | sed 's/^CREDS \([^ ]*\) .*/\1/')"
			MYPASSWORD="$(echo "$resp" | sed 's/^CREDS [^ ]* //')"
		;;
	esac

}

dostore () {
	local key="$1"
	local file="$2"
	local loc="$3"
	mkdir -p "$(dirname "$loc")"
	# Store in temp file first, so that CHECKPRESENT does not see it
	# until it is all stored.
	mkdir -p "$mydirectory/tmp"
	tmp="$mydirectory/tmp/$key"
	# XXX when at all possible, send PROGRESS while transferring
	# the file.
	rm -f "$tmp"
	if runcmd cp "$file" "$tmp" \
	   && runcmd mv -f "$tmp" "$loc"; then
		echo TRANSFER-SUCCESS STORE "$key"
	else
		echo TRANSFER-FAILURE STORE "$key"
	fi
	rmdir "$mydirectory/tmp"
}

doretrieve () {
	local key="$1"
	local file="$2"
	local loc="$3"
	
	# XXX when easy to do, send PROGRESS while transferring the file
	if [ -e "$loc" ]; then
		if runcmd cp "$loc" "$file"; then
			echo TRANSFER-SUCCESS RETRIEVE "$key"
		else
			echo TRANSFER-FAILURE RETRIEVE "$key"
		fi
	else
		echo TRANSFER-FAILURE RETRIEVE "$key"
	fi
}

docheckpresent () {
	local key="$1"
	local loc="$2"

	if [ -e "$loc" ]; then
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
}

doremove () {
	local key="$1"
	local loc="$2"

	# Note that it's not a failure to remove a
	# file that is not present.
	if [ -e "$loc" ]; then
		if runcmd rm -f "$loc"; then
			echo REMOVE-SUCCESS "$key"
		else
			echo REMOVE-FAILURE "$key"
		fi
	else
		echo REMOVE-SUCCESS "$key"
	fi
}

# This has to come first, to get the protocol started.
echo VERSION 2

while read line; do
	set -- $line
	case "$1" in
		LISTCONFIGS)
			# One CONFIG line for each setting that we GETCONFIG
			# later.
			echo CONFIG directory store data here
			echo CONFIGEND
		;;
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

			# The directory provided by the user
			# could be relative; make it absolute,
			# and store that.
			getconfig directory
			mydirectory="$(readlink -f "$RET")" || true
			setconfig directory "$mydirectory"
			if [ -z "$mydirectory" ]; then
				echo INITREMOTE-FAILURE "You need to set directory="
			else
				if mkdir -p "$mydirectory"; then
					setupcreds
				else
					echo INITREMOTE-FAILURE "Failed to write to $mydirectory"
				fi
			fi
		;;
		PREPARE)
			# Use GETCONFIG to get configuration settings,
			# and do anything needed to get ready for using the
			# special remote here.
			getcreds
			getconfig directory
			mydirectory="$RET"
			if [ -d "$mydirectory" ]; then
				echo PREPARE-SUCCESS
			else
				echo PREPARE-FAILURE "$mydirectory not found"
			fi
		;;
		TRANSFER)
			op="$2"
			key="$3"
			shift 3
			file="$@"
			case "$op" in
				STORE)
					# Store the file to a location
					# based on the key.
					calclocation "$key"
					dostore "$key" "$file" "$LOC"
				;;
				RETRIEVE)
					# Retrieve from a location based on
					# the key, outputting to the file.
					calclocation "$key"
					doretrieve "$key" "$file" "$LOC"
				;;
			esac
		;;
		CHECKPRESENT)
			key="$2"
			calclocation "$key"
			docheckpresent "$key" "$LOC"
		;;
		REMOVE)
			key="$2"
			calclocation "$key"
			doremove "$key" "$LOC"
		;;
		# The requests listed above are all the ones
		# that are required to be supported, so it's fine
		# to respond to any others with UNSUPPORTED-REQUEST.

		# Let's also support exporting...
		EXPORTSUPPORTED)
			echo EXPORTSUPPORTED-SUCCESS
		;;
		EXPORT)
			shift 1
			exportlocation="$mydirectory/$@"
			# No response to this one; this value is used below.
		;;
		TRANSFEREXPORT)
			op="$2"
			key="$3"
			shift 3
			file="$@"
			case "$op" in
				STORE)
					# Store the file to the exportlocation
					dostore "$key" "$file" "$exportlocation"
				;;
				RETRIEVE)
					# Retrieve from the exportlocation,
					# outputting to the file.
					doretrieve "$key" "$exportlocation" "$file"
				;;
			esac
		;;
		CHECKPRESENTEXPORT)
			key="$2"
			docheckpresent "$key" "$exportlocation"
		;;
		REMOVEEXPORT)
			key="$2"
			doremove "$key" "$exportlocation"
		;;
		REMOVEEXPORTDIRECTORY)
			shift 1
			dir="$@"
			if [ ! -d "$dir" ] || rm -rf "$mydirectory/$dir"; then
				echo REMOVEEXPORTDIRECTORY-SUCCESS
			else
				echo REMOVEEXPORTDIRECTORY-FAILURE
			fi
		;;
		RENAMEEXPORT)
			key="$2"
			shift 2
			newexportlocation="$mydirectory/$@"
			mkdir -p "$(dirname "$newexportlocation")"
			if runcmd mv -f "$exportlocation" "$newexportlocation"; then
				echo RENAMEEXPORT-SUCCESS "$key"
			else
				echo RENAMEEXPORT-FAILURE "$key"
			fi
		;;

		# This is optional, only provided as an example.
		GETINFO)
			echo INFOFIELD "repository location"
			echo INFOVALUE "$mydirectory"
			echo INFOFIELD "login"
			echo INFOVALUE "$MYLOGIN"
			echo INFOEND
		;;

		*)
			echo UNSUPPORTED-REQUEST
		;;
	esac	
done

# XXX anything that needs to be done at shutdown can be done here
