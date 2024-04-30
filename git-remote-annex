#!/bin/sh
URL="$2"

TOPDIR="$(echo "$URL" | sed 's/^annex:\/\///')"

set -x

rm -f $GIT_DIR/push-response

# Unfortunately, git bundle omits prerequisites that are omitted once,
# even if they are used by a later ref.
# For example, where x is a ref that points at A, and y is a ref
# that points at B (which has A as its parent), git bundle x A..y
# will omit inclding the x ref in the bundle at all.
check_prereq () {
	# So, if a sha is one of the other refs that will be included in the
	# bundle, it cannot be treated as a prerequisite.
	if git show-ref $push_refs | grep -v " $2$" | awk '{print $1}' | grep -q "$1"; then
		echo "$2"
	else
		# And, if one of the other refs that will be included in the bundle
		# is an ancestor of the sha, it cannot be treated as a prerequisite.
		if [ -n "$(for x in $(git show-ref $push_refs | grep -v " $2$" | awk '{print $1}'); do git log --oneline -n1 $x..$1; done)" ]; then
			echo "$2"
		else
			echo "$1..$2"
		fi
	fi
}

addnewbundle () {
	sha1=$(sha1sum $TOPDIR/new.bundle | awk '{print $1}')
	mv $TOPDIR/new.bundle "$TOPDIR/$sha1.bundle"
	echo "$sha1.bundle" >> $TOPDIR/MANIFEST
}

while read foo; do
	case "$foo" in
		capabilities)
			echo fetch
			echo push
			echo
		;;
		list*)
			if [ -e "$TOPDIR/MANIFEST" ]; then
				for f in $(cat $TOPDIR/MANIFEST); do
					git bundle list-heads $TOPDIR/$f >> $GIT_DIR/listed-refs-new
					if [ "$foo" = "list for-push" ]; then
						# Get all the objects from the bundle. This is done here so that
						# refs/namespaces/mine can be updated with what was listed,
						# and so what when a full repush needs to be done, everything
						# gets pushed.
						git bundle unbundle "$TOPDIR/$f" >/dev/null 2>&1
					fi
				done
				perl -e 'while (<>) { if (m/(.*) (.*)/) { $seen{$2}=$1 } }; foreach my $k (keys %seen) { print "$seen{$k} $k\n" }' < $GIT_DIR/listed-refs-new > $GIT_DIR/listed-refs
				rm -f $GIT_DIR/listed-refs-new

				# when listing for a push, update refs/namespaces/mine to match what was
				# listed. This is necessary in order for a full repush to know what to push.
				if [ "$foo" = "list for-push" ]; then
					for r in $(git for-each-ref refs/namespaces/mine/ | awk '{print $3}'); do
						git update-ref -d "$r"
					done
					IFS="
					"
					for x in $(cat $GIT_DIR/listed-refs); do
						sha="$(echo "$x" | cut -d ' ' -f 1)"
						r="$(echo "$x" | cut -d ' ' -f 2)"
						git update-ref "$r" "$sha"
					done
					unset IFS
				fi

				# respond to git with a list of refs
				sed 's/refs\/namespaces\/mine\///' $GIT_DIR/listed-refs
				# $GIT_DIR/listed-refs is later checked in push
			else
				rm -f $GIT_DIR/listed-refs
				touch $GIT_DIR/listed-refs		
			fi
			echo
		;;
		fetch*)
			dofetch=1
		;;
		push*)
			set -- $foo
			x="$2"
			# src ref is prefixed with a + in a forced push
			forcedpush=""
			if echo "$x" | cut -d : -f 1 | egrep -q '^\+'; then
				forcedpush=1
			fi
			srcref="$(echo "$x" | cut -d : -f 1 | sed 's/^\+//')"
			dstref="$(echo "$x" | cut -d : -f 2)"
			# Need to create a bundle containing $dstref, but
			# don't want to overwrite that ref in the local
			# repo. Unfortunately, git bundle does not support
			# GIT_NAMESPACE, so it's not possible to do that
			# without making a clone of the whole git repo.
			# Instead, just create a ref under the namespace
			# refs/namespaces/mine/ that will be put in the
			# bundle.
			mydstref=refs/namespaces/mine/"$dstref"
			if [ -z "$srcref" ]; then
				# To delete a ref, have to do a repush of
				# all remaining refs.
				REPUSH=1
				git update-ref -d "$mydstref"
				touch $GIT_DIR/push-response
				echo "ok $dstref" >> $GIT_DIR/push-response
			else
				if [ ! "$forcedpush" ]; then
					# check if the push would overwrite
					# work in the ref currently stored in the
					# remote, if so refuse to do it
					prevsha=$(grep " $mydstref$"  $GIT_DIR/listed-refs | awk '{print $1}')
					newsha=$(git rev-parse "$srcref")
					if [ -n "$prevsha" ] && [ "$prevsha" != "$newsha" ] && [ -z "$(git log --oneline $prevsha..$newsha 2>/dev/null)" ]; then
						touch $GIT_DIR/push-response
						echo "error $dstref non-fast-forward" >> $GIT_DIR/push-response
					else
						touch $GIT_DIR/push-response
						echo "ok $dstref" >> $GIT_DIR/push-response
						git update-ref "$mydstref" "$srcref"
						push_refs="$mydstref $push_refs"
					fi
				else	
					git update-ref "$mydstref" "$srcref"
					touch $GIT_DIR/push-response
					echo "ok $dstref" >> $GIT_DIR/push-response
					push_refs="$mydstref $push_refs"
				fi
			fi
			dopush=1
		;;
		# docs say a blank line ends communication, but that's not
		# accurate, actually a blank line comes after a series of
		# fetch or push commands, and also according to the docs,
		# another series of commands could follow
		"")
			if [ "$dofetch" ]; then
				if [ -e "$TOPDIR/MANIFEST" ]; then
					for f in $(cat $TOPDIR/MANIFEST); do
						git bundle unbundle "$TOPDIR/$f" >/dev/null 2>&1
					done
				fi
				echo
				dofetch=""
			fi
			if [ "$dopush" ]; then
				if [ -z "$(git for-each-ref refs/namespaces/mine/)" ]; then
					# deleted all refs
					if [ -e "$TOPDIR/MANIFEST" ]; then
						for f in $(cat $TOPDIR/MANIFEST); do
							rm "$TOPDIR/$f"
						done
						rm $TOPDIR/MANIFEST
						touch $TOPDIR/MANIFEST
					fi
				else
					# set REPUSH=1 to do a full push
					# rather than incremental
					if [ "$REPUSH" ]; then
						rm $TOPDIR/MANIFEST
						rm $TOPDIR/*.bundle
						git for-each-ref refs/namespaces/mine/ | awk '{print $3}' | \
							git bundle create --quiet $TOPDIR/new.bundle --stdin
						addnewbundle
					else
						# incremental bundle
						for r in $push_refs; do
							newsha=$(git show-ref "$r" | awk '{print $1}')
							oldsha=$(grep " $r$" $GIT_DIR/listed-refs | awk '{print $1}')
							if [ -n "$oldsha" ]; then
								# include changes from $oldsha to $r when there are some
								if [ -n "$(git log --oneline $oldsha..$r)" ]; then
									check_prereq "$oldsha" "$r"
								else
									if [ "$oldsha" = "$newsha" ]; then
										# $r is unchanged from last push, so no need to push it
										:
									else
										# $oldsha is not a parent of $r, so 
										# include $r and all its parents
										# XXX (this could be improved by checking other refs that were pushed
										# and only including changes from them)
										echo "$r"
									fi
								fi	
							else
								# no old version was pushed so include $r and all its parents
								# XXX (this could be improved by checking other refs that were pushed
								# and only including changes from them)
								echo "$r"
							fi
						done > $GIT_DIR/tobundle
						if [ -s "$GIT_DIR/tobundle" ]; then
							git bundle create --quiet $TOPDIR/new.bundle --stdin < "$GIT_DIR/tobundle"
							addnewbundle
						fi
					fi
				fi
				cat $GIT_DIR/push-response
				rm -f $GIT_DIR/push-response
				echo 
				dopush=""
			fi
		;;
	esac
done
