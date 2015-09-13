#!/bin/sh
# Generate --ghc-options to pass LDFLAGS, CFLAGS, and CPPFLAGS through ghc
# and on to ld, cc, and cpp.
for w in $LDFLAGS; do
	printf -- "-optl%s\n" "$w"
done
for w in $CFLAGS; do
	printf -- "-optc%s\n" "$w"
done
for w in $CPPFLAGS; do
	printf -- "-optc-Wp,%s\n" "$w"
done
