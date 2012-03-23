/* disk free space checking, C mini-library
 *
 * Copyright 2012 Joey Hess <joey@kitenet.net>
 *
 * Licensed under the GNU GPL version 3 or higher.
 */

/* Include appropriate headers for the OS, and define what will be used to
 * check the free space. */
#if defined(__APPLE__)
# include <sys/param.h>
# include <sys/mount.h>
/* In newer OSX versions, statfs64 is deprecated, in favor of statfs,
 * which is 64 bit only with a build option -- but statfs64 still works,
 * and this keeps older OSX also supported. */
# define STATCALL statfs64
# define STATSTRUCT statfs64
#else
#if defined (__FreeBSD__)
# include <sys/param.h>
# include <sys/mount.h>
# define STATCALL statfs /* statfs64 not yet tested on a real FreeBSD machine */
# define STATSTRUCT statfs
#else
#if defined (__FreeBSD_kernel__) /* Debian kFreeBSD */
# include <sys/param.h>
# include <sys/mount.h>
# define STATCALL statfs64
# define STATSTRUCT statfs
#else
#if defined (__linux__)
/* This is a POSIX standard, so might also work elsewhere. */
# include <sys/statvfs.h>
# define STATCALL statvfs
# define STATSTRUCT statvfs
#else
# warning free space checking code not available for this OS
# define UNKNOWN
#endif
#endif
#endif
#endif

#include <errno.h>
#include <stdio.h>

/* Checks the amount of disk that is available to regular (non-root) users.
 * (If there's an error, or this is not supported,
 * returns 0 and sets errno to nonzero.)
 */
unsigned long long int diskfree(const char *path) {
#ifdef UNKNOWN
	errno = 1;
	return 0;
#else
	unsigned long long int available, blocksize;
	struct STATSTRUCT buf;

	errno = 0;
	if (STATCALL(path, &buf) != 0)
		return 0; /* errno is set */

	available = buf.f_bavail;
	blocksize = buf.f_bsize;
	return available * blocksize;
#endif
}

/*
main () {
	printf("%lli\n", diskfree("."));
}
*/
