/* disk free space checking, C mini-library
 *
 * Copyright 2012, 2014 Joey Hess <id@joeyh.name>
 *
 * License: BSD-2-clause
 */

/* Include appropriate headers for the OS, and define what will be used to
 * check the free space. */
#if defined (__FreeBSD__)
# include <sys/param.h>
# include <sys/mount.h>
# define STATCALL statfs /* statfs64 not yet tested on a real FreeBSD machine */
# define STATSTRUCT statfs
# define BSIZE f_bsize
#else
#if defined __ANDROID__
# warning free space checking code not available for Android
# define UNKNOWN
#else
#if defined (__linux__) || defined (__APPLE__) || defined (__FreeBSD_kernel__) || (defined (__SVR4) && defined (__sun))
/* Linux or OSX or Debian kFreeBSD or Solaris */
/* This is a POSIX standard, so might also work elsewhere too. */
# include <sys/statvfs.h>
# define STATCALL statvfs
# define STATSTRUCT statvfs
# define BSIZE f_frsize
#else
# warning free space checking code not available for this OS
# define UNKNOWN
#endif
#endif
#endif

#include <errno.h>
#include <stdio.h>

unsigned long long int get(const char *path, int req) {
#ifdef UNKNOWN
	errno = 1;
	return 0;
#else
	unsigned long long int v, blocksize;
	struct STATSTRUCT buf;

	if (STATCALL(path, &buf) != 0)
		return 0; /* errno is set */
	else
		errno = 0;

	switch (req) {
		case 0:
			v = buf.f_blocks;
			break;
		case 1:
			v = buf.f_bavail;
			break;
		default:
			v = 0;
	}

	blocksize = buf.BSIZE;
	return v * blocksize;
#endif
}

/* Checks the amount of disk that is available to regular (non-root) users.
 * (If there's an error, or this is not supported,
 * returns 0 and sets errno to nonzero.)
 */
unsigned long long int diskfree(const char *path) {
	return get(path, 1);
}

/* Gets the total size of the disk. */
unsigned long long int disksize(const char *path) {
	return get(path, 0);
}

/*
main () {
	printf("%lli\n", diskfree("."));
}
*/
