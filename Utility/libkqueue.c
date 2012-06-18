/* kqueue interface, C mini-library
 *
 * Copyright 2012 Joey Hess <joey@kitenet.net>
 *
 * Licensed under the GNU GPL version 3 or higher.
 */

#include <errno.h>
#include <stdio.h>

/* Waits for a change event on one of the array of directory fds,
 * and returns the one that changed. */
int waitchange(const int *fds) {
//	if (kqueue(blah, &fds) != 0)
//		return 0; /* errno is set */
//	else
		errno = 0;

	printf("in waitchange!, %i %i\n", fds[0], fds[1]);

	return fds[0];
}
