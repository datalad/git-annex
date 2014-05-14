/* kqueue interface, C mini-library
 *
 * Copyright 2012 Joey Hess <joey@kitenet.net>
 *
 * License: BSD-2-clause
 */

#include <stdio.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <errno.h>

/* The specified fds are added to the set of fds being watched for changes.
 * Fds passed to prior calls still take effect, so it's most efficient to
 * not pass the same fds repeatedly.
 *
 * Returns the fd that changed, or -1 on error.
 */
signed int helper(const int kq, const int fdcnt, const int *fdlist, int nodelay) {
	int i, nev;
	struct kevent evlist[1];
	struct kevent chlist[fdcnt];
	struct timespec avoiddelay = {0, 0};
	struct timespec *timeout = nodelay ? &avoiddelay : NULL;
	
	for (i = 0; i < fdcnt; i++) {
		EV_SET(&chlist[i], fdlist[i], EVFILT_VNODE,
			EV_ADD | EV_ENABLE | EV_CLEAR,
			NOTE_WRITE,
			0, 0);
	}

	nev = kevent(kq, chlist, fdcnt, evlist, 1, timeout);
	if (nev == 1)
		return evlist[0].ident;
	else
		return -1;
}

/* Initializes a new, empty kqueue. */
int init_kqueue() {
	int kq;
	if ((kq = kqueue()) == -1) {
		perror("kqueue");
		exit(1);
	}
	return kq;
}

/* Adds fds to the set that should be watched. */
void addfds_kqueue(const int kq, const int fdcnt, const int *fdlist) {
	helper(kq, fdcnt, fdlist, 1);
}

/* Waits for a change event on a kqueue. */
signed int waitchange_kqueue(const int kq) {
	return helper(kq, 0, NULL, 0);
}

/*
main () {
	int list[1];
	int kq;
	list[0]=open(".", O_RDONLY);
	kq = init_kqueue();
	addfds_kqueue(kq, 1, list)
	printf("change: %i\n", waitchange_kqueue(kq));
}
*/
