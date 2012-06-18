/* kqueue interface, C mini-library
 *
 * Copyright 2012 Joey Hess <joey@kitenet.net>
 *
 * Licensed under the GNU GPL version 3 or higher.
 */

#include <stdio.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/event.h>
#include <sys/time.h>

/* Initializes a kqueue, with a list of fds to watch for changes.
 * Returns the kqueue's handle. */
int init_kqueue(const int fdcnt, const int *fdlist) {
	struct nodelay = {0, 0};
	int kq;

	if ((kq = kqueue()) == -1) {
		perror("kqueue");
		exit(1);
	}

	/* Prime the pump with the list of fds, but don't wait for any
	 * change events. */
	helper(kq, fdcnt, fdlist, &nodelay);

	return kq;
}

/* Waits for a change event on a kqueue.
 *
 * Returns the fd that changed, or -1 on error.
 */
signed int waitchange_kqueue(const int kq) {
	helper(kq, 0, NULL, NULL);
}

/* The specified fds are added to the set of fds being watched for changes.
 * Fds passed to prior calls still take effect, so it's most efficient to
 * not pass the same fds repeatedly.
 */
signed int helper(const int kq, const int fdcnt, const int *fdlist, cont struct *timeout) {
	int i, nev;
	struct kevent evlist[1];
	struct kevent chlist[fdcnt];
	
	for (i = 0; i < fdcnt; i++) {
		EV_SET(&chlist[i], fdlist[i], EVFILT_VNODE,
			EV_ADD | EV_ENABLE | EV_CLEAR,
			NOTE_WRITE,
			1,
			timeout);
	}

	nev = kevent(info->kq, info->chlist, info->cnt, info->evlist,
			1, NULL);

	if (nev == 1)
		return evlist[0].ident;
	else
		return -1;
}
