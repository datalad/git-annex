/* mounted filesystems, C mini-library
 *
 * Copyright (c) 1980, 1989, 1993, 1994
 *      The Regents of the University of California.  All rights reserved.
 * Copyright (c) 2001
 *      David Rufino <daverufino@btinternet.com>
 * Copyright 2012
 *      Joey Hess <joey@kitenet.net>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include "libmounts.h"

#ifdef GETMNTENT
/* direct passthrough the getmntent */
FILE *mounts_start (void) {
	return setmntent("/etc/mtab", "r");
}
int mounts_end (FILE *fp) {
	return endmntent(fp);
}
struct mntent *mounts_next (FILE *fp) {
	return getmntent(fp);
}
#endif

#ifdef GETMNTINFO
/* getmntent emulation using getmntinfo */
FILE *mounts_start (void) {
	return ((FILE *)0x1); /* dummy non-NULL FILE pointer, not used */
}
int mounts_end (FILE *fp) {
	return 1;
}

static struct mntent _mntent;

static struct mntent *statfs_to_mntent (struct statfs *mntbuf) {
	_mntent.mnt_fsname = mntbuf->f_mntfromname;
	_mntent.mnt_dir = mntbuf->f_mntonname;
	_mntent.mnt_type = mntbuf->f_fstypename;

	_mntent.mnt_opts = '\0';
	_mntent.mnt_freq = 0;
	_mntent.mnt_passno = 0;

	return (&_mntent);
}

static int pos = -1;
static int mntsize = -1;
struct statfs *mntbuf = NULL;

struct mntent *mounts_next (FILE *fp) {

	if (pos == -1 || mntsize == -1)
		mntsize = getmntinfo(&mntbuf, MNT_NOWAIT);
	++pos;
	if (pos == mntsize) {
		pos = mntsize = -1;
		mntbuf = NULL;
		return NULL;
	}

	return (statfs_to_mntent(&mntbuf[pos]));
}
#endif

#ifdef UNKNOWN
/* dummy, do-nothing version */
FILE *mounts_start (void) {
	return ((FILE *)0x1);
}
int mounts_end (FILE *fp) {
	return 1;
}
struct mntent *mounts_next (FILE *fp) {
	return NULL;
}
#endif
