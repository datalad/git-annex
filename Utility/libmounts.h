/* Include appropriate headers for the OS, and define what will be used. */
#if defined (__FreeBSD__) || defined (__APPLE__)
# include <sys/param.h>
# include <sys/ucred.h>
# include <sys/mount.h>
# define GETMNTINFO
#else
#if defined __ANDROID__
/* Android is handled by the Haskell code, not here. */
# define UNKNOWN
#else
#if defined (__linux__) || defined (__FreeBSD_kernel__)
/* Linux or Debian kFreeBSD */
#include <mntent.h>
# define GETMNTENT
#else
# warning mounts listing code not available for this OS
# define UNKNOWN
#endif
#endif
#endif

#include <stdio.h>

#ifndef GETMNTENT
struct mntent {
	char *mnt_fsname;
	char *mnt_dir;
	char *mnt_type;
	char *mnt_opts; /* not filled in */
	int mnt_freq; /* not filled in */
	int mnt_passno; /* not filled in */
};
#endif

FILE *mounts_start (void);
int mounts_end (FILE *fp);
struct mntent *mounts_next (FILE *fp);
