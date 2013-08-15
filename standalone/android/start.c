/* Installed as lib.start.so, this bootstraps a working busybox and uses
 * it to run lib.runshell.so. */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

void chopdir (char *s) {
	char *p=strrchr(s, '/');
	if (p == NULL) {
		fprintf(stderr, "cannot find directory in %s", s);
		exit(1);
	}
	p[0] = '\0';
}

main () {
	char buf[1024];
	char *p;
	struct stat st_buf;

	/* Get something like /data/data/ga.androidterm/lib/lib.start.so */
	int n=readlink("/proc/self/exe", buf, 1023);
	if (n < 1) {
		fprintf(stderr, "failed to find own name");
		exit(1);
	}
	buf[n] = '\0';

	/* Change directory to something like /data/data/ga.androidterm */
	chopdir(buf);
	chopdir(buf);
	if (chdir(buf) != 0) {
		perror("chdir");
		exit(1);
	}

	if (stat("lib/lib.busybox.so", &st_buf) != 0) {
		/* TODO my lib dir should be in LD_LIBRARY_PATH; check that */
		fprintf(stderr, "Falling back to hardcoded app location; cannot find expected files in %s\n", buf);
		if (chdir("/data/data/ga.androidterm") != 0) {
			perror("chdir");
			exit(1);
		}
	}

	/* If this is the first run, set up busybox symlink,
	 * which allows busybox to run. */
	if (stat("busybox", &st_buf) != 0) {
		if (symlink("lib/lib.busybox.so", "busybox") != 0) {
			/* Just in case! */
			if (link("lib/lib.busybox.so", "busybox") != 0) {
				perror("link busybox");
				exit(1);
			}
		}
	}

	execl("./busybox", "./busybox", "sh", "lib/lib.runshell.so", NULL);
	perror("error running busybox sh");
}
