/*
 * syscall_stime test
 * Note: stime requires superuser privileges
 */
#include <unistd.h>

long timeval;
int main(void)
{
	int uid;

	uid = getuid();
	if (uid != 0) {
		/* not root, skip test */
		return 0;
	}

	/* get current time */
	if (time(&timeval) < 0)
		return 1;
	/* set it back (no change) */
	if (stime(&timeval) < 0)
		return 2;
	return 0;
}
