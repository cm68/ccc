/*
 * syscall_mount test
 * Note: mount requires superuser privileges
 */
#include <unistd.h>

int main(void)
{
	int uid;

	uid = getuid();
	if (uid != 0) {
		/* not root, skip test */
		return 0;
	}

	/* actual mount test would need a real device */
	/* just verify we can compile and link */
	return 0;
}
