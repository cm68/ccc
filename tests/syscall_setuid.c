/*
 * syscall_setuid test
 */
#include <unistd.h>

int main(void)
{
	int uid;

	uid = getuid();
	/* set to current uid (should always succeed) */
	if (setuid(uid) < 0)
		return 1;
	return 0;
}
