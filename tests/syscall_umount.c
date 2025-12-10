/*
 * syscall_umount test
 * Note: umount requires superuser privileges
 */

int getuid();

int main(void)
{
	int uid;

	uid = getuid();
	if (uid != 0) {
		/* not root, skip test */
		return 0;
	}

	/* actual umount test would need a mounted fs */
	/* just verify we can compile and link */
	return 0;
}
