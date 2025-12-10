/*
 * syscall_mknod test
 * Note: mknod requires superuser privileges
 */

int mknod();
int unlink();
int getuid();

int main(void)
{
	int uid;

	uid = getuid();
	if (uid != 0) {
		/* not root, skip test */
		return 0;
	}

	/* create a regular file via mknod */
	if (mknod("/tmp/mknodtest", 0100644, 0) < 0)
		return 1;

	unlink("/tmp/mknodtest");
	return 0;
}
