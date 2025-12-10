/*
 * syscall_chown test
 * Note: chown requires superuser privileges
 */

int creat();
int close();
int chown();
int unlink();
int getuid();

int main(void)
{
	int fd, uid;

	fd = creat("/tmp/chowntest", 0644);
	if (fd < 0)
		return 1;
	close(fd);

	uid = getuid();
	/* try to chown to self - may fail if not root */
	chown("/tmp/chowntest", uid);

	unlink("/tmp/chowntest");
	return 0;
}
