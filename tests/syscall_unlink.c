/*
 * syscall_unlink test
 */

int creat();
int close();
int unlink();
int access();

int main(void)
{
	int fd;

	fd = creat("/tmp/unlinktest", 0644);
	if (fd < 0)
		return 1;
	close(fd);

	if (unlink("/tmp/unlinktest") < 0)
		return 2;

	/* verify it's gone */
	if (access("/tmp/unlinktest", 0) == 0)
		return 3;

	return 0;
}
