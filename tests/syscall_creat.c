/*
 * syscall_creat test
 */

int creat();
int close();
int unlink();

int main(void)
{
	int fd;

	fd = creat("/tmp/testfile", 0644);
	if (fd < 0)
		return 1;
	close(fd);
	unlink("/tmp/testfile");
	return 0;
}
