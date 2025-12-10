/*
 * syscall_open test
 */

int open();
int close();

int main(void)
{
	int fd;

	fd = open("/etc/passwd", 0);
	if (fd < 0)
		return 1;
	close(fd);
	return 0;
}
