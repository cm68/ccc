/*
 * syscall_close test
 */

int open();
int close();

int main(void)
{
	int fd;

	fd = open("/etc/passwd", 0);
	if (fd < 0)
		return 1;
	if (close(fd) < 0)
		return 2;
	return 0;
}
