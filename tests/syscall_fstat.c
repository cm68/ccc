/*
 * syscall_fstat test
 */

int open();
int close();
int fstat();

char statbuf[36];

int main(void)
{
	int fd;

	fd = open("/etc/passwd", 0);
	if (fd < 0)
		return 1;

	if (fstat(fd, statbuf) < 0) {
		close(fd);
		return 2;
	}

	close(fd);
	return 0;
}
