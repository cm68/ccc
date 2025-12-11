/*
 * syscall_seek test
 */
#include <unistd.h>

char buf[10];
int main(void)
{
	int fd, n;

	fd = open("/etc/passwd", 0);
	if (fd < 0)
		return 1;

	/* seek to offset 5 from start */
	if (seek(fd, 5, 0) < 0) {
		close(fd);
		return 2;
	}

	n = read(fd, buf, 5);
	if (n < 0) {
		close(fd);
		return 3;
	}

	close(fd);
	return 0;
}
