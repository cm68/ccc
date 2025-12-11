/*
 * syscall_read test
 */
#include <unistd.h>

char buf[32];
int main(void)
{
	int fd, n;

	fd = open("/etc/passwd", 0);
	if (fd < 0)
		return 1;
	n = read(fd, buf, 10);
	if (n < 0) {
		close(fd);
		return 2;
	}
	close(fd);
	return 0;
}
