/*
 * syscall_chmod test
 */
#include <unistd.h>

int main(void)
{
	int fd;

	fd = creat("/tmp/chmodtest", 0644);
	if (fd < 0)
		return 1;
	close(fd);

	if (chmod("/tmp/chmodtest", 0600) < 0) {
		unlink("/tmp/chmodtest");
		return 2;
	}

	unlink("/tmp/chmodtest");
	return 0;
}
