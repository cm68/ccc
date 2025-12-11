/*
 * syscall_creat test
 */
#include <unistd.h>

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
