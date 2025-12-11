/*
 * syscall_dup test
 */
#include <unistd.h>

int main(void)
{
	int fd;

	fd = dup(1);	/* dup stdout */
	if (fd < 0)
		return 1;
	close(fd);
	return 0;
}
