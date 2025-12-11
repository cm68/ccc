/*
 * syscall_getpid test
 */
#include <unistd.h>

int main(void)
{
	int pid;

	pid = getpid();
	if (pid <= 0)
		return 1;
	return 0;
}
