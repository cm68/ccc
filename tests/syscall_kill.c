/*
 * syscall_kill test
 */
#include <unistd.h>

int main(void)
{
	int pid;

	pid = getpid();
	/* sending signal 0 just checks if process exists */
	if (kill(pid, 0) < 0)
		return 1;
	return 0;
}
