/*
 * syscall_fork test
 */
#include <unistd.h>

int main(void)
{
	int pid, status;

	pid = fork();
	if (pid < 0)
		return 1;
	if (pid == 0) {
		/* child */
		exit(42);
	}
	/* parent */
	wait(&status);
	return 0;
}
