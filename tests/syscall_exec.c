/*
 * syscall_exec test
 */
#include <unistd.h>

char *args[] = { "/bin/true", 0 };
int main(void)
{
	int pid, status;

	pid = fork();
	if (pid < 0)
		return 1;
	if (pid == 0) {
		/* child execs /bin/true */
		exec("/bin/true", args);
		exit(99);	/* exec failed */
	}
	/* parent waits */
	wait(&status);
	return 0;
}
