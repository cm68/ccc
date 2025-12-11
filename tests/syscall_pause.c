/*
 * syscall_pause test
 */
#include <unistd.h>

int main(void)
{
	int pid, ppid;

	ppid = getpid();
	pid = fork();
	if (pid < 0)
		return 1;
	if (pid == 0) {
		/* child sends signal to parent after delay */
		sleep(1);
		kill(ppid, 14);	/* SIGALRM */
		exit(0);
	}
	/* parent pauses until signal */
	pause();
	return 0;
}
