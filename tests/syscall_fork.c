/*
 * syscall_fork test
 */

int fork();
int wait();
void exit();

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
