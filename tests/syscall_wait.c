/*
 * syscall_wait test
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
		/* child exits with code 5 */
		exit(5);
	}
	/* parent waits for child */
	if (wait(&status) < 0)
		return 2;
	return 0;
}
