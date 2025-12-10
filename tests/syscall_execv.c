/*
 * syscall_execv test
 */

int fork();
int wait();
int execv();
void exit();

char *argv[] = { "/bin/true", 0 };

int main(void)
{
	int pid, status;

	pid = fork();
	if (pid < 0)
		return 1;
	if (pid == 0) {
		/* child execs /bin/true */
		execv("/bin/true", argv);
		exit(99);	/* exec failed */
	}
	/* parent waits */
	wait(&status);
	return 0;
}
