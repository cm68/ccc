/*
 * syscall_exec test
 */

int fork();
int wait();
int exec();
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
		exec("/bin/true", argv);
		exit(99);	/* exec failed */
	}
	/* parent waits */
	wait(&status);
	return 0;
}
