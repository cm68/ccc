/*
 * syscall_getpid test
 */

int getpid();

int main(void)
{
	int pid;

	pid = getpid();
	if (pid <= 0)
		return 1;
	return 0;
}
