/*
 * syscall_pipe test
 */
#include <unistd.h>

int fds[2];
char buf[10];
int main(void)
{
	int n;

	if (pipe(fds) < 0)
		return 1;

	n = write(fds[1], "test", 4);
	if (n != 4) {
		close(fds[0]);
		close(fds[1]);
		return 2;
	}

	n = read(fds[0], buf, 4);
	if (n != 4) {
		close(fds[0]);
		close(fds[1]);
		return 3;
	}

	close(fds[0]);
	close(fds[1]);
	return 0;
}
