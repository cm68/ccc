/*
 * syscall_gtty test
 */
#include <unistd.h>

char ttybuf[6];
int main(void)
{
	/* get tty settings for stdin */
	if (gtty(0, ttybuf) < 0)
		return 1;
	return 0;
}
