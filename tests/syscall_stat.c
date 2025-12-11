/*
 * syscall_stat test
 */
#include <unistd.h>

char statbuf[36];
int main(void)
{
	if (stat("/etc/passwd", statbuf) < 0)
		return 1;
	return 0;
}
