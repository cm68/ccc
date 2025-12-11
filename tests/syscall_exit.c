/*
 * syscall_exit test
 */
#include <unistd.h>

int main(void)
{
	exit(0);
	return 1;	/* should not reach */
}
