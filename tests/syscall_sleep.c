/*
 * syscall_sleep test
 */
#include <unistd.h>

int main(void)
{
	/* sleep for 1 second */
	sleep(1);
	return 0;
}
