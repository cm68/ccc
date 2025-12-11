/*
 * syscall_chdir test
 */
#include <unistd.h>

int main(void)
{
	if (chdir("/tmp") < 0)
		return 1;
	if (chdir("/") < 0)
		return 2;
	return 0;
}
