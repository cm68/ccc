/*
 * syscall_nice test
 */
#include <unistd.h>

int main(void)
{
	/* increase priority by 1 */
	nice(1);
	return 0;
}
