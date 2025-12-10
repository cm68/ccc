/*
 * syscall_nice test
 */

int nice();

int main(void)
{
	/* increase priority by 1 */
	nice(1);
	return 0;
}
