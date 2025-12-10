/*
 * syscall_exit test
 */

void exit();

int main(void)
{
	exit(0);
	return 1;	/* should not reach */
}
