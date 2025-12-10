/*
 * syscall_write test
 */

int write();

int main(void)
{
	int n;

	n = write(1, "ok\n", 3);
	if (n != 3)
		return 1;
	return 0;
}
