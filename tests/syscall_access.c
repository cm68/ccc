/*
 * syscall_access test
 */

int access();

int main(void)
{
	/* check /etc/passwd exists (mode 0) */
	if (access("/etc/passwd", 0) != 0)
		return 1;
	/* check nonexistent file fails */
	if (access("/nonexistent", 0) == 0)
		return 2;
	return 0;
}
