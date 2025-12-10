/*
 * syscall_stat test
 */

int stat();

char statbuf[36];

int main(void)
{
	if (stat("/etc/passwd", statbuf) < 0)
		return 1;
	return 0;
}
