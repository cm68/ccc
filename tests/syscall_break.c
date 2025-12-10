/*
 * syscall_break test
 */

int brk();
char *sbrk();

int main(void)
{
	char *cur, *new;

	/* get current break */
	cur = sbrk(0);
	if (cur == (char *)-1)
		return 1;

	/* extend by 256 bytes */
	new = sbrk(256);
	if (new == (char *)-1)
		return 2;

	return 0;
}
