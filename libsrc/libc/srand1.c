/*
 * initialize random number generator by prompting input
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
extern int	kbhit();

srand1(s)
char *	s;
{
	int	i;
	while (*s)
		putchar(*s++);
	while (!kbhit())
		i++;
	srand(i);
}
