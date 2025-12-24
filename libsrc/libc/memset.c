/*
 * set memory to a character value
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */

memset(p, n, c)
register char *	p;
register int	n;
char		c;
{
	while (n--)
		*p++ = c;
}
