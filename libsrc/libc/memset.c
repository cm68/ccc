/*
 * set memory to a character value
 *
 */

memset(p, n, c)
register char *	p;
register int	n;
char		c;
{
	while (n--)
		*p++ = c;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
