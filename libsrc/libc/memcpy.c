/*
 * copy n bytes from s to d
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */

memcpy(d, s, n)
register char *	d, * s;
register int	n;
{
	while(n--)
		*d++ = *s++;
}
