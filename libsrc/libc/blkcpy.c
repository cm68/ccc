/*
 * copy sp to dp for n bytes
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
blkcpy(dp, sp, n)
	register char *dp, *sp;
	register unsigned n;
{
	while (n--)
		*dp++ = *sp++;
}

