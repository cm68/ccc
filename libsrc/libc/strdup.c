/*
 * strdup - duplicate a string
 */

extern char *malloc();
extern int strlen();

char *
strdup(s)
char *s;
{
	register int len;
	register char *cp;

	len = strlen(s) + 1;
	cp = malloc(len);
	if (cp == (char *)0)
		return ((char *)0);
	bmove(s, cp, len);
	return (cp);
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
