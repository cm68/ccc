/*
 * puts and fputs
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */

#include	<stdio.h>

fputs(s, f)
char *		s;
register FILE *	f;
{
	while (*s)
		putc(*s++, f);
}

puts(s)
char *		s;
{
	fputs(s, stdout);
	putchar('\n');
}
