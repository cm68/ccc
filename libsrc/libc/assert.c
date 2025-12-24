/*
 * print an assertion failure to stderr, 
 * passing a line number, file name and message
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<assert.h>
#include	<stdio.h>

void
_fassert(line, file, exp)
char *	file, * exp;
int	line;
{
	fprintf(stderr, "Assertion failed: %s line %d: \"%s\"\n", 
		file, line, exp);
	abort();
}
