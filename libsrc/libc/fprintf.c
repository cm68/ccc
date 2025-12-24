/*
 * formatted print to a stdio file
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<stdio.h>

extern int	_doprnt();

fprintf(file, f, a)
FILE *	file;
char *	f;
int	a;
{
	return(_doprnt(file, f, &a));
}
