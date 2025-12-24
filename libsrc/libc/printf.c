/*
 * printf to stdout
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<stdio.h>

extern int	_doprnt();

printf(f, a)
char *	f;
int	a;
{
	return(_doprnt(stdout, f, &a));
}
