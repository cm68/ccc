/*
 * scanf - formatted input
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */

#include	<stdio.h>

extern int	_doscan();

scanf(fmt, args)
char *	fmt;
int	args;
{
	return _doscan(stdin, fmt, &args);
}
