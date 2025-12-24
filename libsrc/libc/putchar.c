/*
 * routines for getchar and putchar, usually macros
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */

#include	<stdio.h>

#undef	getchar
#undef	putchar

getchar()
{
	return(fgetc(stdin));
}

putchar(c)
{
	return(fputc(c, stdout));
}
