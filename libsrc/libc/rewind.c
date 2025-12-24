/*
 * rewind a stdio stream
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<stdio.h>

rewind(stream)
FILE *	stream;
{
	fseek(stream, 0L, 0);
}
