/*
 * this is a minimal unix library
 */

#include "ccc.h"

#ifdef __SDCC
char *
strdup(char *s)
{
	char *ret;
	ret = malloc(strlen(s) + 1);
	strcpy(ret, s);
	return (ret);
}

int 
write(int fd, char *buf, int len)
{
    return len;
}

int 
read(int fd, char *buf, int len)
{
    return len;
}

int
creat(char *fn, int mode)
{
    return 0;
}

int
open(char *fn, int mode)
{
    return 0;
}

int
close(int fd)
{
    return 0;
}

void
perror(char *msg)
{
    return;
}

void
putchar(int c)
{
    return;
}

void
exit(int exitcode)
{
    return;
}

#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

