/*
 * j-random utility functions
 * these are mostly prime candidates for assembly code
 */

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

#ifndef PSIZE
#define PSIZE 80
#endif

/* Forward declaration */
int fdprintf(unsigned char fd, const char *fmt, ...);

#ifndef CCC
char patspace[PSIZE];

char *
bitdef(unsigned char v, char **defs)
{
	unsigned char i = 0x01;
	char *patptr;
	unsigned char sep = 0;

	patptr = patspace;
	*patptr = 0;

    while (i) {
		if ((v & i) && *defs) {
			if (sep++) {
				strcat(patptr, ",");
			}
			strcat(patptr, *defs);
		}
        i = i * 2;
        defs++;
	}
	return patptr;
}
#endif

/*
 * append string s at d
 */
void
append(char *d, char *s)
{
    while (*s) {
        *d++ = *s++;
    }
    *d = 0;
}

char
iswhite(unsigned char c)
{
    switch (c) {
    case ' ': case '\t': case '\n':
        return 1;
    default:
        return 0;
    }
}

#ifndef CCC
char xxbuf[200];

void
hexdump(char *tag, char *h, int l)
{
    int i;
    char *z = xxbuf;
    unsigned char c;

    strcpy(xxbuf, tag);

    for (i = 0; i < l; i++) {
        c = h[i];
        if ((i % 16) == 0) {
            fdprintf(2," %s\n%04x  ", xxbuf, i);
            z = xxbuf;
            *z = 0;
        }
        fdprintf(2,"%02x ", c);
        if ((i % 4) == 3) printf(" ");
        if ((c < ' ') || (c > 0x7e)) c = '.';
        *z++ = c;
        *z = 0;
    }
    while ((i++ % 16) != 0) {
        if ((i % 4) == 3) printf(" ");
        fdprintf(2,"   ");
    }
    printf(" %s\n", xxbuf);
}
#endif

/*
 * return the index in an array of the first occurrance of a char
 * return 0xff for miss
 */
unsigned char
lookupc(char *s, char c)
{
    unsigned char i;
    for (i = 0; s[i]; i++) {
        if (c == s[i]) {
            return i;
        }
    }
    return 0xff;
}

#ifndef CCC
/*
 * fdprintf - printf-like function that writes to a Unix file descriptor
 * Uses sprintf to format to a static buffer, then writes via write() syscall
 */
int
fdprintf(unsigned char fd, const char *fmt, ...)
{
    static char buf[1024];
    va_list args;
    int len;
    int result;

    va_start(args, fmt);
    len = vsprintf(buf, fmt, args);
    va_end(args);

    if (len > 0) {
        result = write(fd, buf, len);
        return result;
    }

    return len;
}

/*
 * fdputs - write a string to a Unix file descriptor
 * More efficient than fdprintf for simple string output (no formatting overhead)
 */
int
fdputs(unsigned char fd, const char *s)
{
    int len;

    len = strlen(s);
    if (len > 0) {
        return write(fd, s, len);
    }
    return 0;
}
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
