/*
 * j-random utility functions
 * these are mostly prime candidates for assembly code
 */

#include "cc1.h"
#include <stdarg.h>
#ifndef SDCC
#include <unistd.h>
#endif

#define NPATS   2           // and 2 per printf
char patspace[PSIZE * NPATS];
unsigned char patoff;

/*
 * byte bitoff formatter.
 * usage:  char *foo = { "0", "1", "2", "3", 0, "5", "6", "7" };
 *  printf("%s %s\n", bitdef(0xa, foo), bitdef(0x83, foo));
 * null pointer for defs is fine.
 */
char *
bitdef(unsigned char v, char **defs)
{
	unsigned char i;
	char *patptr;
	int sep = 0;

	patptr = &patspace[PSIZE * patoff];
	patoff = (patoff + 1) % NPATS;
	*patptr = 0;

	if (defs) {
		for (i = 0; i < 8; i++) {
			if ((v & (1 << i)) && defs[i]) {
				if (sep++) {
					strcat(patptr, ",");
				}
				strcat(patptr, defs[i]);
			}
		}
	}
	return patptr;
}

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

/*
 * change a binary character into a form we can print
 */
int
controlify(char *d, unsigned char c)
{
	int ret = 0;
	unsigned char digit;

	if (c == '\n') {
		append(d, "\\n");
		ret = 2;  // Wrote 2 chars: \ and n (null terminator will be overwritten)
	} else if (c == '\\') {
		append(d, "\\\\");  // Need to escape backslash as two backslashes
		ret = 2;  // Wrote 2 chars: two backslashes
	} else if ((c < ' ') || (c >= 0x7f)) {
		int shift;
		*d++ = '\\';
		*d++ = '0';
		ret = 2;
		for (shift = 6; shift >= 0; shift -= 3) {
			digit = (c >> shift) & 0x7;
			if ((ret == 2) && (digit == 0)) {
				continue;
			}
			*d++ = '0' + digit;
			ret++;
		}
		*d = 0;
	} else {
		*d++ = c;
		ret = 1;
	}
	return ret;
}

/*
 * format an integer for output
 */
int
longout(char *d, long v)
{
	int shift = 28;
	int ret = 2;
	int nibble;

	*d++ = '0';
	*d++ = 'x';
	for (shift = 28; shift >= 0; shift -= 4) {
		nibble = (v >> shift) & 0xf;
		if (shift && (ret == 2) && (nibble == 0)) { // skip leading zeros
			continue;
		}
		ret++;
		*d++ = (nibble > 9) ? (nibble - 0xa) + 'a' : nibble + '0';
	}
	return ret;
}

/*
 * output a string in a form that we can emit as source code
 * this means escaping control characters
 */
int
quotedString(char *d, char *s)
{
	int len = *s++;
	int ret = 1;

	d[0] = '\"';
	while (len--) {
		ret += controlify(&d[ret], *s++);
	}
	d[ret] = '\"';
	return ++ret;
}

int
iswhite(unsigned char c)
{
    switch (c) {
    case ' ': case '\t': case '\n':
        return 1;
    default:
        return 0;
    }
}

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
fdprintf(int fd, const char *fmt, ...)
{
    static char buf[4096];
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
fdputs(int fd, const char *s)
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
