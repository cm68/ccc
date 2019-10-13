/*
 * j-random utility functions
 * these are mostly prime candidates for assembly code
 */

#include "ccc.h"

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
static int
controlify(char *d, char c)
{
	int ret = 0;
	char digit;

	if (c == '\n') {
		append(d, "\\n");
		ret = 3;
	} else if (c == '\\') {
		append(d, "\\");
		ret = 2;
	} else if ((c < ' ') || (c >= 0x7f)) {
		char shift;
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
quoted_string(char *d, char *s)
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
iswhite(char c)
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
    char c;

    strcpy(xxbuf, tag);

    for (i = 0; i < l; i++) {
        c = h[i];
        if ((i % 16) == 0) {
            printf(" %s\n%04x  ", xxbuf, i);
            z = xxbuf;
            *z = 0;
        }
        printf("%02x ", c);
        if ((i % 4) == 3) printf(" ");
        if ((c < ' ') || (c > 0x7e)) c = '.';
        *z++ = c;
        *z = 0;
    }
    while ((i++ % 16) != 0) {
        if ((i % 4) == 3) printf(" ");
        printf("   ");
    }
    printf(" %s\n", xxbuf);
}

/*
 * return the index in an array of the first occurrance of a char
 * return 0xff for miss
 */
int
lookupc(char *s, char c)
{
    int i;
    for (i = 0; s[i]; i++) {
        if (c == s[i]) {
            return i;
        }
    }
    return -1;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
