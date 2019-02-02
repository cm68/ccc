/*
 * j-random utility functions
 */

#include "ccc.h"

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
hexdump(char *h, int l, int (*highlight)(int index))
{
    int i;
    char *z = xxbuf;
    char c;

    *z = 0;

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
        if (high(i)) {
            *z++ = 0x1b;
            *z++ = '[';
            *z++ = '7';
            *z++ = 'm';
        }
        *z++ = c;
        if (high(i)) {
            *z++ = 0x1b;
            *z++ = '[';
            *z++ = '0';
            *z++ = 'm';
        }
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
 * return -1 for miss
 * this is a prime candidate for assembly
 */
char
lookupc(char *s, char c)
{
    char i;
    for (i = 0; s[i]; i++) {
        if (c == s[i]) {
            return i;
        }
    }
    return -1;
}

int cpp_file;
/*
 * write to the cpp output file if requested
 *  */
void
cpp_out(char *s)
{
    if (s && cpp_file) {
        write(cpp_file, s, strlen(s));
        write(cpp_file, " ", 1);
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
