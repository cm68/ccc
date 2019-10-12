/*
 * j-random utility functions
 */

#include "ccc.h"

int verbose;

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

char *hion = "\033[7m";
char *hioff = "\033[0m";

char *append(char *z, char *s)
{
    while (*s) {
        *z++ = *s++;
    }
    *z = 0;
    return z;
}

void
hexdump(char *tag, char *h, int l, int (*high)(int index))
{
    int i;
    char *z = xxbuf;
    char c;
    int highlit = 0;

    strcpy(xxbuf, tag);

    for (i = 0; i < l; i++) {
        c = h[i];
        if ((i % 16) == 0) {
            if (highlit) {
                z = append(z, hioff);
                highlit = 0;
            }
            printf(" %s\n%04x  ", xxbuf, i);
            z = xxbuf;
            *z = 0;
        }
        printf("%02x ", c);
        if ((i % 4) == 3) printf(" ");
        if ((c < ' ') || (c > 0x7e)) c = '.';
        if (high(i)) {
            if (!highlit) { 
                z = append(z, hion);
                highlit = 1;
            }
        } else if (highlit) {
            z = append(z, hioff);
            highlit = 0;
        }
        *z++ = c;
        *z = 0;
    }
    if (highlit) {
        z = append(z, hioff);
        highlit = 0;
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
 * this is a prime candidate for assembly
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
