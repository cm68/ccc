/*
 * j-random utility functions
 */

#include "ccc.h"

void
hexdump(char *h, int l)
{
    int i;
    char z[17];
    char c;

    z[0] = 0;

    for (i = 0; i < l; i++) {
        c = *h++;
        if ((i % 16) == 0) {
            printf(" %s\n%04x  ", z, i);
            z[0] = 0;
        }
        printf("%02x ", c);
        if ((i % 4) == 3) printf(" ");
        if ((c < ' ') || (c > 0x7e)) c = '.';
        z[i % 16] = c;
        z[(i % 16) + 1] = 0;
    }
    while ((i++ % 16) != 0) {
        if ((i % 4) == 3) printf(" ");
        printf("   ");
    }
    printf(" %s\n", z);
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
