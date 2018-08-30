/*
 * j-random utility functions
 */

#include "ccc.h"

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

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
