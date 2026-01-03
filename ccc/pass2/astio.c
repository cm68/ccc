/*
 * astio.c - AST input/output functions (binary format)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cc2.h"

/*
 * AST I/O - binary format
 */
void
advance(void)
{
    unsigned char c;
    if (read(infd, &c, 1) == 1) {
        curchar = c;
    } else {
        curchar = ASTEOF;
    }
}

/* Read 1 byte directly */
unsigned char
read1(void)
{
    unsigned char c = curchar;
    advance();
    return c;
}

/* Read 2 bytes little-endian */
unsigned int
read2(void)
{
    unsigned int lo = read1();
    unsigned int hi = read1();
    return lo | (hi << 8);
}

/* Read 4 bytes little-endian */
long
read4(void)
{
    long v = read1();
    v |= (long)read1() << 8;
    v |= (long)read1() << 16;
    v |= (long)read1() << 24;
    return v;
}

/* Read length-prefixed name (binary: 1 byte len + raw chars) */
void
readName(char *buf)
{
    unsigned char len, i;

    len = read1();
    for (i = 0; i < len && i < 13; i++) {
        buf[i] = curchar;
        advance();
    }
    buf[i] = 0;
    while (i < len) {
        advance();
        i++;
    }
}
/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
