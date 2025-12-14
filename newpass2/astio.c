/*
 * astio.c - AST input/output functions
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "cc2.h"

/*
 * AST I/O
 */
void
advance(void)
{
    char c;
    if (read(infd, &c, 1) == 1) {
        curchar = c;
        if (c == '\n') lineno++;
    } else {
        curchar = ASTEOF;
    }
}

void
skipWs(void)
{
    while (curchar == ' ' || curchar == '\t' ||
           curchar == '\n' || curchar == '\r') {
        advance();
    }
}

/*
 * translate a character to it's hex value, case insenstive
 */
unsigned char
hex()
{
	unsigned char c;
        c = curchar;
        advance();
	c |= 0x20;
        if (c >= '0' && c <= '9') {
		c -= '0';
	} else {
		c -= 'a' - 10;
	}
	return c;
}

/* Read 2 hex digits */
unsigned char
hex2(void)
{
    return (hex() << 4) | hex();
}

/* Read 4 hex digits */
unsigned int
hex4(void)
{
    return (hex2() << 8) | hex2();
}

/* Read 8 hex digits */
long
hex8(void)
{
    long v = 0;
    unsigned char neg = 0;
    unsigned char i;

    if (curchar == '-') {
        neg = 1;
        advance();
    }
    for (i = 0; i < 8; i++) {
	v = v << 4 | hex();
    }
    return neg ? -v : v;
}

/* Read length-prefixed name */
void
readName(char *buf)
{
    unsigned char len, i;

    len = hex2();
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
