/*
 * astio.c - AST file I/O
 */
#include "pass2.h"
#include <string.h>
#include <unistd.h>
#include <stdio.h>

#ifdef DEBUG
#include "debug.h"
#endif

int infd;
int outfd;

void
out(char *s)
{
	write(outfd, s, strlen(s));
}

void
outc(char c)
{
	write(outfd, &c, 1);
}

void
outd(int n)
{
	char buf[12], *p = buf + 11;
	int neg = n < 0;
	if (neg) n = -n;
	*p = 0;
	do { *--p = '0' + n % 10; n /= 10; } while (n);
	if (neg) *--p = '-';
	out(p);
}

unsigned char
read1(void)
{
	unsigned char c;
	c = read(infd, &c, 1) == 1 ? c : E_O_F;
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "read1: %d (0x%02x) '%c'\n", c, c,
			(c >= 32 && c < 127) ? c : '.');
#endif
	return c;
}

unsigned short
read2(void)
{
	unsigned char buf[2];
	unsigned short v;
	read(infd, buf, 2);
	v = buf[0] | (buf[1] << 8);
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "read2: %u (0x%04x)\n", v, v);
#endif
	return v;
}

unsigned long
read4(void)
{
	unsigned char buf[4];
	unsigned long v;
	read(infd, buf, 4);
	v = buf[0] | ((unsigned long)buf[1] << 8) |
	    ((unsigned long)buf[2] << 16) | ((unsigned long)buf[3] << 24);
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "read4: %lu (0x%08lx)\n", v, v);
#endif
	return v;
}

void
readS(char *buf)
{
	unsigned char len = read1();
	read(infd, buf, len);
	buf[len] = 0;
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "readS: \"%s\" (len=%d)\n", buf, len);
#endif
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
