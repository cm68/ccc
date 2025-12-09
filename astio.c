/*
 * astio.c - Simple hex-based AST I/O
 *
 * All names/strings are <2-hex-len><hex-bytes>
 * All numbers are hex terminated by '.'
 * No whitespace scanning needed.
 */
#include <stdlib.h>
#include <unistd.h>
#include "astio.h"
#include "cc2.h"

#define BUFSIZE 256

static unsigned char inFd;
static unsigned char buf[BUFSIZE];
static unsigned bufPos;
static unsigned bufValid;

unsigned lineNum = 1;
unsigned char curchar;

static unsigned char symbuf[64];

void 
initAstio(unsigned char fd) {
	inFd = fd;
	bufPos = 0;
	bufValid = 0;
	lineNum = 1;
	curchar = 0;
}

unsigned char 
nextchar(void) {
	if (bufPos >= bufValid) {
		int n = read(inFd, buf, BUFSIZE);
		if (n <= 0) {
			curchar = 0;
			return 0;
		}
		bufValid = n;
		bufPos = 0;
	}
	curchar = buf[bufPos++];
	if (curchar == '\n') lineNum++;
	return curchar;
}

/* Convert hex char to value (0-15) */
static unsigned char
hval(unsigned char c) {
    c |= 0x20;
	if (c >= '0' && c <= '9') return c - '0';
	if (c >= 'a' && c <= 'f') return c - ('a' - 10);
	return 0;
}

/* Read 2 hex chars as byte */
unsigned char
readHex2(void) {
	unsigned char v;
	v = hval(curchar);
	nextchar();
	v <<= 4;
	v |= hval(curchar);
	nextchar();
	return v;
}

/* Read 4 hex chars as unsigned 16-bit */
unsigned short 
readHex4(void) {
    unsigned char i = 5;
	unsigned short v = 0;
	while (--i) {
        v <<= 4;
		v |= hval(curchar);
		nextchar();
	}
	return v;
}

/* Read 8 hex chars as 32-bit (two's complement) */
unsigned long 
readHex8(void) {
	unsigned long v = 0;
	unsigned char i = 9;
	while (--i) {
		v <<= 4;
		v |= hval(curchar);
		nextchar();
	}
	return v;
}

/* Read hex-length-prefixed ASCII name into static buffer */
unsigned char *
readName(void) {
	int len, i;
	len = readHex2();
	for (i = 0; i < len && i < 63; i++) {
		symbuf[i] = curchar;
		nextchar();
	}
	symbuf[i] = 0;
	return symbuf;
}

/* Read hex-length-prefixed hex-encoded string, return malloc'd copy */
unsigned char *
readStr(void) {
	int len = readHex2();
	unsigned char *s = malloc(len + 1);
	int i;
	for (i = 0; i < len; i++)
		s[i] = readHex2();
	s[len] = 0;
	return s;
}

/* Read hex-encoded string (4-digit len + 2*len hex chars) */
unsigned char *
readHexStr(void) {
	int len = readHex4();
	unsigned char *s = malloc(len + 1);
	int i;
	for (i = 0; i < len; i++)
		s[i] = readHex2();
	s[len] = 0;
	return s;
}

/* Skip newlines */
void 
skipNL(void) {
	while (curchar == '\n')
		nextchar();
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
