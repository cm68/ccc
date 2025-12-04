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

#define BUFSIZE 4096

static unsigned char inFd;
static unsigned char buf[BUFSIZE];
static unsigned bufPos;
static unsigned bufValid;

unsigned lineNum = 1;
unsigned char curchar;

static unsigned char symbuf[256];

void initAstio(unsigned char fd) {
	inFd = fd;
	bufPos = 0;
	bufValid = 0;
	lineNum = 1;
	curchar = 0;
}

unsigned char nextchar(void) {
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

/* Convert hex char to value */
static int hval(unsigned char c) {
	if (c >= '0' && c <= '9') return c - '0';
	if (c >= 'a' && c <= 'f') return c - 'a' + 10;
	if (c >= 'A' && c <= 'F') return c - 'A' + 10;
	return 0;
}

/* Read 2 hex chars as byte */
int readHex2(void) {
	int h = hval(curchar);
	int l = hval(nextchar());
	nextchar();
	return (h << 4) | l;
}

/* Read 4 hex chars as unsigned 16-bit */
int readHex4(void) {
	int v = 0, i;
	for (i = 0; i < 4; i++) {
		v = (v << 4) | hval(curchar);
		nextchar();
	}
	return v;
}

/* Read 8 hex chars as unsigned 32-bit, with optional leading - */
long readHex8(void) {
	long v = 0;
	int neg = 0, i;
	if (curchar == '-') { neg = 1; nextchar(); }
	for (i = 0; i < 8; i++) {
		v = (v << 4) | hval(curchar);
		nextchar();
	}
	return neg ? -v : v;
}

/* Read hex-length-prefixed name into static buffer */
unsigned char *readName(void) {
	int len, i;
	len = readHex2();
	for (i = 0; i < len && i < 255; i++)
		symbuf[i] = readHex2();
	symbuf[i] = 0;
	return symbuf;
}

/* Read hex-length-prefixed string, return malloc'd copy */
unsigned char *readStr(void) {
	int len = readHex2();
	unsigned char *s = malloc(len + 1);
	int i;
	for (i = 0; i < len; i++)
		s[i] = readHex2();
	s[len] = 0;
	return s;
}

/* Skip to next line (for comments/newlines) */
void skipLine(void) {
	while (curchar && curchar != '\n')
		nextchar();
	if (curchar == '\n')
		nextchar();
}

/* Skip newlines */
void skipNL(void) {
	while (curchar == '\n')
		nextchar();
}
