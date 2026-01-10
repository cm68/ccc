/*
 * j-random utility functions
 * these are mostly prime candidates for assembly code
 */

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#ifndef PSIZE
#define PSIZE 80
#endif

#ifdef DEBUG
/* Forward declaration - only used for DEBUG builds */
int fdprintf(unsigned char fd, char *fmt, ...);
#endif

/*
 * Minimal formatter - handles %s, %d, %c, %% (no width/padding)
 * Returns pointer to end of written string
 */
char *
fmtstr(char *buf, char *fmt, ...)
{
	va_list ap;
	char *p = buf;
	char *s;
	int n;
	int neg;

	va_start(ap, fmt);
	while (*fmt) {
		if (*fmt != '%') {
			*p++ = *fmt++;
			continue;
		}
		fmt++;
		switch (*fmt++) {
		case 's':
			s = va_arg(ap, char *);
			while (*s) *p++ = *s++;
			break;
		case 'd':
			n = va_arg(ap, int);
			if ((neg = (n < 0))) n = -n;
			s = p;
			do { *p++ = '0' + (n % 10); n /= 10; } while (n);
			if (neg) *p++ = '-';
			/* reverse */
			for (n = 0; n < (p - s) / 2; n++) {
				char t = s[n]; s[n] = p[-1-n]; p[-1-n] = t;
			}
			break;
		case 'c':
			*p++ = (char)va_arg(ap, int);
			break;
		case '%':
			*p++ = '%';
			break;
		}
	}
	va_end(ap);
	*p = 0;
	return p;
}

#ifdef CCC
/* strdup not in CCC libc */
char *
strdup(char *s)
{
    char *d = malloc(strlen(s) + 1);
    if (d)
        strcpy(d, s);
    return d;
}
#endif

#ifndef CCC
char patspace[PSIZE];

char *
bitdef(unsigned char v, char **defs)
{
	unsigned char i = 0x01;
	char *patptr;
	unsigned char sep = 0;

	patptr = patspace;
	*patptr = 0;

    while (i) {
		if ((v & i) && *defs) {
			if (sep++) {
				strcat(patptr, ",");
			}
			strcat(patptr, *defs);
		}
        i = i * 2;
        defs++;
	}
	return patptr;
}
#endif

#ifdef DEBUG
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

char
iswhite(unsigned char c)
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
    unsigned char c;

    strcpy(xxbuf, tag);

    for (i = 0; i < l; i++) {
        c = h[i];
        if ((i % 16) == 0) {
            fdprintf(2," %s\n%04x  ", xxbuf, i);
            z = xxbuf;
            *z = 0;
        }
        fdprintf(2,"%02x ", c);
        if ((i % 4) == 3) printf(" ");
        if ((c < ' ') || (c > 0x7e)) c = '.';
        *z++ = c;
        *z = 0;
    }
    while ((i++ % 16) != 0) {
        if ((i % 4) == 3) printf(" ");
        fdprintf(2,"   ");
    }
    printf(" %s\n", xxbuf);
}

/*
 * return the index in an array of the first occurrance of a char
 * return 0xff for miss
 */
unsigned char
lookupc(char *s, char c)
{
    unsigned char i;
    for (i = 0; s[i]; i++) {
        if (c == s[i]) {
            return i;
        }
    }
    return 0xff;
}

/*
 * fdprintf - printf-like function that writes to a Unix file descriptor
 * Uses sprintf to format to a static buffer, then writes via write() syscall
 */
int
fdprintf(unsigned char fd, char *fmt, ...)
{
    static char buf[1024];
    va_list args;
    int len;
    int result;

    va_start(args, fmt);
    len = vsprintf(buf, fmt, args);
    va_end(args);

    if (len > 0) {
        result = write(fd, buf, len);
        return result;
    }

    return len;
}

/*
 * fdputs - write a string to a Unix file descriptor
 * More efficient than fdprintf for simple string output (no formatting overhead)
 */
int
fdputs(unsigned char fd, char *s)
{
    int len;

    len = strlen(s);
    if (len > 0) {
        return write(fd, s, len);
    }
    return 0;
}
#endif

/* Binary AST emission helpers */
extern unsigned char astFd;

void emit1(unsigned char b)
{
	write(astFd, &b, 1);
}

void emit2(unsigned short w)
{
	unsigned char buf[2];
	buf[0] = w & 0xff;
	buf[1] = (w >> 8) & 0xff;
	write(astFd, buf, 2);
}

void emit4(unsigned long l)
{
	unsigned char buf[4];
	buf[0] = l & 0xff;
	buf[1] = (l >> 8) & 0xff;
	buf[2] = (l >> 16) & 0xff;
	buf[3] = (l >> 24) & 0xff;
	write(astFd, buf, 4);
}

void emitN(char *s, unsigned char len)
{
	emit1(len);
	if (len > 0)
		write(astFd, s, len);
}

void emitS(char *s)
{
	emitN(s, strlen(s));
}

/* Assembly output helpers - write to asmFd */
extern unsigned char asmFd;

void asmStr(char *s)
{
	write(asmFd, s, strlen(s));
}

void asmLine(char *s)
{
	asmStr(s);
	write(asmFd, "\n", 1);
}

void asmLabel(char *name)
{
	asmStr(name);
	asmLine(":");
}

void asmDb(int val)
{
	char buf[16];
	fmtstr(buf, "\t.db %d", val & 0xff);
	asmLine(buf);
}

void asmDw(int val)
{
	char buf[16];
	fmtstr(buf, "\t.dw %d", val & 0xffff);
	asmLine(buf);
}

void asmDwSym(char *name)
{
	asmStr("\t.dw ");
	asmLine(name);
}

void asmDs(int size)
{
	char buf[16];
	fmtstr(buf, "\t.ds %d", size);
	asmLine(buf);
}

/* Segment tracking - emit .text/.data/.bss only when segment changes */
static unsigned char curSeg = 0;  /* SEG_TEXT */

void setSeg(unsigned char seg)
{
	if (seg == curSeg)
		return;
	curSeg = seg;
	switch (seg) {
	case 0: asmLine("\t.text"); break;  /* SEG_TEXT */
	case 1: asmLine("\t.data"); break;  /* SEG_DATA */
	case 2: asmLine("\t.bss"); break;   /* SEG_BSS */
	}
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
