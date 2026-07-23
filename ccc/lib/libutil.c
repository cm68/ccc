/*
 * libutil.c - Shared utility functions for ccc
 */
#include <stdarg.h>
#include "libutil.h"

/*
 * Minimal formatter - handles %s, %d, %ld, %x, %c, %% (no width/padding)
 * Returns pointer to end of written string
 */
char *
fmtstr(char *buf, char *fmt, ...)
{
	va_list ap;
	char *p = buf;
	char *s;
	long ln;
	unsigned int n;
	int neg, i;

	va_start(ap, fmt);
	while (*fmt) {
		if (*fmt != '%') {
			*p++ = *fmt++;
			continue;
		}
		fmt++;
		if (*fmt == 'l') {
			/* %ld: the only conversion that needs long arithmetic */
			fmt += 2;  /* skip 'l' and 'd' */
			ln = va_arg(ap, long);
			neg = ln < 0;
			if (neg) ln = -ln;
			s = p;
			do { *p++ = '0' + (int)(ln % 10); ln /= 10; } while (ln);
			goto revout;
		}
		switch (*fmt++) {
		case 's':
			s = va_arg(ap, char *);
			while (*s) *p++ = *s++;
			break;
		case 'd':
			i = va_arg(ap, int);
			neg = i < 0;
			n = neg ? -(unsigned int)i : (unsigned int)i;
			s = p;
			do { *p++ = '0' + (n % 10); n /= 10; } while (n);
		revout:
			if (neg) *p++ = '-';
			/* reverse */
			for (i = 0; i < (int)(p - s) / 2; i++) {
				char t = s[i]; s[i] = p[-1-i]; p[-1-i] = t;
			}
			break;
		case 'x':
			n = va_arg(ap, unsigned);
			s = p;
			do {
				i = n & 0xf;
				*p++ = i < 10 ? '0' + i : 'a' + i - 10;
				n >>= 4;
			} while (n);
			/* reverse */
			for (i = 0; i < (int)(p - s) / 2; i++) {
				char t = s[i]; s[i] = p[-1-i]; p[-1-i] = t;
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

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
