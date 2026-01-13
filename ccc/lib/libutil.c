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
	long n;
	int neg;

	va_start(ap, fmt);
	while (*fmt) {
		if (*fmt != '%') {
			*p++ = *fmt++;
			continue;
		}
		fmt++;
		if (*fmt == 'l') {
			fmt += 2;  /* skip 'l' and 'd' */
			n = va_arg(ap, long);
			goto donum;
		}
		switch (*fmt++) {
		case 's':
			s = va_arg(ap, char *);
			while (*s) *p++ = *s++;
			break;
		case 'd':
			n = va_arg(ap, int);
		donum:
			if ((neg = (n < 0))) n = -n;
			s = p;
			do { *p++ = '0' + (n % 10); n /= 10; } while (n);
			if (neg) *p++ = '-';
			/* reverse */
			for (n = 0; n < (p - s) / 2; n++) {
				char t = s[n]; s[n] = p[-1-n]; p[-1-n] = t;
			}
			break;
		case 'x':
			n = va_arg(ap, unsigned);
			s = p;
			do {
				int d = n & 0xf;
				*p++ = d < 10 ? '0' + d : 'a' + d - 10;
				n >>= 4;
			} while (n);
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

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
