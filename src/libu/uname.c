/*
 * uname - a unique temporary file name, "/tmp/<pid>-<time>".
 *
 * Not uname(2); the name predates it.  This is a C rewrite of the
 * old uname.s, which was disassembled object code (wsnm -g) calling
 * cpystr, itob and ltob - helpers that no longer exist anywhere in
 * this tree, so the .s could never have linked.
 *
 * The timestamp is read once per process and reduced mod 10000 so
 * the name fits the 16-byte buffer: "/tmp/" (5) + up to five pid
 * digits + '-' + four timestamp digits + NUL.  Every call therefore
 * hands back the same string in the same static buffer; the name is
 * unique against other processes, not against other calls.  A caller
 * that wants to keep it past the next call copies it.
 *
 * The old object stored longs high word first, which is why its
 * modulus assembled as 00 00 10 27; read in this tree's byte order
 * the same bytes would be 0x27100000, a nine-digit number that
 * cannot fit the buffer it was sized for.  10000 is the value that
 * makes the original consistent.
 */
/*
 * time.h is deliberately not included: it declares time() returning
 * time_t while unistd.h declares it returning int, and only one of
 * the two can be in scope.  Nothing here needs time()'s return value.
 */
#include <string.h>
#include <unistd.h>

#define	NAMESZ	16		/* exactly what the longest name needs */
#define	TMOD	10000L		/* keeps the stamp to four digits */
#define	NDIG	10		/* digits in the widest unsigned long */

static char	nmbuf[NAMESZ];
static long	stamp;

/*
 * Decimal digits of v at p, no leading zeros.  Returns the end.
 */
static char *
putdec(char *p, unsigned long v)
{
	char		buf[NDIG];
	register char *	cp;

	cp = &buf[NDIG];
	do {
		*--cp = '0' + (unsigned char) (v % 10);
		v /= 10;
	} while (v);
	while (cp < &buf[NDIG])
		*p++ = *cp++;
	return p;
}

char *
uname(void)
{
	register char *	p;

	if (stamp == 0) {
		time(&stamp);
		stamp = (unsigned long) stamp % TMOD;
	}
	strcpy(nmbuf, "/tmp/");
	p = putdec(nmbuf + 5, (unsigned long) (unsigned) getpid());
	*p++ = '-';
	p = putdec(p, (unsigned long) stamp);
	*p = 0;
	return nmbuf;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
