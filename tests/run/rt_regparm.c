/*
 * register on a parameter, honored and marshalled.
 *
 * The declarator parsed the keyword and threw it away - the comment
 * at the site said so - and the level-2 entry the allocator walks
 * was created without a storage class, so "register" on a parameter
 * had never done anything.  Honoring it exposed a second hole: the
 * BC-to-word pass checked only the pair bit, so a word local took
 * BC while B was carrying a staged byte parameter, and initializing
 * the local overwrote the parameter mid-flight.  sum() below is that
 * exact shape: n staged into B, s wanting BC.
 */
#include "rt.h"

char buf[6];

void
cat(register char *d, register char *s)
{
	while (*s) {
		*d = *s;
		d++;
		s++;
	}
	*d = 0;
}

int
sum(register unsigned char n, register char *p)
{
	int s;

	s = 0;
	while (n--) {
		s = s + *p;
		p++;
	}
	return s;
}

int
main()
{
	buf[0] = 0;
	cat(buf, "ab");
	CHECK(1, buf[0], 'a');
	CHECK(2, buf[1], 'b');
	CHECK(3, buf[2], 0);
	buf[0] = 5;
	buf[1] = 9;
	buf[2] = 100;
	CHECK(4, sum(2, buf), 14);
	CHECK(5, sum(3, buf), 114);
	CHECK(6, sum(0, buf), 0);
	return 0;
}
