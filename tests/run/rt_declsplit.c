/*
 * Declarators after an array, in one declaration - with the
 * initializer that made it matter:
 *
 *	char buf[12], *p = buf + 11;
 *
 * filtdecl used to abandon the declaration at the '[', so the
 * initializer reached pass1 unsplit and was dropped for a
 * register-homed local: p held the caller's junk.  That line is
 * c1's own outd(), so the self-built c1 printed every number as
 * an empty string - and 0 - (n % 10), the digit trick right next
 * to it, had no rule for a constant minus a value in DE or HL.
 * Both fixed; both pinned here, negative digits and all.
 */
#include "rt.h"

char sink[16];
int si;

void
out(char *s)
{
	while (*s)
		sink[si++] = *s++;
}

void
outd(int n)
{
	char buf[12], *p = buf + 11;
	int neg = n < 0;
	if (!neg) n = -n;
	*p = 0;
	do { *--p = '0' - n % 10; n /= 10; } while (n);
	if (neg) *--p = '-';
	out(p);
}

int
main()
{
	si = 0;
	outd(1);
	outd(0);
	outd(-42);
	outd(305);
	CHECK(1, si, 8);
	CHECK(2, sink[0], '1');
	CHECK(3, sink[1], '0');
	CHECK(4, sink[2], '-');
	CHECK(5, sink[3], '4');
	CHECK(6, sink[4], '2');
	CHECK(7, sink[5], '3');
	CHECK(8, sink[6], '0');
	CHECK(9, sink[7], '5');
	return 0;
}
