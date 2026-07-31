/*
 * A register pointer stepped on its own line.
 *
 * Steps through IX existed only fused with a deref - "*p++" worked -
 * so a bare "p++" on an IX-homed pointer matched no rule and emitted
 * nothing: a marker in the listing, an infinite loop on the machine.
 * This is the scan-loop shape that found it.
 */
#include "rt.h"

char buf[6];

int
scan(char *q)
{
	register char *p;
	int s;

	p = q;
	s = 0;
	while (*p) {
		s = s + *p;
		p++;
	}
	return s;
}

int
main()
{
	buf[0] = 3;
	buf[1] = 7;
	buf[2] = 11;
	buf[3] = 0;
	CHECK(1, scan(buf), 21);
	buf[0] = 0;
	CHECK(2, scan(buf), 0);
	return 0;
}
