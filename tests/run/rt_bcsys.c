/* does a syscall preserve BC?  keep a register variable live across
 * one and see whether it comes back. */
#include "rt.h"
extern int write();

int
probe(n)
register int n;
{
	char buf[4];

	buf[0] = 'x';
	write(1, buf, 0);	/* a syscall, writing nothing */
	return n;		/* n lives in BC across it */
}

int
main()
{
	CHECK(1, probe(12345), 12345);
	return 0;
}
