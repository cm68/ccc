/*
 * A pointer compared against an array name.
 *
 * A SYMREF is left unreduced so the store and load rules can use it as
 * an address, so anywhere its *value* is wanted it has to be loaded
 * first.  The equality rules got that and the ordering rules did not,
 * which is a distinction with a reason: "a > b" is canonicalised to
 * "b < a", and that is what moves the symbol to the left, where no
 * rule would take it.  "s == buf" leaves the symbol on the right,
 * where it becomes DE and the ordinary rules match.
 *
 * So == and != and >= worked, and > and < did not - and did not fail
 * cleanly either.  "s > buf" gave -1 and "s < buf" gave an address.
 *
 * Found in cpp.  macro.c trims trailing whitespace from a macro body
 * with
 *
 *	while (s > macbuffer && (s[-1] == ' ' || s[-1] == '\t'))
 *
 * which never ran, so a body kept its trailing blanks - and
 * "#define cdump(x)" with no body at all walked off the end of the
 * definition and ate the first character of the next line, turning
 * "int a;" into "nt a;".
 */
#include "rt.h"

char buf[64];
short arr[8];
char *base;

short
trimmed(s)
char *s;
{
	short n;

	n = 0;
	while (s > buf && s[-1] == ' ') {
		s--;
		n++;
		if (n > 20)
			return -1;	/* runaway */
	}
	return n;
}

main()
{
	char *s;
	short *p;
	short n;

	s = &buf[2];
	base = buf;

	/* as a value */
	CHECK(1, s > buf, 1);
	CHECK(2, s < buf, 0);
	CHECK(3, s >= buf, 1);
	CHECK(4, s <= buf, 0);
	CHECK(5, s == buf, 0);
	CHECK(6, s != buf, 1);

	/* the array on the other side */
	CHECK(7, buf < s, 1);
	CHECK(8, buf > s, 0);
	CHECK(9, buf <= s, 1);
	CHECK(10, buf >= s, 0);

	/* the same address reached three ways must compare equal */
	s = buf;
	CHECK(11, s > buf, 0);
	CHECK(12, s >= buf, 1);
	CHECK(13, s > &buf[0], 0);
	CHECK(14, s > base, 0);

	/*
	 * As a condition rather than a value.  Written with a variable
	 * rather than an if/else around CHECK: CHECK is itself an if,
	 * so an else next to it binds to CHECK's and not to mine.
	 */
	s = &buf[2];
	n = 0;
	if (s > buf)
		n = 1;
	CHECK(15, n, 1);
	n = 0;
	if (buf > s)
		n = 1;
	CHECK(22, n, 0);

	/* and driving a loop, which is macro.c's shape */
	buf[0] = 'a';
	buf[1] = ' ';
	buf[2] = ' ';
	CHECK(16, trimmed(&buf[3]), 2);
	CHECK(17, trimmed(&buf[1]), 0);	/* buf[0] is not a space */
	CHECK(18, trimmed(buf), 0);	/* already at the base */

	/* an array of something wider than a byte scales the same */
	p = &arr[3];
	CHECK(19, p > arr, 1);
	CHECK(20, p < arr, 0);
	CHECK(21, p - arr, 3);

	return 0;
}
