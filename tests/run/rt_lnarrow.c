/*
 * A long where something narrower is wanted, and a local array's name
 * as a value.
 *
 * A long occupies HL:DE whole, so there is no moving it to DE: the
 * exchange that does that for a word only swaps its halves.  Asked
 * for the low word, which is what a narrower use wants, the answer is
 * already in DE and the move is free.  Taking the word path instead
 * indexed by the HIGH half, so "0123456789ABCDEF"[i % base] was a
 * zero for every digit and _pnum printed nothing at all.
 *
 * The other half of that routine measures how many digits it wrote,
 * as the distance from the end of its buffer back to where it
 * stopped.  A local array's name is its address, and MINUS had no
 * form that would take one on the left - so nothing was emitted and
 * printf("%d") printed one character of any number.
 */
#include "rt.h"

char digits[17];

short
digit(i, base)
unsigned long i;
unsigned char base;
{
	return "0123456789ABCDEF"[i % base];
}

/* the same through a table in memory rather than a literal */
short
tdigit(i, base)
unsigned long i;
unsigned char base;
{
	return digits[i % base];
}

/* a long narrowed into a word for ordinary arithmetic */
short
lowword(v)
unsigned long v;
{
	short s;

	s = (short)(v % 1000L);
	return s;
}

/* the span of a buffer that has been filled from the far end */
short
span(n)
short n;
{
	char buf[30];
	char *cp;
	short i;

	cp = &buf[30];
	for (i = 0; i < n; i++)
		*--cp = 'x';
	return (short)(&buf[30] - cp);
}

/*
 * The same span landing in a char is NOT checked here, and the reason
 * is worth writing down: pass1 narrows an operation to the width its
 * result is stored at, but it narrows the operator rather than the
 * operand under it, so "f = (char)(&buf[30] - cp)" reaches pass2 as a
 * byte-wide subtraction with a whole address on its left.  Every byte
 * form takes that side in A and no rule spells -(H,K):b, so nothing
 * is emitted and f keeps its old value.
 *
 * It cannot be repaired where the other operand fixups live: the left
 * is still a frame descriptor when they run and only becomes an HL
 * value once the rule loop has turned over.  Nothing in the compiler
 * or the library is written this way, which is why it is recorded
 * here rather than worked around.
 */

/* an array name on either side of a subtraction */
short
names()
{
	char buf[16];
	char *q;

	q = &buf[6];
	if (q - buf != 6)
		return 0;
	if (buf + 10 - q != 4)
		return 0;
	return 1;
}

main()
{
	short i;

	for (i = 0; i < 16; i++)
		digits[i] = "0123456789ABCDEF"[i];
	digits[16] = 0;

	CHECK(1, digit(9L, 10), '9');
	CHECK(2, digit(1234L, 10), '4');
	CHECK(3, digit(255L, 16), 'F');
	CHECK(4, tdigit(7L, 10), '7');
	CHECK(5, lowword(123456L), (short)(123456L % 1000L));
	CHECK(6, span(1), 1);
	CHECK(7, span(4), 4);
	CHECK(8, span(20), 20);
	CHECK(9, names(), 1);
	return 0;
}
