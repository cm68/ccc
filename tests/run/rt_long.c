/*
 * 32-bit arithmetic.
 *
 * A long lives in HL:DE with the high word in HL, and everything
 * beyond loading and storing one goes through the Hi-Tech runtime
 * helpers, which take the other operand on the stack with its high
 * word pushed first.  That convention is easy to get backwards - one
 * of the helpers in libc has its own header comment inverted - and
 * getting it backwards produces code that assembles, links and runs.
 *
 * short is 16 bits everywhere; long is 32 only because the native
 * reference is built -m32.  See runtests.sh.
 *
 * Long constants carry an explicit L.  zc3 predates the promotions
 * that would widen a plain 0 in "a < 0", and compares it against the
 * low half; whether a compiler does that conversion is a language
 * question, and this file is about the code generated once the widths
 * are settled.
 */
#include "rt.h"

long a, b, c, arr[4], *lp;
unsigned long ua, ub;
short s;

long idl(v) long v; { return v; }
long addl(x, y) long x, y; { return x + y; }

main()
{
	long loc, loc2;

	/* it has to survive a round trip through memory first */
	a = 0x12345678L;
	CHECK(1, a == 0x12345678L, 1);
	CHECK(2, a != 0x12345679L, 1);
	b = a;
	CHECK(3, b == 0x12345678L, 1);

	/* both halves have to move: a value that only differs high up */
	a = 0x00010000L;
	CHECK(4, a == 0x00010000L, 1);
	CHECK(5, a == 0x00000000L, 0);
	a = 0x0000ffffL;
	CHECK(6, a == 0x0000ffffL, 1);
	CHECK(7, a == 0xffffffffL, 0);

	/* addition, with and without a carry out of the low word */
	a = 1L; b = 2L;
	c = a + b;
	CHECK(8, c == 3L, 1);
	a = 0x0000ffffL; b = 1L;
	c = a + b;
	CHECK(9, c == 0x00010000L, 1);
	a = 0x00ff0000L; b = 0x00010000L;
	c = a + b;
	CHECK(10, c == 0x01000000L, 1);

	/* subtraction, with and without a borrow */
	a = 5L; b = 3L;
	CHECK(11, a - b == 2L, 1);
	a = 0x00010000L; b = 1L;
	CHECK(12, a - b == 0x0000ffffL, 1);
	a = 0L; b = 1L;
	CHECK(13, a - b == -1L, 1);

	/* negative values, where the sign lives in the high word */
	a = -1L;
	CHECK(14, a == -1L, 1);
	CHECK(15, a < 0L, 1);
	CHECK(16, a > 0L, 0);
	a = -65536L;
	CHECK(17, a == -65536L, 1);
	CHECK(18, a < 0L, 1);

	/* signed comparison either side of zero */
	a = -1L; b = 1L;
	CHECK(19, a < b, 1);
	CHECK(20, a > b, 0);
	CHECK(21, a <= b, 1);
	CHECK(22, a >= b, 0);
	a = 1L; b = -1L;
	CHECK(23, a < b, 0);
	CHECK(24, a > b, 1);

	/* and where only the low word differs, so the helper has to
	 * carry on past the high word */
	a = 0x00010000L; b = 0x00010001L;
	CHECK(25, a < b, 1);
	CHECK(26, a > b, 0);
	CHECK(27, a == b, 0);

	/* the extremes */
	a = 0x7fffffffL; b = 0x80000000L;
	CHECK(28, a > b, 1);
	CHECK(29, b < a, 1);

	/* unsigned keeps 0xffffffff large rather than -1 */
	ua = 0xffffffffL; ub = 1L;
	CHECK(30, ua > ub, 1);
	CHECK(31, ua < ub, 0);

	/* multiply */
	a = 1000L; b = 1000L;
	CHECK(32, a * b == 1000000L, 1);
	a = 65536L; b = 2L;
	CHECK(33, a * b == 131072L, 1);
	a = -3L; b = 4L;
	CHECK(34, a * b == -12L, 1);

	/* divide and remainder, signed */
	a = 1000000L; b = 1000L;
	CHECK(35, a / b == 1000L, 1);
	CHECK(36, a % b == 0L, 1);
	a = 1000003L; b = 1000L;
	CHECK(37, a / b == 1000L, 1);
	CHECK(38, a % b == 3L, 1);
	a = -12L; b = 4L;
	CHECK(39, a / b == -3L, 1);

	/* unsigned divide, where the top bit is a value not a sign */
	ua = 0x80000000L; ub = 2L;
	CHECK(40, ua / ub == 0x40000000L, 1);

	/* bitwise, both halves */
	a = 0x12345678L; b = 0x0f0f0f0fL;
	CHECK(41, (a & b) == 0x02040608L, 1);
	CHECK(42, (a | b) == 0x1f3f5f7fL, 1);
	CHECK(43, (a ^ b) == 0x1d3b5977L, 1);
	a = 0L;
	CHECK(44, ~a == -1L, 1);

	/* shifts, by a constant and by a variable */
	a = 1L;
	CHECK(45, a << 1 == 2L, 1);
	CHECK(46, a << 16 == 0x00010000L, 1);
	CHECK(47, a << 31 == 0x80000000L, 1);
	a = 0x00010000L;
	CHECK(48, a >> 16 == 1L, 1);
	a = -256L;
	CHECK(49, a >> 8 == -1L, 1);
	ua = 0x80000000L;
	CHECK(50, ua >> 31 == 1L, 1);
	a = 1L; s = 16;
	CHECK(51, a << s == 0x00010000L, 1);

	/* widening a short into a long carries the sign */
	s = -1;
	a = s;
	CHECK(52, a == -1L, 1);
	s = 1;
	a = s;
	CHECK(53, a == 1L, 1);
	s = -32768;
	a = s;
	CHECK(54, a == -32768L, 1);

	/* narrowing a long into a short keeps the low half */
	a = 0x12345678L;
	s = a;
	CHECK(55, s == 0x5678, 1);

	/* increment and decrement, across the word boundary */
	a = 0x0000ffffL;
	a++;
	CHECK(56, a == 0x00010000L, 1);
	a--;
	CHECK(57, a == 0x0000ffffL, 1);
	a = 0L;
	--a;
	CHECK(58, a == -1L, 1);

	/* compound assignment */
	a = 100L;
	a += 5L;
	CHECK(59, a == 105L, 1);
	a -= 10L;
	CHECK(60, a == 95L, 1);
	a *= 2L;
	CHECK(61, a == 190L, 1);
	a /= 19L;
	CHECK(62, a == 10L, 1);
	a <<= 4;
	CHECK(63, a == 160L, 1);
	a >>= 2;
	CHECK(64, a == 40L, 1);
	a &= 0x18L;
	CHECK(65, a == 8L, 1);
	a |= 1L;
	CHECK(66, a == 9L, 1);

	/*
	 * A long used as a truth value has to test all four bytes - a
	 * value with nothing in its low half is still true.  Written with
	 * if rather than ?:, which is a separate mechanism and is broken
	 * at every width; see the ternary note in the runtime tests.
	 */
	b = 0L;
	a = 0L;
	if (a) b = 1L;
	CHECK(67, b == 0L, 1);
	a = 0x00010000L;
	b = 0L;
	if (a) b = 1L;
	CHECK(68, b == 1L, 1);
	a = 0x00000001L;
	b = 0L;
	if (a) b = 1L;
	CHECK(69, b == 1L, 1);
	CHECK(70, !a, 0);
	a = 0L;
	CHECK(71, !a, 1);

	/*
	 * Everything above lives in a global.  The storage a long is kept
	 * in decides how it is reached, so the rest of the classes get
	 * their own turn: a local, a parameter, a return value, a pointer
	 * target and an array element.
	 */
	loc = 0x12345678L;
	CHECK(72, loc == 0x12345678L, 1);
	loc = loc + 1L;
	CHECK(73, loc == 0x12345679L, 1);
	loc2 = loc;
	CHECK(74, loc2 == 0x12345679L, 1);

	CHECK(75, idl(0x00010002L) == 0x00010002L, 1);
	CHECK(76, addl(0x00010000L, 5L) == 0x00010005L, 1);
	loc = 7L;
	CHECK(77, idl(loc) == 7L, 1);

	a = 0x0a0b0c0dL;
	lp = &a;
	CHECK(78, *lp == 0x0a0b0c0dL, 1);
	*lp = 0x01020304L;
	CHECK(79, a == 0x01020304L, 1);

	arr[0] = 100L;
	arr[1] = 200L;
	CHECK(80, arr[0] == 100L, 1);
	CHECK(81, arr[1] == 200L, 1);
	s = 1;
	CHECK(82, arr[s] == 200L, 1);
	arr[s] = 300L;
	CHECK(83, arr[1] == 300L, 1);
	CHECK(84, arr[0] == 100L, 1);

	/*
	 * A comparison widened into a long.  Also the case that found the
	 * helpers clobbering BC: s lives there, and every one of them
	 * takes its second operand off the stack with a pop bc.
	 */
	s = 1;
	loc = (s < 2);
	CHECK(85, loc == 1L, 1);
	loc = (s > 2);
	CHECK(86, loc == 0L, 1);
	loc = (1 < 2);
	CHECK(87, loc == 1L, 1);

	/* stepping a local, where the address has to be worked out */
	loc = 0x0000ffffL;
	loc++;
	CHECK(88, loc == 0x00010000L, 1);
	++loc;
	CHECK(89, loc == 0x00010001L, 1);
	loc--;
	CHECK(90, loc == 0x00010000L, 1);

	return 0;
}
