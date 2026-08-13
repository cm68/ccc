/*
 * Which way up a long sits in memory.
 *
 * Micronix keeps the HIGH word at the LOWER address, and ccc used to
 * put the low word there.  Every long that crossed between the two -
 * out of a system call, into an on-disk structure, through a file a
 * Whitesmiths-built program also reads - arrived with its halves
 * swapped.  ls -l printed a believable date in the wrong decade, and a
 * one-second difference read as nineteen hours, because promoting the
 * low word to the high multiplies it by 65536.  See NUXI.
 *
 * The bytes inside each half were never in dispute and neither was the
 * register convention; only the pairing of the halves moved.  So the
 * value checks here hold whatever the layout is, and the layout checks
 * are the ones that say which way up.
 *
 * The host is little-endian and its long is genuinely low word first,
 * so the layout checks are indexed by which build this is.  Everything
 * else has to agree in both.
 */
#include "rt.h"

/* which half sits at the lower address */
#ifdef RT_CCC
#define WHI 0
#define WLO 1
#else
#define WHI 1
#define WLO 0
#endif

long	g = 0x11223344;		/* an initialised global - the data path */
long	l;
long	arr[3];
unsigned short *wp;

/* the callee reads what it was handed: the argument push order */
long thru(a, v, b) short a; long v; short b; { return v; }

short hiof(v) long v; { return (short)((v >> 16) & 0xffff); }
short loof(v) long v; { return (short)(v & 0xffff); }

/* through a pointer, which is where the helpers do the work */
setl(p, v) long *p; long v; { *p = v; }
long getl(p) long *p; { return *p; }

main()
{
	long t;

	/* a long in a global, stored by the compiler */
	l = 0x11223344;
	wp = (unsigned short *)&l;
	CHECK(1, wp[WHI], 0x1122);
	CHECK(2, wp[WLO], 0x3344);

	/* the same long laid down by the assembler as an initialiser */
	wp = (unsigned short *)&g;
	CHECK(3, wp[WHI], 0x1122);
	CHECK(4, wp[WLO], 0x3344);
	CHECK(5, hiof(g), 0x1122);
	CHECK(6, loof(g), 0x3344);

	/* the halves must still be the halves, whatever the order */
	CHECK(7, hiof(l), 0x1122);
	CHECK(8, loof(l), 0x3344);

	/* pushed as an argument, and read back by the callee */
	t = thru(1, 0x55667788, 2);
	CHECK(9, hiof(t), 0x5566);
	CHECK(10, loof(t), 0x7788);

	/* stored and loaded through a pointer */
	setl(&l, 0x0A0B0C0DL);
	wp = (unsigned short *)&l;
	CHECK(11, wp[WHI], 0x0A0B);
	CHECK(12, wp[WLO], 0x0C0D);
	CHECK(13, hiof(getl(&l)), 0x0A0B);
	CHECK(14, loof(getl(&l)), 0x0C0D);

	/* an array element, which is reached by index rather than by name */
	arr[1] = 0x11223344;
	wp = (unsigned short *)&arr[1];
	CHECK(15, wp[WHI], 0x1122);
	CHECK(16, wp[WLO], 0x3344);
	CHECK(17, hiof(arr[1]), 0x1122);

	/* neighbours are not disturbed */
	arr[0] = 0x01020304;
	arr[2] = 0x05060708;
	CHECK(18, hiof(arr[1]), 0x1122);
	CHECK(19, loof(arr[1]), 0x3344);
	CHECK(20, hiof(arr[0]), 0x0102);
	CHECK(21, loof(arr[2]), 0x0708);

	/*
	 * The carry between the halves, which is the thing a swap makes
	 * plausible rather than obviously wrong: stepping 0xFFFF has to
	 * reach 0x10000 and not 0x1.
	 */
	l = 0xFFFFL;
	l = l + 1;
	CHECK(22, hiof(l), 1);
	CHECK(23, loof(l), 0);

	/* the same through the step helpers, on a long in memory */
	l = 0xFFFFL;
	l++;
	CHECK(24, hiof(l), 1);
	CHECK(25, loof(l), 0);
	/*
	 * Compared whole rather than half at a time: 0xFFFF in a short is
	 * -1 where an int is 16 bits and 65535 where it is 32, so the
	 * halves of this one would not mean the same thing in the two
	 * builds even when the layout is right.
	 */
	l--;
	CHECK(26, l == 0xFFFFL, 1);
	CHECK(27, hiof(l), 0);

	/*
	 * And the direction the bug was found in: two words written the
	 * way the kernel writes them, read back as one long.
	 */
	wp = (unsigned short *)&l;
	wp[WHI] = 0x2000;
	wp[WLO] = 0x5635;
	CHECK(28, hiof(l), 0x2000);
	CHECK(29, loof(l), 0x5635);
	CHECK(30, l == 0x20005635L, 1);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
