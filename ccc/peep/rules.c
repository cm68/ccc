/*
 * rules.c - the rewrites
 *
 * Each rule looks at the head of the window, and either rewrites it
 * and returns 1 or leaves it alone and returns 0.  When one fires the
 * window is examined again from the top, so a rule may feed another.
 *
 * The counts are what justify each of these.  They were taken over
 * ccc/pass2/rewrite.c, the largest source the compiler can build -
 * 25315 bytes of text, 13161 instructions:
 *
 *	inc sp runs before a pop	445 sites	~600 bytes
 *	push X / pop Y / push Y		 58 sites	 116 bytes
 *	ld a,<mem> then ld r,a		131 sites	 131 bytes
 *	ld a,r then and 0			 14 sites	  28 bytes
 *
 * Control flow was already clean - no dead jumps, no unreachable
 * instructions, no jump chains - so there is nothing here for the
 * branch rewrites a peephole usually carries.
 */
#include <string.h>
#include <stdio.h>
#include "peep.h"

long n_incsp = 0;
long n_pushpop = 0;
long n_bounce = 0;
long n_and0 = 0;
long n_frame = 0;
long saved = 0;

/* does the key at window line i match s exactly */
static int
is(int i, char *s)
{
	if (i >= nwin || win[i].kind != L_INSN)
		return 0;
	return strcmp(win[i].key, s) == 0;
}

/* does the key at window line i start with s */
static int
starts(int i, char *s)
{
	if (i >= nwin || win[i].kind != L_INSN)
		return 0;
	return strncmp(win[i].key, s, strlen(s)) == 0;
}

/*
 * Stack cleanup after a call is emitted as one inc sp per byte.  When
 * the run is followed by a pop, that pop's register is about to be
 * loaded from the stack anyway, so it is free to use as a sink: two
 * inc sp (two bytes) become one pop of the same register (one byte).
 *
 * This is the only rule here that needs nothing proved.  The register
 * is dead by inspection - the very next instruction writes it - and
 * the stack moves by the same amount either way.
 */
static int
r_incsp(void)
{
	int n = 0, k, i;
	char buf[LLEN + 8];

	while (is(n, "inc sp"))
		n++;
	if (n < 2)
		return 0;
	if (!starts(n, "pop "))
		return 0;

	k = n / 2;
	sprintf(buf, "\t%s\n", win[n].key);
	delline(0, n - (n & 1));
	for (i = 0; i < k; i++)
		insline(0, buf);
	/* an odd byte, if there ever is one, keeps its inc sp */
	if (n & 1)
		insline(k, "\tinc sp\n");

	n_incsp++;
	saved += n - k - (n & 1);
	return 1;
}

/*
 * push X / pop Y / push Y - the value is moved into Y only to be
 * pushed straight back.  If nothing wants Y afterwards the move is
 * dead and the first push already put the right thing on the stack.
 *
 * This shows up because pass2 materialises an operand into hl before
 * pushing it, without knowing that the rule that produced the operand
 * had already left it somewhere pushable.
 */
static int
r_pushpop(void)
{
	char x[LLEN], y[LLEN];

	if (!starts(0, "push ") || !starts(1, "pop ") || !starts(2, "push "))
		return 0;
	strcpy(x, win[0].key + 5);
	strcpy(y, win[1].key + 4);
	if (strcmp(y, win[2].key + 5) != 0)
		return 0;
	if (strcmp(x, y) == 0)
		return 0;				/* push hl/pop hl/push hl: not ours */

	/* is Y wanted after the third line */
	if (!isdead(reads(win[1].key) | writes(win[1].key), 3))
		return 0;

	delline(1, 2);
	n_pushpop++;
	saved += 2;
	return 1;
}

/*
 * ld a,<something> then ld r,a - the Z80 can load most things straight
 * into r, and going through a costs the extra ld.  Absolute addresses
 * are the exception: only a can be loaded from (nn), so those stay.
 */
static int
r_bounce(void)
{
	char src[LLEN], dst;
	char buf[LLEN + 8];

	if (!starts(0, "ld a,") || !starts(1, "ld "))
		return 0;
	if (strlen(win[1].key) != 6)
		return 0;				/* "ld r,a" and nothing else */
	if (strcmp(win[1].key + 4, ",a") != 0)
		return 0;
	dst = win[1].key[3];
	if (!strchr("bcdehl", dst))
		return 0;

	strcpy(src, win[0].key + 5);
	/* (nn) loads only into a; (hl) and (ix+d) load into any register */
	if (src[0] == '(' && !memok(src))
		return 0;
	/* ld r,r' and ld r,(hl)/(ix+d)/(iy+d) and ld r,n are all encodable */
	if (!isdead(R_A, 2))
		return 0;

	sprintf(buf, "\tld %c,%s\n", dst, src);
	delline(0, 2);
	insline(0, buf);
	n_bounce++;
	saved += 1;
	return 1;
}

/*
 * and 0 always produces zero, so whatever was loaded into a first was
 * loaded for nothing.  Three bytes become one.
 *
 * pass2 emits this when a mask works out to zero rather than noticing
 * that the answer is a constant; the fix belongs in the rules table
 * too, but the sequence is cheap to spot here and the table is not the
 * only thing that can produce it.
 */
static int
r_and0(void)
{
	if (!starts(0, "ld a,") || !is(1, "and 0"))
		return 0;
	/* the load is only there to feed the and, whose answer ignores it */
	delline(0, 2);
	insline(0, "\txor a\n");
	n_and0++;
	saved += 2;
	return 1;
}

/*
 * The frame setup and teardown every function carries, moved into one
 * shared copy.  This is zc3's arrangement - it calls ncsv and jumps to
 * cret - but it cannot use zc3's helpers: those point ix at the frame
 * and save both index registers, and ccc points iy at the frame and
 * needs ix free.  fenter and fexit in libc/csv.s lay the frame out
 * exactly as these three instructions did, so every (iy+d) already
 * emitted still addresses what it did before.
 *
 *	push iy / ld iy,0 / add iy,sp	8 bytes -> call fenter	3
 *	ld sp,iy / pop iy / ret			5 bytes -> jp fexit		3
 *
 * 49 functions in rewrite.c, so 343 bytes there.  The register saves
 * and the frame allocation stay where they are: they are conditional,
 * and they have to happen after iy is pointing at the frame.
 */
static int
r_fenter(void)
{
	if (!is(0, "push iy") || !is(1, "ld iy,0") || !is(2, "add iy,sp"))
		return 0;
	/*
	 * fenter reaches its caller through hl.  On entry to a function
	 * that takes its arguments on the stack hl holds nothing, and
	 * this sequence only occurs there - but it is cheap to insist,
	 * and the day some other code grows an iy frame it will matter.
	 */
	if (!isdead(R_HL, 3))
		return 0;

	delline(0, 3);
	insline(0, "\tcall\tfenter\n");
	n_frame++;
	saved += 5;
	return 1;
}

static int
r_fexit(void)
{
	if (!is(0, "ld sp,iy") || !is(1, "pop iy") || !is(2, "ret"))
		return 0;

	delline(0, 3);
	insline(0, "\tjp\tfexit\n");
	n_frame++;
	saved += 2;
	return 1;
}

int
applyrules(void)
{
	if (r_fenter())
		return 1;
	if (r_fexit())
		return 1;
	if (r_incsp())
		return 1;
	if (r_pushpop())
		return 1;
	if (r_bounce())
		return 1;
	if (r_and0())
		return 1;
	return 0;
}

void
report(void)
{
	fprintf(stderr, "peep: frame %ld  incsp %ld  pushpop %ld  bounce %ld"
		"  and0 %ld = %ld bytes\n", n_frame, n_incsp, n_pushpop,
		n_bounce, n_and0, saved);
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
