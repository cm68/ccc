/*
 * Reading and writing through a pointer.
 *
 * An array's name is its address; a pointer's name is not, and the
 * value has to be loaded before anything can be done through it.
 * pass1 unwrapped both alike, so "p[0]" was the byte at the pointer -
 * the low half of the pointer itself - rather than the byte it points
 * at.  "*p" was right, because that path never unwrapped anything,
 * which is why the two have to be asked separately here.
 *
 * The store side has its own trap: the address waits on the stack
 * while the value is worked out, and a byte comes back in A while a
 * word comes back in HL.  Assuming the word case stored the low half
 * of the address.  Both were live in the CP/M library.
 */
#include "rt.h"

struct s { short a; short b; };

char cbuf[4];
short sbuf[4];
char *cp;
short *sp;
char *lit = "ab";
struct s obj;
struct s *op;

/* a pointer parameter, which lands in a register rather than memory */
setb(where, v) char *where; short v;
{
	where[0] = v;
	where[1] = v + 1;
}

main()
{
	short i;

	cbuf[0] = 'x'; cbuf[1] = 'y';
	sbuf[0] = 100; sbuf[1] = 200;
	cp = cbuf;
	sp = sbuf;
	i = 1;

	/* reading: dereference and subscript have to agree */
	CHECK(1, *cp, 'x');
	CHECK(2, cp[0], 'x');
	CHECK(3, cp[1], 'y');
	CHECK(4, cp[i], 'y');
	CHECK(5, *sp, 100);
	CHECK(6, sp[0], 100);
	CHECK(7, sp[1], 200);
	CHECK(8, sp[i], 200);

	/* one pointing at a literal */
	CHECK(9, *lit, 'a');
	CHECK(10, lit[0], 'a');
	CHECK(11, lit[1], 'b');

	/* writing */
	cp[0] = 'p';
	CHECK(12, cbuf[0], 'p');
	cp[i] = 'q';
	CHECK(13, cbuf[1], 'q');
	sp[0] = 7;
	CHECK(14, sbuf[0], 7);
	sp[i] = 8;
	CHECK(15, sbuf[1], 8);
	*cp = 'r';
	CHECK(16, cbuf[0], 'r');

	/* compound assignment, where the address waits on the stack */
	cbuf[0] = 1; cbuf[1] = 2;
	sbuf[0] = 5; sbuf[1] = 6;
	cp[0] += 3;
	CHECK(17, cbuf[0], 4);
	cp[i] += 3;
	CHECK(18, cbuf[1], 5);
	sp[0] += 3;
	CHECK(19, sbuf[0], 8);
	sp[i] += 3;
	CHECK(20, sbuf[1], 9);

	/* stepping what it points at, and the pointer itself */
	cp[0]++;
	CHECK(21, cbuf[0], 5);
	++sp[1];
	CHECK(22, sbuf[1], 10);
	CHECK(23, *cp++, 5);
	CHECK(24, *cp, 5);
	cp = cbuf;
	CHECK(25, *cp, 5);

	/* through a struct pointer */
	obj.a = 10; obj.b = 20;
	op = &obj;
	CHECK(26, op->a, 10);
	CHECK(27, op->b, 20);
	op->a = 11;
	CHECK(28, obj.a, 11);
	op->b += 5;
	CHECK(29, obj.b, 25);

	/* and through one held in a register, which is where a parameter
	 * goes - the shape that was corrupting the library */
	cbuf[0] = 0; cbuf[1] = 0;
	setb(cbuf, 40);
	CHECK(30, cbuf[0], 40);
	CHECK(31, cbuf[1], 41);

	return 0;
}
