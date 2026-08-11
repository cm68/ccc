/*
 * K&R array parameters, run rather than merely compiled.
 *
 * Compiling proves cpp stopped aborting on the [.  It does not prove
 * the parameter DECAYED - that "int a[]" became "int *a" and not
 * something that reads the array by value or off by an indirection.
 * These read through the parameter and check the answer, which is the
 * only thing that tells a decayed pointer from a plausible-looking
 * one.
 *
 * Both legs run this: there is nothing target-specific about it, so
 * the host compiler is the reference, and it is the reference for the
 * SHAPE too - if ccc and the host disagree about what "char *av[]"
 * means as a parameter, one of them is wrong about C.  The compile
 * side of this is tests/kr_arrparm.c.
 */
#include "rt.h"

int sum(v, n)
int v[];
int n;
{
	int i, t;

	t = 0;
	for (i = 0; i < n; i++)
		t += v[i];
	return t;
}

int firstb(s)
char s[];
{
	return s[0] & 0xff;
}

/* the argv shape: walking a null-terminated vector of pointers */
int cnt(av)
char *av[];
{
	int i;

	i = 0;
	while (av[i])
		i++;
	return i;
}

/* two indirections, which is what av[1][2] is */
int deep(av)
char *av[];
{
	return av[1][2] & 0xff;
}

/* a written bound is ignored: this is int *, so it sees all four */
int bounded(a)
int a[2];
{
	return a[3];
}

/* two array declarators sharing a base type, and the order of them */
int both(a, b)
int a[], b[];
{
	return a[1] - b[1];
}

int mixed(n, av)
int n;
char *av[];
{
	return n + (av[0][0] & 0xff);
}

/* storing THROUGH the parameter: a decayed pointer is not read-only */
int bump(v, n)
int v[];
int n;
{
	int i;

	for (i = 0; i < n; i++)
		v[i] = v[i] + 1;
	return v[0];
}


/*
 * The ANSI spelling of the same thing, which is a different path and
 * was wrong in a different way.  "char *a[]" as a parameter is
 * "char **a", but the decay in prmDecl took a pointer level OFF
 * instead of adding one, so it came out "char *a" - the same type as
 * "char a[]".  *a was then a byte load out of a two-byte pointer.
 *
 * It hid because "int *a[]" is the spelling anyone tries first and
 * pointer-to-int and pointer-to-pointer are both two bytes on this
 * machine, so it read the right number by luck.  char and long do not
 * agree, and they are what these check.
 */
static int acnt(char *av[])
{
	int i;

	i = 0;
	while (av[i])
		i++;
	return i;
}

static int adeep(char *av[])
{
	return av[1][2] & 0xff;
}

static long alsum(long *a[], int n)
{
	int i;
	long t;

	t = 0;
	for (i = 0; i < n; i++)
		t += *a[i];
	return t;
}

static int aisum(int *a[], int n)
{
	int i, t;

	t = 0;
	for (i = 0; i < n; i++)
		t += *a[i];
	return t;
}

main()
{
	int a[4];
	int b[4];
	char *v[4];

	a[0] = 10; a[1] = 20; a[2] = 30; a[3] = 40;
	b[0] = 1;  b[1] = 2;  b[2] = 3;  b[3] = 4;
	v[0] = "zero";
	v[1] = "onexy";
	v[2] = "two";
	v[3] = 0;

	CHECK(1, sum(a, 4), 100);
	CHECK(2, firstb("A"), 'A');
	CHECK(3, cnt(v), 3);
	CHECK(4, deep(v), 'e');
	CHECK(5, bounded(a), 40);
	CHECK(6, both(a, b), 18);
	CHECK(7, mixed(5, v), 5 + 'z');

	CHECK(8, bump(a, 4), 11);
	CHECK(9, a[3], 41);

	/* the address arithmetic a decayed parameter allows */
	CHECK(10, sum(&a[1], 3), 21 + 31 + 41);

	/* the ANSI spelling of an array-of-pointer parameter */
	{
		long l0, l1, *lp[2];
		int i0, i1, *ip[2];

		CHECK(11, acnt(v), 3);
		CHECK(12, adeep(v), 'e');

		l0 = 100000L; l1 = 23L;
		lp[0] = &l0; lp[1] = &l1;
		CHECK(13, alsum(lp, 2) == 100023L, 1);

		i0 = 7; i1 = 9;
		ip[0] = &i0; ip[1] = &i1;
		CHECK(14, aisum(ip, 2), 16);
	}

	return 0;
}
