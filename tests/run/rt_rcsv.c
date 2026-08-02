/*
 * The routines that share the rcsv prologue.
 *
 * rcsv loads the first three argument words into HL, DE and BC - so
 * it destroyed the caller's register variable, and every one of
 * these clobbered BC.  It saves it now, which shifted the argument
 * offsets by two and gave them a new exit, rcret, that pops it.  If
 * either half of that is wrong these return the right answer with
 * the wrong register, or the wrong answer.
 */
#include "rt.h"

extern char *strncpy(), *strncat(), *index(), *rindex();
extern char *strchr(), *strrchr();
extern int strncmp();

char dst[16];
char src[8];

/* keep a register variable live across each call */
int
guard(n, which)
register int n;
int which;
{
	char b[16];
	char *p;

	strcpy(src, "abcdef");
	switch (which) {
	case 0: strncpy(b, src, 4); b[4] = 0; if (b[3] != 'd') return -1; break;
	case 1: strcpy(b, "xy"); strncat(b, src, 3); if (b[4] != 'c') return -2; break;
	case 2: if (strncmp(src, "abcXXX", 3) != 0) return -3; break;
	case 3: p = index(src, 'c'); if (!p || *p != 'c') return -4; break;
	case 4: p = rindex(src, 'e'); if (!p || *p != 'e') return -5; break;
	case 5: p = strchr(src, 'd'); if (!p || *p != 'd') return -6; break;
	case 6: p = strrchr(src, 'b'); if (!p || *p != 'b') return -7; break;
	}
	return n;			/* n must survive in BC */
}

int
main()
{
	int i;

	for (i = 0; i < 7; i++)
		CHECK(i + 1, guard(4242, i), 4242);

	/* and the values themselves */
	strcpy(src, "abcdef");
	strncpy(dst, src, 4);
	dst[4] = 0;
	CHECK(8, dst[0] == 'a' && dst[3] == 'd', 1);
	CHECK(9, strncmp("abc", "abd", 2), 0);
	CHECK(10, strncmp("abc", "abd", 3) < 0, 1);
	CHECK(11, *index(src, 'f'), 'f');
	CHECK(12, *strrchr(src, 'a'), 'a');
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
