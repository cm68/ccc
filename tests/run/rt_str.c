/*
 * String literals, and the joining of adjacent ones.
 *
 * C joins adjacent string literals in translation phase 6, which comes
 * after macro expansion in phase 4.  A preprocessor that instead joins
 * them in its lexer gets the common case right and every macro case
 * wrong, because two literals that only became neighbours when a macro
 * expanded were never adjacent in the character stream the lexer saw.
 *
 * That is not a corner: pass2's rule table is written as named
 * instruction fragments set side by side, so nearly every template in
 * the compiler is a run of expanded macros.  The failure shows up as a
 * syntax error in the pass that reads the token stream, a long way from
 * the cause, so the checks below name the shapes directly.
 *
 * Bytes with the high bit set matter too - the fragment names expand to
 * exactly those - so a few of these carry one across the join.
 */
#include "rt.h"

#define A	"ab"
#define B	"cd"
#define C	"ef"
#define AB	A B
#define HI	"\300"
#define LO	"\201"

char *p, *q;
char *tab[3];
short i;

/* strlen is not assumed to be linkable on every path here */
short len(s) char *s; { short n; n = 0; while (*s++) n++; return n; }

short
same(a, b) char *a, *b;
{
	while (*a && *a == *b) { a++; b++; }
	return *a == *b;
}

main()
{
	/* two literals, which the lexer already handled */
	p = "ab" "cd";
	CHECK(1, len(p), 4);
	CHECK(2, same(p, "abcd"), 1);

	/* a macro beside a literal, either way round */
	p = A "cd";
	CHECK(3, same(p, "abcd"), 1);
	p = "ab" B;
	CHECK(4, same(p, "abcd"), 1);

	/* two macros, which is the case the lexer cannot see */
	p = A B;
	CHECK(5, len(p), 4);
	CHECK(6, same(p, "abcd"), 1);

	/* three of them, so the join has to accumulate */
	p = A B C;
	CHECK(7, len(p), 6);
	CHECK(8, same(p, "abcdef"), 1);

	/* a macro that is itself a join, expanded inside another */
	p = AB C;
	CHECK(9, same(p, "abcdef"), 1);
	p = C AB;
	CHECK(10, same(p, "efabcd"), 1);

	/* high-bit bytes survive the join in both positions */
	p = HI LO;
	CHECK(11, len(p), 2);
	CHECK(12, (p[0] & 0xff), 0300);
	CHECK(13, (p[1] & 0xff), 0201);
	p = HI "or a\n";
	CHECK(14, (p[0] & 0xff), 0300);
	CHECK(15, len(p), 6);
	p = "ld a,c\n" LO;
	CHECK(16, len(p), 8);
	CHECK(17, (p[7] & 0xff), 0201);

	/* an embedded NUL still ends the string, and does not end the join */
	p = A B;
	CHECK(18, p[4], 0);

	/* a comma between them keeps them apart */
	tab[0] = A;
	tab[1] = B;
	tab[2] = C;
	CHECK(19, same(tab[0], "ab"), 1);
	CHECK(20, same(tab[1], "cd"), 1);
	CHECK(21, same(tab[2], "ef"), 1);
	CHECK(22, len(tab[0]), 2);

	/* and so does anything else: a join must not reach past its run */
	p = A;
	q = B;
	CHECK(23, len(p), 2);
	CHECK(24, len(q), 2);
	CHECK(25, same(p, "ab"), 1);

	/* joined literals as an argument, where the run ends at the paren */
	CHECK(26, len(A B), 4);
	CHECK(27, same(A B, "abcd"), 1);
	CHECK(28, same(A, "ab"), 1);

	/* a join split over lines, which the line markers run through */
	p = A
	    B
	    C;
	CHECK(29, same(p, "abcdef"), 1);

	/* an empty literal contributes nothing but must not truncate */
	p = A "" B;
	CHECK(30, len(p), 4);
	CHECK(31, same(p, "abcd"), 1);
	p = "" A;
	CHECK(32, same(p, "ab"), 1);

	/* the run is still a string, so it indexes and walks like one */
	p = A B C;
	for (i = 0; i < 6; i++)
		if (p[i] != "abcdef"[i])
			return 33;
	CHECK(34, p[5], 'f');

	return 0;
}
