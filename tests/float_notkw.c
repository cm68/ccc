/*
 * float and double are not reserved words.
 *
 * There is no floating point: no float or double type, no float
 * constants, no float rules in pass2.  The two words were dropped
 * from the keyword table rather than kept as types that gripe, so a
 * program that has to port floating point code can name its own
 * representation with them:
 *
 *	struct myflt { unsigned long bits; };
 *	typedef struct myflt double;
 *
 * and carry on.  What such a program gives up is binary operators on
 * the type - and structure assignment and structure return, which the
 * compiler does not have either - so the arithmetic has to be calls
 * taking pointers.  See libsrc/include/math.h.
 *
 * This file only has to compile.  It is in the cpp corpus so the
 * lexeme stream is watched too: if either word went back into the
 * keyword table, the tokens here would stop being SYM.
 */

struct myflt { unsigned long bits; };

typedef struct myflt double;
typedef struct myflt float;

double done;
float fone;

/*
 * Arithmetic has to be written as calls taking pointers, since there
 * are no binary operators for the type and a structure is neither
 * passed by value nor returned.
 */
void
fladd(a, b, r)
double *a;
double *b;
double *r;
{
	r->bits = a->bits + b->bits;
}

int
usefl()
{
	struct myflt t;

	done.bits = 0x3f800000L;
	fone.bits = 0x40000000L;
	fladd(&done, &fone, &t);
	return (int)(t.bits >> 24);
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
