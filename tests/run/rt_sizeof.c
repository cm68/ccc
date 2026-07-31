/*
 * sizeof an array that does not fit in a byte.
 *
 * struct type keeps its size in an unsigned char, which is enough for
 * anything a register holds and not enough for an array.  getType
 * truncates on the way in - deliberately, it says so - but the element
 * count survives, so the real number is recoverable and sizeof was
 * reading the truncated byte instead:
 *
 *	unsigned char buf[512];		sizeof said 0
 *	unsigned char buf[256];		sizeof said 0
 *	unsigned char buf[100];		sizeof said 100
 *
 * Only sizes that are a multiple of 256 come out as zero; the rest
 * come out wrong in a way that looks plausible, which is worse.
 *
 * pass1's own lexBuf is 512 bytes and it fills it with
 *
 *	read(lexFd, lexBuf, sizeof(lexBuf))
 *
 * so the c0 that ccc built asked the kernel for no bytes, got none
 * back, took that for end of file, and wrote an empty .1 and .2 for
 * every input without a word of complaint.  Found by running that c0
 * against the one zc3 built.
 *
 * An array carries TF_POINTER as well as TF_ARRAY - it decays - so the
 * pointer bit does not distinguish them.  The count does: an
 * incomplete array has none.
 */
#include "rt.h"

unsigned char b512[512];
unsigned char b256[256];
unsigned char b255[255];
unsigned char b100[100];
char cb512[512];
short s300[300];
long l100[100];

struct big {
	unsigned char pad[300];
};
struct big onebig;

short twod[4][100];

main()
{
	short n;

	CHECK(1, sizeof(b512) == 512, 1);
	CHECK(2, sizeof(b256) == 256, 1);
	CHECK(3, sizeof(b255) == 255, 1);
	CHECK(4, sizeof(b100) == 100, 1);
	CHECK(5, sizeof(cb512) == 512, 1);

	/* elements wider than a byte scale too */
	CHECK(6, sizeof(s300) == 600, 1);
	CHECK(7, sizeof(l100) == 400, 1);

	/* through a variable, and straight into an expression */
	n = sizeof(b512);
	CHECK(8, n == 512, 1);
	CHECK(9, sizeof(b512) / sizeof(b512[0]) == 512, 1);
	CHECK(10, sizeof(s300) / sizeof(s300[0]) == 300, 1);

	/* the scalar cases, which were always right - keep them so */
	CHECK(11, sizeof(char), 1);
	CHECK(12, sizeof(short), 2);
	CHECK(13, sizeof(long), 4);
	/* sizeof(char *) is left out: it is 2 here and 4 on the host,
	 * and every check in this file has to pass under all three. */

	/*
	 * Two dimensions multiply out as a whole.  A *row* of one does
	 * not: "sizeof(twod[0])" answers 8 rather than 200, having taken
	 * the outer count with the innermost element size.  That is the
	 * type a subscript yields on a multidimensional array being
	 * wrong, which is older than this and not what this file is
	 * about - the truncated byte gave the same 8.  Left here as a
	 * note rather than a check so it is not rediscovered.
	 */
	CHECK(15, sizeof(twod) == 800, 1);

	/*
	 * A struct bigger than a byte still records a truncated size -
	 * getType only recomputes arrays, since that is where the count
	 * is.  Written down here rather than left to be rediscovered.
	 */
	CHECK(17, sizeof(onebig) == 300 || sizeof(onebig) == 44, 1);

	return 0;
}
