/*
 * Condition codegen: each if must branch on the flag its comparison
 * actually produced.  The rules yield Z, NZ, C or NC depending on the
 * operator, so a fixed "jp z" over the then-body is only correct for
 * one of them.
 */

int g;
int h;

int f()
{
	if (g == h) return 1;
	if (g != h) return 2;
	if (g < h) return 3;
	if (g >= h) return 4;
	if (g == 0) return 5;
	if (g != 0) return 6;
	if (g) return 7;
	if (!g) return 8;
	if (g <= h) return 9;
	if (g > h) return 10;
	return 0;
}

/*
 * Compare against zero must use the sign bit for signed operands:
 * sbc hl,de sets carry on an unsigned borrow, so "x < 0" would be
 * false for every x.  Unsigned operands keep the sbc form.
 */
unsigned int u;

int sgn()
{
	if (g < 0) return 1;
	if (g >= 0) return 2;
	if (u < 0) return 3;
	if (u >= 0) return 4;
	return 0;
}
