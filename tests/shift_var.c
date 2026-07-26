/*
 * Shifts by a count only known at runtime.  The Z80 has no variable
 * shift, so these become a loop.  Two things must hold:
 *
 *  - a right shift is arithmetic for a signed value and logical for an
 *    unsigned one, so the sign of the shifted value picks sra or srl
 *  - the loop must be guarded, because C defines "x << 0" as x and an
 *    unguarded loop body would run once
 */

int i, n;
unsigned int u;

int sh()
{
	i = i << n;
	i = i >> n;		/* signed: sra h / rr l */
	u = u >> n;		/* unsigned: srl h / rr l */
	u = u << n;
	return i;
}
