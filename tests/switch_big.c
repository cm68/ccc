/*
 * A case value that does not fit a byte.
 *
 * The dispatch compares eight bits in all three of its shapes - see
 * src/pass2/CONDITIONS.md - and a word control goes to the no-match
 * label before the table is consulted, so an arm labelled outside
 * 0..255 can never be reached.
 *
 * It used to be reached by nobody and said by nobody: "case 256:" is
 * ordinary C, it compiled without a word, and it silently took the
 * default.  Where two out-of-range values shared a low byte the
 * duplicate-case check fired instead, on a switch that was perfectly
 * good C - which is how it surfaced, on a sector size switched over
 * 128, 256, 512, 1024 and 2048.
 *
 * c1 counts them and fails, so the complaint names the source being
 * compiled.  A .error goes into the output as well, for anyone who
 * assembles a .s kept from a -s run.
 *
 * The fix in the source is to switch on something that fits;
 * tests/formatmw.c divides its size by 128 and its labels become
 * 1, 2, 4, 8, 16.
 */
int r;

wide(v)
int v;
{
	switch (v) {
	case 1:
		r = 1;
		break;
	case 300:		/* past a byte: unreachable */
		r = 2;
		break;
	}
	return r;
}

negative(v)
int v;
{
	switch (v) {
	case -1:		/* also past a byte, the other way */
		r = 3;
		break;
	}
	return r;
}
