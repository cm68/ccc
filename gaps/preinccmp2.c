/* pointer pre-increment compared against an inline &arr[N] of a
   LOCAL array.  The bound (&arr[N] = frame + N*size) is computed
   into hl, then the frame-based pre-increment loads its operand
   into hl and the bound is lost; sbc runs against the size constant
   left in de.  The pre-increment operand must sit in the frame, so
   four register pointers hold the registers and force p1 to spill. */
main()
{
	int arr[150];
	register int *p1, *p2, *p3, *p4, *p5;

	p1 = p2 = p3 = p4 = p5 = &arr[0];
	if (++p1 > &arr[150])	/* false: &arr[0] > &arr[150] is not */
		return 1;
	return (p2 == p3) && (p4 == p5) && (p1 != 0) ? 0 : 0;
}
