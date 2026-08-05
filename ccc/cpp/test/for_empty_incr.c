/* Test: inner for increment must not leak into an outer for(;;)
 * trailer - the empty outer increment saved nothing, so nothing
 * must come back when the inner loop pops */
main()
{
	int i;
	int total;
	total = 0;
	for (;;) {
		for (i = 0; i < 3; i = i + 1) {
			total = total + 1;
		}
		break;
	}
	return total;
}
