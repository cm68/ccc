/* Test: multi-line for loop - each part on different line */
main()
{
	int i;
	int x;
	for (
		i = 0;
		i < 10;
		i = i + 1
	) {
		x = i;
	}
	return x;
}
