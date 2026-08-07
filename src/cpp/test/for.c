/* Test: simple for loop */
main()
{
	int i;
	int sum;
	sum = 0;
	for (i = 0; i < 10; i = i + 1) {
		sum = sum + i;
	}
	return sum;
}
