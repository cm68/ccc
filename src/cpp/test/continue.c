/* Test: continue statement */
main()
{
	int i;
	int sum;
	sum = 0;
	for (i = 0; i < 10; i = i + 1) {
		if (i == 5) {
			continue;
		}
		sum = sum + i;
	}
	return sum;
}
