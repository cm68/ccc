/* Test: mixed loop types */
main()
{
	int i;
	int j;
	int k;
	for (i = 0; i < 3; i = i + 1) {
		j = 0;
		while (j < 3) {
			k = 0;
			do {
				k = k + 1;
			} while (k < 2);
			j = j + 1;
		}
	}
	return i;
}
