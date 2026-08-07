/* Test: nested loops */
main()
{
	int i;
	int j;
	for (i = 0; i < 5; i = i + 1) {
		for (j = 0; j < 5; j = j + 1) {
			if (j == 3) {
				break;
			}
		}
	}
	return i;
}
