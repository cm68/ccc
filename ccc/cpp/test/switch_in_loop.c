/* Test: switch inside loop - break in switch should NOT be transformed */
main()
{
	int i;
	int x;
	x = 0;
	for (i = 0; i < 10; i = i + 1) {
		switch (i) {
		case 5:
			break;
		default:
			x = x + i;
			break;
		}
		if (i == 7) {
			break;
		}
	}
	return x;
}
