/* Test: break statement */
main()
{
	int i;
	for (i = 0; i < 100; i = i + 1) {
		if (i == 50) {
			break;
		}
	}
	return i;
}
