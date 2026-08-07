/* Test: nested do-while loops */
main()
{
	int i;
	int j;
	i = 0;
	do {
		j = 0;
		do {
			j = j + 1;
		} while (j < 3);
		i = i + 1;
	} while (i < 5);
	return i;
}
