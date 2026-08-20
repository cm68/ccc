char buf[33];
main()
{
	char *b;
	long t, nd;
	int d;

	b = &buf[10];
	nd = 100;
	d = &buf[33] - b;
	t = nd;
	t -= d;
	printf("%d %ld\n", d, t);
	return 0;
}
