char buf[33];
main()
{
	char *b;
	long t, nd;

	b = &buf[10];
	nd = 100;
	t = nd;
	t -= &buf[33] - b;
	printf("%ld\n", t);
	return 0;
}
