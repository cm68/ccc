char buf[33];
main()
{
	char *b, *e;
	long t, nd;

	b = &buf[10];
	e = &buf[33];
	nd = 100;
	t = nd - (e - b);
	printf("%ld\n", t);
	return 0;
}
