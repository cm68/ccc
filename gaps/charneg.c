char opt;
f()
{
	opt = -1;
	if (opt == -1)
		return (1);
	return (0);
}
main()
{
	printf("f()=%d (want 1)\n", f());
	return 0;
}
