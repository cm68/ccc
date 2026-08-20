char opt;
f(a, b, c, d)
int a, b, c, d;
{
	if (a > b)
		return (0);
	if ((opt == -1 || opt == 1) && c <= d)
		return (1);
	return (0);
}
main()
{
	opt = -1;
	printf("f=%d (want 1)\n", f(1, 2, 3, 4));
	return 0;
}
