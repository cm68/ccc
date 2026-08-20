int arr[10];
main()
{
	register int *p, *lim;
	int n;

	p = arr;
	lim = &arr[3];
	n = 0;
	while (++p < lim)
		n++;
	printf("%d\n", n);
	return 0;
}
