int state;
int ue_width;
int sc_width = 80;
f(newcol)
int newcol;
{
	if (newcol + (state ? ue_width : 0) > sc_width)
		return (1);
	return (0);
}
main()
{
	printf("f(1)=%d f(200)=%d\n", f(1), f(200));
	return 0;
}
