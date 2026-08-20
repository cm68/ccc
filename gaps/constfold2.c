char ybuf[256];
f(a)
char *a;
{
	register char *s;

	s = a;
	if (s >= ybuf+256-5)
		return 1;
	return 0;
}
