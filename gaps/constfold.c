char ybuf[256];
f(s)
char *s;
{
	if (s >= ybuf+256-5)
		return 1;
	return 0;
}
