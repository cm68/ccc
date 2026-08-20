f(s)
char *s;
{
	register char *p;

	p = s;
	if (*p++=='.')
		return 1;
	return 0;
}
