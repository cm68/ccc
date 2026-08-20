char buffer[9000];
char *colp[72];
f()
{
	register char **p;

	p = colp;
	if (*p >= buffer)
		return 1;
	return 0;
}
