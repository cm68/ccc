char buffer[9000];
char *colp[72];
f(i)
int i;
{
	register char **p;

	p = colp + i;
	if (*p >= buffer)
		return 1;
	return 0;
}
