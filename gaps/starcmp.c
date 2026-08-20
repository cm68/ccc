#define BUFS 9000
char buffer[BUFS];
char *colp[72];
f(i)
int i;
{
	register char **p;

	p = &colp[i];
	(*p)++;
	if (*p >= &buffer[BUFS])
		*p = buffer;
	return 0;
}
