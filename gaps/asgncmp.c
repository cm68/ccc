char *reend;
struct r {
	char *ad2;
} rep;
f(p)
char *p;
{
	if ((rep.ad2 = p) > reend)
		return 1;
	return 0;
}
