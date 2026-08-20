char *reend;
struct r {
	char *ad2;
};
struct r *rep;
f()
{
	register char *p;

	p = reend;
	if ((rep->ad2 = p) > reend)
		return 1;
	return 0;
}
