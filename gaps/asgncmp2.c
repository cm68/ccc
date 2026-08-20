char *reend;
struct r {
	char *ad2;
};
struct r reps[10];
f(p)
char *p;
{
	register struct r *rep;

	rep = reps;
	if ((rep->ad2 = p) > reend)
		return 1;
	return 0;
}
