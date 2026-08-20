struct opt {
	char oletter;
	char *ovar;
	int odefault;
};
char a = 0;
char b = 1;
struct opt oa = { 'a', &a, 1 };
struct opt ob = { 'b', &b, 0 };
f(o)
struct opt *o;
{
	*(o->ovar) = ! *(o->ovar);
}
g(o)
struct opt *o;
{
	*(o->ovar) = ! o->odefault;
}
main()
{
	f(&oa); f(&ob);
	printf("toggle: %d %d (want 1 0)\n", a, b);
	a = 0; b = 1;
	g(&oa); g(&ob);
	printf("default: %d %d (want 0 1)\n", a, b);
	return 0;
}
