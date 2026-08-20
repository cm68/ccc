struct opt {
	char oletter;
	char *ovar;
	int odefault;
};
char flag;
f(o)
struct opt *o;
{
	*(o->ovar) = ! *(o->ovar);
	return (0);
}
g(o)
struct opt *o;
{
	*(o->ovar) = ! o->odefault;
	return (0);
}
