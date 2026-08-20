struct opt {
	char oletter;
	int *ovar;
	int odefault;
};
int flag;
struct opt options[2];
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
