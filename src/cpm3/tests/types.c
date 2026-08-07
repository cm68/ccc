struct s { int a; char b; long c; };
struct s v;
char *p;
unsigned u;
long l;

f()
{
	v.a = 1;
	v.b = 2;
	v.c = 3;
	u = 40000;
	l = 100000L;
	p = "hello";
	return v.a + v.b + (int)v.c + (int)u + (int)l + *p;
}
main() { return f(); }
