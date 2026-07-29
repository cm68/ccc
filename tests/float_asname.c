/*
 * The companion to float_notkw.c: float and double as ordinary
 * identifiers, with no typedef in sight.  Nothing here would parse if
 * either word were still in the keyword table.
 */

int float;
long double;

struct pair {
	int float;
	long double;
};

struct pair thepair;

int
setpair(f, d)
int f;
long d;
{
	thepair.float = f;
	thepair.double = d;
	float = f;
	double = d;
	return thepair.float + (int)thepair.double;
}

/* as a function name, and as a parameter name */
int
double_it(float)
int float;
{
	return float + float;
}

/* and as a local, shadowing the globals above */
int
localname()
{
	int double;

	double = 7;
	return double;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
