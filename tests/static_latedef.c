/*
 * A function called before its static definition.
 *
 * K&R says a name called before it is declared is extern int, and
 * pass1 obliges - so the call in early() below compiles as _g.  The
 * definition further down says static, and the calls already emitted
 * are not revisited: phase 1 and phase 2 run per function, so
 * everything above the definition has been written out by the time it
 * is read.  One name, one file, two functions: the calls above go to
 * _g and the calls below to the local label.
 *
 * That is a link error only when nothing else in the program defines
 * the global.  The names that get made static are the ordinary ones -
 * expand, compare, lookup, getline - so let another file define one
 * and it links, quietly, with the two halves calling different code.
 *
 * Expected to FAIL.  The compiler has to say so at the definition,
 * where the file has not been written yet.  Adding "static int g();"
 * at the top is what the source wants and makes both calls agree.
 * See STATICLATEDEF.
 */

early()
{
	return g(1);
}

static int
g(a)
int a;
{
	return a + 1;
}

late()
{
	return g(2);
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
