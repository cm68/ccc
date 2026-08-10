/*
 * K&R parameters whose declared type is an ARRAY.
 *
 *	main(ac, av) int ac; char *av[];
 *
 * A parameter declared as an array is a pointer - "int a[]" as a
 * parameter means "int *a", and a bound, if one is written, means
 * nothing at all.  Every program of the period spells argv this way,
 * which is how this was met: Morrow's 1982 hard disc formatter went
 * through in full except for its entry point.
 *
 * cpp's K&R normalizer had no case for the [ and fell into its abort
 * path, which puts the original K&R text back and steps aside.  c0 is
 * ANSI only, so it then met raw K&R and answered "fn array" - an
 * honest complaint about a shape it should never have been shown.
 * The same types spelled as pointers, and the same types in an ANSI
 * prototype, always worked.  See ARRAY_ERROR.
 */

int one(a)
int a[];
{
	return a[0];
}

int bytes(a)
char a[];
{
	return a[0];
}

/* the argv shape: an array of pointers is a pointer to a pointer */
int argvish(av)
char *av[];
{
	return **av;
}

int ptrs(a)
int *a[];
{
	return **a;
}

/* a bound on a parameter is ignored - this is still int * */
int bounded(a)
int a[10];
{
	return a[3];
}

/* an array parameter beside an ordinary one, in either order */
int mixed(n, av)
int n;
char *av[];
{
	return n + **av;
}

int mixed2(av, n)
char *av[];
int n;
{
	return n + **av;
}

/* two array declarators sharing one base type */
int two(a, b)
int a[], b[];
{
	return a[0] + b[0];
}

int twoptr(a, b)
char *a[], *b[];
{
	return **a + **b;
}

/* the spelling that always worked, so the two stay comparable */
int asptr(a)
char **a;
{
	return **a;
}

int main(ac, av)
int ac;
char *av[];
{
	return ac + **av;
}
