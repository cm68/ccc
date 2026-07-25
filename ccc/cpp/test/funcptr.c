/* Test: function pointer declarator syntax through filtknr
 *
 * - ANSI prototype of a function returning a function pointer
 * - K&R definition of the same shape (the signal() case), with a
 *   function pointer parameter declared K&R style
 * - function pointer variables at file scope
 * - ANSI definition with a function pointer parameter (passthrough)
 * - K&R definition mixing plain and function pointer parameters
 */

extern void (*signal(int, void (*)(int)))(int);

static void (*where)();

void (*
signal(sig, action))()
int	sig;
void (*	action)(int);
{
	void (*	prev)();

	prev = where;
	where = action;
	return prev;
}

int apply(fn, x)
int (*	fn)(int);
int	x;
{
	return (*fn)(x);
}

int use(int (*cmp)(int, int), int a, int b)
{
	return cmp(a, b);
}
