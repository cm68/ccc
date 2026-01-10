/*
 * test_filtknr.c - Worst case test for K&R to ANSI conversion filter
 *
 * Tests:
 * - K&R function definitions with multiple parameters
 * - Mixed pointer and non-pointer parameters
 * - Struct/union/enum type parameters
 * - Long parameter lists
 * - Parameters with complex types (pointers to functions, arrays)
 * - Already-ANSI functions (should pass through unchanged)
 */

/* Simple K&R function */
int simple(a, b)
int a;
int b;
{
	return a + b;
}

/* K&R with pointer parameters */
char *strcat_knr(dst, src)
char *dst;
char *src;
{
	char *p = dst;
	while (*p) p++;
	while ((*p++ = *src++));
	return dst;
}

/* K&R with struct parameter */
struct point { int x, y; };
int distance(p1, p2)
struct point p1;
struct point p2;
{
	int dx = p2.x - p1.x;
	int dy = p2.y - p1.y;
	return dx*dx + dy*dy;
}

/* K&R with many parameters (stress test) */
int many_params(a, b, c, d, e, f, g, h)
int a;
int b;
int c;
int d;
int e;
int f;
int g;
int h;
{
	return a + b + c + d + e + f + g + h;
}

/* K&R with mixed types */
long mixed_types(i, cp, l, vp)
int i;
char *cp;
long l;
void *vp;
{
	return i + l;
}

/* K&R with unsigned and short */
unsigned short convert(ui, us, uc)
unsigned int ui;
unsigned short us;
unsigned char uc;
{
	return us + uc;
}

/* Already ANSI - should pass through unchanged */
int ansi_func(int x, char *s, long l)
{
	return x + l;
}

/* ANSI void parameter */
int no_args(void)
{
	return 42;
}

/* K&R with no parameters (empty parens) */
int old_no_args()
{
	return 0;
}

/* K&R with array parameter (decays to pointer) */
int sum_array(arr, n)
int arr[];
int n;
{
	int i, sum = 0;
	for (i = 0; i < n; i++)
		sum += arr[i];
	return sum;
}

/* K&R with function pointer parameter */
int apply(fn, x)
int (*fn)();
int x;
{
	return (*fn)(x);
}

/* Nested struct types */
struct outer {
	struct inner {
		int val;
	} in;
};

int nested_struct(o)
struct outer o;
{
	return o.in.val;
}

/* Union parameter */
union data { int i; float f; };

int union_param(d)
union data d;
{
	return d.i;
}

/* Enum parameter */
enum color { RED, GREEN, BLUE };

int enum_param(c)
enum color c;
{
	return c;
}
