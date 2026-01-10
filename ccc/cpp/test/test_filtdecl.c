/*
 * test_filtdecl.c - Worst case test for declaration/initializer separation
 *
 * Tests:
 * - Simple declarations with initializers
 * - Multiple variables with mixed initializers
 * - Pointer declarations with stars
 * - Struct/union/enum definitions (should NOT be transformed)
 * - Nested initializers (struct/array)
 * - Function call initializers
 * - Cast expressions that look like declarations
 * - Typedefs (should register new type names)
 * - Array declarations with sizes
 * - Complex pointer combinations
 */

/* Type setup */
typedef int myint;
typedef char *string;
typedef struct node *nodeptr;

struct point { int x, y; };
union data { int i; float f; };
enum color { RED, GREEN, BLUE };

void test_simple_init(void)
{
	int x = 5;
	int y = 10, z = 20;
	char c = 'A';
	long l = 12345L;
}

void test_pointer_init(void)
{
	int *p = 0;
	char *s = "hello";
	int **pp = 0;
	void *vp = 0;
}

void test_multi_var_mixed(void)
{
	/* Some with init, some without */
	int a = 1, b, c = 3, d;
	char *s1 = "one", *s2, *s3 = "three";
}

void test_struct_init(void)
{
	struct point p1 = { 10, 20 };
	struct point p2 = { 0 };
}

void test_union_init(void)
{
	union data d1 = { 42 };
}

void test_array_decl(void)
{
	int arr[10];
	char str[100];
	int matrix[3][4];
}

void test_array_init(void)
{
	int arr[] = { 1, 2, 3, 4, 5 };
	char str[] = "hello";
}

void test_func_call_init(void)
{
	int x = foo();
	char *p = malloc(100);
	int y = bar(1, 2, 3);
}

void test_expr_init(void)
{
	int x = 1 + 2 * 3;
	int y = (5 > 3) ? 10 : 20;
	int z = sizeof(int);
}

void test_cast_not_decl(void)
{
	int x;
	/* These are casts, not declarations */
	x = (int)3.14;
	x = (int *)0;
	x = (struct point *)0;
	x = (unsigned long)p;
}

void test_struct_def_inline(void)
{
	/* Inline struct definition - braces should stay with struct */
	struct { int a; int b; } anon = { 1, 2 };
}

void test_union_def_inline(void)
{
	/* Inline union definition */
	union { int i; char c; } u = { 0 };
}

void test_enum_def_inline(void)
{
	/* Inline enum definition */
	enum { ONE = 1, TWO, THREE } e = ONE;
}

void test_typedef_usage(void)
{
	myint mi = 42;
	string s = "test";
	nodeptr np = 0;
}

void test_complex_ptr(void)
{
	int *a, *b, *c;
	int **pp, ***ppp;
	char *const cp = "const";
}

void test_nested_init(void)
{
	struct point pts[] = { {1,2}, {3,4}, {5,6} };
	int matrix[2][3] = { {1,2,3}, {4,5,6} };
}

void test_no_init(void)
{
	/* No initializers - should pass through unchanged */
	int a, b, c;
	char *p, *q;
	struct point pt;
}

void test_global_scope(void)
{
	/* These are inside function, but test the pattern */
	static int s = 100;
	extern int e;
}

/* Global declarations - filtdecl only processes inside functions */
int global_uninit;
int global_init = 42;
char *global_str = "global";

/* Struct definition at global scope - should pass through */
struct global_struct {
	int a;
	int b;
	struct point p;
};

/* Function declarations should pass through */
int foo(void);
char *bar(int x, int y);
