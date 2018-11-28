int i;
int j = 8;

#include "test1.h"
#define	m y
#define	z(a,b) a = #b
#define	j
#define	glom(a,b) a ## b

struct test_s {
	int i;
	int k;
} instance;

foo(int a, char **b)
{
	struct test_s bar;

	bar.i = a;

	printf("%s: %d\n", b[0], bar.i);
	z(a, test);
	m;
	glom(xy,zzy);
}

int
main(a,b)
int a;
char **b;
{
	foo(a,b);
}




