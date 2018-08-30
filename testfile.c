int i;
int j = 8;

struct test_s {
	int i;
	int k;
} instance;

foo(int a, char **b)
{
	struct test_s bar;

	bar.i = a;

	printf("%s: %d\n", b[0], bar.i);
}

int
main(a,b)
int a;
char **b;
{
	foo(a,b);
}




