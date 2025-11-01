/*
 * Tests declarations up to and including struct definitions with macros
 */

#define add(a,b) a+b
#define stringify(a) #a

int i;
int j = 8;
char moo[] = "this is a test";

char *m1 = stringify(this is another);

struct test_s {
	int i;
	int k;
} instance;
