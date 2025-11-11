/*
 * Tests preprocessor macro definition and expansion with stringify operator
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

int k = add(3,5);

/* Test #undef */
#define TESTMACRO 100
int before_undef = TESTMACRO;

#undef TESTMACRO
/* After #undef, TESTMACRO is no longer defined */
/* int after_undef = TESTMACRO; -- would be undefined identifier error */

/* Test redefine after undef */
#define TESTMACRO 200
int after_redefine = TESTMACRO;




