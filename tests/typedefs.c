/* small suite of typedef tests for parser/type support */

typedef int I;
I global_i;

typedef int *P;
P p1, p2;            /* p1 and p2 should both be int * */

typedef int A10[10];
A10 arr;             /* arr should be int[10] */

typedef int (*fn_t)(int);
fn_t fptr;           /* fptr is pointer-to-function(int)->int */

typedef struct S S_t;
struct S { int x; };
S_t s_inst;

void test_fn(void)
{
    typedef char InnerT;
    InnerT c;         /* InnerT should be char and be scoped to this block */
}

/* typedef redeclaration errors / shadowing test
   (depending on how strict you want the compiler: this file demonstrates
    shadowing inside a block and redeclaration at same scope) */

typedef int TD;
void shadow_test(void) {
    typedef char TD;  /* allowed: typedef name should shadow outer in block */
    TD local_td;      /* char */
}
/* end of tests/typedefs.c */