/* Test type casting including structure pointers */

struct foo {
    int x;
    int y;
};

struct bar {
    char c;
    int i;
};

int x;
char c;
long l;

/* Pointer-to-pointer casts (should NOT create CAST nodes in AST) */
int *ip;
char *cp1 = (char *) ip;
void *vp = (void *) ip;
int *ip2 = (int *) vp;

/* Structure pointer casts (should NOT create CAST nodes) */
struct foo *fp;
struct bar *bp;
void *vp2 = (void *) fp;
struct foo *fp2 = (struct foo *) vp2;
struct bar *bp2 = (struct bar *) fp;  /* different struct types */

/* Scalar casts (should create CAST nodes) */
char c1 = (char) x;
long l1 = (long) x;
int i1 = (int) l;
int i2 = (int) c;

/* Pointer to/from scalar casts (should create CAST nodes) */
int addr = (int) ip;
long addr2 = (long) fp;
char *cp2 = (char *) x;
struct foo *fp3 = (struct foo *) l;

/* Nested casts */
char c2 = (char) (int) (long) x;
void *vp3 = (void *) (char *) ip;

/* Cast in expressions */
int tCastExpr(int n)
{
    int a;
    char b;
    struct foo *p;

    a = (int) b + 10;
    b = (char) (a * 2);
    p = (struct foo *) (a + 100);

    return (int) p;
}

/* TODO: Function pointer casts not yet fully supported
 * int (*func_ptr)(int, int);
 * void *vp4 = (void *) func_ptr;
 * int (*func_ptr2)(int, int) = (int (*)(int, int)) vp4;
 */
