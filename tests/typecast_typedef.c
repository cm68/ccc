/* Test type casting with typedefs */

typedef int myint_t;
typedef char *string_t;
typedef long *longptr_t;

struct point {
    int x;
    int y;
};

typedef struct point point_t;
typedef point_t *pointptr_t;

int x;
char *s;

/* Typedef scalar casts (should create CAST nodes) */
myint_t i1 = (myint_t) x;
int i2 = (int) i1;
myint_t i3 = (myint_t) 100;

/* Typedef pointer casts (should NOT create CAST nodes) */
string_t str1 = (string_t) s;
char *s2 = (char *) str1;
void *vp = (void *) str1;
string_t str2 = (string_t) vp;

/* Typedef struct pointer casts (should NOT create CAST nodes) */
point_t *pp1;
pointptr_t pp2 = (pointptr_t) pp1;
struct point *pp3 = (struct point *) pp2;
void *vp2 = (void *) pp2;

/* Mixed typedef and direct type casts */
longptr_t lp = (longptr_t) &x;
long *lp2 = (long *) lp;
int *ip = (int *) lp2;

/* Cast between typedef and base type for scalars */
int test_func(int arg)
{
    myint_t local = (myint_t) arg;
    int result = (int) local + 10;
    return result;
}
