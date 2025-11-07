/*
 * Test pointer type compatibility checking - cases that should work
 */

struct point {
    int x;
    int y;
};

char carr[10];
int iarr[10];

int test_array_to_ptr()
{
    char *cp;
    int *ip;

    /* Array decays to pointer of same base type - OK */
    cp = carr;
    ip = iarr;

    return 0;
}

int test_same_base_type()
{
    int *p1;
    int *p2;
    char *c1;
    char *c2;

    /* Same base type pointers - OK */
    p1 = p2;
    c1 = c2;

    return 0;
}

int test_same_struct()
{
    struct point *pt1;
    struct point *pt2;

    /* Same struct type pointers - OK */
    pt1 = pt2;

    return 0;
}
