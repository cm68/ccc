/*
 * Test pointer type compatibility checking
 */

struct foo {
    int x;
};

struct bar {
    int y;
};

char carr[10];
int iarr[10];

int test_array_ptr_compat()
{
    char *cp;
    int *ip;

    /* Array to pointer - should be compatible */
    cp = carr;      /* OK: char[] -> char* */
    ip = iarr;      /* OK: int[] -> int* */

    return 0;
}

int test_same_type_ptr()
{
    int *p1;
    int *p2;

    /* Same type pointers - should be compatible */
    p1 = p2;        /* OK: int* -> int* */

    return 0;
}

int test_struct_ptr_compat()
{
    struct foo *fp1;
    struct foo *fp2;
    struct bar *bp;

    /* Same struct type - should be compatible */
    fp1 = fp2;      /* OK: struct foo* -> struct foo* */

    /* Different struct types - should ERROR */
    fp1 = bp;       /* ERROR: struct bar* -> struct foo* */

    return 0;
}

int test_base_type_compat()
{
    char *cp;
    int *ip;

    /* Different base types - should ERROR */
    cp = ip;        /* ERROR: int* -> char* */
    ip = cp;        /* ERROR: char* -> int* */

    return 0;
}

int test_signed_unsigned()
{
    unsigned int *up;
    int *sp;

    /* Signed vs unsigned - should ERROR */
    up = sp;        /* ERROR: int* -> unsigned int* */
    sp = up;        /* ERROR: unsigned int* -> int* */

    return 0;
}
