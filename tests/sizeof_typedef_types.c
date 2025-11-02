/* Test sizeof with typedef types (no variables) */

typedef int myint;
typedef char mychar;
typedef long mylong;
typedef short myshort;

/* Test sizeof(typedef_type) */
int test1 = sizeof(myint);
int test2 = sizeof(mychar);
int test3 = sizeof(mylong);
int test4 = sizeof(myshort);

/* Typedef of pointer */
typedef int *intptr;
typedef char *charptr;

int test5 = sizeof(intptr);
int test6 = sizeof(charptr);

/* Typedef of multi-level pointer */
typedef int **intptr2;
int test7 = sizeof(intptr2);

/* Typedef of struct */
typedef struct {
    int x;
    int y;
} point;

int test8 = sizeof(point);

/* Typedef of array */
typedef int intarray[10];
int test9 = sizeof(intarray);

/* Compare with direct types */
int test_int = sizeof(int);
int test_char = sizeof(char);
int test_long = sizeof(long);
int test_short = sizeof(short);
int test_intptr_direct = sizeof(int *);
