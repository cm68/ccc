/* Test sizeof with typedef'd types */

typedef int myint;
typedef char mychar;
typedef long mylong;

/* Basic typedef usage in sizeof */
int test1 = sizeof(myint);
int test2 = sizeof(mychar);
int test3 = sizeof(mylong);

/* Typedef'd pointer types */
typedef int *intptr;
typedef char *charptr;

int test4 = sizeof(intptr);
int test5 = sizeof(charptr);

/* Typedef of pointer used to make another pointer */
int test6 = sizeof(intptr *);

/* Typedef'd struct */
typedef struct {
    int x;
    int y;
} point_t;

int test7 = sizeof(point_t);

/* Typedef'd array */
typedef int arr10[10];
int test8 = sizeof(arr10);

/* Using typedef'd type in declarations */
myint a;
int test9 = sizeof(a);

point_t p;
int test10 = sizeof(p);
