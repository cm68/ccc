/* Test sizeof with typedef'd variables */

typedef int myint;

myint a;
int test1 = sizeof a;
int test2 = sizeof(a);

int b;
int test3 = sizeof b;
int test4 = sizeof(b);
