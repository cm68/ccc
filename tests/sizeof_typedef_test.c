/* Test sizeof with typedef - comprehensive test */

typedef int myint;
typedef char mychar;
typedef long mylong;

/* Test 1: sizeof(typedef_name) - type directly */
int test1a = sizeof(myint);
int test1b = sizeof(mychar);
int test1c = sizeof(mylong);

/* Test 2: Variables declared with typedef */
myint x;
mychar y;
mylong z;

/* Test 3: sizeof variable without parens */
int test3a = sizeof x;
int test3b = sizeof y;
int test3c = sizeof z;

/* Test 4: Regular int for comparison */
int a;
int test4 = sizeof a;
