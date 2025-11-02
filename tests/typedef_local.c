/* Test typedef with variables and sizeof
 *
 * Note: Local variable declarations inside function bodies don't work yet,
 * so this test uses global variables instead.
 */

typedef int myint;
typedef char mychar;

/* Global variables declared with typedef'd types */
myint x;
mychar y;

/* Test sizeof with typedef'd variables */
int test1 = sizeof x;
int test2 = sizeof(x);
int test3 = sizeof y;
int test4 = sizeof(y);

/* Test sizeof with typedef'd type names */
int test5 = sizeof(myint);
int test6 = sizeof(mychar);
