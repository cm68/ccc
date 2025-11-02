/* Simple test of sizeof with typedef - focuses on sizeof(typedef_name) */

/* These typedef declarations will be parsed but not yet functional */
typedef int myint;
typedef char mychar;
typedef long mylong;
typedef int *intptr;
typedef char *charptr;

typedef struct {
    int x;
    int y;
} point_t;

/* Note: sizeof(typedef_name) won't work until typedef implementation is complete
 * This test file documents the expected behavior for when typedef is implemented.
 *
 * Expected results when typedef works:
 * - sizeof(myint) should equal sizeof(int) = 4
 * - sizeof(mychar) should equal sizeof(char) = 1
 * - sizeof(mylong) should equal sizeof(long) = 4
 * - sizeof(intptr) should equal sizeof(int *) = 4
 * - sizeof(charptr) should equal sizeof(char *) = 4
 * - sizeof(point_t) should equal 8 (two ints)
 */

int x;  /* Regular declaration that should work */
