/* Test sizeof operator */

/* Test sizeof with basic types */
int test_char = sizeof(char);
int test_short = sizeof(short);
int test_int = sizeof(int);
int test_long = sizeof(long);

/* Test sizeof with unsigned types */
int test_uchar = sizeof(unsigned char);
int test_ushort = sizeof(unsigned short);
int test_ulong = sizeof(unsigned long);

/* Test sizeof with pointers */
int test_ptr = sizeof(int *);
int test_charptr = sizeof(char *);

/* Test sizeof without parentheses (expression form) */
int x;
int test_expr = sizeof x;

/* Test sizeof with expression */
int test_expr2 = sizeof(x + 1);

/* Final marker */
int final = 99;
