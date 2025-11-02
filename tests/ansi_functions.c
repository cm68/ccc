/*
 * Comprehensive test for ANSI-style function declarations and definitions
 */

/* Forward declarations (prototypes) - various signatures */

/* No parameters */
int func_void(void);
void func_void_ret(void);

/* Single parameter */
int func_one_int(int x);
char func_one_char(char c);
long func_one_long(long l);
float func_one_float(float f);
double func_one_double(double d);

/* Multiple parameters - same type */
int func_two_ints(int a, int b);
int func_three_ints(int a, int b, int c);

/* Multiple parameters - mixed types */
int func_mixed_2(int x, char c);
int func_mixed_3(int x, char c, long l);
int func_mixed_4(int a, char b, long c, float d);

/* Pointer parameters */
int func_ptr_int(int *p);
int func_ptr_char(char *s);
int func_two_ptrs(int *p1, char *p2);

/* Pointer return types */
int *func_ret_ptr(int x);
char *func_ret_str(void);

/* Array parameters (decay to pointers) */
int func_array(int arr[]);
int func_array_sized(int arr[10]);
int func_2d_array(int arr[10][20]);

/* Mixed pointers and values */
int func_mixed_ptr(int x, int *p, char c, char *s);

/* Now provide definitions for some of these */

int func_void(void) {
    return 42;
}

int func_one_int(int x) {
    return x + 1;
}

int func_two_ints(int a, int b) {
    return a + b;
}

int func_mixed_2(int x, char c) {
    return x + c;
}

int func_mixed_3(int x, char c, long l) {
    return x + c + l;
}

int func_ptr_int(int *p) {
    return *p;
}

int func_two_ptrs(int *p1, char *p2) {
    return *p1 + *p2;
}

int *func_ret_ptr(int x) {
    return 0;
}

/* Test that forward declaration matches definition */
int forward_test(int a, char b);

int forward_test(int a, char b) {
    return a + b;
}

/* Multiple forward declarations of same function (should be OK) */
int multi_forward(int x);
int multi_forward(int x);
int multi_forward(int x);

int multi_forward(int x) {
    return x * 2;
}
