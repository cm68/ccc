/*
 * Test CPP conditional directives: #if, #elif, #else, #endif
 * Note: This file tests #if/#elif/#else/#endif. See ifdef_test.c and ifndef_test.c for #ifdef/#ifndef tests.
 */

/* Test 1: Basic #if 1 (always true) */
#if 1
int if_true = 1;
#endif

/* Test 2: #if 0 (always false - should be skipped) */
#if 0
int if_false = 999;
#endif

/* Test 3: #if with #else - true path */
#if 1
int if_else_true = 1;
#else
int if_else_false = 0;
#endif

/* Test 4: #if with #else - false path */
#if 0
int if_else2_true = 1;
#else
int if_else2_false = 0;
#endif

/* Test 5: Nested #if */
#if 1
int nested_outer = 1;
#if 1
int nested_inner = 2;
#endif
int nested_outer_end = 3;
#endif

/* Test 6: Nested #if with mixed true/false */
#if 1
int nested_mix1 = 1;
#if 0
int nested_mix2 = 999;
#endif
int nested_mix3 = 3;
#endif

/* Test 7: Nested #if with #else branches */
#if 1
int nested_else1 = 1;
#if 0
int nested_else2 = 999;
#else
int nested_else3 = 3;
#endif
int nested_else4 = 4;
#endif

/* Test 8: Complex nesting with multiple levels */
#if 1
int complex1 = 1;
#if 1
int complex2 = 2;
#if 0
int complex3 = 999;
#else
int complex3_else = 3;
#endif
int complex4 = 4;
#endif
int complex5 = 5;
#endif

/* Test 9: Multiple sequential conditionals */
#if 1
int seq1 = 1;
#endif

#if 0
int seq2 = 999;
#endif

#if 1
int seq3 = 3;
#endif

/* Test 10: Conditional around function declaration */
#if 1
int conditional_func(int x, int y);
#endif

/* Test 11: Conditional around struct definition */
#if 1
struct conditional_struct {
    int x;
    int y;
};
#endif

/* Test 12: Multiple #else-if pattern (using #elif) */
#if 1
int config_value = 1;
#elif 1
int config_value = 2;
#else
int config_value = 3;
#endif

/* Test 13: Multiple #elif with last taken */
#if 0
int elif_test = 1;
#elif 0
int elif_test = 2;
#elif 1
int elif_test = 3;
#else
int elif_test = 4;
#endif

/* Test 14: Empty conditional blocks */
#if 1
#endif

#if 0
#endif

/* Test 15: Conditional with only whitespace/comments */
#if 1
/* Just a comment */
#endif

/* Test 16: Back-to-back conditionals without gap */
#if 1
int backtoback1 = 1;
#endif
#if 1
int backtoback2 = 2;
#endif

/* Test 17: Nested with all branches inactive */
#if 0
int inactive1 = 999;
#if 0
int inactive2 = 999;
#endif
int inactive3 = 999;
#endif

/* Test 18: Deep nesting (4 levels) */
#if 1
int deep1 = 1;
#if 1
int deep2 = 2;
#if 1
int deep3 = 3;
#if 1
int deep4 = 4;
#endif
#endif
#endif
#endif

/* Test 19: Conditional around multiple declarations */
#if 1
int multi1 = 1;
int multi2 = 2;
char multi3 = 3;
long multi4 = 4;
#endif

/* Test 20: #else without matching condition (all false) */
#if 0
int first = 1;
#elif 0
int second = 2;
#else
int neither = 3;
#endif

/* Test 21: Conditional compilation of array */
#if 1
int conditional_array[10];
#endif

/* Test 22: Conditional compilation of pointer */
#if 1
char *conditional_ptr;
#endif

/* Test 23: Expression evaluation in #if (if supported) */
#define VALUE 5
#if VALUE > 3
int expr_true = 1;
#else
int expr_false = 0;
#endif
