/*
 * Test forward declaration of struct in typedef
 */

/* Forward declare struct S via typedef */
typedef struct S S_t;

/* Now define struct S */
struct S { int x; };

/* Use the typedef */
S_t s_inst;
