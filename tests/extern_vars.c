/*
 * Test extern variable declarations
 */

/* Global variable definition */
int global_var;

/* Extern variable declaration (references variable defined elsewhere) */
extern int external_var;

/* Multiple extern declarations */
extern char ext_char;
extern short ext_short;
extern long ext_long;

/* Extern pointer */
extern int *ext_ptr;

/* Extern array declaration */
extern int ext_array[10];

/* Function using extern variables */
int
use_externs()
{
    int x;
    x = external_var;
    x = ext_char;
    x = ext_short;
    x = ext_long;
    return x;
}

/* Mix of extern and regular declarations */
extern int x1;
int x2;
extern int x3;
