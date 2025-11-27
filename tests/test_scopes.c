/* Test variable scope prefixes in AST output */

/* Global variables (should have _ prefix) */
int global_var;

/* Extern variables (should have _ prefix) */
extern int extern_var;

/* Static global (should have S prefix) */
static int static_global;

/* Test function with all scope types */
int
tAllScopes(x, y)
int x;   /* Arguments should have A prefix */
int y;
{
    int local1;           /* Locals should have no prefix */
    int local2;
    static int static_local;  /* Static local should have S prefix */

    local1 = x + y;
    local2 = global_var + extern_var;
    static_local = static_local + 1;

    return local1 + local2 + static_global + static_local;
}
