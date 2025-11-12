/*
 * cc2.h - Data structures for code generation (pass 2)
 *
 * Extends the basic expr/stmt structures from cc1.h with
 * code generation fields (asm_block and label numbers)
 */

#include <stdlib.h>
#include <string.h>

/*
 * Expression flags
 */
#define E_UNSIGNED  0x01        // Value is unsigned

/*
 * Expression tree node for code generation
 * This is the parse tree representation of expressions
 */
struct expr {
    unsigned char op;           // Operation ('+', '-', 'M', '=', '@', etc.)
    struct expr *left;          // Left operand
    struct expr *right;         // Right operand

    /* Type and value information from AST */
    char *type_str;             // Type annotation from AST (":s", ":b", ":l", ":p", etc.)
    long value;                 // Constant value (for numeric constants)
    char *symbol;               // Symbol name (for SYM nodes)
    unsigned char size;         // Result size in bytes (1=byte, 2=short/ptr, 4=long/float, 8=double)
    unsigned char flags;        // E_UNSIGNED, etc.

    /* Code generation fields */
    char *asm_block;            // Generated assembly code (or NULL)
    int label;                  // Label number (if needed for this expression)
};

/*
 * Statement tree node for code generation
 * This is the parse tree representation of statements
 */
struct stmt {
    unsigned char type;         // Statement type ('I', 'W', 'R', 'B', 'E', etc.)
    struct expr *expr;          // Expression (condition for if/while, value for return, etc.)
    struct expr *expr2;         // Second expression (for for-loops, assignments, etc.)
    struct expr *expr3;         // Third expression (for for-loops)

    struct stmt *then_branch;   // Then/body branch
    struct stmt *else_branch;   // Else branch (or NULL)
    struct stmt *next;          // Next statement in sequence

    char *symbol;               // Symbol name (for labels, declarations)
    char *type_str;             // Type annotation (for declarations)

    /* Code generation fields */
    int label;                  // Primary label number for this statement
    int label2;                 // Secondary label (for if/else end)
    char *asm_block;            // Generated assembly code (or NULL)
};

/*
 * Function context - holds parsed function tree and generation state
 */
struct function_ctx {
    char *name;                 // Function name
    char *params;               // Parameter list string
    char *rettype;              // Return type
    struct stmt *body;          // Function body statement tree
    int label_counter;          // For generating unique labels
};

/* Forward declarations from util.c */
int fdprintf(int fd, const char *fmt, ...);

/* Tree construction functions */
struct expr *new_expr(unsigned char op);
struct stmt *new_stmt(unsigned char type);
void free_expr(struct expr *e);
void free_stmt(struct stmt *s);

/* Width and signedness extraction from type annotations */
unsigned char get_size_from_type_str(const char *type_str);
unsigned char get_signedness_from_type_str(const char *type_str);

/* Code generation functions */
void generate_code(struct function_ctx *ctx);
void emit_assembly(struct function_ctx *ctx, int out_fd);

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
