/*
 * parseast.c - Table-driven parser for AST S-expressions
 *
 * Reads AST output from cc1 and builds parse trees for code generation.
 * Modified to return tree nodes instead of printing directly.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "cc2.h"
#include "astio.h"

/* Forward declarations for static helper functions */
static struct expr *parse_expr(void);
static struct stmt *parse_stmt(void);

/* Parser state */
int out_fd = 1;  /* Assembly output (default: stdout) */
static int label_counter = 0;  /* For generating unique labels */

/*
 * Tree node allocation helpers
 */
struct expr *
new_expr(unsigned char op)
{
    struct expr *e = malloc(sizeof(struct expr));
    if (!e) {
        fdprintf(2, "parseast: out of memory allocating expr\n");
        exit(1);
    }
    e->op = op;
    e->left = NULL;
    e->right = NULL;
    e->type_str = NULL;
    e->value = 0;
    e->symbol = NULL;
    e->size = 2;  /* Default to short size */
    e->flags = 0; /* Default to signed */
    e->asm_block = NULL;
    e->cleanup_block = NULL;
    e->label = 0;
    return e;
}

struct stmt *
new_stmt(unsigned char type)
{
    struct stmt *s = malloc(sizeof(struct stmt));
    if (!s) {
        fdprintf(2, "parseast: out of memory allocating stmt\n");
        exit(1);
    }
    s->type = type;
    s->expr = NULL;
    s->expr2 = NULL;
    s->expr3 = NULL;
    s->then_branch = NULL;
    s->else_branch = NULL;
    s->next = NULL;
    s->symbol = NULL;
    s->type_str = NULL;
    s->label = 0;
    s->label2 = 0;
    s->asm_block = NULL;
    return s;
}

/*
 * Extract size in bytes from type annotation string
 * Type strings: ":b" (byte/1), ":s" (short/2), ":l" (long/4), ":p" (pointer/2),
 *               ":f" (float/4), ":d" (double/8)
 * Returns: size in bytes, or 2 (default short size) if no annotation
 */
unsigned char
get_size_from_type_str(const char *type_str)
{
    if (!type_str || *type_str != ':') {
        return 2;  /* Default to short size */
    }

    switch (type_str[1]) {
    case 'b':  /* byte/char */
        return 1;
    case 's':  /* short/int */
    case 'p':  /* pointer */
        return 2;
    case 'l':  /* long */
    case 'f':  /* float */
        return 4;
    case 'd':  /* double */
        return 8;
    default:
        return 2;  /* Default to short size */
    }
}

/*
 * Extract signedness from type annotation string
 * Pointers (:p) are unsigned (addresses)
 * Other types default to signed (signedness comes from ops like WIDEN/SEXT)
 * Returns: E_UNSIGNED flag if unsigned, 0 if signed
 */
unsigned char
get_signedness_from_type_str(const char *type_str)
{
    if (!type_str || *type_str != ':') {
        return 0;  /* Default to signed */
    }

    /* Pointers are unsigned (addresses) */
    if (type_str[1] == 'p') {
        return E_UNSIGNED;
    }

    /* All other types default to signed */
    /* Actual signedness comes from WIDEN (unsigned) vs SEXT (signed) ops */
    return 0;
}

/*
 * Extract size from full type name (used for declarations)
 * Type names: "_char_", "_uchar_", "_short_", "_ushort_", "_long_", "_ulong_",
 *             "_void_", "_boolean_", "_float_", "_double_", "_ptr_"
 * Returns: size in bytes
 */
unsigned char
get_size_from_typename(const char *typename)
{
    if (!typename) return 2;  /* Default to short */

    /* Check for common type names */
    if (strstr(typename, "char")) return 1;
    if (strstr(typename, "short")) return 2;
    if (strstr(typename, "long")) return 4;
    if (strstr(typename, "ptr")) return 2;
    if (strstr(typename, "float")) return 4;
    if (strstr(typename, "double")) return 8;
    if (strstr(typename, "void")) return 0;

    /* Default to short size */
    return 2;
}

/*
 * Check if a value is a power of 2 and return the shift amount
 * Returns: shift amount (0-31) if power of 2, -1 otherwise
 * Examples: 1->0, 2->1, 4->2, 8->3, 16->4, etc.
 */
static int
is_power_of_2(long value)
{
    int shift;

    /* Must be positive */
    if (value <= 0) {
        return -1;
    }

    /* Check if exactly one bit is set */
    if ((value & (value - 1)) != 0) {
        return -1;  /* Not a power of 2 */
    }

    /* Count trailing zeros to get shift amount */
    shift = 0;
    while ((value & 1) == 0) {
        value >>= 1;
        shift++;
    }

    return shift;
}

/*
 * Recognize multiplication by constant power of 2: 
 * This pattern can be strength-reduced to left shift: 
 * (* <expr> <const>) becomes (<< <expr> <shift_amount>)
 *
 * Pattern breakdown:
 *   - *: multiply operator
 *   - left: any expression
 *   - right: constant that is a power of 2
 *
 * Returns: shift amount (0-31) if pattern matches and constant is power of 2
 *          -1 if pattern doesn't match or constant is not power of 2
 * If matches, optionally fills out_expr with the expression to be shifted
 */
int
is_multiply_by_power_of_2(struct expr *e, struct expr **out_expr)
{
    int shift;

    /* Check multiply operator */
    if (!e || e->op != '*') {
        return -1;
    }

    /* Check right operand is constant */
    if (!e->right || e->right->op != 'C') {
        return -1;
    }

    /* Check if constant is power of 2 */
    shift = is_power_of_2(e->right->value);
    if (shift < 0) {
        return -1;
    }

    /* Pattern matches - extract expression if requested */
    if (out_expr) {
        *out_expr = e->left;
    }

    return shift;
}

/*
 * Recognize structure member access pattern: (M (+ (M:p <var>) <const>))
 * This pattern represents: *((struct_ptr) + offset)
 *
 * Pattern breakdown:
 *   - Outer M: dereference to get member value
 *   - +: pointer arithmetic (base + offset)
 *   - Inner M:p: load pointer-sized struct base address from variable
 *   - const: member offset within struct
 *
 * Returns: 1 if pattern matches, 0 otherwise
 * If matches, optionally fills out_var and out_offset with extracted values
 */
int
is_struct_member_access(struct expr *e, char **out_var, long *out_offset)
{
    struct expr *add;
    struct expr *ptr_load;
    struct expr *var;
    struct expr *offset;

    /* Check outer M (dereference) */
    if (!e || e->op != 'M') {
        return 0;
    }

    /* Check + (addition) */
    add = e->left;
    if (!add || add->op != '+') {
        return 0;
    }

    /* Check inner M:p (pointer dereference) */
    ptr_load = add->left;
    if (!ptr_load || ptr_load->op != 'M') {
        return 0;
    }
    if (!ptr_load->type_str || strcmp(ptr_load->type_str, ":p") != 0) {
        return 0;
    }

    /* Check variable reference */
    var = ptr_load->left;
    if (!var || var->op != '$' || !var->symbol) {
        return 0;
    }

    /* Check constant offset */
    offset = add->right;
    if (!offset || offset->op != 'C') {  /* 'C' is CONST operator */
        return 0;
    }

    /* Pattern matches - extract values if requested */
    if (out_var) {
        *out_var = var->symbol;
    }
    if (out_offset) {
        *out_offset = offset->value;
    }

    return 1;
}

/*
 * Create an ASM statement node for a label
 * Label format: "label_name:\n"
 */
static struct stmt *
create_label_asm(const char *label_name)
{
    struct stmt *s = new_stmt('A');
    char buf[128];

    snprintf(buf, sizeof(buf), "%s:", label_name);
    s->asm_block = malloc(strlen(buf) + 1);
    strcpy(s->asm_block, buf);

    return s;
}

void
free_expr(struct expr *e)
{
    if (!e) return;
    free_expr(e->left);
    free_expr(e->right);
    /* NOTE: type_str and symbol point to static buffers from
     * read_type()/read_symbol()
     * They should NOT be freed. Only asm_block and cleanup_block are dynamically allocated. */
    if (e->asm_block) free(e->asm_block);
    if (e->cleanup_block) free(e->cleanup_block);
    free(e);
}

void
free_stmt(struct stmt *s)
{
    if (!s) return;
    free_expr(s->expr);
    free_expr(s->expr2);
    free_expr(s->expr3);
    free_stmt(s->then_branch);
    free_stmt(s->else_branch);
    free_stmt(s->next);
    /* Free dynamically allocated fields */
    if (s->symbol) free(s->symbol);
    if (s->type_str) free(s->type_str);
    if (s->asm_block) free(s->asm_block);
    free(s);
}

/*
 * Handler functions for each operation
 * Each handler consumes the entire s-expression including closing paren
 */

/* Handler function type - now returns expr* */
typedef struct expr* (*handler_fn)(unsigned char op);

/* Generic handler that builds a generic expr node */
static struct expr *
handle_generic(unsigned char op)
{
    struct expr *e;
    int depth;

    e = new_expr(op);

    fdprintf(2, "OP_%02x", op);
    skip();

    /* Skip to closing paren - recursively handle any nested expressions */
    depth = 1;
    while (curchar && depth > 0) {
        if (curchar == '(') {
            depth++;
        } else if (curchar == ')') {
            depth--;
            if (depth == 0) {
                nextchar();  /* consume closing paren */
                return e;
            }
        }
        nextchar();
    }
    return e;
}

/* Expression handlers */

static struct expr *
handle_const(void)
{
    struct expr *e = new_expr('C');  // 'C' for constant
    e->value = read_number();

    /* Infer size from value magnitude */
    /* If value fits in 16 bits (signed or unsigned), use size 2, else size 4 */
    if (e->value >= -32768 && e->value <= 65535) {
        e->size = 2;  /* Fits in 16 bits */
    } else {
        e->size = 4;  /* Requires 32 bits (long) */
    }

    fdprintf(2, "CONST %ld", e->value);
    return e;
}

static struct expr *
handle_symbol(void)
{
    struct expr *e = new_expr('$');  // '$' for symbol
    char *sym = read_symbol();
    e->symbol = strdup(sym);
    fdprintf(2, "SYM %s", e->symbol);
    return e;
}

static struct expr *
handle_string(void)
{
    struct expr *e = new_expr('S');  // 'S' for string
    /* String literal: S followed by index */
    e->value = read_number();
    fdprintf(2, "STRING S%ld", e->value);
    return e;
}

static struct expr *
handle_compound_assign(unsigned char op)
{
    struct expr *e;
    char width;
    char width_str[3];

    e = new_expr(op);
    width = 's';  /* default */

    /* Check for width annotation */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        width_str[0] = ':';
        width_str[1] = width;
        width_str[2] = '\0';
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        e->flags = get_signedness_from_type_str(e->type_str);
    }

    skip();
    e->left = parse_expr();  /* lvalue */
    skip();
    e->right = parse_expr();  /* rvalue */
    expect(')');

    return e;
}

static struct expr *
handle_binary_op(unsigned char op)
{
    struct expr *e = new_expr(op);

    fdprintf(2, "BINOP %c (", op);
    skip();
    e->left = parse_expr();  /* left operand - now returns tree */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* right operand - now returns tree */
    fdprintf(2, ")");
    expect(')');

    /* Result size is the larger of the two operand sizes */
    if (e->left && e->right) {
        e->size = (e->left->size > e->right->size) ? 
            e->left->size : e->right->size;
    } else if (e->left) {
        e->size = e->left->size;
    } else if (e->right) {
        e->size = e->right->size;
    }

    /* Result signedness: if either operand is unsigned, result is unsigned */
    if (e->left && e->right) {
        e->flags = (e->left->flags | e->right->flags) & E_UNSIGNED;
    } else if (e->left) {
        e->flags = e->left->flags;
    } else if (e->right) {
        e->flags = e->right->flags;
    }

    /* Strength reduction: multiply by power of 2 -> left shift */
    if (op == '*') {
        int shift;
        struct expr *old_right;

        shift = is_multiply_by_power_of_2(e, NULL);
        if (shift >= 0) {
            /* Transform (* expr const_2^n) to (y expr const_n) */
            e->op = 'y';  /* LSHIFT */
            /* Replace right operand with shift amount constant */
            old_right = e->right;
            e->right = new_expr('C');
            e->right->op = 'C';
            e->right->value = shift;
            e->right->size = 1;  /* shift amounts are byte-sized */
            free_expr(old_right);
        }
    }

    return e;
}

/*
 * Short-circuit evaluation for && (LAND)
 * If left operand is false, skip right operand evaluation
 */
static struct expr *
handle_land(unsigned char op)
{
    struct expr *e = new_expr(op);
    e->label = label_counter++;

    fdprintf(2, "LAND_%d (", e->label);
    skip();
    e->left = parse_expr();  /* left operand */
    fdprintf(2, " ? ");

    /* Will emit conditional jump during code generation */
    fdprintf(2, "JZ skip_%d : ", e->label);

    skip();
    e->right = parse_expr();  /* right operand */

    /* Will emit skip label during code generation */
    fdprintf(2, " : skip_%d)", e->label);
    expect(')');
    return e;
}

/*
 * Short-circuit evaluation for || (LOR)
 * If left operand is true, skip right operand evaluation
 */
static struct expr *
handle_lor(unsigned char op)
{
    struct expr *e = new_expr(op);
    e->label = label_counter++;

    fdprintf(2, "LOR_%d (", e->label);
    skip();
    e->left = parse_expr();  /* left operand */
    fdprintf(2, " ? ");

    /* Will emit conditional jump during code generation */
    fdprintf(2, "JNZ skip_%d : ", e->label);

    skip();
    e->right = parse_expr();  /* right operand */

    /* Will emit skip label during code generation */
    fdprintf(2, " : skip_%d)", e->label);
    expect(')');
    return e;
}

static struct expr *
handle_unary_op(unsigned char op)
{
    struct expr *e;
    char width;
    char width_str[3];

    e = new_expr(op);
    width = ' ';

    /* Check for width annotation :b :s :l :p (for type conversion ops) */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        width_str[0] = ':';
        width_str[1] = width;
        width_str[2] = '\0';
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        fdprintf(2, "UNOP %c:%c (", op, width);
    } else {
        fdprintf(2, "UNOP %c (", op);
    }

    skip();
    e->left = parse_expr();  /* operand */
    fdprintf(2, ")");
    expect(')');

    /* If no type annotation, inherit size from operand */
    if (width == ' ' && e->left) {
        e->size = e->left->size;
    }

    /* Set signedness based on operator */
    if (op == 0xb6) {  /* WIDEN - unsigned (zero extend) */
        e->flags = E_UNSIGNED;
    } else if (op == 0xab) {  /* SEXT - signed (sign extend) */
        e->flags = 0;  /* explicitly signed */
    } else if (e->type_str) {
        /* For other operators with type annotation, use annotation */
        e->flags = get_signedness_from_type_str(e->type_str);
    } else if (e->left) {
        /* Otherwise inherit from operand */
        e->flags = e->left->flags;
    }

    return e;
}

static struct expr *
handle_bfextract(unsigned char op)
{
    struct expr *e = new_expr(op);
    /* Bitfield extract: (0xa7:offset:width addr) */
    int offset = 0, width = 0;

    /* Parse offset:width */
    skip();
    if (curchar == ':') {
        nextchar();
        offset = (int)read_number();
        skip();
        if (curchar == ':') {
            nextchar();
            width = (int)read_number();
        }
    }

    /* Store offset and width in value field (pack into long) */
    e->value = (offset << 16) | width;

    fdprintf(2, "BFEXTRACT<%d:%d> (", offset, width);
    skip();
    e->left = parse_expr();  /* address */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_bfassign(unsigned char op)
{
    struct expr *e = new_expr(op);
    /* Bitfield assign: (0xdd:offset:width addr value) */
    int offset = 0, width = 0;

    /* Parse offset:width */
    skip();
    if (curchar == ':') {
        nextchar();
        offset = (int)read_number();
        skip();
        if (curchar == ':') {
            nextchar();
            width = (int)read_number();
        }
    }

    /* Store offset and width in value field (pack into long) */
    e->value = (offset << 16) | width;

    skip();
    e->left = parse_expr();  /* address */
    skip();
    e->right = parse_expr();  /* value */
    expect(')');
    return e;
}


/*
 * COLON is only used as part of ternary, 
 * but handle it gracefully if standalone
 */
static struct expr *
handle_colon(unsigned char op)
{
    struct expr *e = new_expr(op);

    fdprintf(2, "COLON (");
    skip();
    e->left = parse_expr();  /* left */
    fdprintf(2, ", ");
    skip();
    e->right = parse_expr();  /* right */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_deref(unsigned char op)
{
    struct expr *e;
    char width;
    char width_str[3];

    e = new_expr('M');  /* 'M' for memory/deref */
    width = 's';  /* default */

    /* Check for width annotation :b :s :l :p :f :d */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        width_str[0] = ':';
        width_str[1] = width;
        width_str[2] = '\0';
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        e->flags = get_signedness_from_type_str(e->type_str);
    }

    fdprintf(2, "DEREF:%c (", width);
    skip();
    e->left = parse_expr();  /* address expression */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_assign(unsigned char op)
{
    struct expr *e;
    char width;
    char width_str[3];

    e = new_expr('=');  /* '=' for assignment */
    width = 's';  /* default */

    /* Check for width annotation */
    skip();
    if (curchar == ':') {
        nextchar();
        width = curchar;
        nextchar();
        /* Store width annotation */
        width_str[0] = ':';
        width_str[1] = width;
        width_str[2] = '\0';
        e->type_str = strdup(width_str);
        e->size = get_size_from_type_str(e->type_str);
        e->flags = get_signedness_from_type_str(e->type_str);
    }

    skip();
    e->left = parse_expr();  /* lvalue */
    skip();
    e->right = parse_expr();  /* rvalue */
    expect(')');
    return e;
}

static struct expr *
handle_call(unsigned char op)
{
    struct expr *e;
    char arg_buf[4096];  /* Buffer to capture argument expressions */
    int arg_start[32];  /* Start position of each argument */
    int arg_len[32];    /* Length of each argument */
    struct expr *args[32];  /* Parsed argument trees */
    int arg_count;
    int arg_buf_pos;
    int i;
    char func_buf[512];
    int func_len;
    int depth;
    struct parser_state saved_state;
    struct expr *prev;

    e = new_expr('@');  /* '@' for call */
    arg_count = 0;
    arg_buf_pos = 0;
    func_len = 0;

    fdprintf(2, "CALL (");

    skip();

    /* Collect function expression into func_buf */
    depth = 0;

    while (1) {
        if (curchar == 0) break;

        if (curchar == '(') {
            depth++;
        } else if (curchar == ')') {
            if (depth == 0) {
                /* End of function expr, no args */
                break;
            }
            depth--;
        } else if (depth == 0 && 
                (curchar == ' ' || curchar == '\t' || curchar == '\n')) {
            /* Whitespace at depth 0 = separator between fn and first arg */
            skip();
            break;
        }

        if (func_len < sizeof(func_buf) - 1) {
            func_buf[func_len++] = curchar;
        }
        nextchar();
    }
    func_buf[func_len] = 0;

    /* Collect arguments into arg_buf */
    while (curchar != ')' && curchar != 0) {
        skip();
        if (curchar == ')') break;

        arg_start[arg_count] = arg_buf_pos;
        depth = 0;

        /* Copy one argument expression */
        while (1) {
            if (curchar == 0) break;

            if (curchar == '(') {
                depth++;
            } else if (curchar == ')') {
                if (depth == 0) {
                    /* End of arguments */
                    break;
                }
                depth--;
            } else if (depth == 0 && 
                    (curchar == ' ' || curchar == '\t' || curchar == '\n')) {
                /* Whitespace at depth 0 = end of this arg */
                skip();
                break;
            }

            if (arg_buf_pos < sizeof(arg_buf) - 1) {
                arg_buf[arg_buf_pos++] = curchar;
            }
            nextchar();
        }

        arg_len[arg_count] = arg_buf_pos - arg_start[arg_count];
        arg_count++;

        if (arg_count >= 32) break;  /* Max 32 arguments */
    }

    /* Now recursively parse: function and arguments */
    save_parser_state(&saved_state);

    /* Parse function expression */
    fdprintf(2, "\n  CALL_FUNC: ");
    setup_string_input(func_buf, func_len);
    e->left = parse_expr();  /* function address */
    restore_parser_state(&saved_state);

    /* Parse arguments */
    for (i = 0; i < arg_count; i++) {
        fdprintf(2, "\n  ARG%d: ", i);
        setup_string_input(&arg_buf[arg_start[i]], arg_len[i]);
        args[i] = parse_expr();
        restore_parser_state(&saved_state);
    }

    /* Build argument chain using wrapper nodes to avoid corrupting argument trees */
    /* Store arg_count in value field */
    e->value = arg_count;
    if (arg_count > 0) {
        struct expr *wrappers[32];  /* Wrapper nodes for each argument */

        /* Create wrapper nodes for each argument */
        for (i = 0; i < arg_count; i++) {
            wrappers[i] = new_expr(',');  /* ',' represents argument wrapper */
            wrappers[i]->left = args[i];  /* Actual argument in left */
            wrappers[i]->right = NULL;    /* Will be set to next wrapper */
        }

        /* Chain wrappers together via right pointers */
        e->right = wrappers[0];
        for (i = 0; i < arg_count - 1; i++) {
            wrappers[i]->right = wrappers[i+1];
        }
        /* Last wrapper's right is already NULL */
    }

    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
handle_ternary(unsigned char op)
{
    struct expr *e;
    struct expr *colon;

    e = new_expr('?');  /* '?' for ternary */

    fdprintf(2, "TERNARY (");
    skip();
    e->left = parse_expr();  /* condition */
    fdprintf(2, " ? ");
    skip();

    /* Expect COLON node - this becomes the right child */
    if (curchar == '(') {
        nextchar();
        skip();
        if (curchar == ':') {
            nextchar();
            skip();
            /* Build COLON node with true/false branches */
            colon = new_expr(':');
            colon->left = parse_expr();  /* true expr */
            fdprintf(2, " : ");
            skip();
            colon->right = parse_expr();  /* false expr */
            expect(')');
            e->right = colon;
        }
    }

    fdprintf(2, ")");
    expect(')');
    return e;
}

/* Forward declarations for statement handlers (needed for mutual recursion) */
static struct stmt *handle_block(void);
static struct stmt *handle_if(void);
static struct stmt *handle_while(void);
static struct stmt *handle_do(void);
static struct stmt *handle_for(void);
static struct stmt *handle_return(void);
static struct stmt *handle_expr_stmt(void);
static struct stmt *handle_empty_stmt(void);
static struct stmt *handle_asm(void);
static struct stmt *handle_label(void);
static struct stmt *handle_goto(void);
static struct stmt *handle_switch(void);
static struct stmt *handle_case_in_block(void);
static struct stmt *handle_default_in_block(void);

/* Handlers for case/default when they appear in block context */
static struct stmt *
handle_case_in_block(void)
{
    /* Case statement: (C value ()) */
    struct stmt *child = new_stmt('C');

    fdprintf(2, "CASE ");
    skip();
    child->expr = parse_expr();  /* case value */
    fdprintf(2, ": ");
    skip();
    /* Skip empty body placeholder () */
    if (curchar == '(') {
        nextchar();
        skip();
        if (curchar == ')') {
            nextchar();
        }
    }
    expect(')');

    return child;
}

static struct stmt *
handle_default_in_block(void)
{
    /* Default statement: (O ()) */
    struct stmt *child = new_stmt('O');

    fdprintf(2, "DEFAULT: ");
    skip();
    /* Skip empty body placeholder () */
    if (curchar == '(') {
        nextchar();
        skip();
        if (curchar == ')') {
            nextchar();
        }
    }
    expect(')');

    return child;
}

static struct stmt *
handle_block(void)
{
    struct stmt *s;
    struct stmt *first_child;
    struct stmt *last_child;
    struct stmt *child;
    char op;
    char *name;
    char *type;

    s = new_stmt('B');
    first_child = NULL;
    last_child = NULL;

    fdprintf(2, "BLOCK { ");

    skip();
    while (curchar != ')') {
        /* Could be declaration or statement */
        skip();
        if (curchar == '(') {
            nextchar();
            /*
             * Don't call skip() yet - need to read op first to
             * avoid ';' being treated as comment
             */
            op = curchar;  /* Read operator immediately after '(' */
            nextchar();  /* Consume operator */
            skip();      /* Now safe to skip whitespace */

            if (op == 'd') {
                /* Declaration */
                name = read_symbol();
                type = read_type();
                fdprintf(2, "  DECL %s %s\n", name, type);

                /* Create declaration statement node */
                child = new_stmt('d');
                child->symbol = strdup(name);  // symbuf is reused
                child->type_str = strdup(type);  // typebuf is reused

                expect(')');
            } else {
                /* Statement - dispatch based on operator */

                switch (op) {
                case 'B':  /* Block */
                    child = handle_block();
                    break;
                case 'I':  /* If */
                    child = handle_if();
                    break;
                case 'W':  /* While */
                    child = handle_while();
                    break;
                case 'D':  /* Do-while */
                    child = handle_do();
                    break;
                case 'F':  /* For */
                    child = handle_for();
                    break;
                case 'R':  /* Return */
                    child = handle_return();
                    break;
                case 'E':  /* Expression statement */
                    child = handle_expr_stmt();
                    break;
                case ';':  /* Empty statement */
                    child = handle_empty_stmt();
                    break;
                case 'A':  /* Asm block */
                    child = handle_asm();
                    break;
                case 'L':  /* Label */
                    child = handle_label();
                    break;
                case 'G':  /* Goto */
                    child = handle_goto();
                    break;
                case 'S':  /* Switch */
                    child = handle_switch();
                    break;
                case 'C':  /* Case (inside switch body) */
                    /* Case statements only valid inside switch, 
                     * but may appear in block */
                    child = handle_case_in_block();
                    break;
                case 'O':  /* Default (inside switch body) */
                    /* Default statements only valid inside switch, 
                     * but may appear in block */
                    child = handle_default_in_block();
                    break;
                case 'K':  /* Break */
                    fdprintf(2, "BREAK");
                    child = new_stmt('K');
                    expect(')');
                    break;
                case 'N':  /* Continue */
                    fdprintf(2, "CONTINUE");
                    child = new_stmt('N');
                    expect(')');
                    break;
                default:
                    fdprintf(2, "parseast: line %d: unknown" 
                        " stmt op '%c' in block\n", line_num, op);
                    /* Skip to closing paren */
                    while (curchar && curchar != ')') {
                        nextchar();
                    }
                    if (curchar == ')') {
                        nextchar();
                    }
                    child = NULL;
                    break;
                }
                fdprintf(2, "\n");
            }

            /* Add child to linked list and update tail pointer */
            if (child) {
                if (!first_child) {
                    first_child = child;
                } else {
                    last_child->next = child;
                }
                /* Update last_child to point to the actual end of the chain */
                last_child = child;
                while (last_child->next) {
                    last_child = last_child->next;
                }
            }
        }
        skip();
    }

    fdprintf(2, "}");
    expect(')');

    s->then_branch = first_child;  /* Use then_branch for block's child list */
    return s;
}

static struct stmt *
handle_if(void)
{
    struct stmt *s = new_stmt('I');
    char label_buf[64];

    s->label = label_counter++;

    fdprintf(2, "IF (");
    skip();
    s->expr = parse_expr();  /* condition */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parse_stmt();  /* then branch */

    skip();
    if (curchar != ')') {
        /* Has else branch - need second label for end of else */
        s->label2 = label_counter++;

        fdprintf(2, " ELSE ");
        s->else_branch = parse_stmt();  /* else branch */

        /* Insert end label after the IF statement */
        snprintf(label_buf, sizeof(label_buf), "_if_end_%d", s->label2);
        s->next = create_label_asm(label_buf);
    } else {
        s->else_branch = NULL;

        /* Insert end label after the IF statement */
        snprintf(label_buf, sizeof(label_buf), "_if_%d", s->label);
        s->next = create_label_asm(label_buf);
    }

    expect(')');
    return s;
}

static struct stmt *
handle_while(void)
{
    struct stmt *s;
    char label_buf[64];
    struct stmt *start_label;

    s = new_stmt('W');
    s->label = label_counter++;  /* Loop start label */

    fdprintf(2, "WHILE (");
    skip();
    s->expr = parse_expr();  /* condition */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parse_stmt();  /* body */

    expect(')');

    /* Insert loop start label before the while, and end label after */
    /* Create label for loop start */
    snprintf(label_buf, sizeof(label_buf), "_while_%d", s->label);
    start_label = create_label_asm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the while */
    snprintf(label_buf, sizeof(label_buf), "_while_end_%d", s->label);
    s->next = create_label_asm(label_buf);

    return start_label;  /* Return the label, not the while node */
}

static struct stmt *
handle_do(void)
{
    struct stmt *s;
    char label_buf[64];
    struct stmt *start_label;

    s = new_stmt('D');
    s->label = label_counter++;  /* Loop start label */

    fdprintf(2, "DO ");
    skip();
    s->then_branch = parse_stmt();  /* body */

    fdprintf(2, " WHILE (");
    skip();
    s->expr = parse_expr();  /* condition */
    fdprintf(2, ")");

    expect(')');

    /* Insert loop start label before the do, and end label after */
    snprintf(label_buf, sizeof(label_buf), "_do_%d", s->label);
    start_label = create_label_asm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the do-while */
    snprintf(label_buf, sizeof(label_buf), "_do_end_%d", s->label);
    s->next = create_label_asm(label_buf);

    return start_label;
}

static struct stmt *
handle_for(void)
{
    struct stmt *s;
    char label_buf[64];
    struct stmt *start_label;

    s = new_stmt('F');
    s->label = label_counter++;  /* Loop start label */

    fdprintf(2, "FOR (");
    skip();
    s->expr = parse_expr();  /* init */
    fdprintf(2, "; ");
    skip();
    s->expr2 = parse_expr();  /* condition */
    fdprintf(2, "; ");
    skip();
    s->expr3 = parse_expr();  /* increment */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parse_stmt();  /* body */

    expect(')');

    /* Insert loop start label before the for, and end label after */
    snprintf(label_buf, sizeof(label_buf), "_for_%d", s->label);
    start_label = create_label_asm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the for */
    snprintf(label_buf, sizeof(label_buf), "_for_end_%d", s->label);
    s->next = create_label_asm(label_buf);

    return start_label;
}

static struct stmt *
handle_return(void)
{
    struct stmt *s = new_stmt('R');

    fdprintf(2, "RETURN");
    skip();
    if (curchar != ')') {
        fdprintf(2, " ");
        s->expr = parse_expr();
    } else {
        s->expr = NULL;
    }
    expect(')');
    return s;
}

static struct stmt *
handle_expr_stmt(void)
{
    struct stmt *s = new_stmt('E');

    fdprintf(2, "EXPR (");
    skip();
    s->expr = parse_expr();
    fdprintf(2, ")");
    expect(')');
    return s;
}

static struct stmt *
handle_empty_stmt(void)
{
    struct stmt *s = new_stmt(';');

    fdprintf(2, ";");
    expect(')');
    return s;
}

static struct stmt *
handle_asm(void)
{
    struct stmt *s = new_stmt('A');
    char asm_buf[4096];
    char *p;

    /* ASM statement: (A "assembly text") */
    skip();
    if (curchar == '"') {
        /* Read the quoted string into buffer */
        nextchar();
        p = asm_buf;
        while (curchar && curchar != '"' && 
                (p - asm_buf) < sizeof(asm_buf) - 1) {
            *p++ = curchar;
            nextchar();
        }
        *p = '\0';

        if (curchar == '"') {
            nextchar();
        }

        /* Diagnostic output to stderr */
        fdprintf(2, "ASM \"%s\"\n", asm_buf);

        /* Store assembly text in statement node */
        s->asm_block = malloc(strlen(asm_buf) + 1);
        strcpy(s->asm_block, asm_buf);
    }
    expect(')');
    return s;
}

static struct stmt *
handle_label(void)
{
    struct stmt *s = new_stmt('L');
    char *label_name;

    /* Label statement: (L label_name) */
    skip();
    label_name = read_symbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "LABEL %s", label_name);

    /* Store label name in statement node */
    s->symbol = label_name;

    expect(')');
    return s;
}

static struct stmt *
handle_goto(void)
{
    struct stmt *s = new_stmt('G');
    char *label_name;

    /* Goto statement: (G label_name) */
    skip();
    label_name = read_symbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "GOTO %s", label_name);

    /* Store label name in statement node */
    s->symbol = label_name;

    expect(')');
    return s;
}

static struct stmt *
handle_switch(void)
{
    struct stmt *s;
    struct stmt *first_child;
    struct stmt *last_child;
    struct stmt *child;
    char clause_type;

    s = new_stmt('S');
    first_child = NULL;
    last_child = NULL;

    /* Switch statement: (S expr (C val ...) (C val ...) (O ...) ) */
    fdprintf(2, "SWITCH (");
    skip();
    s->expr = parse_expr();  /* switch expression */
    fdprintf(2, ") {");

    /* Parse case and default clauses */
    skip();
    while (curchar != ')') {
        skip();
        if (curchar == '(') {
            nextchar();
            skip();
            clause_type = curchar;
            nextchar();

            if (clause_type == 'C') {
                /* Case clause: (C value ()) - body is always empty */
                fdprintf(2, "\n  CASE ");
                child = new_stmt('C');
                skip();
                child->expr = parse_expr();  /* case value */
                fdprintf(2, ": ");
                skip();
                /* Skip empty body placeholder () */
                if (curchar == '(') {
                    nextchar();
                    skip();
                    if (curchar == ')') {
                        nextchar();
                    }
                }
                expect(')');
            } else if (clause_type == 'O') {
                /* Default clause: (O ()) - body placeholder is always empty */
                fdprintf(2, "\n  DEFAULT: ");
                child = new_stmt('O');
                skip();
                /* Skip empty body placeholder () */
                if (curchar == '(') {
                    nextchar();
                    skip();
                    if (curchar == ')') {
                        nextchar();
                    }
                }
                expect(')');
            } else if (clause_type == 'R' || clause_type == 'G' ||
                       clause_type == 'B' || clause_type == 'E' ||
                       clause_type == 'I' || clause_type == 'W' ||
                       clause_type == 'D' || clause_type == 'F' ||
                       clause_type == 'L' || clause_type == 'A' ||
                       clause_type == 'K' || clause_type == 'N' ||
                       clause_type == ';') {
                /* Statements inside switch body (between cases) */
                /* Back up - we need to reparse this as a statement */
                /* Unfortunately we already consumed the '(' and type char */
                /* For now, just handle common ones inline */
                fdprintf(2, "\n");
                if (clause_type == 'R') {
                    child = handle_return();
                } else if (clause_type == 'G') {
                    child = new_stmt('G');
                    skip();
                    child->symbol = read_symbol();
                    fdprintf(2, "GOTO %s", child->symbol);
                    expect(')');
                } else if (clause_type == 'B') {
                    /* BLOCK statement */
                    child = handle_block();
                } else if (clause_type == 'K') {
                    /* BREAK statement */
                    fdprintf(2, "BREAK");
                    child = new_stmt('K');
                    expect(')');
                } else if (clause_type == 'E') {
                    child = handle_expr_stmt();
                } else if (clause_type == 'I') {
                    child = handle_if();
                } else if (clause_type == 'W') {
                    child = handle_while();
                } else if (clause_type == 'D') {
                    child = handle_do();
                } else if (clause_type == 'F') {
                    child = handle_for();
                } else if (clause_type == 'L') {
                    child = handle_label();
                } else if (clause_type == 'A') {
                    child = handle_asm();
                } else if (clause_type == 'N') {
                    /* CONTINUE statement */
                    fdprintf(2, "CONTINUE");
                    child = new_stmt('N');
                    expect(')');
                } else if (clause_type == ';') {
                    child = handle_empty_stmt();
                } else {
                    child = NULL;
                }
            } else {
                /* Unknown - skip it */
                fdprintf(2, "\n  UNKNOWN_%c ", clause_type);
                while (curchar && curchar != ')') {
                    nextchar();
                }
                if (curchar == ')') {
                    nextchar();
                }
                child = NULL;
            }

            /* Add child to linked list and update tail pointer */
            if (child) {
                if (!first_child) {
                    first_child = child;
                } else {
                    last_child->next = child;
                }
                /* Update last_child to point to the actual end of the chain */
                last_child = child;
                while (last_child->next) {
                    last_child = last_child->next;
                }
            }
        }
        skip();
    }

    fdprintf(2, "\n}");
    expect(')');

    s->then_branch = first_child;  /* Use then_branch for switch body */
    return s;
}

/* Top-level handlers */

/*
 * Helper: Get register name string from register_id enum
 */
static void
handle_function(void)
{
    struct function_ctx ctx;
    char name_buf[64];  /* Stack buffer for function name */
    char params_buf[256];
    char *p;
    int first_param;
    struct stmt *first_child = NULL;
    struct stmt *last_child = NULL;
    struct stmt *child;
    char *dname;
    char *dtype;

    /* (f name (params) return_type declarations body) */
    /* Copy function name to stack buffer before reading parameters */
    strncpy(name_buf, read_symbol(), sizeof(name_buf) - 1);
    name_buf[sizeof(name_buf) - 1] = '\0';
    ctx.name = name_buf;
    fdprintf(2, "\nFUNCTION %s\n", ctx.name);

    /* Parameters - collect into buffer for prologue */
    skip();
    expect('(');
    fdprintf(2, "  PARAMS: ");
    p = params_buf;
    params_buf[0] = '\0';
    first_param = 1;
    skip();
    while (curchar != ')') {
        char *param = read_symbol();
        char *ptype = NULL;

        fdprintf(2, "%s ", param);

        /* Add to params buffer */
        if (!first_param) {
            if (p < params_buf + sizeof(params_buf) - 2) {
                *p++ = ',';
                *p++ = ' ';
            }
        }
        first_param = 0;

        /* Copy parameter name */
        while (*param && p < params_buf + sizeof(params_buf) - 20) {
            *p++ = *param++;
        }

        skip();
        /* Read type annotation if present (format: name:type) */
        if (curchar == ':') {
            nextchar();
            ptype = read_type();  /* Get type */
            /* Add type to params buffer */
            if (ptype && p < params_buf + sizeof(params_buf) - 20) {
                *p++ = ':';
                while (*ptype && p < params_buf + sizeof(params_buf) - 1) {
                    *p++ = *ptype++;
                }
            }
            skip();
        }
    }
    *p = '\0';
    ctx.params = params_buf;
    expect(')');
    fdprintf(2, "\n");

    /* Return type */
    ctx.rettype = read_type();
    fdprintf(2, "  RETURNS: %s\n", ctx.rettype);

    /* Declarations and body - collect into statement tree */
    skip();
    while (curchar != ')') {
        skip();
        if (curchar == '(') {
            /* Peek ahead to see if it's a declaration or statement */
            nextchar();
            skip();
            if (curchar == 'd') {
                /* Declaration - we're already past the '(' */
                nextchar();
                dname = read_symbol();
                dtype = read_type();
                fdprintf(2, "  DECL %s %s\n", dname, dtype);

                /* Create declaration statement node */
                child = new_stmt('d');
                child->symbol = strdup(dname);  /* symbuf is reused */
                child->type_str = strdup(dtype);  /* typebuf is reused */

                expect(')');
            } else {
                /* Body statement - we've already consumed 
                 * '(', now consume operator */
                char op = curchar;
                nextchar();

                fdprintf(2, "  BODY: ");
                switch (op) {
                case 'B':  /* Block */
                    child = handle_block();
                    break;
                case 'I':  /* If */
                    child = handle_if();
                    break;
                case 'W':  /* While */
                    child = handle_while();
                    break;
                case 'D':  /* Do-while */
                    child = handle_do();
                    break;
                case 'F':  /* For */
                    child = handle_for();
                    break;
                case 'R':  /* Return */
                    child = handle_return();
                    break;
                case 'E':  /* Expression statement */
                    child = handle_expr_stmt();
                    break;
                case ';':  /* Empty statement */
                    child = handle_empty_stmt();
                    break;
                case 'A':  /* Asm block */
                    child = handle_asm();
                    break;
                case 'L':  /* Label */
                    child = handle_label();
                    break;
                case 'G':  /* Goto */
                    child = handle_goto();
                    break;
                case 'S':  /* Switch */
                    child = handle_switch();
                    break;
                default:
                    fdprintf(2, "parseast: line %d: unknown stmt op '%c'\n", 
                        line_num, op);
                    /* Skip to closing paren */
                    while (curchar && curchar != ')') {
                        nextchar();
                    }
                    if (curchar == ')') {
                        nextchar();
                    }
                    child = NULL;
                    break;
                }
                fdprintf(2, "\n");
            }

            /* Add child to linked list and update tail pointer */
            if (child) {
                if (!first_child) {
                    first_child = child;
                } else {
                    last_child->next = child;
                }
                /* Update last_child to point to the actual end of the chain */
                last_child = child;
                while (last_child->next) {
                    last_child = last_child->next;
                }
            }
        }
        skip();
    }

    /* Store body tree in context */
    ctx.body = first_child;
    ctx.label_counter = label_counter;  /* Save current label counter */
    ctx.locals = NULL;  /* No local variables yet */
    ctx.frame_size = 0;  /* No frame size yet */
    ctx.de_save_count = 0;  /* No nested DE saves yet */
    ctx.d_in_use = 0;  /* D register not in use yet */

    expect(')');

    /* Phase 1.5: Assign stack frame offsets to local variables */
    assign_frame_offsets(&ctx);

    /* Phase 2: Generate assembly code blocks for tree nodes */
    generate_code(&ctx);

    /* Phase 2.5: Allocate registers based on usage patterns */
    allocate_registers(&ctx);

    /* Phase 3: Emit assembly and free tree nodes */
    emit_assembly(&ctx, out_fd);
}

static void
handle_global(void)
{
    char *name, *type;

    /* (g name type [init]) */
    name = read_symbol();
    type = read_type();

    fdprintf(2, "\nGLOBAL %s %s", name, type);

    skip();
    if (curchar != ')') {
        fdprintf(2, " = ");
        parse_expr();
    }

    fdprintf(2, "\n");
    expect(')');
}

static void
handle_string_literal(void)
{
    char *name, *data;

    /* (s name "data") */
    name = read_symbol();
    data = read_quoted_string();

    fdprintf(2, "\nSTRING %s \"%s\"\n", name, data);

    expect(')');
}

/*
 * Operator lookup table
 * Maps each operator character to its handler function
 * Table is sparse - only operators that need handling are listed
 */
static handler_fn expr_handlers[256];

static void
init_expr_handlers(void)
{
    int i;

    /* Initialize all to generic handler */
    for (i = 0; i < 256; i++) {
        expr_handlers[i] = handle_generic;
    }

    /* Register specific handlers */
    expr_handlers['M'] = handle_deref;     /* DEREF */
    expr_handlers['='] = handle_assign;    /* ASSIGN */
    expr_handlers['@'] = handle_call;      /* CALL */
    expr_handlers['?'] = handle_ternary;   /* TERNARY */
    expr_handlers[':'] = handle_colon;     /* COLON */

    /* Binary operators */
    expr_handlers['+'] = handle_binary_op;
    expr_handlers['-'] = handle_binary_op;
    expr_handlers['*'] = handle_binary_op;
    expr_handlers['/'] = handle_binary_op;
    expr_handlers['%'] = handle_binary_op;
    expr_handlers['&'] = handle_binary_op;
    expr_handlers['|'] = handle_binary_op;
    expr_handlers['^'] = handle_binary_op;
    expr_handlers['<'] = handle_binary_op;
    expr_handlers['>'] = handle_binary_op;
    expr_handlers['Q'] = handle_binary_op;  /* EQ == */
    expr_handlers['n'] = handle_binary_op;  /* NEQ != */
    expr_handlers['L'] = handle_binary_op;  /* LE <= */
    expr_handlers['g'] = handle_binary_op;  /* GE >= */
    expr_handlers['y'] = handle_binary_op;  /* LSHIFT << */
    expr_handlers['w'] = handle_binary_op;  /* RSHIFT >> */
    expr_handlers['h'] = handle_lor;        /* LOR || */
    expr_handlers['j'] = handle_land;       /* LAND && */

    /* Compound assignment operators */
    expr_handlers['P'] = handle_compound_assign;  /* PLUSEQ += */
    expr_handlers[0xdf] = handle_compound_assign; /* SUBEQ -= */
    expr_handlers['T'] = handle_compound_assign;  /* MULTEQ *= */
    expr_handlers['2'] = handle_compound_assign;  /* DIVEQ /= */
    expr_handlers[0xfe] = handle_compound_assign; /* MODEQ %= */
    expr_handlers[0xc6] = handle_compound_assign; /* ANDEQ &= */
    expr_handlers['1'] = handle_compound_assign;  /* OREQ |= */
    expr_handlers['X'] = handle_compound_assign;  /* XOREQ ^= */
    expr_handlers['0'] = handle_compound_assign;  /* LSHIFTEQ <<= */
    expr_handlers['6'] = handle_compound_assign;  /* RSHIFTEQ >>= */

    /* Unary operators */
    expr_handlers['!'] = handle_unary_op;
    expr_handlers['~'] = handle_unary_op;
    expr_handlers['\\'] = handle_unary_op; /* NEG */
    expr_handlers['\''] = handle_unary_op; /* NOT */

    /* Type conversion operators */
    expr_handlers['N'] = handle_unary_op;  /* NARROW */
    expr_handlers['W'] = handle_unary_op;  /* WIDEN */
    expr_handlers[0xab] = handle_unary_op;  /* SEXT (sign extend) */

    /* Increment/decrement */
    expr_handlers[0xcf] = handle_unary_op; /* PREINC */
    expr_handlers[0xef] = handle_unary_op; /* POSTINC */
    expr_handlers[0xd6] = handle_unary_op; /* PREDEC */
    expr_handlers[0xf6] = handle_unary_op; /* POSTDEC */

    /* COPY operator */
    expr_handlers[0xbb] = handle_binary_op; /* COPY */

    /* Bitfield operators */
    expr_handlers[0xa7] = handle_bfextract;  /* BFEXTRACT */
    expr_handlers[0xdd] = handle_bfassign;   /* BFASSIGN */
}

/*
 * Parse an expression (recursive)
 */
static struct expr *
parse_expr(void)
{
    struct expr *e;
    unsigned char op;

    skip();

    if (curchar == '(') {
        nextchar();
        skip();

        op = curchar;
        nextchar();

        /* Use lookup table to dispatch to handler */
        e = expr_handlers[op](op);

    } else if (curchar == '$') {
        /* Symbol */
        e = handle_symbol();
    } else if (curchar == 'S') {
        /* String literal */
        nextchar();
        e = handle_string();
    } else if ((curchar >= '0' && curchar <= '9') || curchar == '-') {
        /* Constant */
        e = handle_const();
    } else {
        fdprintf(2, "parseast: line %d: unexpected char '%c' in expr\n",
                 line_num, curchar);
        nextchar();
        e = NULL;
    }

    return e;
}

/*
 * Parse a statement (recursive)
 */
static struct stmt *
parse_stmt(void)
{
    struct stmt *s;
    char op;

    skip();

    if (curchar != '(') {
        fdprintf(2, "parseast: line %d: expected '(' at start of statement\n",
            line_num);
        return NULL;
    }

    nextchar();
    skip();

    /* Handle empty statement: () */
    if (curchar == ')') {
        nextchar();
        return NULL;
    }

    op = curchar;
    nextchar();

    switch (op) {
    case 'B':  /* Block */
        s = handle_block();
        break;
    case 'I':  /* If */
        s = handle_if();
        break;
    case 'W':  /* While */
        s = handle_while();
        break;
    case 'D':  /* Do-while */
        s = handle_do();
        break;
    case 'F':  /* For */
        s = handle_for();
        break;
    case 'R':  /* Return */
        s = handle_return();
        break;
    case 'E':  /* Expression statement */
        s = handle_expr_stmt();
        break;
    case ';':  /* Empty statement */
        s = handle_empty_stmt();
        break;
    case 'A':  /* Asm block */
        s = handle_asm();
        break;
    case 'L':  /* Label */
        s = handle_label();
        break;
    case 'G':  /* Goto */
        s = handle_goto();
        break;
    case 'S':  /* Switch */
        s = handle_switch();
        break;
    default:
        fdprintf(2, "parseast: line %d: unknown stmt op '%c'\n", line_num, op);
        /* Skip to closing paren */
        while (curchar && curchar != ')') {
            nextchar();
        }
        if (curchar == ')') {
            nextchar();
        }
        s = NULL;
        break;
    }

    return s;
}

/*
 * Parse top-level constructs
 */
static void
parse_toplevel(void)
{
    char op;

    skip();

    if (curchar != '(') {
        return;
    }

    nextchar();
    skip();

    op = curchar;
    nextchar();

    switch (op) {
    case 'f':  /* Function */
        handle_function();
        break;
    case 'g':  /* Global variable */
        handle_global();
        break;
    case 's':  /* String literal */
        handle_string_literal();
        break;
    default:
        fdprintf(2, "parseast: line %d: unknown top-level op '%c'\n", 
            line_num, op);
        /* Skip to closing paren */
        while (curchar && curchar != ')') {
            nextchar();
        }
        if (curchar == ')') {
            nextchar();
        }
        break;
    }
}

/*
 * Initialize parser and read AST file
 */
int
parse_ast_file(int in, int out)
{
    /* Initialize low-level I/O */
    init_astio(in);
    out_fd = out;

    /* Initialize expression handler lookup table */
    init_expr_handlers();

    /* Prime the input */
    nextchar();

    /* Parse all top-level constructs */
    while (curchar) {
        skip();
        if (curchar) {
            parse_toplevel();
        }
    }

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
