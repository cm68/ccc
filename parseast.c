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

/* Symbol tracking for EXTERN declarations */
static void addDefinedSymbol(const char *name);
void addReferencedSymbol(const char *name);

/* Parser state */
int outFd = 1;  /* Assembly output (default: stdout) */
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
isStructMemberAccess(struct expr *e, char **out_var, long *out_offset)
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
freeExpr(struct expr *e)
{
    if (!e) return;
    freeExpr(e->left);
    freeExpr(e->right);
    /* NOTE: type_str and symbol point to static buffers from
     * readType()/readSymbol()
     * They should NOT be freed. Only asm_block and cleanup_block are dynamically allocated. */
    if (e->asm_block) free(e->asm_block);
    if (e->cleanup_block) free(e->cleanup_block);
    free(e);
}

void
frStmt(struct stmt *s)
{
    if (!s) return;
    freeExpr(s->expr);
    freeExpr(s->expr2);
    freeExpr(s->expr3);
    frStmt(s->then_branch);
    frStmt(s->else_branch);
    frStmt(s->next);
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
doGeneric(unsigned char op)
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
doConst(void)
{
    struct expr *e = new_expr('C');  // 'C' for constant
    e->value = readNumber();

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
doSymbol(void)
{
    struct expr *e = new_expr('$');  // '$' for symbol
    char *sym = readSymbol();
    e->symbol = strdup(sym);
    fdprintf(2, "SYM %s", e->symbol);
    return e;
}

static struct expr *
doString(void)
{
    struct expr *e = new_expr('S');  // 'S' for string
    /* String literal: S followed by index */
    e->value = readNumber();
    fdprintf(2, "STRING S%ld", e->value);
    return e;
}

static struct expr *
doCompoundAssign(unsigned char op)
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
doBinaryOp(unsigned char op)
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
            freeExpr(old_right);
        }
    }

    return e;
}

/*
 * Short-circuit evaluation for && (LAND)
 * If left operand is false, skip right operand evaluation
 */
static struct expr *
doLand(unsigned char op)
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
doLor(unsigned char op)
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
doUnaryOp(unsigned char op)
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
doBfextract(unsigned char op)
{
    struct expr *e = new_expr(op);
    /* Bitfield extract: (0xa7:offset:width addr) */
    int offset = 0, width = 0;

    /* Parse offset:width */
    skip();
    if (curchar == ':') {
        nextchar();
        offset = (int)readNumber();
        skip();
        if (curchar == ':') {
            nextchar();
            width = (int)readNumber();
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
doBfassign(unsigned char op)
{
    struct expr *e = new_expr(op);
    /* Bitfield assign: (0xdd:offset:width addr value) */
    int offset = 0, width = 0;

    /* Parse offset:width */
    skip();
    if (curchar == ':') {
        nextchar();
        offset = (int)readNumber();
        skip();
        if (curchar == ':') {
            nextchar();
            width = (int)readNumber();
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
doColon(unsigned char op)
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
doDeref(unsigned char op)
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
doAssign(unsigned char op)
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
doCall(unsigned char op)
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
    saveParserState(&saved_state);

    /* Parse function expression */
    fdprintf(2, "\n  CALL_FUNC: ");
    setupStringInput(func_buf, func_len);
    e->left = parse_expr();  /* function address */
    restoreParserState(&saved_state);

    /* Parse arguments */
    for (i = 0; i < arg_count; i++) {
        fdprintf(2, "\n  ARG%d: ", i);
        setupStringInput(&arg_buf[arg_start[i]], arg_len[i]);
        args[i] = parse_expr();
        restoreParserState(&saved_state);
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
doTernary(unsigned char op)
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
static struct stmt *doBlock(void);
static struct stmt *doIf(void);
static struct stmt *doWhile(void);
static struct stmt *doDo(void);
static struct stmt *doFor(void);
static struct stmt *doReturn(void);
static struct stmt *doExprStmt(void);
static struct stmt *doEmptyStmt(void);
static struct stmt *doAsm(void);
static struct stmt *doLabel(void);
static struct stmt *doGoto(void);
static struct stmt *doSwitch(void);
static struct stmt *doCaseInBlock(void);
static struct stmt *doDefaultInBlock(void);

/* Handlers for case/default when they appear in block context */
static struct stmt *
doCaseInBlock(void)
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
doDefaultInBlock(void)
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
doBlock(void)
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
                name = readSymbol();
                type = readType();
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
                    child = doBlock();
                    break;
                case 'I':  /* If */
                    child = doIf();
                    break;
                case 'W':  /* While */
                    child = doWhile();
                    break;
                case 'D':  /* Do-while */
                    child = doDo();
                    break;
                case 'F':  /* For */
                    child = doFor();
                    break;
                case 'R':  /* Return */
                    child = doReturn();
                    break;
                case 'E':  /* Expression statement */
                    child = doExprStmt();
                    break;
                case ';':  /* Empty statement */
                    child = doEmptyStmt();
                    break;
                case 'A':  /* Asm block */
                    child = doAsm();
                    break;
                case 'L':  /* Label */
                    child = doLabel();
                    break;
                case 'G':  /* Goto */
                    child = doGoto();
                    break;
                case 'S':  /* Switch */
                    child = doSwitch();
                    break;
                case 'C':  /* Case (inside switch body) */
                    /* Case statements only valid inside switch, 
                     * but may appear in block */
                    child = doCaseInBlock();
                    break;
                case 'O':  /* Default (inside switch body) */
                    /* Default statements only valid inside switch, 
                     * but may appear in block */
                    child = doDefaultInBlock();
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
doIf(void)
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
doWhile(void)
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
doDo(void)
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
doFor(void)
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
doReturn(void)
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
doExprStmt(void)
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
doEmptyStmt(void)
{
    struct stmt *s = new_stmt(';');

    fdprintf(2, ";");
    expect(')');
    return s;
}

static struct stmt *
doAsm(void)
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
doLabel(void)
{
    struct stmt *s = new_stmt('L');
    char *label_name;

    /* Label statement: (L label_name) */
    skip();
    label_name = readSymbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "LABEL %s", label_name);

    /* Store label name in statement node */
    s->symbol = label_name;

    expect(')');
    return s;
}

static struct stmt *
doGoto(void)
{
    struct stmt *s = new_stmt('G');
    char *label_name;

    /* Goto statement: (G label_name) */
    skip();
    label_name = readSymbol();

    /* Diagnostic output to stderr */
    fdprintf(2, "GOTO %s", label_name);

    /* Store label name in statement node */
    s->symbol = label_name;

    expect(')');
    return s;
}

static struct stmt *
doSwitch(void)
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
                    child = doReturn();
                } else if (clause_type == 'G') {
                    child = new_stmt('G');
                    skip();
                    child->symbol = readSymbol();
                    fdprintf(2, "GOTO %s", child->symbol);
                    expect(')');
                } else if (clause_type == 'B') {
                    /* BLOCK statement */
                    child = doBlock();
                } else if (clause_type == 'K') {
                    /* BREAK statement */
                    fdprintf(2, "BREAK");
                    child = new_stmt('K');
                    expect(')');
                } else if (clause_type == 'E') {
                    child = doExprStmt();
                } else if (clause_type == 'I') {
                    child = doIf();
                } else if (clause_type == 'W') {
                    child = doWhile();
                } else if (clause_type == 'D') {
                    child = doDo();
                } else if (clause_type == 'F') {
                    child = doFor();
                } else if (clause_type == 'L') {
                    child = doLabel();
                } else if (clause_type == 'A') {
                    child = doAsm();
                } else if (clause_type == 'N') {
                    /* CONTINUE statement */
                    fdprintf(2, "CONTINUE");
                    child = new_stmt('N');
                    expect(')');
                } else if (clause_type == ';') {
                    child = doEmptyStmt();
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
doFunction(void)
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
    strncpy(name_buf, readSymbol(), sizeof(name_buf) - 1);
    name_buf[sizeof(name_buf) - 1] = '\0';
    ctx.name = name_buf;
    fdprintf(2, "\nFUNCTION %s\n", ctx.name);

    /* Track this function as defined (prepend "_" for assembly label format) */
    {
        char func_label[128];
        snprintf(func_label, sizeof(func_label), "_%s", ctx.name);
        addDefinedSymbol(func_label);
    }

    /* Parameters - collect into buffer for prologue */
    skip();
    expect('(');
    fdprintf(2, "  PARAMS: ");
    p = params_buf;
    params_buf[0] = '\0';
    first_param = 1;
    skip();
    while (curchar != ')') {
        char *param = readSymbol();
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
            ptype = readType();  /* Get type */
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
    ctx.rettype = readType();
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
                dname = readSymbol();
                dtype = readType();
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
                    child = doBlock();
                    break;
                case 'I':  /* If */
                    child = doIf();
                    break;
                case 'W':  /* While */
                    child = doWhile();
                    break;
                case 'D':  /* Do-while */
                    child = doDo();
                    break;
                case 'F':  /* For */
                    child = doFor();
                    break;
                case 'R':  /* Return */
                    child = doReturn();
                    break;
                case 'E':  /* Expression statement */
                    child = doExprStmt();
                    break;
                case ';':  /* Empty statement */
                    child = doEmptyStmt();
                    break;
                case 'A':  /* Asm block */
                    child = doAsm();
                    break;
                case 'L':  /* Label */
                    child = doLabel();
                    break;
                case 'G':  /* Goto */
                    child = doGoto();
                    break;
                case 'S':  /* Switch */
                    child = doSwitch();
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
    assignFrameOffsets(&ctx);

    /* Phase 2: Generate assembly code blocks for tree nodes */
    generate_code(&ctx);

    /* Phase 2.25: Optimize frame layout based on lifetime analysis */
    optimizeFrameLayout(&ctx);

    /* Phase 2.5: Allocate registers based on usage patterns */
    allocateRegisters(&ctx);

    /* Phase 3: Emit assembly and free tree nodes */
    emit_assembly(&ctx, outFd);
}

static void
doGlobal(void)
{
    char *name, *type;
    const char *global_label;

    /* (g name type [init]) */
    name = readSymbol();
    type = readType();

    fdprintf(2, "\nGLOBAL %s %s", name, type);

    /* Track this global as defined (strip "$" prefix for assembly format) */
    global_label = name;
    if (global_label[0] == '$') {
        global_label++;  /* Skip $ to get _varname */
    }
    addDefinedSymbol(global_label);

    skip();
    if (curchar != ')') {
        fdprintf(2, " = ");
        parse_expr();
    }

    fdprintf(2, "\n");
    expect(')');
}

static void
doStringLiteral(void)
{
    char *name, *data;

    /* (s name "data") */
    name = readSymbol();
    data = readQuotedString();

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
initExprHandlers(void)
{
    int i;

    /* Initialize all to generic handler */
    for (i = 0; i < 256; i++) {
        expr_handlers[i] = doGeneric;
    }

    /* Register specific handlers */
    expr_handlers['M'] = doDeref;     /* DEREF */
    expr_handlers['='] = doAssign;    /* ASSIGN */
    expr_handlers['@'] = doCall;      /* CALL */
    expr_handlers['?'] = doTernary;   /* TERNARY */
    expr_handlers[':'] = doColon;     /* COLON */

    /* Binary operators */
    expr_handlers['+'] = doBinaryOp;
    expr_handlers['-'] = doBinaryOp;
    expr_handlers['*'] = doBinaryOp;
    expr_handlers['/'] = doBinaryOp;
    expr_handlers['%'] = doBinaryOp;
    expr_handlers['&'] = doBinaryOp;
    expr_handlers['|'] = doBinaryOp;
    expr_handlers['^'] = doBinaryOp;
    expr_handlers['<'] = doBinaryOp;
    expr_handlers['>'] = doBinaryOp;
    expr_handlers['Q'] = doBinaryOp;  /* EQ == */
    expr_handlers['n'] = doBinaryOp;  /* NEQ != */
    expr_handlers['L'] = doBinaryOp;  /* LE <= */
    expr_handlers['g'] = doBinaryOp;  /* GE >= */
    expr_handlers['y'] = doBinaryOp;  /* LSHIFT << */
    expr_handlers['w'] = doBinaryOp;  /* RSHIFT >> */
    expr_handlers['h'] = doLor;        /* LOR || */
    expr_handlers['j'] = doLand;       /* LAND && */

    /* Compound assignment operators */
    expr_handlers['P'] = doCompoundAssign;  /* PLUSEQ += */
    expr_handlers[0xdf] = doCompoundAssign; /* SUBEQ -= */
    expr_handlers['T'] = doCompoundAssign;  /* MULTEQ *= */
    expr_handlers['2'] = doCompoundAssign;  /* DIVEQ /= */
    expr_handlers[0xfe] = doCompoundAssign; /* MODEQ %= */
    expr_handlers[0xc6] = doCompoundAssign; /* ANDEQ &= */
    expr_handlers['1'] = doCompoundAssign;  /* OREQ |= */
    expr_handlers['X'] = doCompoundAssign;  /* XOREQ ^= */
    expr_handlers['0'] = doCompoundAssign;  /* LSHIFTEQ <<= */
    expr_handlers['6'] = doCompoundAssign;  /* RSHIFTEQ >>= */

    /* Unary operators */
    expr_handlers['!'] = doUnaryOp;
    expr_handlers['~'] = doUnaryOp;
    expr_handlers['\\'] = doUnaryOp; /* NEG */
    expr_handlers['\''] = doUnaryOp; /* NOT */

    /* Type conversion operators */
    expr_handlers['N'] = doUnaryOp;  /* NARROW */
    expr_handlers['W'] = doUnaryOp;  /* WIDEN */
    expr_handlers[0xab] = doUnaryOp;  /* SEXT (sign extend) */

    /* Increment/decrement */
    expr_handlers[0xcf] = doUnaryOp; /* PREINC */
    expr_handlers[0xef] = doUnaryOp; /* POSTINC */
    expr_handlers[0xd6] = doUnaryOp; /* PREDEC */
    expr_handlers[0xf6] = doUnaryOp; /* POSTDEC */

    /* COPY operator */
    expr_handlers[0xbb] = doBinaryOp; /* COPY */

    /* Bitfield operators */
    expr_handlers[0xa7] = doBfextract;  /* BFEXTRACT */
    expr_handlers[0xdd] = doBfassign;   /* BFASSIGN */
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
        e = doSymbol();
    } else if (curchar == 'S') {
        /* String literal */
        nextchar();
        e = doString();
    } else if ((curchar >= '0' && curchar <= '9') || curchar == '-') {
        /* Constant */
        e = doConst();
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
        s = doBlock();
        break;
    case 'I':  /* If */
        s = doIf();
        break;
    case 'W':  /* While */
        s = doWhile();
        break;
    case 'D':  /* Do-while */
        s = doDo();
        break;
    case 'F':  /* For */
        s = doFor();
        break;
    case 'R':  /* Return */
        s = doReturn();
        break;
    case 'E':  /* Expression statement */
        s = doExprStmt();
        break;
    case ';':  /* Empty statement */
        s = doEmptyStmt();
        break;
    case 'A':  /* Asm block */
        s = doAsm();
        break;
    case 'L':  /* Label */
        s = doLabel();
        break;
    case 'G':  /* Goto */
        s = doGoto();
        break;
    case 'S':  /* Switch */
        s = doSwitch();
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
        doFunction();
        break;
    case 'g':  /* Global variable */
        doGlobal();
        break;
    case 's':  /* String literal */
        doStringLiteral();
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
 * Symbol tracking for EXTERN declarations
 */
#define MAX_SYMBOLS 512

static char *defined_symbols[MAX_SYMBOLS];
static int num_defined = 0;

static char *referenced_symbols[MAX_SYMBOLS];
static int num_referenced = 0;

/*
 * Track a symbol definition (function or global variable)
 */
static void
addDefinedSymbol(const char *name)
{
    int i;

    if (num_defined >= MAX_SYMBOLS) return;

    /* Check if already recorded */
    for (i = 0; i < num_defined; i++) {
        if (strcmp(defined_symbols[i], name) == 0) {
            return;
        }
    }

    /* Add new symbol */
    defined_symbols[num_defined++] = strdup(name);
}

/*
 * Track a symbol reference (function call or variable use)
 */
void
addReferencedSymbol(const char *name)
{
    int i;

    if (num_referenced >= MAX_SYMBOLS) return;

    /* Check if already recorded */
    for (i = 0; i < num_referenced; i++) {
        if (strcmp(referenced_symbols[i], name) == 0) {
            return;
        }
    }

    /* Add new symbol */
    referenced_symbols[num_referenced++] = strdup(name);
}

/*
 * Emit GLOBAL and EXTERN declarations
 * GLOBAL for symbols defined in this file
 * EXTERN for symbols referenced but not defined
 */
static void
emitSymbolDeclarations(void)
{
    int i, j;
    int is_defined;

    /* First emit GLOBAL declarations for all defined symbols */
    for (i = 0; i < num_defined; i++) {
        fdprintf(outFd, "GLOBAL %s\n", defined_symbols[i]);
    }

    /* Blank line after GLOBAL declarations */
    if (num_defined > 0) {
        fdprintf(outFd, "\n");
    }

    /* Then emit EXTERN declarations for undefined references */
    for (i = 0; i < num_referenced; i++) {
        is_defined = 0;

        /* Check if this symbol is defined in this file */
        for (j = 0; j < num_defined; j++) {
            if (strcmp(referenced_symbols[i], defined_symbols[j]) == 0) {
                is_defined = 1;
                break;
            }
        }

        /* If not defined, emit EXTERN declaration */
        if (!is_defined) {
            fdprintf(outFd, "EXTERN %s\n", referenced_symbols[i]);
        }
    }

    /* Blank line after EXTERN declarations */
    if (num_referenced > 0) {
        fdprintf(outFd, "\n");
    }
}

/*
 * Initialize parser and read AST file
 */
int
parseAstFile(int in, int out)
{
    /* Initialize low-level I/O */
    initAstio(in);
    outFd = out;

    /* Initialize expression handler lookup table */
    initExprHandlers();

    /* Prime the input */
    nextchar();

    /* Parse all top-level constructs */
    while (curchar) {
        skip();
        if (curchar) {
            parse_toplevel();
        }
    }

    /* Emit GLOBAL and EXTERN declarations at the end (we know all symbols now) */
    emitSymbolDeclarations();

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
