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
static struct expr *parseExpr(void);
static struct stmt *parseStmt(void);

/* Symbol tracking for EXTERN declarations */
static void addDefSym(const char *name);
void addRefSym(const char *name);

/*
 * Check if a name is a mangled static function name
 * Mangled names end with 4 hex digits
 */
static int
isMangledName(const char *name)
{
    int len;
    int i;

    if (!name)
        return 0;

    len = strlen(name);
    if (len < 4)
        return 0;

    /* Check if last 4 characters are hex digits */
    for (i = len - 4; i < len; i++) {
        char c = name[i];
        if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
            return 0;
        }
    }

    return 1;
}

/* Parser state */
int outFd = 1;  /* Assembly output (default: stdout) */
static int labelCounter = 0;  /* For generating unique labels */

/* Segment tracking */
#define SEG_NONE 0
#define SEG_TEXT 1
#define SEG_DATA 2
#define SEG_BSS  3
static int currentSeg = SEG_NONE;

/*
 * Switch to a different segment if needed
 * Only emits directive if segment is different from current
 */
static void
switchToSeg(int seg)
{
    if (seg == currentSeg) {
        return;  /* Already in this segment */
    }

    switch (seg) {
    case SEG_TEXT:
        fdputs(outFd, "\n.text\n");
        break;
    case SEG_DATA:
        fdputs(outFd, "\n.data\n");
        break;
    case SEG_BSS:
        fdputs(outFd, "\n.bss\n");
        break;
    }

    currentSeg = seg;
}

/* Output buffering for symbol declarations */
static char *outputBuffer = NULL;
static int outBufSize = 0;
static int outBufUsed = 0;
static int realOutFd = 1;  /* Real output file descriptor */
static int bufEnabled = 0;

/*
 * Tree node allocation helpers
 */
struct expr *
newExpr(unsigned char op)
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
    e->jump = NULL;
    return e;
}

struct stmt *
newStmt(unsigned char type)
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
    s->jump = NULL;
    return s;
}

/*
 * Extract size in bytes from type annotation string
 * Type strings: ":b" (byte/1), ":s" (short/2), ":l" (long/4), ":p" (pointer/2),
 *               ":f" (float/4), ":d" (double/8)
 * Returns: size in bytes, or 2 (default short size) if no annotation
 */
unsigned char
getSizeFTStr(const char *type_str)
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
getSignFTStr(const char *type_str)
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
getSizeFromTN(const char *typename)
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
isPowerOf2(long value)
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
isMulByPow2(struct expr *e, struct expr **out_expr)
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
    shift = isPowerOf2(e->right->value);
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
isStructMem(struct expr *e, char **out_var, long *out_offset)
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
createLblAsm(const char *label_name)
{
    struct stmt *s = newStmt('A');
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
typedef struct expr* (*handlerFn)(unsigned char op);

/* Generic handler that builds a generic expr node */
static struct expr *
doGeneric(unsigned char op)
{
    struct expr *e;
    int depth;

    e = newExpr(op);

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
    struct expr *e = newExpr('C');  // 'C' for constant
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
    struct expr *e = newExpr('$');  // '$' for symbol
    char *sym = readSymbol();
    e->symbol = strdup(sym);
    fdprintf(2, "SYM %s", e->symbol);
    return e;
}

static struct expr *
doString(void)
{
    struct expr *e = newExpr('S');  // 'S' for string
    /* String literal: S followed by index */
    e->value = readNumber();
    fdprintf(2, "STRING S%ld", e->value);
    return e;
}

static struct expr *
handleCmpAsn(unsigned char op)
{
    struct expr *e;
    char width;
    char width_str[3];

    e = newExpr(op);
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
        e->size = getSizeFTStr(e->type_str);
        e->flags = getSignFTStr(e->type_str);
    }

    skip();
    e->left = parseExpr();  /* lvalue */
    skip();
    e->right = parseExpr();  /* rvalue */
    expect(')');

    return e;
}

static struct expr *
doBinaryOp(unsigned char op)
{
    struct expr *e = newExpr(op);

    fdprintf(2, "BINOP %c (", op);
    skip();
    e->left = parseExpr();  /* left operand - now returns tree */
    fdprintf(2, ", ");
    skip();
    e->right = parseExpr();  /* right operand - now returns tree */
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

        shift = isMulByPow2(e, NULL);
        if (shift >= 0) {
            /* Transform (* expr const_2^n) to (y expr const_n) */
            e->op = 'y';  /* LSHIFT */
            /* Replace right operand with shift amount constant */
            old_right = e->right;
            e->right = newExpr('C');
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
    struct expr *e = newExpr(op);
    e->label = labelCounter++;

    fdprintf(2, "LAND_%d (", e->label);
    skip();
    e->left = parseExpr();  /* left operand */
    fdprintf(2, " ? ");

    /* Will emit conditional jump during code generation */
    fdprintf(2, "JZ skip_%d : ", e->label);

    skip();
    e->right = parseExpr();  /* right operand */

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
    struct expr *e = newExpr(op);
    e->label = labelCounter++;

    fdprintf(2, "LOR_%d (", e->label);
    skip();
    e->left = parseExpr();  /* left operand */
    fdprintf(2, " ? ");

    /* Will emit conditional jump during code generation */
    fdprintf(2, "JNZ skip_%d : ", e->label);

    skip();
    e->right = parseExpr();  /* right operand */

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

    e = newExpr(op);
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
        e->size = getSizeFTStr(e->type_str);
        fdprintf(2, "UNOP %c:%c (", op, width);
    } else {
        fdprintf(2, "UNOP %c (", op);
    }

    skip();
    e->left = parseExpr();  /* operand */
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
        e->flags = getSignFTStr(e->type_str);
    } else if (e->left) {
        /* Otherwise inherit from operand */
        e->flags = e->left->flags;
    }

    return e;
}

static struct expr *
doIncDec(unsigned char op)
{
    struct expr *e;
    long amount;

    e = newExpr(op);

    fdprintf(2, "INCDEC %c (", op);

    skip();
    e->left = parseExpr();  /* lvalue */

    /* Parse increment amount */
    skip();
    amount = readNumber();
    e->value = amount;  /* Store increment amount in value field */

    fdprintf(2, " %ld)", amount);
    expect(')');

    /* Inherit size from operand */
    if (e->left) {
        e->size = e->left->size;
        e->flags = e->left->flags;
    }

    return e;
}

static struct expr *
doBfextract(unsigned char op)
{
    struct expr *e = newExpr(op);
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
    e->left = parseExpr();  /* address */
    fdprintf(2, ")");
    expect(')');
    return e;
}

static struct expr *
doBfassign(unsigned char op)
{
    struct expr *e = newExpr(op);
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
    e->left = parseExpr();  /* address */
    skip();
    e->right = parseExpr();  /* value */
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
    struct expr *e = newExpr(op);

    fdprintf(2, "COLON (");
    skip();
    e->left = parseExpr();  /* left */
    fdprintf(2, ", ");
    skip();
    e->right = parseExpr();  /* right */
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

    e = newExpr('M');  /* 'M' for memory/deref */
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
        e->size = getSizeFTStr(e->type_str);
        e->flags = getSignFTStr(e->type_str);
    }

    fdprintf(2, "DEREF:%c (", width);
    skip();
    e->left = parseExpr();  /* address expression */
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

    e = newExpr('=');  /* '=' for assignment */
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
        e->size = getSizeFTStr(e->type_str);
        e->flags = getSignFTStr(e->type_str);
    }

    skip();
    e->left = parseExpr();  /* lvalue */
    skip();
    e->right = parseExpr();  /* rvalue */
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

    e = newExpr('@');  /* '@' for call */
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
    saveParseSt(&saved_state);

    /* Parse function expression */
    fdprintf(2, "\n  CALL_FUNC: ");
    setupStrInput(func_buf, func_len);
    e->left = parseExpr();  /* function address */
    restoreParse(&saved_state);

    /* Parse arguments */
    for (i = 0; i < arg_count; i++) {
        fdprintf(2, "\n  ARG%d: ", i);
        setupStrInput(&arg_buf[arg_start[i]], arg_len[i]);
        args[i] = parseExpr();
        restoreParse(&saved_state);
    }

    /* Done with saved state */
    freeParseSt(&saved_state);

    /* Build argument chain using wrapper nodes to avoid corrupting argument trees */
    /* Store arg_count in value field */
    e->value = arg_count;
    if (arg_count > 0) {
        struct expr *wrappers[32];  /* Wrapper nodes for each argument */

        /* Create wrapper nodes for each argument */
        for (i = 0; i < arg_count; i++) {
            wrappers[i] = newExpr(',');  /* ',' represents argument wrapper */
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

    e = newExpr('?');  /* '?' for ternary */

    fdprintf(2, "TERNARY (");
    skip();
    e->left = parseExpr();  /* condition */
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
            colon = newExpr(':');
            colon->left = parseExpr();  /* true expr */
            fdprintf(2, " : ");
            skip();
            colon->right = parseExpr();  /* false expr */
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
static struct stmt *doDefInBlock(void);

/* Handlers for case/default when they appear in block context */
static struct stmt *
doCaseInBlock(void)
{
    /* Case statement: (C value ()) */
    struct stmt *child = newStmt('C');

    fdprintf(2, "CASE ");
    skip();
    child->expr = parseExpr();  /* case value */
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
doDefInBlock(void)
{
    /* Default statement: (O ()) */
    struct stmt *child = newStmt('O');

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

    s = newStmt('B');
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
                child = newStmt('d');
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
                    child = doDefInBlock();
                    break;
                case 'K':  /* Break */
                    fdprintf(2, "BREAK");
                    child = newStmt('K');
                    expect(')');
                    break;
                case 'N':  /* Continue */
                    fdprintf(2, "CONTINUE");
                    child = newStmt('N');
                    expect(')');
                    break;
                default:
                    fdprintf(2, "parseast: line %d: unknown" 
                        " stmt op '%c' in block\n", lineNum, op);
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
    struct stmt *s = newStmt('I');
    char label_buf[64];

    s->label = labelCounter++;

    fdprintf(2, "IF (");
    skip();
    s->expr = parseExpr();  /* condition */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parseStmt();  /* then branch */

    skip();
    if (curchar != ')') {
        /* Has else branch - need second label for end of else */
        s->label2 = labelCounter++;

        fdprintf(2, " ELSE ");
        s->else_branch = parseStmt();  /* else branch */

        /* Insert end label after the IF statement */
        snprintf(label_buf, sizeof(label_buf), "_if_end_%d", s->label2);
        s->next = createLblAsm(label_buf);
    } else {
        s->else_branch = NULL;

        /* Insert end label after the IF statement */
        snprintf(label_buf, sizeof(label_buf), "_if_end_%d", s->label);
        s->next = createLblAsm(label_buf);
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

    s = newStmt('W');
    s->label = labelCounter++;  /* Loop start label */

    fdprintf(2, "WHILE (");
    skip();
    s->expr = parseExpr();  /* condition */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parseStmt();  /* body */

    expect(')');

    /* Insert loop start label before the while, and end label after */
    /* Create label for loop start */
    snprintf(label_buf, sizeof(label_buf), "_while_%d", s->label);
    start_label = createLblAsm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the while */
    snprintf(label_buf, sizeof(label_buf), "_while_end_%d", s->label);
    s->next = createLblAsm(label_buf);

    return start_label;  /* Return the label, not the while node */
}

static struct stmt *
doDo(void)
{
    struct stmt *s;
    char label_buf[64];
    struct stmt *start_label;

    s = newStmt('D');
    s->label = labelCounter++;  /* Loop start label */

    fdprintf(2, "DO ");
    skip();
    s->then_branch = parseStmt();  /* body */

    fdprintf(2, " WHILE (");
    skip();
    s->expr = parseExpr();  /* condition */
    fdprintf(2, ")");

    expect(')');

    /* Insert loop start label before the do, and end label after */
    snprintf(label_buf, sizeof(label_buf), "_do_%d", s->label);
    start_label = createLblAsm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the do-while */
    snprintf(label_buf, sizeof(label_buf), "_do_end_%d", s->label);
    s->next = createLblAsm(label_buf);

    return start_label;
}

static struct stmt *
doFor(void)
{
    struct stmt *s;
    char label_buf[64];
    struct stmt *start_label;

    s = newStmt('F');
    s->label = labelCounter++;  /* Loop start label */

    fdprintf(2, "FOR (");
    skip();
    s->expr = parseExpr();  /* init */
    fdprintf(2, "; ");
    skip();
    s->expr2 = parseExpr();  /* condition */
    fdprintf(2, "; ");
    skip();
    s->expr3 = parseExpr();  /* increment */
    fdprintf(2, ") ");

    skip();
    s->then_branch = parseStmt();  /* body */

    expect(')');

    /* Insert loop start label before the for, and end label after */
    snprintf(label_buf, sizeof(label_buf), "_for_%d", s->label);
    start_label = createLblAsm(label_buf);
    start_label->next = s;

    /* Insert end/break label after the for */
    snprintf(label_buf, sizeof(label_buf), "_for_end_%d", s->label);
    s->next = createLblAsm(label_buf);

    return start_label;
}

static struct stmt *
doReturn(void)
{
    struct stmt *s = newStmt('R');

    fdprintf(2, "RETURN");
    skip();
    if (curchar != ')') {
        fdprintf(2, " ");
        s->expr = parseExpr();
    } else {
        s->expr = NULL;
    }
    expect(')');
    return s;
}

static struct stmt *
doExprStmt(void)
{
    struct stmt *s = newStmt('E');

    fdprintf(2, "EXPR (");
    skip();
    s->expr = parseExpr();
    fdprintf(2, ")");
    expect(')');
    return s;
}

static struct stmt *
doEmptyStmt(void)
{
    struct stmt *s = newStmt(';');

    fdprintf(2, ";");
    expect(')');
    return s;
}

static struct stmt *
doAsm(void)
{
    struct stmt *s = newStmt('A');
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
    struct stmt *s = newStmt('L');
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
    struct stmt *s = newStmt('G');
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

    s = newStmt('S');
    first_child = NULL;
    last_child = NULL;

    /* Switch statement: (S expr (C val ...) (C val ...) (O ...) ) */
    fdprintf(2, "SWITCH (");
    skip();
    s->expr = parseExpr();  /* switch expression */
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
                child = newStmt('C');
                skip();
                child->expr = parseExpr();  /* case value */
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
                child = newStmt('O');
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
                    child = newStmt('G');
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
                    child = newStmt('K');
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
                    child = newStmt('N');
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

    /* Switch to .text segment for function code */
    switchToSeg(SEG_TEXT);

    /* Track this function as defined (prepend "_" for assembly label format) */
    /* Static functions (mangled names) don't get the underscore prefix */
    {
        char func_label[128];
        if (isMangledName(ctx.name)) {
            snprintf(func_label, sizeof(func_label), "%s", ctx.name);
        } else {
            snprintf(func_label, sizeof(func_label), "_%s", ctx.name);
        }
        addDefSym(func_label);
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
                child = newStmt('d');
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
                        lineNum, op);
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
    ctx.labelCounter = labelCounter;  /* Save current label counter */
    ctx.locals = NULL;  /* No local variables yet */
    ctx.frame_size = 0;  /* No frame size yet */
    ctx.de_save_count = 0;  /* No nested DE saves yet */
    ctx.d_in_use = 0;  /* D register not in use yet */

    expect(')');

    /* Phase 1.5: Assign stack frame offsets to local variables */
    assignFrmOff(&ctx);

    /* Phase 2: Generate assembly code blocks for tree nodes */
    generateCode(&ctx);

    /* Phase 2.25: Optimize frame layout based on lifetime analysis */
    optFrmLayout(&ctx);

    /* Phase 2.5: Allocate registers based on usage patterns */
    allocRegs(&ctx);

    /* Phase 3: Emit assembly and free tree nodes */
    emitAssembly(&ctx, outFd);
}

/* Forward declaration for symbol tracking */
static int isDefSym(const char *name);

static void
doGlobal(void)
{
    char *name, *type;
    const char *global_label;
    char label_buf[128];
    int has_init = 0;
    int isDefined;

    /* (g name type [init]) */
    name = readSymbol();
    type = readType();

    fdprintf(2, "\nGLOBAL %s %s", name, type);

    /* Track this global as defined (strip "$" prefix for assembly format) */
    global_label = name;
    if (global_label[0] == '$') {
        global_label++;  /* Skip $ to get _varname */
    }

    /* Copy label to buffer before parseExpr() modifies input buffer */
    strncpy(label_buf, global_label, sizeof(label_buf) - 1);
    label_buf[sizeof(label_buf) - 1] = '\0';

    /* Check if this symbol was already emitted */
    isDefined = isDefSym(label_buf);
    addDefSym(label_buf);

    skip();
    if (curchar != ')') {
        fdprintf(2, " = ");
        parseExpr();
        has_init = 1;
    }

    fdprintf(2, "\n");

    /* Emit assembly for global variable only if not already emitted */
    if (!isDefined) {
        /* Switch to appropriate segment based on initialization */
        if (has_init) {
            /* Initialized data goes in .data segment */
            switchToSeg(SEG_DATA);
        } else {
            /* Uninitialized data goes in .bss segment */
            switchToSeg(SEG_BSS);
        }

        fdprintf(outFd, "%s:\n", label_buf);
        fdprintf(outFd, "\t.dw 0\n");  /* TODO: Handle initializer values and sizes */
    }

    expect(')');
}

/*
 * Emit a string with proper escaping for assembly .db directive
 * Escapes: newline, tab, quote, backslash, and non-printable chars
 */
static void
emitEscStr(int fd, const char *str)
{
    unsigned char c;

    while ((c = *str++)) {
        switch (c) {
        case '\n':
            fdputs(fd, "\\n");
            break;
        case '\t':
            fdputs(fd, "\\t");
            break;
        case '\r':
            fdputs(fd, "\\r");
            break;
        case '"':
            fdputs(fd, "\\\"");
            break;
        case '\\':
            fdputs(fd, "\\\\");
            break;
        default:
            if (c >= 32 && c < 127) {
                /* Printable ASCII */
                fdprintf(fd, "%c", c);
            } else {
                /* Non-printable: use hex escape */
                fdprintf(fd, "\\x%02x", c);
            }
            break;
        }
    }
}

static void
doStrLiteral(void)
{
    char *name, *data;

    /* (s name "data") */
    name = readSymbol();
    data = readQuotedStr();

    fdprintf(2, "\nSTRING %s \"%s\"\n", name, data);

    /* Switch to .data segment for string literals */
    switchToSeg(SEG_DATA);

    /* Emit string literal with underscore prefix */
    fdprintf(outFd, "_%s:\n", name);
    fdputs(outFd, "\t.db \"");
    emitEscStr(outFd, data);
    fdputs(outFd, "\\0\"\n");

    /* Track this string as defined to avoid duplicate emission */
    {
        char str_label[128];
        snprintf(str_label, sizeof(str_label), "_%s", name);
        addDefSym(str_label);
    }

    expect(')');
}

/*
 * Operator lookup table
 * Maps each operator character to its handler function
 * Table is sparse - only operators that need handling are listed
 */
static handlerFn exprHandlers[256];

static void
initExprHndl(void)
{
    int i;

    /* Initialize all to generic handler */
    for (i = 0; i < 256; i++) {
        exprHandlers[i] = doGeneric;
    }

    /* Register specific handlers */
    exprHandlers['M'] = doDeref;     /* DEREF */
    exprHandlers['='] = doAssign;    /* ASSIGN */
    exprHandlers['@'] = doCall;      /* CALL */
    exprHandlers['?'] = doTernary;   /* TERNARY */
    exprHandlers[':'] = doColon;     /* COLON */

    /* Binary operators */
    exprHandlers['+'] = doBinaryOp;
    exprHandlers['-'] = doBinaryOp;
    exprHandlers['*'] = doBinaryOp;
    exprHandlers['/'] = doBinaryOp;
    exprHandlers['%'] = doBinaryOp;
    exprHandlers['&'] = doBinaryOp;
    exprHandlers['|'] = doBinaryOp;
    exprHandlers['^'] = doBinaryOp;
    exprHandlers['<'] = doBinaryOp;
    exprHandlers['>'] = doBinaryOp;
    exprHandlers['Q'] = doBinaryOp;  /* EQ == */
    exprHandlers['n'] = doBinaryOp;  /* NEQ != */
    exprHandlers['L'] = doBinaryOp;  /* LE <= */
    exprHandlers['g'] = doBinaryOp;  /* GE >= */
    exprHandlers['y'] = doBinaryOp;  /* LSHIFT << */
    exprHandlers['w'] = doBinaryOp;  /* RSHIFT >> */
    exprHandlers['h'] = doLor;        /* LOR || */
    exprHandlers['j'] = doLand;       /* LAND && */

    /* Compound assignment operators */
    exprHandlers['P'] = handleCmpAsn;  /* PLUSEQ += */
    exprHandlers[0xdf] = handleCmpAsn; /* SUBEQ -= */
    exprHandlers['T'] = handleCmpAsn;  /* MULTEQ *= */
    exprHandlers['2'] = handleCmpAsn;  /* DIVEQ /= */
    exprHandlers[0xfe] = handleCmpAsn; /* MODEQ %= */
    exprHandlers[0xc6] = handleCmpAsn; /* ANDEQ &= */
    exprHandlers['1'] = handleCmpAsn;  /* OREQ |= */
    exprHandlers['X'] = handleCmpAsn;  /* XOREQ ^= */
    exprHandlers['0'] = handleCmpAsn;  /* LSHIFTEQ <<= */
    exprHandlers['6'] = handleCmpAsn;  /* RSHIFTEQ >>= */

    /* Unary operators */
    exprHandlers['!'] = doUnaryOp;
    exprHandlers['~'] = doUnaryOp;
    exprHandlers['\\'] = doUnaryOp; /* NEG */
    exprHandlers['\''] = doUnaryOp; /* NOT */

    /* Type conversion operators */
    exprHandlers['N'] = doUnaryOp;  /* NARROW */
    exprHandlers['W'] = doUnaryOp;  /* WIDEN */
    exprHandlers[0xab] = doUnaryOp;  /* SEXT (sign extend) */

    /* Increment/decrement */
    exprHandlers[0xcf] = doIncDec; /* PREINC */
    exprHandlers[0xef] = doIncDec; /* POSTINC */
    exprHandlers[0xd6] = doIncDec; /* PREDEC */
    exprHandlers[0xf6] = doIncDec; /* POSTDEC */

    /* COPY operator */
    exprHandlers[0xbb] = doBinaryOp; /* COPY */

    /* Bitfield operators */
    exprHandlers[0xa7] = doBfextract;  /* BFEXTRACT */
    exprHandlers[0xdd] = doBfassign;   /* BFASSIGN */
}

/*
 * Parse an expression (recursive)
 */
static struct expr *
parseExpr(void)
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
        e = exprHandlers[op](op);

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
                 lineNum, curchar);
        nextchar();
        e = NULL;
    }

    return e;
}

/*
 * Parse a statement (recursive)
 */
static struct stmt *
parseStmt(void)
{
    struct stmt *s;
    char op;

    skip();

    if (curchar != '(') {
        fdprintf(2, "parseast: line %d: expected '(' at start of statement\n",
            lineNum);
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
        fdprintf(2, "parseast: line %d: unknown stmt op '%c'\n", lineNum, op);
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
parseToplvl(void)
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
        doStrLiteral();
        break;
    default:
        fdprintf(2, "parseast: line %d: unknown top-level op '%c'\n", 
            lineNum, op);
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

static char *defSymbols[MAX_SYMBOLS];
static int numDefined = 0;

static char *refSymbols[MAX_SYMBOLS];
static int numReferenced = 0;

/*
 * Check if a symbol has already been defined
 */
static int
isDefSym(const char *name)
{
    int i;
    for (i = 0; i < numDefined; i++) {
        if (strcmp(defSymbols[i], name) == 0) {
            return 1;
        }
    }
    return 0;
}

/*
 * Track a symbol definition (function or global variable)
 */
static void
addDefSym(const char *name)
{
    int i;

    if (numDefined >= MAX_SYMBOLS) return;

    /* Check if already recorded */
    for (i = 0; i < numDefined; i++) {
        if (strcmp(defSymbols[i], name) == 0) {
            return;
        }
    }

    /* Add new symbol */
    defSymbols[numDefined++] = strdup(name);
}

/*
 * Track a symbol reference (function call or variable use)
 */
void
addRefSym(const char *name)
{
    int i;

    if (numReferenced >= MAX_SYMBOLS) return;

    /* Check if already recorded */
    for (i = 0; i < numReferenced; i++) {
        if (strcmp(refSymbols[i], name) == 0) {
            return;
        }
    }

    /* Add new symbol */
    refSymbols[numReferenced++] = strdup(name);
}

/*
 * Emit GLOBAL and EXTERN declarations
 * GLOBAL for symbols defined in this file
 * EXTERN for symbols referenced but not defined
 */
static void
emitSymDecls(void)
{
    int i, j;
    int is_defined;

    /* First emit GLOBAL declarations for all defined symbols */
    for (i = 0; i < numDefined; i++) {
        fdputs(outFd, ASM_GLOBAL " ");
        fdputs(outFd, defSymbols[i]);
        fdputs(outFd, "\n");
    }

    /* Blank line after GLOBAL declarations */
    if (numDefined > 0) {
        fdputs(outFd, "\n");
    }

    /* Then emit EXTERN declarations for undefined references */
    for (i = 0; i < numReferenced; i++) {
        is_defined = 0;

        /* Check if this symbol is defined in this file */
        for (j = 0; j < numDefined; j++) {
            if (strcmp(refSymbols[i], defSymbols[j]) == 0) {
                is_defined = 1;
                break;
            }
        }

        /* If not defined, emit EXTERN declaration */
        if (!is_defined) {
            fdputs(outFd, ASM_EXTERN " ");
            fdputs(outFd, refSymbols[i]);
            fdputs(outFd, "\n");
        }
    }

    /* Blank line after EXTERN declarations */
    if (numReferenced > 0) {
        fdputs(outFd, "\n");
    }
}

/*
 * Emit EXTERN declarations for runtime helper functions
 * These are provided by the runtime library (ccclib.s)
 */
static void
emitRtHelpers(void)
{
    /* Frame management */
    fdputs(outFd, ASM_EXTERN " framealloc\n");
    fdputs(outFd, ASM_EXTERN " framefree\n");

    /* Long (32-bit) operations */
    fdputs(outFd, ASM_EXTERN " getlong\n");
    fdputs(outFd, ASM_EXTERN " putlong\n");
    fdputs(outFd, ASM_EXTERN " load32i\n");

    /* 32-bit arithmetic */
    fdputs(outFd, ASM_EXTERN " add3232\n");
    fdputs(outFd, ASM_EXTERN " sub3232\n");
    fdputs(outFd, ASM_EXTERN " mul3232\n");
    fdputs(outFd, ASM_EXTERN " div3232\n");
    fdputs(outFd, ASM_EXTERN " mod3232\n");
    fdputs(outFd, ASM_EXTERN " shr3232\n");

    /* 32-bit comparisons */
    fdputs(outFd, ASM_EXTERN " lt3232\n");
    fdputs(outFd, ASM_EXTERN " gt3232\n");
    fdputs(outFd, ASM_EXTERN " le3232\n");
    fdputs(outFd, ASM_EXTERN " ge3232\n");
    fdputs(outFd, ASM_EXTERN " eq3232\n");
    fdputs(outFd, ASM_EXTERN " ne3232\n");

    /* 32-bit bitwise */
    fdputs(outFd, ASM_EXTERN " and3232\n");
    fdputs(outFd, ASM_EXTERN " or3232\n");
    fdputs(outFd, ASM_EXTERN " xor3232\n");

    /* 16-bit operations */
    fdputs(outFd, ASM_EXTERN " lt1616\n");
    fdputs(outFd, ASM_EXTERN " gt1616\n");
    fdputs(outFd, ASM_EXTERN " le1616\n");
    fdputs(outFd, ASM_EXTERN " ge1616\n");
    fdputs(outFd, ASM_EXTERN " eq1616\n");
    fdputs(outFd, ASM_EXTERN " ne1616\n");
    fdputs(outFd, ASM_EXTERN " ueq1616\n");
    fdputs(outFd, ASM_EXTERN " and1616\n");
    fdputs(outFd, ASM_EXTERN " or1616\n");
    fdputs(outFd, ASM_EXTERN " xor1616\n");
    fdputs(outFd, ASM_EXTERN " sub1616\n");
    fdputs(outFd, ASM_EXTERN " mul1616\n");
    fdputs(outFd, ASM_EXTERN " mod1616\n");
    fdputs(outFd, ASM_EXTERN " shr1616\n");
    fdputs(outFd, ASM_EXTERN " ult1616\n");
    fdputs(outFd, ASM_EXTERN " ugt1616\n");
    fdputs(outFd, ASM_EXTERN " uge1616\n");
    fdputs(outFd, ASM_EXTERN " une1616\n");
    fdputs(outFd, ASM_EXTERN " uand1616\n");
    fdputs(outFd, ASM_EXTERN " usub1616\n");

    /* Mixed size operations */
    fdputs(outFd, ASM_EXTERN " lt816\n");
    fdputs(outFd, ASM_EXTERN " le816\n");
    fdputs(outFd, ASM_EXTERN " gt816\n");
    fdputs(outFd, ASM_EXTERN " ge816\n");
    fdputs(outFd, ASM_EXTERN " sub816\n");
    fdputs(outFd, ASM_EXTERN " eq816\n");
    fdputs(outFd, ASM_EXTERN " ne816\n");
    fdputs(outFd, ASM_EXTERN " ueq816\n");
    fdputs(outFd, ASM_EXTERN " ule3216\n");
    fdputs(outFd, ASM_EXTERN " ult168\n");
    fdputs(outFd, ASM_EXTERN " ult816\n");
    fdputs(outFd, ASM_EXTERN " une816\n");
    fdputs(outFd, ASM_EXTERN " shr816\n");
    fdputs(outFd, ASM_EXTERN " ushr816\n");
    fdputs(outFd, ASM_EXTERN " uand3216\n");
    fdputs(outFd, ASM_EXTERN " and816\n");
    fdputs(outFd, ASM_EXTERN " gt168\n");

    /* 8-bit operations */
    fdputs(outFd, ASM_EXTERN " add88\n");
    fdputs(outFd, ASM_EXTERN " eq88\n");
    fdputs(outFd, ASM_EXTERN " ne88\n");
    fdputs(outFd, ASM_EXTERN " lt88\n");
    fdputs(outFd, ASM_EXTERN " gt88\n");
    fdputs(outFd, ASM_EXTERN " ge88\n");
    fdputs(outFd, ASM_EXTERN " and88\n");
    fdputs(outFd, ASM_EXTERN " or88\n");

    fdputs(outFd, "\n");
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
    initExprHndl();

    /* Emit runtime helper EXTERN declarations at the beginning */
    emitRtHelpers();

    /* Prime the input */
    nextchar();

    /* Parse all top-level constructs and collect symbols */
    while (curchar) {
        skip();
        if (curchar) {
            parseToplvl();
        }
    }

    /* Emit GLOBAL and EXTERN declarations for user symbols at the end */
    emitSymDecls();

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
