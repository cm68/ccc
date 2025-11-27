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
static struct stmt *doStmtBody(char op);

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
unsigned char outFd = 1;  /* Assembly output (default: stdout) */
static int labelCounter = 0;  /* For generating unique labels */

/* Function context globals */
char *fnName;
char *fnParams;
char *fnRettype;
struct stmt *fnBody;
int fnLblCnt;
struct local_var *fnLocals;
int fnFrmSize;
int fnCurLbl;
int fnDESaveCnt;
int fnDInUse;
int fnPendClean;
int fnLoopDep;
int fnDEValid;
int fnZValid;
struct expr *fnHLCache;
struct expr *fnDECache;
struct expr *fnACache;
char fnIXAOfs;           /* When >=0, A has byte from (ix+fnIXAOfs) */
char fnIXHLOfs;          /* When >=0, HL has word from (ix+fnIXHLOfs) */
char fnIYHLOfs;          /* When valid, HL has word from (iy+fnIYHLOfs) */
char fnIYHLValid;        /* 1 if fnIYHLOfs is valid */

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

    /* Check for type suffix letters (from AST: :b, :s, :l, :p) */
    if (typename[0] == 'b') return 1;  /* byte */
    if (typename[0] == 's') return 2;  /* short */
    if (typename[0] == 'l') return 4;  /* long */
    if (typename[0] == 'p') return 2;  /* pointer */

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

    return e;
}

static struct expr *
doSymbol(void)
{
    struct expr *e = newExpr('$');  // '$' for symbol
    char *sym = readSymbol();
    e->symbol = strdup(sym);
    return e;
}

static struct expr *
doString(void)
{
    struct expr *e = newExpr('S');  // 'S' for string
    /* String literal: S followed by index */
    e->value = readNumber();
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

    /* If RHS is a constant, inherit size from assignment */
    if (e->right && e->right->op == 'C') {
        e->right->size = e->size;
    }

    return e;
}

static struct expr *
doBinaryOp(unsigned char op)
{
    struct expr *e = newExpr(op);

    skip();
    e->left = parseExpr();  /* left operand - now returns tree */
    skip();
    e->right = parseExpr();  /* right operand - now returns tree */
    expect(')');

    /* For comparisons with a constant, narrow the constant to match */
    if (e->left && e->right) {
        if (e->left->op == 'C' && e->right->op != 'C') {
            e->left->size = e->right->size;
        } else if (e->right->op == 'C' && e->left->op != 'C') {
            e->right->size = e->left->size;
        }
    }

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

    skip();
    e->left = parseExpr();  /* left operand */

    /* Will emit conditional jump during code generation */

    skip();
    e->right = parseExpr();  /* right operand */

    /* Will emit skip label during code generation */
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

    skip();
    e->left = parseExpr();  /* left operand */

    /* Will emit conditional jump during code generation */

    skip();
    e->right = parseExpr();  /* right operand */

    /* Will emit skip label during code generation */
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
    } else {
    }

    skip();
    e->left = parseExpr();  /* operand */
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


    skip();
    e->left = parseExpr();  /* lvalue */

    /* Parse increment amount */
    skip();
    amount = readNumber();
    e->value = amount;  /* Store increment amount in value field */

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

    skip();
    e->left = parseExpr();  /* address */
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

    skip();
    e->left = parseExpr();  /* left */
    skip();
    e->right = parseExpr();  /* right */
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

    skip();
    e->left = parseExpr();  /* address expression */
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

    /* If RHS is a constant, inherit size from assignment */
    if (e->right && e->right->op == 'C') {
        e->right->size = e->size;
    }

    return e;
}

static struct expr *
doCall(unsigned char op)
{
    struct expr *e;
    struct expr *args[32];
    struct expr *wrapper, *prev;
    int arg_count = 0;
    int i;

    e = newExpr('@');  /* '@' for call */

    skip();
    e->left = parseExpr();  /* function address */

    /* Parse arguments */
    skip();
    while (curchar != ')' && curchar != 0) {
        if (arg_count >= 32) break;
        args[arg_count++] = parseExpr();
        skip();
    }

    /* Build argument chain using wrapper nodes */
    e->value = arg_count;
    prev = NULL;
    for (i = 0; i < arg_count; i++) {
        wrapper = newExpr(',');
        wrapper->left = args[i];
        wrapper->right = NULL;
        if (prev) {
            prev->right = wrapper;
        } else {
            e->right = wrapper;
        }
        prev = wrapper;
    }

    expect(')');
    return e;
}

static struct expr *
doTernary(unsigned char op)
{
    struct expr *e;
    struct expr *colon;

    e = newExpr('?');  /* '?' for ternary */

    skip();
    e->left = parseExpr();  /* condition */
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
            skip();
            colon->right = parseExpr();  /* false expr */
            expect(')');
            e->right = colon;
        }
    }

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

    skip();
    child->expr = parseExpr();  /* case value */
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

    s = newStmt('B');
    first_child = NULL;
    last_child = NULL;

    if (TRACE(T_AST)) {
        fdprintf(2, "doBlock: enter, curchar=%d '%c'\n",
            curchar, curchar > 31 ? curchar : '?');
    }
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
                /* Declaration: (d:suffix name) */
                char type_suffix[3];
                expect(':');
                type_suffix[0] = curchar;  /* b, s, l, p, etc. */
                type_suffix[1] = '\0';
                nextchar();
                name = readSymbol();

                /* Create declaration statement node */
                child = newStmt('d');
                child->symbol = strdup(name);
                child->type_str = strdup(type_suffix);

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
                    child = newStmt('K');
                    expect(')');
                    break;
                case 'N':  /* Continue */
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

    if (TRACE(T_AST)) {
        fdprintf(2, "doBlock: exiting while, curchar=%d '%c'\n",
            curchar, curchar > 31 ? curchar : '?');
    }
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

    skip();
    s->expr = parseExpr();  /* condition */

    skip();
    s->then_branch = parseStmt();  /* then branch */

    skip();
    if (curchar != ')') {
        /* Has else branch - need second label for end of else */
        s->label2 = labelCounter++;

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

    skip();
    s->expr = parseExpr();  /* condition */

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

    skip();
    s->then_branch = parseStmt();  /* body */

    skip();
    s->expr = parseExpr();  /* condition */

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

    skip();
    s->expr = parseExpr();  /* init */
    skip();
    s->expr2 = parseExpr();  /* condition */
    skip();
    s->expr3 = parseExpr();  /* increment */

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

    skip();
    if (curchar != ')') {
        s->expr = parseExpr();
        /* If return value is a constant, set size from function return type */
        if (s->expr && s->expr->op == 'C') {
            s->expr->size = fnRettype[0] == 'b' ? 1 :
                            fnRettype[0] == 'l' ? 4 : 2;
        }
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

    skip();
    s->expr = parseExpr();
    expect(')');
    return s;
}

static struct stmt *
doEmptyStmt(void)
{
    struct stmt *s = newStmt(';');

    expect(')');
    return s;
}

static struct stmt *
doAsm(void)
{
    struct stmt *s = newStmt('A');
    char asm_buf[512];
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

    /* Store label name in statement node - must strdup since readSymbol uses static buffer */
    s->symbol = strdup(label_name);

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

    /* Store label name in statement node - must strdup since readSymbol uses static buffer */
    s->symbol = strdup(label_name);

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
    skip();
    s->expr = parseExpr();  /* switch expression */

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
                child = newStmt('C');
                skip();
                child->expr = parseExpr();  /* case value */
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
                if (clause_type == 'R') {
                    child = doReturn();
                } else if (clause_type == 'G') {
                    child = newStmt('G');
                    skip();
                    child->symbol = readSymbol();
                    expect(')');
                } else if (clause_type == 'B') {
                    /* BLOCK statement */
                    child = doBlock();
                } else if (clause_type == 'K') {
                    /* BREAK statement */
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
                    child = newStmt('N');
                    expect(')');
                } else if (clause_type == ';') {
                    child = doEmptyStmt();
                } else {
                    child = NULL;
                }
            } else {
                /* Unknown - skip it */
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

    expect(')');

    s->then_branch = first_child;  /* Use then_branch for switch body */
    return s;
}

/* Top-level handlers */

/*
 * Helper: Get register name string from register_id enum
 */
static void
doFunction(char rettype)
{
    static char name_buf[64];  /* Buffer for function name */
    static char params_buf[256];
    static char rettype_buf[2];
    char *p;
    int first_param;

    /* (f:rettype name (params) body) - rettype already consumed */
    rettype_buf[0] = rettype;
    rettype_buf[1] = '\0';
    fnRettype = rettype_buf;

    /* Copy function name to stack buffer before reading parameters */
    strncpy(name_buf, readSymbol(), sizeof(name_buf) - 1);
    name_buf[sizeof(name_buf) - 1] = '\0';
    fnName = name_buf;

    /* Switch to .text segment for function code */
    switchToSeg(SEG_TEXT);

    /* Track this function as defined (prepend "_" for assembly label format) */
    /* Static functions (mangled names) don't get the underscore prefix */
    {
        char func_label[128];
        if (isMangledName(fnName)) {
            snprintf(func_label, sizeof(func_label), "%s", fnName);
        } else {
            snprintf(func_label, sizeof(func_label), "_%s", fnName);
        }
        addDefSym(func_label);
    }

    /* Parameters - format: ((d:suffix name) (d:suffix name) ...) */
    skip();
    expect('(');
    p = params_buf;
    params_buf[0] = '\0';
    first_param = 1;
    skip();
    while (curchar != ')') {
        char *param;
        char ptype;

        /* Each param is (d:suffix name) */
        expect('(');
        skip();
        if (curchar != 'd') {
            fdprintf(2, "parseast: expected 'd' in param decl\n");
            break;
        }
        nextchar();
        expect(':');
        ptype = curchar;  /* b, s, l, p */
        nextchar();
        param = readSymbol();

        /* Add to params buffer */
        if (!first_param) {
            if (p < params_buf + sizeof(params_buf) - 2) {
                *p++ = ',';
                *p++ = ' ';
            }
        }
        first_param = 0;

        /* Copy parameter name:type */
        while (*param && p < params_buf + sizeof(params_buf) - 20) {
            *p++ = *param++;
        }
        if (p < params_buf + sizeof(params_buf) - 3) {
            *p++ = ':';
            *p++ = ptype;
        }

        expect(')');
        skip();
    }
    *p = '\0';
    fnParams = params_buf;
    expect(')');

    /* Body is a single statement (typically a block) */
    skip();
    fnBody = parseStmt();
    fnLblCnt = labelCounter;
    fnLocals = NULL;
    fnFrmSize = 0;
    fnDESaveCnt = 0;
    fnDInUse = 0;
    fnLoopDep = 0;
    fnDEValid = 0;
    fnZValid = 0;
    fnHLCache = NULL;
    fnDECache = NULL;
    fnACache = NULL;
    fnIXAOfs = -1;
    fnIXHLOfs = -1;
    fnIYHLValid = 0;

    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: before expect ')' curchar=%d '%c'\n",
            curchar, curchar > 31 ? curchar : '?');
    }
    expect(')');
    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: after expect ')' curchar=%d '%c'\n",
            curchar, curchar > 31 ? curchar : '?');
    }

    /* Phase 1.5: Assign stack frame offsets to local variables */
    assignFrmOff();
    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: after assignFrmOff curchar=%d\n", curchar);
    }

    /* Phase 2: Generate assembly code blocks for tree nodes */
    generateCode();
    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: after generateCode curchar=%d\n", curchar);
    }

    /* Phase 2.25: Optimize frame layout based on lifetime analysis */
    optFrmLayout();
    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: after optFrmLayout curchar=%d\n", curchar);
    }

    /* Phase 2.5: Allocate registers based on usage patterns */
    allocRegs();
    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: after allocRegs curchar=%d\n", curchar);
    }

    /* Phase 3: Emit assembly and free tree nodes */
    emitAssembly(outFd);
    if (TRACE(T_AST)) {
        fdprintf(2, "doFunction: after emitAssembly curchar=%d\n", curchar);
    }
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
    int is_array_init = 0;
    int isDefined;

    /* (g name type [init]) */
    name = readSymbol();
    type = readType();


    /* Track this global as defined (strip "$" prefix for assembly format) */
    global_label = name;
    if (global_label[0] == '$') {
        global_label++;  /* Skip $ to get _varname */
    }

    /* Copy label to buffer before parseExpr() modifies input buffer */
    strncpy(label_buf, global_label, sizeof(label_buf) - 1);
    label_buf[sizeof(label_buf) - 1] = '\0';

    skip();
    if (curchar != ')') {
        has_init = 1;

        /* Check if initializer is a byte array ([:b ...]) */
        if (curchar == '(') {
            expect('(');
            skip();
            if (curchar == '[') {
                int array_type;
                is_array_init = 1;
                expect('[');
                skip();
                expect(':');
                skip();
                array_type = curchar;  /* Save type: 'b' for bytes, 'p' for pointers, etc */
                expect(array_type);
                skip();

            /* Only handle byte arrays - skip other types */
            if (array_type == 'b') {
            /* Parse byte array initializer - we're now at first number */

            /*
             * Skip tentative array definitions - don't emit yet.
             * The real definition with data comes later.
             */
            if (strstr(type, ":array:-1:")) {
                /* Skip the rest of the array */
                while (curchar != ')') {
                    readNumber();
                    skip();
                }
                expect(')');
                expect(')');
                return;
            }

            /* Emit byte array */
            isDefined = isDefSym(label_buf);
            if (!isDefined) {
                int col = 0;
                int first = 1;

                addDefSym(label_buf);
                switchToSeg(SEG_DATA);
                fdprintf(outFd, "%s:\n", label_buf);

                /* Emit byte values, breaking lines at ~80 columns */
                while (curchar != ')') {
                    int val = readNumber();
                    int len;
                    int is_last;

                    skip();
                    is_last = (curchar == ')');

                    /* Calculate length of this value (1-3 digits) */
                    if (val < 10) len = 1;
                    else if (val < 100) len = 2;
                    else len = 3;

                    /* Check if we need to break before this value */
                    if (!first && col + len + (is_last ? 0 : 2) > 80) {
                        /* This value (+comma if not last) would exceed 80, start new line */
                        fdputs(outFd, "\n");
                        col = 0;
                    }

                    /* Start new line if needed */
                    if (first || col == 0) {
                        fdputs(outFd, "\t.db ");
                        col = 12;  /* Tab to column 8 + ".db " is 4 more */
                        first = 0;
                    }

                    /* Emit the value */
                    fdprintf(outFd, "%d", val);
                    col += len;

                    if (!is_last) {
                        /* Check if comma + max next value (3 digits) would fit within 75 cols */
                        if (col + 2 + 3 < 75) {
                            /* Comma and next value can fit, emit comma */
                            fdputs(outFd, ", ");
                            col += 2;
                        } else {
                            /* Line is getting full, break now without comma */
                            fdputs(outFd, "\n");
                            col = 0;
                        }
                    }
                }
                fdputs(outFd, "\n");  /* Close last line */
            } else {
                /* Already defined, skip the data */
                while (curchar != ')') {
                    readNumber();
                    skip();
                }
            }
                /* After loop, curchar is ')' that closes value list */
                expect(')');  /* Close ([:b values...) */
                skip();
                expect(')');  /* Close global declaration */
                return;  /* Done - byte array was emitted */
            } else if (array_type == 'p') {
                /* Pointer array - emit as .dw directives */
                isDefined = isDefSym(label_buf);
                if (!isDefined) {
                    addDefSym(label_buf);
                    switchToSeg(SEG_DATA);
                    fdprintf(outFd, "%s:\n", label_buf);

                    /* Emit pointer values */
                    while (curchar != ')') {
                        skip();
                        if (curchar == ')') break;

                        if (curchar == '$') {
                            /* Symbol reference - skip $ and read name */
                            char sym[256];
                            int i = 0;
                            nextchar();  /* Skip $ */
                            while (curchar != ' ' && curchar != ')' && curchar != '\n' && i < 255) {
                                sym[i++] = curchar;
                                nextchar();
                            }
                            sym[i] = '\0';
                            fdprintf(outFd, "\t.dw %s\n", sym);
                        } else if (curchar >= '0' && curchar <= '9') {
                            /* Numeric value (e.g., 0 for NULL) */
                            int val = readNumber();
                            fdprintf(outFd, "\t.dw %d\n", val);
                        }
                        skip();
                    }
                } else {
                    /* Already defined, skip the data */
                    while (curchar != ')') {
                        nextchar();
                    }
                }
                /* After loop, curchar is ')' that closes value list */
                expect(')');  /* Close ([:p values...) */
                skip();
                expect(')');  /* Close global declaration */
                return;
            } else if (array_type == 's') {
                /* Struct array - emit as .dw directives with padding */
                isDefined = isDefSym(label_buf);
                if (!isDefined) {
                    addDefSym(label_buf);
                    switchToSeg(SEG_DATA);
                    fdprintf(outFd, "%s:\n", label_buf);

                    /* Emit struct values - each element gets 2 bytes + 3 bytes padding */
                    while (curchar != ')') {
                        skip();
                        if (curchar == ')') break;

                        if (curchar == '$') {
                            /* Symbol reference - skip $ and read name */
                            char sym[256];
                            int i = 0;
                            nextchar();  /* Skip $ */
                            while (curchar != ' ' && curchar != ')' && curchar != '\n' && i < 255) {
                                sym[i++] = curchar;
                                nextchar();
                            }
                            sym[i] = '\0';
                            fdprintf(outFd, "\t.dw %s\n", sym);
                            fdprintf(outFd, "\t.db 0, 0, 0\n");  /* Pad to 5 bytes */
                        } else if (curchar >= '0' && curchar <= '9') {
                            /* Numeric value */
                            int val = readNumber();
                            fdprintf(outFd, "\t.dw %d\n", val);
                            fdprintf(outFd, "\t.db 0, 0, 0\n");  /* Pad to 5 bytes */
                        }
                        skip();
                    }
                } else {
                    /* Already defined, skip the data */
                    while (curchar != ')') {
                        nextchar();
                    }
                }
                /* After loop, curchar is ')' that closes value list */
                expect(')');  /* Close ([:s values...) */
                skip();
                expect(')');  /* Close global declaration */
                return;
            } else {
                /* Other array types - skip the initializer */
                int depth = 1;
                while (depth > 0) {
                    if (curchar == '(') depth++;
                    else if (curchar == ')') depth--;
                    else if (curchar == EOF) break;
                    nextchar();
                }
                skip();
                expect(')');  /* Close outer paren */
                expect(')');  /* Close global declaration */
                return;
            }
            } else {
                /* Non-array initializer wrapped in parens - skip it */
                int depth = 1;
                while (depth > 0) {
                    if (curchar == '(') depth++;
                    else if (curchar == ')') depth--;
                    else if (curchar == EOF) break;
                    nextchar();
                }
            }
        } else {
            /* Non-array initializer - skip to closing paren */
            while (curchar != ')' && curchar != EOF) {
                nextchar();
            }
        }
    }


    /*
     * Skip tentative array definitions (array with size -1).
     * These are forward declarations; the real definition comes later.
     */
    if (!has_init && strstr(type, ":array:-1:")) {
        expect(')');
        return;
    }

    /* For non-array initializers, emit the data */
    if (is_array_init) {
        /* Array initializer was already emitted above */
    } else if (!has_init) {
        /* Uninitialized data */
        isDefined = isDefSym(label_buf);
        if (!isDefined) {
            int size = 2;  /* Default word size */
            char *arr = strstr(type, ":array:");
            if (arr) {
                /* Parse :array:SIZE:ELEMSIZE */
                int arrlen, elemsize;
                if (sscanf(arr, ":array:%d:%d", &arrlen, &elemsize) == 2) {
                    size = arrlen * elemsize;
                }
            }
            addDefSym(label_buf);
            switchToSeg(SEG_BSS);
            fdprintf(outFd, "%s:\n", label_buf);
            fdprintf(outFd, "\t.ds %d\n", size);
        }
    } else {
        /* Scalar initializer (not yet implemented) */
        isDefined = isDefSym(label_buf);
        if (!isDefined) {
            addDefSym(label_buf);
            switchToSeg(SEG_DATA);
            fdprintf(outFd, "%s:\n", label_buf);
            fdprintf(outFd, "\t.dw 0\n");  /* TODO: Emit actual scalar value */
        }
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
 * Parse statement body after '(' and op have been consumed
 */
static struct stmt *
doStmtBody(char op)
{
    struct stmt *s;

    switch (op) {
    case 'B':  s = doBlock();     break;
    case 'I':  s = doIf();        break;
    case 'W':  s = doWhile();     break;
    case 'D':  s = doDo();        break;
    case 'F':  s = doFor();       break;
    case 'R':  s = doReturn();    break;
    case 'E':  s = doExprStmt();  break;
    case ';':  s = doEmptyStmt(); break;
    case 'A':  s = doAsm();       break;
    case 'L':  s = doLabel();     break;
    case 'G':  s = doGoto();      break;
    case 'S':  s = doSwitch();    break;
    default:
        fdprintf(2, "parseast: line %d: unknown stmt op '%c'\n", lineNum, op);
        while (curchar && curchar != ')') nextchar();
        if (curchar == ')') nextchar();
        s = NULL;
        break;
    }
    return s;
}

/*
 * Parse a statement (recursive)
 */
static struct stmt *
parseStmt(void)
{
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
    return doStmtBody(op);
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

    if (TRACE(T_AST)) {
        fdprintf(2, "parseToplvl: op='%c'\n", op);
    }

    switch (op) {
    case 'f':  /* Function: (f:rettype name ...) */
        {
            char rettype;
            expect(':');
            rettype = curchar;
            nextchar();
            if (TRACE(T_AST)) {
                fdprintf(2, "parseToplvl: calling doFunction rettype='%c'\n", rettype);
            }
            doFunction(rettype);
            if (TRACE(T_AST)) {
                fdprintf(2, "parseToplvl: doFunction returned, curchar=%d '%c'\n",
                    curchar, curchar > 31 ? curchar : '?');
            }
        }
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

    /* Prime the input */
    nextchar();

    /* Parse all top-level constructs and collect symbols */
    while (curchar) {
        if (TRACE(T_AST)) {
            fdprintf(2, "parseAstFile: line %d, loop top, curchar=%d '%c'\n",
                lineNum, curchar, curchar > 31 ? curchar : '?');
        }
        skip();
        if (TRACE(T_AST)) {
            fdprintf(2, "parseAstFile: line %d, after skip, curchar=%d '%c'\n",
                lineNum, curchar, curchar > 31 ? curchar : '?');
        }
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
