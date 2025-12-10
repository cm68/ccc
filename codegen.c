/*
 * codegen.c - Code generation phase for cc2
 *
 * Walks expression and statement trees, analyzing patterns and scheduling
 * for code emission.
 *
 * Key responsibilities:
 * - assignFrmOff(): Assign stack offsets to local variables and parameters
 * - specialize(): Detect patterns (inc/dec, bit ops) and mark for emit
 * - scheduleCode(): Set location/dest fields for expressions
 *
 * Note: Register allocation is now done in pass1 (outast.c)
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"

/* Global loop depth for tracking whether we're inside a loop */
static char g_loop_depth = 0;

/* Accumulated call argument bytes outside loops (cleaned at function exit) */
char fnCallStk = 0;

/*
 * Register state for scheduler - tracks what each register holds
 * and flag state for conditional optimization
 */
static struct {
    /* Register contents: 0=unknown, else expr node ID */
    unsigned char hl;
    unsigned char de;
    unsigned char a;
    unsigned char bc;       /* Only valid if BC is a regvar */

    /* Temp register demand tracking */
    unsigned char depth;    /* Current temp registers in use */
    unsigned char max;      /* Max demand seen in subtree */

    /* Flag state - critical for conditionals */
    unsigned char zvalid;   /* Z flag: 0=invalid, 1=Z=1 means TRUE, 2=Z=1 means FALSE */
    unsigned char cmpflag;  /* Carry: 'c'=NC is TRUE, 'C'=C is TRUE, 0=invalid */
    unsigned char znode;    /* Node ID whose result set the flags */
} rs;

/* Reset register state at function entry */
static void
rsReset(void)
{
    rs.hl = rs.de = rs.a = rs.bc = 0;
    rs.depth = rs.max = 0;
    rs.zvalid = rs.cmpflag = rs.znode = 0;
}

/* Global tree walk counter for loop detection */
static long g_walk_count = 0;
#define MAX_WALKS 1000000
#define CHECK_WALK() do { \
    if (++g_walk_count > MAX_WALKS) { \
        fdprintf(2, "walk overflow\n"); \
        exit(1); \
    } \
} while(0)

/*
 * Helper: Check if value is a power of 2, return bit number (0-7) or -1
 */
static int
getBitNum(long val)
{
    int bit;
    if (val <= 0 || val > 128) return -1;
    for (bit = 0; bit < 8; bit++) {
        if (val == (1L << bit)) return bit;
    }
    return -1;
}


/*
 * Helper: Look up a variable by symbol name
 * Strips leading $ and A prefixes from symbol name before lookup
 * Returns NULL if not found
 */
struct local_var *
findVar(const char *symbol)
{
    const char *var_name;
    struct local_var *var;
    int count = 0;

    if (!symbol) return NULL;

    /* Strip $ prefix if present */
    var_name = symbol;
    if (var_name[0] == '$') {
        var_name++;
    }

    /* Search locals list */
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "      findVar(%p): looking for '%s' in locals\n", fnName, var_name);
    }
#endif
    for (var = fnLocals; var; var = var->next) {
        count++;
#ifdef DEBUG
        if (TRACE(T_VAR)) {
            fdprintf(2, "      findVar: checking '%s' (count=%d)\n", var->name, count);
        }
        if (count > 1000) {
            fdprintf(2, "findVar: loop detected, count > 1000\n");
            exit(1);
        }
#endif
        if (strcmp(var->name, var_name) == 0) {
#ifdef DEBUG
            if (TRACE(T_VAR)) {
                fdprintf(2, "      findVar: found!\n");
            }
#endif
            return var;
        }
    }
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "      findVar: not found\n");
    }
#endif

    return NULL;
}

/*
 * Helper: Add a variable (param or local) to the function context
 * For params: is_param=1, offset=positive (caller's stack)
 * For locals: is_param=0, offset computed from fnFrmSize
 */
static void
addVar(const char *name, unsigned char size, int is_param, int offset, int is_array)
{
    struct local_var *var = malloc(sizeof(struct local_var));
    if (!var) { fdprintf(2, "oom\n"); exit(1); }
    var->name = strdup(name);
    var->size = size;
    var->offset = offset;  /* Use pre-computed offset from pass1 */
    var->is_param = is_param;
    var->is_array = is_array;
    var->ref_count = 0;
    var->agg_refs = 0;
    var->reg = REG_NO;
    var->next = fnLocals;
    fnLocals = var;
}

#define addParam(n,sz,ofs) addVar(n,sz,1,ofs,0)
#define addLocalVar(n,sz,arr) addVar(n,sz,0,0,arr)

/*
 * Helper: Check if a name is a parameter
 */
static int
isParameter(const char *name)
{
    struct local_var *var;
    for (var = fnLocals; var; var = var->next) {
        if (var->is_param && strcmp(var->name, name) == 0) {
            return 1;
        }
    }
    return 0;
}

/*
 * Walk statement tree and assign stack frame offsets to local variables
 */
static void
walkLocals(struct stmt *s)
{
    if (!s) return;

    /* If this is a declaration, add it to locals list (unless it's a param) */
    if (s->type == 'd' && s->symbol) {
        /* Skip parameter declarations - they already have offsets */
        if (!isParameter(s->symbol)) {
            unsigned char size = getSizeFTStr(s->type_str);
            struct local_var *var;
            /* Use pre-computed offset from pass1 */
            addVar(s->symbol, size, 0, s->frm_off, 0);
            /* Apply register allocation from AST (stored in s->label) */
            if (s->label) {
                var = findVar(s->symbol);
                if (var) var->reg = s->label;
            }
        }
    }

    /* Recursively walk child statements */
    if (s->then_branch) walkLocals(s->then_branch);
    if (s->else_branch) walkLocals(s->else_branch);
    if (s->next) walkLocals(s->next);
}

/*
 * Set operand pattern flags (opflags)
 * These flags allow codegen to quickly check for common patterns
 */

/* Helper: cache var lookup and set basic opflags */
static void
cacheVar(struct expr *e, const char *sym)
{
    struct local_var *var;
    if (sym && sym[0] == '$') sym++;
    var = findVar(sym);
    e->cached_var = var;
    if (var) {
        if (var->reg != REG_NO) {
            e->opflags |= OP_REGVAR;
            if (var->reg == REG_IX) e->opflags |= OP_IXMEM;
        } else {
            e->opflags |= OP_IYMEM;
        }
    } else {
        e->opflags |= OP_GLOBAL;
    }
}

/* Helper: set opflags for left operand patterns and cache var lookup */
static void
setLeftFlags(struct expr *e)
{
    struct expr *left = e->left;
    struct expr *ll;
    unsigned char lop;

    if (!left) return;
    lop = left->op;
    ll = left->left;

    /* Check for simple variable deref: (M $var) */
    if (lop == 'M' && ll && ll->op == '$' && ll->symbol) {
        e->opflags |= OP_SIMPLEVAR;
        cacheVar(e, ll->symbol);
    }
    /* Check for bare variable: ($var) - used by OREQ/ANDEQ */
    else if (lop == '$' && left->symbol) {
        e->opflags |= OP_SIMPLEVAR;
        cacheVar(e, left->symbol);
    }
    /* Check for struct member address: (+ (M:p $var) ofs) - used by OREQ/ANDEQ */
    else if (lop == '+' && ll && ll->op == 'M' &&
             ll->left && ll->left->op == '$' &&
             left->right && left->right->op == 'C') {
        cacheVar(e, ll->left->symbol);
        if (e->cached_var && e->cached_var->reg == REG_IX)
            e->opflags |= OP_IXMEM;
        e->value = left->right->value;
    }
    /* Check for IX-indexed struct member: (M (+ (M:p $var) ofs)) */
    else if (lop == 'M' && (left->flags & E_IXDEREF)) {
        /* Already flagged during analyzeExpr - check if var is in IX */
        if (ll && ll->left && ll->left->left &&
            ll->left->left->op == '$') {
            cacheVar(e, ll->left->left->symbol);
            if (e->cached_var && e->cached_var->reg == REG_IX)
                e->opflags |= OP_IXMEM;
        }
    }
    /* Check for indirect through pointer: (M (M $ptr)) */
    else if (lop == 'M' && ll && ll->op == 'M' &&
             ll->left && ll->left->op == '$') {
        e->opflags |= OP_INDIR;
        cacheVar(e, ll->left->symbol);
    }
}

/* Set opflags for an expression and its children */
static void
setExprFlags(struct expr *e)
{
    struct expr *left, *right, *ll;

    if (!e) return;
    CHECK_WALK();

    left = e->left;
    right = e->right;

    /* Clear opflags first */
    e->opflags = 0;

    /* Check right operand for constant */
    if (right && right->op == 'C') {
        e->opflags |= OP_CONST;
    }

    /* Set left operand pattern flags */
    setLeftFlags(e);

    /* For DEREF nodes, check if this is a simple var or IX-indexed */
    if (e->op == 'M') {
        if (left && left->op == '$' && left->symbol) {
            const char *sym = left->symbol;
            struct local_var *var;
            if (sym[0] == '$') sym++;
            var = findVar(sym);
            if (var) {
                if (var->reg == REG_BC) {
                    /* Pointer in BC - use ld a,(bc) for bytes */
                    if (e->size == 1) {
                        e->opflags |= OP_BCINDIR;
                    } else {
                        e->opflags |= OP_REGVAR;  /* word deref needs staging */
                    }
                } else if (var->reg == REG_IX) {
                    /* Pointer in IX - use ld a,(ix+0) */
                    e->opflags |= OP_IXMEM;
                    e->offset = 0;
                } else if (var->reg != REG_NO) {
                    e->opflags |= OP_REGVAR;
                } else {
                    e->opflags |= OP_IYMEM;
                }
            } else {
                e->opflags |= OP_GLOBAL;
            }
        }
        /* Check for local member access: (M (+ $var C)) - struct/union member */
        else if (left && left->op == '+') {
            ll = left->left;
            if (ll && ll->op == '$' && ll->symbol &&
                left->right && left->right->op == 'C') {
                const char *sym = ll->symbol;
                struct local_var *var;
                if (sym[0] == '$') sym++;
                var = findVar(sym);
                if (var && var->reg == REG_NO) {
                    /* IY-indexed with constant offset - store combined offset */
                    e->opflags |= OP_IYMEM;
                    e->offset = var->offset + left->right->value;
                    e->cached_var = var;
                }
            }
        }
    }

    /* Recurse on children */
    setExprFlags(left);
    setExprFlags(right);
}

/* Set opflags for all expressions in statement tree */
static void
setStmtFlags(struct stmt *s)
{
    if (!s) return;

    /* For expression statements, mark the expr as unused (result discarded) */
    if (s->type == 'E' && s->expr) s->expr->flags |= E_UNUSED;

    if (s->expr) setExprFlags(s->expr);
    if (s->expr2) setExprFlags(s->expr2);
    if (s->expr3) setExprFlags(s->expr3);

    if (s->then_branch) setStmtFlags(s->then_branch);
    if (s->else_branch) setStmtFlags(s->else_branch);
    if (s->next) setStmtFlags(s->next);
}

/* Entry point: set opflags for all expressions in function */
void
setOpFlags()
{
    if (!fnBody) return;
    setStmtFlags(fnBody);
}

/*
 * Phase 3: Specialize - detect patterns and replace subtrees with asm
 *
 * This pass walks the tree BEFORE emit, detecting patterns like:
 * - INC/DEC of variables (simple and complex)
 * - OREQ/ANDEQ single-bit operations (set/res instructions)
 * - Shift operations with constant counts
 *
 * When a pattern matches, children are freed and opflags/value are set
 * so emit can generate the optimized code.
 */

/* Forward declarations */
static void specExpr(struct expr *e);
static void specStmt(struct stmt *s);

/* Helper: free both children and clear pointers after specialization */
static void
freeKids(struct expr *e)
{
    freeExpr(e->left);
    freeExpr(e->right);
    e->left = e->right = NULL;
}

/*
 * Specialize INC/DEC operations
 * Patterns:
 *   - Simple var: (INC/DEC $var) -> placeholder for emit phase
 *   - DEREF/ADD: (INC/DEC (M addr)) or (INC/DEC (+ base ofs)) -> load, inc/dec, store
 */
static int
specIncDec(struct expr *e)
{
    (void)e->value;  /* amount - used by emit */

    /* Pattern 1: Simple variable - defer to emit phase */
    if (e->left && e->left->op == '$' && e->left->symbol) {
        struct local_var *var = findVar(e->left->symbol);
        if (var) e->size = var->size;
        /* Save symbol for emit, then free children */
        e->symbol = strdup(e->left->symbol);
        freeKids(e);
        return 1;
    }

    /* Pattern 2/3: DEREF or ADD - defer to emit phase, keep address tree */
    if (e->left && (e->left->op == 'M' || e->left->op == '+')) {
        struct expr *addr = (e->left->op == 'M') ? e->left->left : e->left;
        specExpr(addr);
        /* Flatten: move address directly under inc/dec node */
        if (e->left->op == 'M') {
            e->left->left = NULL;
            freeExpr(e->left);
            e->left = addr;
        }
        /* e->left now has address expr, emit will handle */
        return 1;
    }

    return 0;
}

/*
 * Specialize OREQ/ANDEQ for single-bit operations
 * Uses opflags set by setLeftFlags() to avoid repeated findVar calls
 * Patterns:
 *   - var |= (1<<n) -> set n, reg/mem
 *   - var &= ~(1<<n) -> res n, reg/mem
 *   - struct->field |= (1<<n) -> set n, (ix+d) or set n, (hl)
 */
static int
specBitOp(struct expr *e)
{
    int bitnum;
    int is_or = (e->op == '1');  /* OREQ vs ANDEQ */

    /* Must be byte operation with constant RHS (OP_CONST cached by setOpFlags) */
    if (e->size != 1 || !(e->opflags & OP_CONST))
        return 0;

    /* Get bit number */
    if (is_or) {
        bitnum = getBitNum(e->right->value);
    } else {
        long mask = e->right->value & 0xFF;
        bitnum = getBitNum(~mask & 0xFF);
    }
    if (bitnum < 0) return 0;

    /* Pattern 1: Simple variable - use cached opflags and cached_var */
    if ((e->opflags & OP_SIMPLEVAR) && e->left && e->left->op == '$') {
        struct local_var *var = e->cached_var;  /* Use cached lookup */
        if (e->opflags & OP_REGVAR) {
            /* Register-allocated */
            if (var) {
                const char *rn = byteRegName(var->reg);
                if (var->reg == REG_BC) rn = "c";
                if (rn) {
                    e->value = bitnum;  /* Store bit number; emit will generate */
                    freeKids(e);
                    return 1;
                }
            }
        } else if (e->opflags & OP_IYMEM) {
            /* Stack variable */
            if (var) {
                e->value = bitnum;  /* Store bit number; emit will generate */
                freeKids(e);
                return 1;
            }
        }
    }

    /* Pattern 2: Struct member via pointer - use cached OP_IXMEM and offset */
    if (e->left && e->left->op == '+') {
        if (e->opflags & OP_IXMEM) {
            /* IX-indexed - offset already in e->value from setLeftFlags */
            /* Store bit number in high byte, keep offset in low */
            e->value = (bitnum << 8) | (e->value & 0xff);
            freeKids(e);
            return 1;
        } else {
            /* Non-IX: compute address, then use (hl)
             * Keep e->left so emit will process it for address generation */
            e->value = bitnum;
            freeExpr(e->right);
            e->right = NULL;
            return 1;
        }
    }

    return 0;
}

/*
 * Specialize LSHIFTEQ/RSHIFTEQ for byte register variables
 * Pattern: var <<= N or var >>= N where var is in register B or C
 * Generates: N x (sla r) or N x (srl r)
 */
static int
specShiftOp(struct expr *e)
{
    int count;
    const char *rn;
    struct local_var *var;

    /* Must be byte operation with constant RHS */
    if (e->size != 1 || !e->right || e->right->op != 'C')
        return 0;

    count = e->right->value & 0xff;
    if (count < 1 || count > 7) return 0;  /* Reasonable range */

    /* Check for simple register variable */
    if (!(e->opflags & OP_SIMPLEVAR) || !e->left || e->left->op != '$')
        return 0;

    if (!(e->opflags & OP_REGVAR))
        return 0;

    var = e->cached_var;
    if (!var) return 0;

    rn = byteRegName(var->reg);
    if (var->reg == REG_BC) rn = "c";
    if (!rn) return 0;

    /* Store shift count in e->value; emit will generate code */
    e->value = count;
    freeKids(e);
    return 1;
}

/*
 * Specialize PLUSEQ/SUBEQ for byte register variables
 * Pattern: var += expr or var -= expr where var is in register B or C
 * Generates: add a,r; ld r,a  or  sub r; neg; ld r,a (for subtraction)
 * For SUBEQ: result is var - expr, so we need: ld a,r; sub (expr); ld r,a
 */
static int
specAddSubOp(struct expr *e)
{
    const char *rn;
    struct local_var *var;

    /* Must be byte operation */
    if (e->size != 1)
        return 0;

    /* Check for simple register variable */
    if (!(e->opflags & OP_SIMPLEVAR) || !e->left || e->left->op != '$')
        return 0;

    if (!(e->opflags & OP_REGVAR))
        return 0;

    var = e->cached_var;
    if (!var) return 0;

    rn = byteRegName(var->reg);
    if (var->reg == REG_BC) rn = "c";
    if (!rn) return 0;

    /* Keep e->right for RHS evaluation, free e->left (the variable reference)
     * Emit will generate the add/sub code based on e->op and opflags */
    freeExpr(e->left);
    e->left = NULL;
    return 1;
}

/*
 * Specialize OREQ/ANDEQ/XOREQ for byte register variables (non-bit patterns)
 * Pattern: regvar |= expr, regvar &= expr, regvar ^= expr
 * Generates: or a,r / and a,r / xor a,r; ld r,a
 */
static int
specLogicOp(struct expr *e)
{
    struct local_var *var;

    /* Must be byte operation */
    if (e->size != 1)
        return 0;

    /* Check for simple register variable */
    if (!(e->opflags & OP_SIMPLEVAR) || !e->left || e->left->op != '$')
        return 0;

    if (!(e->opflags & OP_REGVAR))
        return 0;

    var = e->cached_var;
    if (!var) return 0;

    if (!byteRegName(var->reg) && var->reg != REG_BC)
        return 0;

    /* Keep e->right for RHS evaluation, free e->left */
    freeExpr(e->left);
    e->left = NULL;
    return 1;
}

/*
 * Specialize an expression - check for patterns and replace with asm
 * Must be called BEFORE generateExpr
 */
static void
specExpr(struct expr *e)
{
    if (!e) return;
    CHECK_WALK();

    /* Try INC/DEC specialization */
    if (e->op == AST_PREINC || e->op == AST_POSTINC || e->op == AST_PREDEC || e->op == AST_POSTDEC) {
        if (specIncDec(e)) return;
    }

    /* Try OREQ/ANDEQ bit operations */
    if (e->op == '1' || e->op == AST_ANDEQ) {
        if (specBitOp(e)) return;
    }

    /* Try OREQ/ANDEQ/XOREQ for byte register variables (non-bit patterns) */
    if (e->op == '1' || e->op == AST_ANDEQ || e->op == 'X') {
        if (specLogicOp(e)) return;
    }

    /* Try LSHIFTEQ/RSHIFTEQ for register variables */
    if (e->op == '0' || e->op == '6') {
        if (specShiftOp(e)) return;
    }

    /* Try PLUSEQ/SUBEQ for byte register variables */
    if (e->op == 'P' || e->op == AST_SUBEQ) {
        if (specAddSubOp(e)) return;
    }

    /* Propagate ASSIGN size to constant right operand */
    if (e->op == '=' && e->right && e->right->op == 'C') {
        e->right->size = e->size;
    }

    /* Recurse on children */
    specExpr(e->left);
    specExpr(e->right);
}

/*
 * Specialize all expressions in a statement
 */
static void
specStmt(struct stmt *s)
{
    if (!s) return;

    specExpr(s->expr);
    specExpr(s->expr2);
    specExpr(s->expr3);

    specStmt(s->then_branch);
    specStmt(s->else_branch);
    specStmt(s->next);
}

/*
 * Entry point: specialize all expressions in function
 */
void
specialize()
{
    if (!fnBody) return;
    specStmt(fnBody);
}

/*
 * Validate frame size (computed by pass1, stored in fnFrmSize)
 */
void
optFrmLayout()
{
    if (fnFrmSize > 127) {
        fdprintf(2, "%s: frame > 127\n", fnName);
        exit(1);
    }
}

/*
 * Add local variables to fnLocals list.
 * Parameters are already added by parseast.c; this adds locals from decl stmts.
 */
void
assignFrmOff()
{
    if (!fnBody) return;
    walkLocals(fnBody);
}

/* Forward declaration */
static char *buildStkCln(int bytes);

/*
 * Code generation phase (Phase 2)
 * Walk expression tree and generate assembly code blocks
 *
 * Accumulator Management:
 *   - All expressions evaluate to PRIMARY accumulator (HL for word, A for byte)
 *   - Binary operators: left PRIMARY, move to SECONDARY, right PRIMARY, operate
 *   - M (DEREF) operations generate actual load instructions
 */
static int expr_count = 0;
static void
generateExpr(struct expr *e)
{
    struct expr *arg;
    int arg_count;
    int i;

    if (!e) return;
    CHECK_WALK();
    expr_count++;
    if (expr_count > 100000) {
        fdprintf(2, "expr overflow\n");
        exit(1);
    }

    /* Special handling for CALL - emitCall handles it directly from AST */
    if (e->op == '@') {
        arg_count = e->value;

        /* Generate code for each argument */
        arg = e->right;
        for (i = 0; i < arg_count && arg; i++) {
            if (arg->left) {
                generateExpr(arg->left);
            }
            arg = arg->right;
        }

        /* Stack cleanup: in loops emit after call, otherwise defer to exit */
        if (arg_count > 0) {
            if (g_loop_depth > 0) {
                e->cleanup_block = buildStkCln(arg_count * 2);
            } else {
                fnCallStk += arg_count * 2;
            }
        }

        return;  /* emitCall will handle the rest */
    }

    /* Special handling for ternary operator (? :) */
    if (e->op == '?') {
        /* Ternary: condition ? true_expr : false_expr */
        /* Tree structure: '?' node has condition in left, ':' node in right */
        /* ':' node has true_expr in left, false_expr in right */

        /* Recursively generate code for all three sub-expressions */
        generateExpr(e->left);  /* condition */
        if (e->right) {
            generateExpr(e->right->left);   /* true expr */
            generateExpr(e->right->right);  /* false expr */
        }

        /* Allocate labels for branches */
        e->label = newLabel();  /* false_label */
        e->flags |= E_JUMP;     /* Mark: jump to false_label if condition is zero */
        if (e->right) {
            e->right->label = newLabel();  /* end_label */
            e->right->flags |= E_JUMP;     /* Mark: jump to end_label after true branch */
        }

        return;  /* Early return - custom traversal done */
    }

    /* For assignments, propagate size to constant RHS before code gen */
    if (e->op == '=' && e->right && e->right->op == 'C') {
        e->right->size = e->size;
    }

    /* Recursively generate code for children (postorder traversal) */
    generateExpr(e->left);
    generateExpr(e->right);

    /* NOTE: Variable usage tracking is now done in analyzeExpr() */

    /* Generate assembly code for this node based on operator */
    switch (e->op) {
    case 'C':  /* CONST - emit handles based on e->value and e->size */
        break;

    case 'M':  /* DEREF - emit handles based on opflags and scheduling fields */
    case '=':  /* ASSIGN - emit handles based on opflags and scheduling fields */
        break;

    case '+':  /* ADD - emit handles based on opflags and operand sizes */
        break;

    case '-':  /* SUB - emit handles */
    case '*':  /* MUL - emit handles */
    case '/':  /* DIV - emit handles */
    case '%':  /* MOD - emit handles */
    case '&':  /* AND - emit handles, including bit test optimizations */
    case '|':  /* OR - emit handles */
    case '^':  /* XOR - emit handles */
    case 'y':  /* LSHIFT - emit handles */
    case 'w':  /* RSHIFT - emit handles */
    case '>':  /* GT - emit handles */
    case '<':  /* LT - emit handles */
    case 'g':  /* GE - emit handles */
    case 'L':  /* LE - emit handles */
    case 'Q':  /* EQ - emit handles */
    case 'n':  /* NEQ - emit handles */
        break;

    case AST_SEXT:  /* SEXT - sign extend, emit handles */
    case 'W':  /* WIDEN - zero extend, emit handles */
    case '1':  /* OREQ (|=) - emit handles */
    case AST_ANDEQ:  /* ANDEQ (&=) - emit handles */
        break;

    case '$':  /* SYM - symbol reference (address) */
        /* Emit handles loading address into HL */
        break;

    default:
        /* Emit handles all other operators */
        break;
    }
}

/*
 * Build pending stack cleanup asm code (for CALL arguments)
 * Returns a malloc'd string with the cleanup code
 */
static char *
buildStkCln(int bytes)
{
    char *cleanup;
    int i;

    if (bytes <= 6) {
        /* inc sp is 1 byte each, cheaper than ld/add/ld (6 bytes) */
        cleanup = malloc(bytes * 10);  /* "\tinc sp\n" = ~8 chars each */
        cleanup[0] = '\0';
        for (i = 0; i < bytes; i++) {
            strcat(cleanup, "\tinc sp\n");
        }
    } else {
        /* For larger adjustments, use ld/add/ld */
        cleanup = malloc(128);
        snprintf(cleanup, 128,
                "\tld hl, %d\n\tadd hl, sp\n\tld sp, hl  ; cleanup %d bytes\n",
                bytes, bytes);
    }
    return cleanup;
}

/*
 * Walk statement tree and generate assembly code blocks
 */
static void
generateStmt(struct stmt *s)
{
    if (!s) return;

    /* Track loop depth globally for call cleanup optimization */
    if (s->type == 'L' && s->symbol) {
        if (strstr(s->symbol, "_top")) {
            g_loop_depth++;
        } else if (strstr(s->symbol, "_break")) {
            g_loop_depth--;
        }
    }

    /* Recursively generate code for expressions */
    /* For expression statements, mark the expr as unused (result discarded) */
    if (s->type == 'E' && s->expr) s->expr->flags |= E_UNUSED;
    generateExpr(s->expr);
    generateExpr(s->expr2);
    generateExpr(s->expr3);

    /* Recursively generate code for child statements */
    generateStmt(s->then_branch);
    generateStmt(s->else_branch);
    generateStmt(s->next);

    /* Create jump nodes for control flow statements
     * These will be resolved and optimized during emission
     *
     * Note: For IF statements, the conditional jumps are created during
     * emission because we need to evaluate the condition first. But we
     * create the unconditional jump over the else branch here.
     */
    switch (s->type) {
    case 'I':  /* IF statement */
        if (s->label2 > 0) {
            /* Has else branch - create unconditional jump over else at end of then */
            /* This jump will be emitted after the then branch */
            /* We'll attach it to the statement for now, and emit.c will use it */
        }
        /* Conditional jumps are created in emit.c based on condition evaluation */
        break;

    case 'R':  /* RETURN statement - handled directly in emit */
        break;

    default:
        /* Other statement types don't generate jumps */
        break;
    }
}

/*
 * Generate assembly code for entire function
 */
void
generateCode()
{
    if (!fnBody) return;

    /* Reset global state at start of each function */
    g_loop_depth = 0;
    fnCallStk = 0;

    generateStmt(fnBody);
}

/*
 * ============================================================================
 * SCHEDULING PASS - Set location/dest fields on expression nodes
 *
 * Walks expressions and tags them with:
 *   - loc: where the value currently lives
 *   - reg: which register (if loc=REG or INDIR)
 *   - dest: where the value needs to go
 *   - offset: stack/IX offset (if loc=STACK or IX)
 * ============================================================================
 */

/*
 * Schedule expression - propagate dest hints down the tree
 * loc/offset/reg/cond are now set during parsing by schedNode()
 * This just handles the top-down dest propagation
 */
static void
schedExpr(struct expr *e, int dest)
{
    struct local_var *var;

    if (!e) return;

    /* Set destination if provided */
    if (dest != R_NONE) {
        e->dest = dest;
    } else {
        /* Default destination based on size */
        e->dest = (e->size == 1) ? R_A : R_HL;
    }

    switch (e->op) {
    case 'C':  /* Constant - no children */
    case '$':  /* Symbol - no children */
    case 'S':  /* Stack offset - no children */
        break;

    case 'M':  /* DEREF */
        /* Only recurse if not a simple variable reference or IX-indexed */
        if (e->loc == LOC_INDIR)
            schedExpr(e->left, R_HL);
        break;

    case '+':
    case '-':
    case '*':
    case '/':
    case '%':
    case '&':
    case '|':
    case '^':
    case 'y':  /* LSHIFT */
    case 'w':  /* RSHIFT */
        /* Binary ops: left -> DE, right -> HL (for word ops) */
        if (e->size == 1) {
            schedExpr(e->left, R_A);
            schedExpr(e->right, R_A);
        } else {
            schedExpr(e->left, R_DE);
            schedExpr(e->right, R_HL);
        }
        break;

    case '<':
    case '>':
    case 'g':  /* >= */
    case 'L':  /* <= */
    case 'Q':  /* == */
    case 'n':  /* != */
        /* Comparison with 0: test in place */
        if (e->right && e->right->op == 'C' && e->right->value == 0 &&
            e->left && e->left->size == 2 &&
            ((e->op == 'Q' || e->op == 'n') || !(e->flags & E_UNSIGNED))) {
            schedExpr(e->left, R_NONE);
            e->right->dest = R_NONE;
            break;
        }
        /* Comparisons: schedule so sbc hl,de gives correct result */
        if (e->size == 1 || (e->left && e->left->size == 1)) {
            schedExpr(e->left, R_A);
            schedExpr(e->right, R_A);
        } else if (e->op == '<' || e->op == 'g') {
            schedExpr(e->left, R_HL);
            schedExpr(e->right, R_DE);
        } else {
            schedExpr(e->left, R_DE);
            schedExpr(e->right, R_HL);
        }
        break;

    case '=':  /* Assignment */
        if (e->size == 1) {
            schedExpr(e->right, R_A);
        } else {
            int rhs_dest = R_HL;
            /* Check if LHS is a register variable */
            if (e->left && e->left->op == '$' && e->left->symbol) {
                var = findVar(e->left->symbol);
                if (var && var->reg == REG_BC) rhs_dest = R_BC;
            }
            schedExpr(e->right, rhs_dest);
        }
        schedExpr(e->left, R_NONE);
        break;

    default:
        /* Recurse with default destinations */
        schedExpr(e->left, R_NONE);
        schedExpr(e->right, R_NONE);
        break;
    }
}

/*
 * Schedule statement tree
 */
static void
schedStmt(struct stmt *s)
{
    if (!s) return;

    /* Schedule expressions in this statement */
    schedExpr(s->expr, R_NONE);
    schedExpr(s->expr2, R_NONE);
    schedExpr(s->expr3, R_NONE);

    /* Recurse into branches and next */
    schedStmt(s->then_branch);
    schedStmt(s->else_branch);
    schedStmt(s->next);
}

/*
 * Entry point: schedule all expressions in function
 */
void
scheduleCode(void)
{
    if (!fnBody) return;
    schedStmt(fnBody);
}

/*
 * ============================================================================
 * NEW SCHEDULER - prescriptive instruction selection
 *
 * This replaces the hint-based scheduling above with explicit instruction
 * selection. The scheduler tracks register contents and flag state, then
 * records exactly which instructions to emit in e->ins[].
 * ============================================================================
 */

/*
 * Add an instruction to a node's instruction sequence
 */
static void
addIns(struct expr *e, unsigned char ins)
{
    if (e->nins < 3) {
        e->ins[e->nins++] = ins;
    }
}

/* Forward declarations for scheduler */
static void sched2Expr(struct expr *e);

/*
 * Schedule expression - pass 1: calculate temp register demand
 * Returns number of temp registers needed to evaluate this subtree
 */
static unsigned char
sched2Demand(struct expr *e)
{
    struct expr *left;
    unsigned char op, left_demand, right_demand, my_demand;

    if (!e) return 0;
    op = e->op;
    left = e->left;

    /* Leaves need 1 register for their result */
    if (op == 'C' || op == '$') {
        return 1;
    }

    /* DEREF of simple var needs 1 register */
    if (op == 'M' && left && left->op == '$') {
        return 1;
    }

    /* Binary ops: evaluate children, need max of (left, right+1) */
    left_demand = sched2Demand(left);
    right_demand = sched2Demand(e->right);

    /* When evaluating right, left result must be held somewhere */
    if (right_demand > 0) {
        my_demand = (left_demand > right_demand + 1) ? left_demand : right_demand + 1;
    } else {
        my_demand = left_demand;
    }

    return my_demand;
}

/*
 * Schedule loads for constant nodes
 * Sets e->dest to natural register (R_A for byte, R_HL for word)
 */
static void
sched2Const(struct expr *e)
{
    e->nins = 0;
    /* Use dest if set by parent (byte assign sets R_A), else use size */
    if (e->dest == R_A || e->size == 1) {
        e->dest = R_A;
        addIns(e, EO_A_CONST);
    } else {
        if (e->dest == R_NONE) e->dest = R_HL;
        addIns(e, EO_HL_CONST);
    }
}

/*
 * Schedule loads for DEREF of simple variable
 * Pattern: (M $var) - opflags has OP_REGVAR, OP_IYMEM, or OP_GLOBAL
 */
static void
sched2Deref(struct expr *e)
{
    struct local_var *var;
    const char *sym;
    unsigned char opf = e->opflags;
    unsigned char size = e->size;

    e->nins = 0;

    /* BC indirect (byte through pointer in BC) - check first! */
    if (opf & OP_BCINDIR) {
        e->dest = R_A;
        addIns(e, EO_A_BC_IND);     /* ld a,(bc) */
    }
    /* IX indirect (through pointer in IX) - check before generic REGVAR */
    else if ((opf & OP_IXMEM) && !(opf & OP_IYMEM)) {
        if (size == 1) {
            e->dest = R_A;
            addIns(e, EO_A_IX);     /* ld a,(ix+ofs) */
        } else if (size == 4) {
            e->dest = R_HL;
            addIns(e, EO_HLHL_IXL); /* ld a,ofs; call getLix */
        } else {
            e->dest = R_HL;
            addIns(e, EO_HL_IXW);   /* ld l,(ix+ofs); ld h,(ix+ofs+1) */
        }
    }
    /* BC register variable (not a pointer deref) */
    else if (opf & OP_REGVAR) {
        if (size == 1) {
            e->dest = R_A;
            /* Check which register the byte regvar is in */
            if (e->cached_var && e->cached_var->reg == REG_B)
                addIns(e, EO_A_B);  /* ld a,b */
            else
                addIns(e, EO_A_C);  /* ld a,c */
        } else {
            e->dest = R_HL;
            addIns(e, EO_HL_BC);    /* ld h,b; ld l,c */
        }
    }
    /* IY-indexed stack variable */
    else if (opf & OP_IYMEM) {
        /* Get offset from cached_var or look it up */
        if (e->cached_var) {
            e->offset = e->cached_var->offset;
        } else if (e->left && e->left->symbol) {
            sym = e->left->symbol;
            if (sym[0] == '$') sym++;
            var = findVar(sym);
            if (var) e->offset = var->offset;
        }
        if (size == 1) {
            e->dest = R_A;
            addIns(e, EO_A_IY);     /* ld a,(iy+ofs) */
        } else if (size == 4) {
            e->dest = R_HL;
            addIns(e, EO_HLHL_IYL); /* ld a,ofs; call getlong */
        } else {
            e->dest = R_HL;
            addIns(e, EO_HL_IYW);   /* ld l,(iy+ofs); ld h,(iy+ofs+1) */
        }
    }
    /* Global variable */
    else if (opf & OP_GLOBAL) {
        if (size == 1) {
            e->dest = R_A;
            addIns(e, EO_A_MEM);    /* ld a,(symbol) */
        } else {
            e->dest = R_HL;
            addIns(e, EO_HL_MEM);   /* ld hl,(symbol) */
        }
    }
    /* Default - complex DEREF, children handle it */
    else {
        e->dest = (size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
    }
}

/*
 * Schedule loads for symbol address
 * Pattern: $var - load address of variable
 */
static void
sched2Symbol(struct expr *e)
{
    e->nins = 0;
    /* Respect dest if already set by parent (e.g., R_DE for left side of +) */
    if (e->dest == R_DE) {
        addIns(e, EO_DE_CONST);
    } else {
        e->dest = R_HL;
        addIns(e, EO_HL_CONST);
    }
}

/*
 * Schedule binary operations
 * Word ops: left in HL, right in DE, result in HL
 * Byte ops: left in A, right immediate or (hl), result in A
 */
static void
sched2Binary(struct expr *e)
{
    struct expr *left = e->left;
    struct expr *right = e->right;
    unsigned char op = e->op;
    unsigned char size = e->size;

    e->nins = 0;

    /* Set child destinations for word ops: left->DE, right->HL */
    if (size == 2) {
        if (left) left->dest = R_DE;
        if (right) right->dest = R_HL;
    }

    /* Recurse to schedule children */
    sched2Expr(left);
    sched2Expr(right);

    /* Select instruction based on operator */
    switch (op) {
    case '+':
        if (size == 2) {
            e->dest = R_HL;
            addIns(e, EO_ADD_HL_DE);
        } else {
            e->dest = R_A;
            addIns(e, EO_ADD_A_N);
        }
        break;
    case '-':
        if (size == 2) {
            e->dest = R_HL;
            addIns(e, EO_SBC_HL_DE);
        } else {
            e->dest = R_A;
            addIns(e, EO_SUB_A_N);
        }
        break;
    case '&':
        e->dest = R_A;
        addIns(e, EO_AND_A_N);
        break;
    case '|':
        e->dest = R_A;
        addIns(e, EO_OR_A_N);
        break;
    case '^':
        e->dest = R_A;
        addIns(e, EO_XOR_A_N);
        break;
    /* Comparison operators - these set flags, result in A (0 or 1) */
    case '<':
    case '>':
    case 'L':   /* <= */
    case 'g':   /* >= */
    case 'Q':   /* == */
    case 'n':   /* != */
        e->dest = R_A;
        if (size == 1) {
            addIns(e, EO_CP_N);  /* Byte comparison */
        } else {
            addIns(e, EO_SBC_HL_DE);  /* Word comparison via subtract */
        }
        break;
    default:
        e->dest = (size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
        break;
    }
}

/*
 * Schedule type conversion
 * N (NARROW): truncate to smaller type
 * W (WIDEN): zero-extend unsigned
 * X (SEXT): sign-extend signed
 */
static void
sched2Conv(struct expr *e)
{
    struct expr *left = e->left;

    e->nins = 0;

    /* Schedule child first */
    sched2Expr(left);

    switch (e->op) {
    case 'N':   /* NARROW - truncate */
        if (e->size == 1) {
            /* Word to byte - take L into A */
            e->dest = R_A;
            addIns(e, EO_A_L);
        } else {
            e->dest = R_HL;
            addIns(e, EO_NOP);
        }
        break;
    case 'W':   /* WIDEN - zero extend */
        if (left && left->size == 1) {
            /* Byte to word - A to HL with zero high byte */
            e->dest = R_HL;
            addIns(e, EO_WIDEN);
        } else {
            e->dest = R_HL;
            addIns(e, EO_NOP);
        }
        break;
    case 'X':   /* SEXT - sign extend */
        if (left && left->size == 1) {
            /* Byte to word with sign extension */
            e->dest = R_HL;
            addIns(e, EO_SEXT);
        } else {
            e->dest = R_HL;
            addIns(e, EO_NOP);
        }
        break;
    default:
        e->dest = (e->size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
        break;
    }
}

/*
 * Schedule store instruction based on destination
 * Pattern: (= dest value) - store value to dest
 */
static void
sched2Store(struct expr *e)
{
    struct expr *dest = e->left;
    struct expr *src = e->right;
    struct local_var *var;
    const char *sym;

    e->nins = 0;

    /* Optimize: BC regvar -> global uses ED-prefixed ld (addr),bc
     * Pattern: (= $global (M $bcvar)) where bcvar is in BC
     * Saves 3 bytes: ld (sym),bc is 4 bytes vs ld h,b;ld l,c;ld (sym),hl is 7 */
    if (e->size == 2 && dest && src &&
        dest->op == '$' && dest->symbol &&
        src->op == 'M' && (src->opflags & OP_REGVAR) &&
        src->left && src->left->op == '$' && src->left->symbol) {
        /* Check dest is global (not a local) */
        sym = dest->symbol;
        if (sym[0] == '$') sym++;
        if (!findVar(sym)) {
            /* Dest is global - check if src is BC regvar */
            sym = src->left->symbol;
            if (sym[0] == '$') sym++;
            var = findVar(sym);
            if (var && var->reg == REG_BC) {
                /* Don't schedule RHS - value is already in BC */
                src->nins = 0;
                addIns(e, EO_MEM_BC);
                e->dest = R_BC;
                return;
            }
        }
    }

    /* Set dest on RHS to indicate target register before scheduling */
    if (src) {
        src->dest = (e->size == 1) ? R_A : R_HL;
        sched2Expr(src);
    }

    /* Destination is simple var deref: (M $var) */
    if (dest && dest->op == 'M' && dest->left && dest->left->op == '$') {
        /* Check destination type */
        if (dest->opflags & OP_REGVAR) {
            /* Store to BC regvar */
            if (e->size == 1) {
                addIns(e, EO_NOP);  /* TODO: ld c,a */
            } else {
                addIns(e, EO_BC_HL);
            }
        } else if (dest->opflags & OP_IYMEM) {
            /* Get offset from cached_var or look it up */
            if (dest->cached_var) {
                e->offset = dest->cached_var->offset;
            } else if (dest->left && dest->left->symbol) {
                sym = dest->left->symbol;
                if (sym[0] == '$') sym++;
                var = findVar(sym);
                if (var) e->offset = var->offset;
            }
            /* Store to IY-indexed stack var */
            if (e->size == 1) {
                addIns(e, EO_IY_A);
            } else {
                addIns(e, EO_IYW_HL);
            }
        } else if (dest->opflags & OP_GLOBAL) {
            /* Store to global */
            if (e->size == 1) {
                addIns(e, EO_MEM_A);
            } else {
                addIns(e, EO_MEM_HL);
            }
        } else {
            addIns(e, EO_NOP);
        }
    } else {
        /* Complex destination - schedule address, then store */
        sched2Expr(dest);
        addIns(e, EO_NOP);
    }

    /* Assignment result is in the value we stored */
    e->dest = (e->size == 1) ? R_A : R_HL;
}

/*
 * Schedule expression - pass 2: select instructions
 * Tracks register state, selects optimal instructions, records in e->ins[]
 */
static void
sched2Expr(struct expr *e)
{
    struct expr *left;
    unsigned char op, demand;

    if (!e) return;
    op = e->op;
    left = e->left;

    /* Initialize instruction list */
    e->nins = 0;

    /* Calculate temp register demand for this subtree */
    demand = sched2Demand(e);
    (void)demand;  /* TODO: use for spill decisions */

    /* Select instructions based on node type */
    switch (op) {
    case 'C':   /* Constant */
        sched2Const(e);
        break;

    case 'M':   /* DEREF - load from memory */
        if (left && left->op == '$') {
            /* Simple variable deref: (M $var) */
            sched2Expr(left);  /* Schedule address load (usually NOP) */
            sched2Deref(e);
        } else {
            /* Complex deref - recurse to children */
            sched2Expr(left);
            e->dest = (e->size == 1) ? R_A : R_HL;
            addIns(e, EO_NOP);
        }
        break;

    case '$':   /* Symbol address */
        sched2Symbol(e);
        break;

    case '=':   /* Assignment */
        sched2Store(e);
        break;

    case 'N':   /* NARROW */
    case 'W':   /* WIDEN */
    case 'X':   /* SEXT */
        sched2Conv(e);
        break;

    case '@':   /* Function call */
        /* Schedule arguments first */
        sched2Expr(e->right);
        sched2Expr(left);
        e->dest = R_HL;  /* Function returns in HL */
        addIns(e, EO_CALL);
        break;

    case AST_PREINC:    /* ++x */
    case AST_PREDEC:    /* --x */
    case AST_POSTINC:   /* x++ */
    case AST_POSTDEC:   /* x-- */
        /* Handled by specIncDec + emitIncDec - don't schedule */
        break;

    case '+':
    case '-':
    case '&':
    case '|':
    case '^':
    case '<':
    case '>':
    case 'L':   /* <= */
    case 'g':   /* >= */
    case 'Q':   /* == */
    case 'n':   /* != */
    case '*':   /* Multiply */
    case '/':   /* Divide */
    case '%':   /* Modulo */
    case 'y':   /* Left shift << */
    case 'z':   /* Right shift >> */
    case 'j':   /* Logical and && */
    case 'h':   /* Logical or || */
        /* Binary operations */
        sched2Binary(e);
        break;

    case '~':   /* Bitwise not */
    case '!':   /* Logical not */
        sched2Expr(left);
        e->dest = (e->size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
        break;

    case 'A':   /* Address of */
        sched2Expr(left);
        e->dest = R_HL;  /* Address always in HL */
        addIns(e, EO_NOP);
        break;

    case ',':   /* Comma operator - evaluate both, return right */
        sched2Expr(left);
        sched2Expr(e->right);
        e->dest = (e->size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
        break;

    case '?':   /* Ternary operator */
        sched2Expr(left);
        sched2Expr(e->right);
        e->dest = (e->size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
        break;

    /* Compound assignment operators - specialized by specExpr, emit handles */
    case '0':   /* <<= */
    case '6':   /* >>= */
    case '1':   /* |= */
    case AST_ANDEQ:  /* &= */
    case 'P':   /* += */
    case AST_SUBEQ:  /* -= */
        /* These may have remaining children if specialize didn't fully handle */
        sched2Expr(left);
        sched2Expr(e->right);
        e->dest = (e->size == 1) ? R_A : R_HL;
        /* Don't add NOP - emit handlers check flags and generate code */
        break;

    default:
        /* Unknown - just recurse and add NOP */
        sched2Expr(left);
        sched2Expr(e->right);
        e->dest = (e->size == 1) ? R_A : R_HL;
        addIns(e, EO_NOP);
        break;
    }
}

/*
 * Schedule statement tree with new scheduler
 */
static void
sched2Stmt(struct stmt *s)
{
    if (!s) return;

    /* Schedule expressions in this statement */
    if (s->expr) {
        /* For return statements, set dest based on return type */
        if (s->type == 'R' && fnRettype && getSizeFTStr(fnRettype[0]) == 1) {
            s->expr->dest = R_A;
            if (s->expr->op == 'C')
                s->expr->size = 1;  /* Force byte constant */
        }
        sched2Expr(s->expr);
    }
    sched2Expr(s->expr2);
    sched2Expr(s->expr3);

    /* Recurse into branches */
    sched2Stmt(s->then_branch);
    sched2Stmt(s->else_branch);

    /* Continue to next statement */
    sched2Stmt(s->next);
}

/*
 * Entry point: new scheduler with instruction selection
 */
void
sched2Code(void)
{
    if (!fnBody) return;
    rsReset();
    sched2Stmt(fnBody);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
