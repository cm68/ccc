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
    var->offset = is_param ? offset : -(fnFrmSize + size);
    var->is_param = is_param;
    var->is_array = is_array;
    var->ref_count = 0;
    var->agg_refs = 0;
    var->reg = REG_NO;
    var->next = fnLocals;
    fnLocals = var;
    if (!is_param) fnFrmSize += size;
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
            /* Local arrays are typed as pointers (p) in the AST */
            int is_array = 0;
            struct local_var *var;
            addLocalVar(s->symbol, size, is_array);
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

    if (!left) return;

    /* Check for simple variable deref: (M $var) */
    if (left->op == 'M' && left->left && left->left->op == '$' &&
        left->left->symbol) {
        e->opflags |= OP_SIMPLEVAR;
        cacheVar(e, left->left->symbol);
    }
    /* Check for bare variable: ($var) - used by OREQ/ANDEQ */
    else if (left->op == '$' && left->symbol) {
        e->opflags |= OP_SIMPLEVAR;
        cacheVar(e, left->symbol);
    }
    /* Check for struct member address: (+ (M:p $var) ofs) - used by OREQ/ANDEQ */
    else if (left->op == '+' &&
             left->left && left->left->op == 'M' &&
             left->left->left && left->left->left->op == '$' &&
             left->right && left->right->op == 'C') {
        cacheVar(e, left->left->left->symbol);
        if (e->cached_var && e->cached_var->reg == REG_IX)
            e->opflags |= OP_IXMEM;
        e->value = left->right->value;
    }
    /* Check for IX-indexed struct member: (M (+ (M:p $var) ofs)) */
    else if (left->op == 'M' && (left->flags & E_IXDEREF)) {
        /* Already flagged during analyzeExpr - check if var is in IX */
        if (left->left && left->left->left && left->left->left->left &&
            left->left->left->left->op == '$') {
            cacheVar(e, left->left->left->left->symbol);
            if (e->cached_var && e->cached_var->reg == REG_IX)
                e->opflags |= OP_IXMEM;
        }
    }
    /* Check for indirect through pointer: (M (M $ptr)) */
    else if (left->op == 'M' && left->left && left->left->op == 'M' &&
             left->left->left && left->left->left->op == '$') {
        e->opflags |= OP_INDIR;
        cacheVar(e, left->left->left->symbol);
    }
}

/* Set opflags for an expression and its children */
static void
setExprFlags(struct expr *e)
{
    if (!e) return;
    CHECK_WALK();

    /* Clear opflags first */
    e->opflags = 0;

    /* Check right operand for constant */
    if (e->right && e->right->op == 'C') {
        e->opflags |= OP_CONST;
    }

    /* Set left operand pattern flags */
    setLeftFlags(e);

    /* For DEREF nodes, check if this is a simple var or IX-indexed */
    if (e->op == 'M') {
        if (e->left && e->left->op == '$' && e->left->symbol) {
            const char *sym = e->left->symbol;
            struct local_var *var;
            if (sym[0] == '$') sym++;
            var = findVar(sym);
            if (var) {
                if (var->reg != REG_NO) {
                    e->opflags |= OP_REGVAR;
                } else {
                    e->opflags |= OP_IYMEM;
                }
            } else {
                e->opflags |= OP_GLOBAL;
            }
        }
        /* Check for local member access: (M (+ $var C)) - struct/union member */
        else if (e->left && e->left->op == '+' &&
                 e->left->left && e->left->left->op == '$' && e->left->left->symbol &&
                 e->left->right && e->left->right->op == 'C') {
            const char *sym = e->left->left->symbol;
            struct local_var *var;
            if (sym[0] == '$') sym++;
            var = findVar(sym);
            if (var && var->reg == REG_NO) {
                /* IY-indexed with constant offset - store combined offset */
                e->opflags |= OP_IYMEM;
                e->offset = var->offset + e->left->right->value;
                e->cached_var = var;
            }
        }
        /* Check for byte indirect through BC: (M:b (M:p $bcvar)) */
        else if (e->size == 1 && e->left && e->left->op == 'M' &&
                 e->left->left && e->left->left->op == '$' && e->left->left->symbol) {
            const char *sym = e->left->left->symbol;
            struct local_var *var;
            if (sym[0] == '$') sym++;
            var = findVar(sym);
            if (var && var->reg == REG_BC) {
                e->opflags |= OP_BCINDIR;
            }
        }
    }

    /* Recurse on children */
    setExprFlags(e->left);
    setExprFlags(e->right);
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

    if (s->expr) specExpr(s->expr);
    if (s->expr2) specExpr(s->expr2);
    if (s->expr3) specExpr(s->expr3);

    if (s->then_branch) specStmt(s->then_branch);
    if (s->else_branch) specStmt(s->else_branch);
    if (s->next) specStmt(s->next);
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
 * Assign stack frame offsets to locals (skips register-allocated vars)
 */
void
optFrmLayout()
{
    struct local_var *var;
    int offset = 0;

    if (!fnLocals) return;

    for (var = fnLocals; var; var = var->next) {
        if (var->is_param) continue;
        if (var->reg != REG_NO) continue;
        offset += var->size;
        var->offset = -offset;
    }

    fnFrmSize = offset;

    if (fnFrmSize > 127) {
        fdprintf(2, "%s: frame > 127\n", fnName);
        exit(1);
    }
}

/*
 * Phase 1.5: Assign stack frame offsets to all local variables and parameters
 */
void
assignFrmOff()
{
    char *p;
    char name_buf[64];
    char type_buf[64];
    int param_offset;
    int i;

    if (!fnBody) return;

    

    /* First, assign offsets to parameters (positive offsets above FP) */
    /* Stack layout: FP+0=saved FP, FP+2=return addr, FP+4=first param */
    param_offset = 4;  /* Skip saved FP (2 bytes) + return address (2 bytes) */

    if (fnParams && fnParams[0]) {
        p = fnParams;
        while (*p) {
            unsigned char preg = REG_NO;
            struct local_var *var;

            /* Skip whitespace and commas */
            while (*p == ' ' || *p == ',') p++;
            if (!*p) break;

            /* Read parameter name */
            i = 0;
            while (*p && *p != ':' && *p != ',' && *p != ' ' &&
                    i < sizeof(name_buf) - 1) {
                name_buf[i++] = *p++;
            }
            name_buf[i] = '\0';

            /* Read parameter type if present */
            type_buf[0] = '\0';
            if (*p == ':') {
                p++;  /* Skip ':' */
                i = 0;
                while (*p && *p != ':' && *p != ',' && *p != ' ' &&
                        i < sizeof(type_buf) - 1) {
                    type_buf[i++] = *p++;
                }
                type_buf[i] = '\0';
            }

            /* Read register allocation if present (format :N where N is digit) */
            if (*p == ':') {
                p++;  /* Skip ':' */
                if (*p >= '0' && *p <= '9') {
                    preg = *p - '0';
                    p++;
                }
            }

            /* Add parameter with positive offset */
            if (name_buf[0]) {
                unsigned char size = type_buf[0] ?
                    getSizeFromTN(type_buf) : 2;
                addParam(name_buf, size, param_offset);
                /* Apply register allocation from AST */
                if (preg) {
                    var = findVar(name_buf);
                    if (var) var->reg = preg;
                }
                /* All params take at least 2 bytes on stack (push af/hl) */
                param_offset += (size < 2) ? 2 : size;
            }
        }
    }

    /* Then, assign offsets to local variables (negative offsets below FP) */
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
static void generateExpr(struct expr *e)
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
        if (e->left) generateExpr(e->left);  /* condition */
        if (e->right && e->right->left) generateExpr(e->right->left);   /* true expr */
        if (e->right && e->right->right) generateExpr(e->right->right); /* false expr */

        /* Allocate labels for branches */
        e->label = newLabel();  /* false_label */
        if (e->right) {
            e->right->label = newLabel();  /* end_label */
        }

        /* Create jump nodes that will be resolved during emission */
        /* Jump from condition evaluation to false branch */
        e->jump = newJump(JUMP_IF_ZERO, e->label);  /* if cond is zero (false), jump to false_label */

        /* Jump from true branch to end (skip false branch) */
        if (e->right) {
            e->right->jump = newJump(JMP_UNCOND, e->right->label);  /* jump to end_label */
        }

        return;  /* Early return - custom traversal done */
    }

    /* For assignments, propagate size to constant RHS before code gen */
    if (e->op == '=' && e->right && e->right->op == 'C') {
        e->right->size = e->size;
    }

    /* Recursively generate code for children (postorder traversal) */
    if (e->left) generateExpr(e->left);
    if (e->right) generateExpr(e->right);

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
static char *buildStkCln(int bytes)
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
 * Create a new jump instruction node
 */
struct jump_instr *
newJump(enum jump_type type, int target_label)
{
    struct jump_instr *j;

    j = (struct jump_instr *)malloc(sizeof(struct jump_instr));
    if (!j) return NULL;

    j->type = type;
    j->target_label = target_label;
    j->condition = NULL;
    j->next = NULL;

    return j;
}

/*
 * Free a jump instruction node (and its chain)
 */
void
freeJump(struct jump_instr *j)
{
    struct jump_instr *next;

    while (j) {
        next = j->next;
        xfree(j->condition);
        free(j);
        j = next;
    }
}

/*
 * Walk statement tree and generate assembly code blocks
 */
static void generateStmt(struct stmt *s)
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
    if (s->expr) generateExpr(s->expr);
    if (s->expr2) generateExpr(s->expr2);
    if (s->expr3) generateExpr(s->expr3);

    /* Recursively generate code for child statements */
    if (s->then_branch) generateStmt(s->then_branch);
    if (s->else_branch) generateStmt(s->else_branch);
    if (s->next) generateStmt(s->next);

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

    case 'R':  /* RETURN statement */
        /* Create unconditional jump to function exit */
        s->jump = newJump(JMP_UNCOND, -1);  /* -1 = exit label, resolved in emit */
        break;

    default:
        /* Other statement types don't generate jumps */
        break;
    }
}

/*
 * Generate assembly code for entire function
 */
void generateCode()
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
 * Schedule expression with a destination hint
 * dest: R_HL, R_DE, R_A, or R_NONE (use default)
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
    case 'C':  /* Constant */
        e->loc = LOC_CONST;
        /* value already in e->value */
        break;

    case '$':  /* Symbol reference (address of) */
        e->loc = LOC_MEM;
        /* symbol already in e->symbol */
        break;

    case 'M':  /* DEREF - memory load */
        /* Check what we're dereferencing */
        if (e->left && e->left->op == '$' && e->left->symbol) {
            /* Direct variable reference: (M $var) */
            var = findVar(e->left->symbol);
            if (var) {
                /* Local variable */
                if (var->reg != REG_NO) {
                    /* Register variable */
                    e->loc = LOC_REG;
                    switch (var->reg) {
                    case REG_BC: e->reg = R_BC; break;
                    case REG_IX: e->reg = R_IX; break;
                    default:     e->reg = R_NONE; break;
                    }
                } else {
                    /* Stack variable */
                    e->loc = LOC_STACK;
                    e->offset = var->offset;
                }
            } else {
                /* Global variable */
                e->loc = LOC_MEM;
            }
            /* Don't recurse into $ child - we handled it */
            e->left->loc = LOC_MEM;
        } else if (e->left && e->left->op == 'M' &&
                   e->left->left && e->left->left->op == '$') {
            /* Dereference of pointer variable: (M (M $ptr))
             * Check if ptr is IX-allocated for (ix+0) addressing */
            var = findVar(e->left->left->symbol);
            if (var && var->reg == REG_IX) {
                e->loc = LOC_IX;
                e->offset = 0;
            } else {
                /* Not IX - fall through to indirect */
                schedExpr(e->left, R_HL);
                e->loc = LOC_INDIR;
                e->reg = R_HL;
            }
        } else if (e->left && e->left->op == '+' &&
                   e->left->left && e->left->left->op == 'M' &&
                   e->left->left->left && e->left->left->left->op == '$' &&
                   e->left->right && e->left->right->op == 'C') {
            /* Struct member access: (M (+ (M $ptr) const))
             * Check if ptr is IX-allocated for (ix+offset) addressing */
            var = findVar(e->left->left->left->symbol);
            if (var && var->reg == REG_IX) {
                e->loc = LOC_IX;
                e->offset = e->left->right->value;
            } else {
                /* Not IX - fall through to indirect */
                schedExpr(e->left, R_HL);
                e->loc = LOC_INDIR;
                e->reg = R_HL;
            }
        } else {
            /* Complex dereference - recurse */
            schedExpr(e->left, R_HL);
            e->loc = LOC_INDIR;
            e->reg = R_HL;
        }
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
        /* Comparison with 0: test in place, no register scheduling
         * For == and !=, signedness doesn't matter (just test for zero)
         * For relational ops, only signed can use simple test */
        if (e->right && e->right->op == 'C' &&
            e->right->value == 0 && e->left && e->left->size == 2 &&
            ((e->op == 'Q' || e->op == 'n') || !(e->flags & E_UNSIGNED))) {
            schedExpr(e->left, R_NONE);  /* just set loc, don't force register */
            e->right->loc = LOC_CONST;
            e->right->dest = R_NONE;
            e->loc = LOC_FLAGS;
            break;
        }
        /* Comparisons: schedule so sbc hl,de gives correct result
         * < and >=: compute left-right, so left->HL, right->DE
         * > and <=: compute right-left, so left->DE, right->HL
         */
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
        /* Set condition code based on operator and signedness */
        e->loc = LOC_FLAGS;
        switch (e->op) {
        case 'Q':  /* == */
            e->cond = CC_Z;
            break;
        case 'n':  /* != */
            e->cond = CC_NZ;
            break;
        case '<':
            /* unsigned: C set means less; signed: use helper result */
            e->cond = (e->flags & E_UNSIGNED) ? CC_C : CC_C;
            break;
        case '>':
            /* unsigned: C clear AND NZ; signed: use helper result */
            e->cond = (e->flags & E_UNSIGNED) ? CC_NZ : CC_NZ;
            break;
        case 'L':  /* <= */
            /* unsigned: C set OR Z; signed: M or Z */
            e->cond = (e->flags & E_UNSIGNED) ? CC_Z : CC_Z;
            break;
        case 'g':  /* >= */
            /* unsigned: C clear; signed: P or Z */
            e->cond = (e->flags & E_UNSIGNED) ? CC_NC : CC_NC;
            break;
        }
        break;

    case '=':  /* Assignment */
        /* LHS is destination, RHS provides value
         * If LHS is a register-allocated variable, target that register */
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
    if (s->expr)  schedExpr(s->expr, R_NONE);
    if (s->expr2) schedExpr(s->expr2, R_NONE);
    if (s->expr3) schedExpr(s->expr3, R_NONE);

    /* Recurse into branches */
    if (s->then_branch) schedStmt(s->then_branch);
    if (s->else_branch) schedStmt(s->else_branch);

    /* Continue to next statement */
    if (s->next) schedStmt(s->next);
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
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
