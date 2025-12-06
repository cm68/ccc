/*
 * codegen.c - Code generation phase for cc2
 *
 * Walks expression and statement trees, generating assembly code blocks
 * for each node. This phase builds the asm_block strings but does not
 * emit them - that's done by emit.c.
 *
 * Key responsibilities:
 * - assignFrmOff(): Assign stack offsets to local variables and parameters
 * - generateCode(): Walk trees and generate assembly code blocks
 * - allocRegs(): Allocate variables to registers based on usage patterns
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"

/* Global loop depth for tracking whether we're inside a loop */
static int g_loop_depth = 0;


/* Global tree walk counter for loop detection */
static long g_walk_count = 0;
#define MAX_WALKS 1000000
#define CHECK_WALK() do { \
    if (++g_walk_count > MAX_WALKS) { \
        fdprintf(2, "codegen: exceeded %ld tree walks\n", (long)MAX_WALKS); \
        exit(1); \
    } \
} while(0)

/* Forward declaration from parseast.c for symbol tracking */
void addRefSym(const char *name);

/*
 * Helper: Get the original operand size, looking through SEXT/WIDEN/NARROW
 */
static int
getOrigSize(struct expr *e)
{
    if (!e) return 2;
    /* Look through type conversions to find original size */
    if ((e->op == 'X' || e->op == 'W' || e->op == 'N') && e->left)
        return e->left->size;
    return e->size;
}

/* Check if operand is unsigned (WIDEN or E_UNSIGNED flag) */
static int
isOpndUnsign(struct expr *e)
{
    return e && (e->op == 'W' || (e->flags & E_UNSIGNED));
}

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
 * Predicate: byte left operand with constant 0-255 right operand
 */
static int
isByteConstOp(struct expr *e)
{
    return e->left && e->left->size == 1 && (e->opflags & OP_CONST) &&
           e->right && e->right->value >= 0 && e->right->value <= 255;
}

/*
 * Predicate: right child is |/& with constant (for set/res bit patterns)
 */
static int
getBitOpConst(struct expr *e, int *is_or)
{
    if (!e->right || !e->right->right || e->right->right->op != 'C')
        return -1;
    if (e->right->op == '|') {
        *is_or = 1;
        return getBitNum(e->right->right->value);
    }
    if (e->right->op == '&') {
        *is_or = 0;
        return getBitNum(~e->right->right->value & 0xFF);
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
    if (TRACE(T_VAR)) {
        fdprintf(2, "      findVar(%p): looking for '%s' in locals\n", fnName, var_name);
    }
    for (var = fnLocals; var; var = var->next) {
        count++;
        if (TRACE(T_VAR)) {
            fdprintf(2, "      findVar: checking '%s' (count=%d)\n", var->name, count);
        }
        if (count > 1000) {
            fdprintf(2, "findVar: loop detected, count > 1000\n");
            exit(1);
        }
        if (strcmp(var->name, var_name) == 0) {
            if (TRACE(T_VAR)) {
                fdprintf(2, "      findVar: found!\n");
            }
            return var;
        }
    }
    if (TRACE(T_VAR)) {
        fdprintf(2, "      findVar: not found\n");
    }

    return NULL;
}

/*
 * Helper: Generate function name for binary arithmetic operations
 * Format: [u]<op><leftwidth><rightwidth>
 * Examples: mul88, umul816, div1616, add168
 */
static void
mkBinopFnName(char *buf, size_t bufsize, const char *opname,
                    struct expr *e)
{
    int left_bits = getOrigSize(e->left) * 8;
    int right_bits = getOrigSize(e->right) * 8;

    /* Operation is unsigned if either operand is unsigned */
    int is_unsigned = isOpndUnsign(e->left) || 
        isOpndUnsign(e->right);
    const char *prefix = is_unsigned ? "u" : "";

    snprintf(buf, bufsize, "%s%s%d%d", prefix, opname, left_bits, right_bits);
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
    if (!var) {
        fdprintf(2, "parseast: out of memory allocating local_var\n");
        exit(1);
    }
    var->name = strdup(name);
    var->size = size;
    var->offset = is_param ? offset : -(fnFrmSize + size);
    var->is_param = is_param;
    var->is_array = is_array;
    var->first_label = 255;
    var->last_label = 255;
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
 * Helper: Update variable lifetime tracking
 * Called whenever a variable is used, updates first_label and last_label
 */
static void
updVarLife(const char *name)
{
    struct local_var *var;

    if (!fnLocals || !name) return;

    /* Find the variable in locals list */
    for (var = fnLocals; var; var = var->next) {
        if (strcmp(var->name, name) == 0) {
            /* Update first use if not set */
            if (var->first_label == 255) {
                var->first_label = fnCurLbl;
            }
            /* Always update last use (high water mark) */
            if (fnCurLbl > var->last_label) {
                var->last_label = fnCurLbl;
            }
            /* Increment reference count */
            var->ref_count++;
            return;
        }
    }
}

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
            addLocalVar(s->symbol, size, is_array);
        }
    }

    /* Recursively walk child statements */
    if (s->then_branch) walkLocals(s->then_branch);
    if (s->else_branch) walkLocals(s->else_branch);
    if (s->next) walkLocals(s->next);
}

/*
 * Phase 2.5: Allocate registers to local variables and parameters
 * Called after code generation (Phase 2) which computes 
 * ref_count, agg_refs, lifetimes
 *
 * Both function parameters and local variables are candidates for 
 * register allocation.
 * Parameters start on the stack (passed by caller) but can be loaded into 
 * registers in the function prologue for efficiency.
 *
 * Allocation priority:
 *   1. IX register: allocated to struct pointers with aggregate member accesses
 *   2. Byte/word registers: allocated by reference count (by frequency)
 *
 * Variables excluded from register allocation:
 *   - Arrays (must remain on stack)
 *   - Unused variables (ref_count == 0)
 *   - Single-use variables (ref_count == 1) - no benefit to register allocation
 *   - Variables whose address is taken (future enhancement)
 */
void
allocRegs()
{
    struct local_var *var;
    int byteRegsUsed = 0;  /* Count of byte registers allocated */
    int wordRegsUsed = 0;  /* Count of word registers allocated */
    int ix_allocated = 0;    /* IX register allocated flag */

    if (!fnLocals) return;

    /* First pass: allocate IX to struct pointer with highest agg_refs */
    {
        struct local_var *best_ix_cand = NULL;
        int best_agg_refs = 0;

        for (var = fnLocals; var; var = var->next) {
            /* Skip if not a word/pointer variable */
            if (var->size != 2) continue;
            /* Prefer variables with aggregate member accesses */
            if (var->agg_refs > best_agg_refs) {
                best_agg_refs = var->agg_refs;
                best_ix_cand = var;
            }
        }

        if (best_ix_cand && best_agg_refs > 0) {
            best_ix_cand->reg = REG_IX;
            ix_allocated = 1;
            
        }
    }

    /* Second pass: allocate word registers first (BC must be allocated before B/C) */
    for (var = fnLocals; var; var = var->next) {
        /* Skip if already allocated */
        if (var->reg != REG_NO) continue;

        /* Skip arrays (they must stay on stack) */
        if (var->is_array) continue;

        /* Skip unused or single-use variables */
        if (var->ref_count <= 1) continue;

        /* Allocate word registers (BC and IX only)
         * BC must be allocated before B or C to avoid conflicts
         * BC' excluded because exx swaps both BC and HL, making HL inaccessible
         */
        if (var->size == 2 && wordRegsUsed < 2) {
            enum register_id regs[] = {REG_BC, REG_IX};
            /* If IX already allocated to struct pointer, skip it */
            if (wordRegsUsed == 1 && ix_allocated) {
                /* No more registers available */
                continue;
            }
            var->reg = regs[wordRegsUsed];
            if (var->reg == REG_IX) {
                ix_allocated = 1;
            }
            /* Mark B and C as unavailable if BC is allocated */
            if (var->reg == REG_BC) {
                byteRegsUsed = 2; /* B and C are now unavailable */
            }
            wordRegsUsed++;
            
        }
    }

    /* Third pass: allocate byte registers after all word registers are allocated */
    for (var = fnLocals; var; var = var->next) {
        /* Skip if already allocated */
        if (var->reg != REG_NO) continue;

        /* Skip arrays (they must stay on stack) */
        if (var->is_array) continue;

        /* Skip unused or single-use variables */
        if (var->ref_count <= 1) continue;

        /* Allocate byte registers (B, C only for now)
         * B' and C' disabled until exx codegen is tested
         * Cannot allocate B or C if BC is already allocated (they conflict)
         */
        if (var->size == 1 && byteRegsUsed < 2) {
            enum register_id regs[] = {REG_B, REG_C};
            var->reg = regs[byteRegsUsed];
            byteRegsUsed++;
            
        }
    }

    
}

/*
 * Phase 2: Analyze variable usage for register allocation
 * Walk expression/statement trees to count references and struct member accesses
 * Must run BEFORE allocRegs() and generateCode()
 */

/* Helper: increment agg_refs for struct member access pattern */
static void
incAggRef(const char *var_symbol)
{
    const char *var_name = var_symbol;
    struct local_var *var;

    if (!var_name) return;
    if (var_name[0] == '$') var_name++;
    if (var_name[0] == 'A') var_name++;

    for (var = fnLocals; var; var = var->next) {
        if (strcmp(var->name, var_name) == 0) {
            var->agg_refs++;
            return;
        }
    }
}

/* Analyze expression tree for variable usage */
static void
analyzeExpr(struct expr *e)
{
    if (!e) return;
    CHECK_WALK();

    /* Count variable references */
    if (e->op == '$' && e->symbol) {
        const char *var_name = e->symbol;
        if (var_name[0] == '$') var_name++;
        updVarLife(var_name);
    }

    /* Pattern 1: (= (+ (M:p $var) const) value) - struct member write */
    if (e->op == '=' && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str == 'p' &&
        e->left->left->left && e->left->left->left->op == '$' &&
        e->left->left->left->symbol &&
        e->left->right && e->left->right->op == 'C') {
        incAggRef(e->left->left->left->symbol);
        e->flags |= E_IXASSIGN;
        e->value = e->left->right->value;
    }

    /* Pattern 2: (M (+ (M:p $var) const)) - struct member read */
    if (e->op == 'M' && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str == 'p' &&
        e->left->left->left && e->left->left->left->op == '$' &&
        e->left->left->left->symbol &&
        e->left->right && e->left->right->op == 'C') {
        incAggRef(e->left->left->left->symbol);
        e->flags |= E_IXDEREF;
        e->value = e->left->right->value;
    }

    /* Recurse on children */
    analyzeExpr(e->left);
    analyzeExpr(e->right);
}

/* Analyze statement tree for variable usage */
static void
analyzeStmt(struct stmt *s)
{
    if (!s) return;

    /* Track label for lifetime analysis */
    if (s->type == 'L') {
        fnCurLbl++;
    }

    /* Analyze expression if present */
    if (s->expr) {
        analyzeExpr(s->expr);
    }

    /* Recurse on child statements */
    if (s->then_branch) analyzeStmt(s->then_branch);
    if (s->else_branch) analyzeStmt(s->else_branch);
    if (s->next) analyzeStmt(s->next);
}

/* Entry point: analyze all variables in function */
void
analyzeVars()
{
    if (!fnBody) return;
    fnCurLbl = 0;
    analyzeStmt(fnBody);
}

/*
 * Phase 2.75: Set operand pattern flags (opflags)
 * Must run AFTER allocRegs() so we know which variables are register-allocated
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
 * This pass walks the tree BEFORE generateExpr, detecting patterns like:
 * - INC/DEC of variables (simple and complex)
 * - OREQ/ANDEQ single-bit operations (set/res instructions)
 * - Other patterns that can be replaced with efficient asm
 *
 * When a pattern matches, the entire subtree is replaced with an asm_block
 * and children are freed. generateExpr then just emits what remains.
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

/* Helper: get IY-indexed offset for a local/param variable */
static int
varIYOfs(struct local_var *var)
{
    return var->offset + (var->is_param ? 1 : 0);
}

/* Helper: format IY-indexed operand into buffer, return chars written */
static int
fmtIY(char *buf, size_t sz, const char *fmt, int ofs)
{
    return snprintf(buf, sz, fmt, ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
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
    char buf[512];
    int is_post = (e->op == 0xef || e->op == 0xf6);
    int is_dec = (e->op == 0xd6 || e->op == 0xf6);
    int unused = (e->flags & E_UNUSED) ? 1 : 0;
    int need_old = is_post && !unused;
    long amount = e->value;
    char *p;

    /* Pattern 1: Simple variable - defer to emit phase */
    if (e->left && e->left->op == '$' && e->left->symbol) {
        struct local_var *var = findVar(e->left->symbol);
        if (var) e->size = var->size;
        snprintf(buf, sizeof(buf), "INCDEC_PLACEHOLDER:%d:%d:%ld:%d:%s",
                 (int)e->op, e->size, amount, unused, e->left->symbol);
        e->asm_block = strdup(buf);
        freeKids(e);
        return 1;
    }

    /* Pattern 2/3: DEREF or ADD - address in HL after specExpr */
    if (e->left && (e->left->op == 'M' || e->left->op == '+')) {
        struct expr *addr = (e->left->op == 'M') ? e->left->left : e->left;
        specExpr(addr);
        if (e->left->op == 'M') e->left->left = NULL;

        p = buf;
        /* Save address to DE */
        p += sprintf(p, "\tld e, l\n\tld d, h\n");

        if (e->size == 1) {
            /* Byte: load, modify, store */
            p += sprintf(p, "\tld a, (hl)\n");
            if (need_old) p += sprintf(p, "\tld c, a\n");
            if (amount == 1)
                p += sprintf(p, "\t%s a\n", is_dec ? "dec" : "inc");
            else
                p += sprintf(p, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
            p += sprintf(p, "\tld (de), a");
            if (need_old) p += sprintf(p, "\n\tld a, c");
        } else {
            /* Word: load to HL via BC */
            p += sprintf(p, "\tld a, (hl)\n\tld c, a\n\tinc hl\n");
            p += sprintf(p, "\tld a, (hl)\n");
            if (need_old) p += sprintf(p, "\tld b, a\n\tld h, b\n");
            else p += sprintf(p, "\tld h, a\n");
            p += sprintf(p, "\tld l, c\n");

            if (need_old && amount != 1) p += sprintf(p, "\tpush hl\n");

            /* Modify HL */
            if (amount == 1)
                p += sprintf(p, "\t%s hl\n", is_dec ? "dec" : "inc");
            else
                p += sprintf(p, "\tld bc, %ld\n\t%s hl, bc\n",
                             amount, is_dec ? "or a\n\tsbc" : "add");

            /* Store new value */
            if (amount != 1) {
                p += sprintf(p, "\tex de, hl\n\tld a, e\n\tld (hl), a\n");
                p += sprintf(p, "\tinc hl\n\tld a, d\n\tld (hl), a");
            } else {
                p += sprintf(p, "\tld a, l\n\tld (de), a\n");
                p += sprintf(p, "\tinc de\n\tld a, h\n\tld (de), a");
            }

            /* Restore old value if needed */
            if (need_old) {
                if (amount != 1) p += sprintf(p, "\n\tpop hl");
                else p += sprintf(p, "\n\tld h, b\n\tld l, c");
            }
        }

        e->asm_block = strdup(buf);
        e->left = NULL;
        freeExpr(e->right);
        e->right = NULL;
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
    char buf[256];
    int bitnum;
    int is_or = (e->op == 0x31);  /* OREQ vs ANDEQ */
    const char *inst = is_or ? "set" : "res";

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
                    if (isAltReg(var->reg))
                        snprintf(buf, sizeof(buf), "\texx\n\t%s %d, %s\n\texx", inst, bitnum, rn);
                    else
                        snprintf(buf, sizeof(buf), "\t%s %d, %s", inst, bitnum, rn);
                    e->asm_block = strdup(buf);
                    freeKids(e);
                    return 1;
                }
            }
        } else if (e->opflags & OP_IYMEM) {
            /* Stack variable */
            if (var) {
                char *p = buf;
                p += sprintf(p, "\t%s %d, (iy ", inst, bitnum);
                fmtIY(p, sizeof(buf) - (p - buf), "%c %d)", varIYOfs(var));
                e->asm_block = strdup(buf);
                freeKids(e);
                return 1;
            }
        }
    }

    /* Pattern 2: Struct member via pointer - use cached OP_IXMEM and e->value */
    if (e->left && e->left->op == '+') {
        if (e->opflags & OP_IXMEM) {
            /* IX-indexed - offset cached in e->value by setLeftFlags */
            long ofs = e->value;
            snprintf(buf, sizeof(buf), "\t%s %d, (ix + %ld)", inst, bitnum, ofs);
            e->asm_block = strdup(buf);
            freeKids(e);
            return 1;
        } else {
            /* Non-IX: compute address, then use (hl)
             * Keep e->left so generateExpr will process it and generate
             * the address computation code. Just set our asm_block. */
            snprintf(buf, sizeof(buf), "\t%s %d, (hl)", inst, bitnum);
            e->asm_block = strdup(buf);
            /* Free right (the constant), but keep left for address generation */
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
    char buf[256];
    int i, count;
    char *p;
    const char *inst;
    const char *rn;
    struct local_var *var;

    /* Must be byte operation with constant RHS */
    if (e->size != 1 || !e->right || e->right->op != 'C')
        return 0;

    count = e->right->value & 0xff;
    if (count < 1 || count > 7) return 0;  /* Reasonable range */

    inst = (e->op == '0') ? "sla" : "srl";  /* LSHIFTEQ vs RSHIFTEQ */

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

    /* Generate N shift instructions */
    p = buf;
    for (i = 0; i < count; i++) {
        if (isAltReg(var->reg)) {
            p += sprintf(p, "%s\texx\n\t%s %s\n\texx",
                i > 0 ? "\n" : "", inst, rn);
        } else {
            p += sprintf(p, "%s\t%s %s", i > 0 ? "\n" : "", inst, rn);
        }
    }

    e->asm_block = strdup(buf);
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
    char buf[128];
    const char *rn;
    struct local_var *var;
    int is_add = (e->op == 'P');

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

    /* For PLUSEQ: evaluate RHS to A, then add a,r; ld r,a
     * For SUBEQ: need to do var - expr, so: save RHS, load var, sub, store */
    if (is_add) {
        /* RHS will be in A after evaluation, add register to it, store back */
        if (isAltReg(var->reg)) {
            snprintf(buf, sizeof(buf), "\texx\n\tadd a, %s\n\tld %s, a\n\texx", rn, rn);
        } else {
            snprintf(buf, sizeof(buf), "\tadd a, %s\n\tld %s, a", rn, rn);
        }
    } else {
        /* SUBEQ: var -= expr means var = var - expr
         * RHS in A, need to compute var - A
         * Use: ld e,a; ld a,r; sub e; ld r,a */
        if (isAltReg(var->reg)) {
            snprintf(buf, sizeof(buf), "\tld e, a\n\texx\n\tld a, %s\n\texx\n\tsub e\n\texx\n\tld %s, a\n\texx", rn, rn);
        } else {
            snprintf(buf, sizeof(buf), "\tld e, a\n\tld a, %s\n\tsub e\n\tld %s, a", rn, rn);
        }
    }

    e->asm_block = strdup(buf);
    /* Keep e->right for RHS evaluation, free e->left (the variable reference) */
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

    /* Skip if already has asm_block */
    if (e->asm_block) return;

    /* Try INC/DEC specialization */
    if (e->op == 0xcf || e->op == 0xef || e->op == 0xd6 || e->op == 0xf6) {
        if (specIncDec(e)) return;
    }

    /* Try OREQ/ANDEQ bit operations */
    if (e->op == 0x31 || e->op == 0xc6) {
        if (specBitOp(e)) return;
    }

    /* Try LSHIFTEQ/RSHIFTEQ for register variables */
    if (e->op == '0' || e->op == '6') {
        if (specShiftOp(e)) return;
    }

    /* Try PLUSEQ/SUBEQ for byte register variables */
    if (e->op == 'P' || e->op == 0xdf) {
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
 * Stack slot structure for frame optimization
 * A slot can hold multiple variables whose lifetimes don't overlap
 */
struct stackSlot {
    int offset;              /* Negative offset from frame pointer */
    int size;                /* Size of this slot in bytes */
    struct local_var **vars; /* Array of variables using this slot */
    int numVars;            /* Number of variables in this slot */
    int capacity;            /* Allocated capacity of vars array */
};

/*
 * Helper: Check if two variables' lifetimes overlap
 * Returns 1 if they overlap (cannot share a slot), 0 if they don't overlap
 */
static int
lifeOverlap(struct local_var *v1, struct local_var *v2)
{
    /* If either variable is never used, they don't conflict */
    if (v1->first_label == 255 || v2->first_label == 255) {
        return 0;
    }

    /* Variables overlap if their ranges [first, last] intersect
     * Ranges [a1,a2] and [b1,b2] intersect if: a1 <= b2 AND b1 <= a2 */
    return (v1->first_label <= v2->last_label &&
            v2->first_label <= v1->last_label);
}

/*
 * Helper: Check if a variable can fit in a slot (no lifetime conflicts)
 * Returns 1 if variable can use this slot, 0 otherwise
 */
static int
canUseSlot(struct stackSlot *slot, struct local_var *var)
{
    int i;

    /* Variable must be same size as slot */
    if (var->size != slot->size) {
        return 0;
    }

    /* Check for lifetime conflicts with all variables in this slot */
    for (i = 0; i < slot->numVars; i++) {
        if (lifeOverlap(var, slot->vars[i])) {
            return 0;  /* Conflict - cannot use this slot */
        }
    }

    return 1;  /* No conflicts - can use this slot */
}

/*
 * Helper: Add a variable to a slot
 */
static void
addVarToSlot(struct stackSlot *slot, struct local_var *var)
{
    /* Grow array if needed */
    if (slot->numVars >= slot->capacity) {
        slot->capacity = slot->capacity ? slot->capacity * 2 : 4;
        slot->vars = realloc(slot->vars,
                             slot->capacity * sizeof(struct local_var *));
    }

    slot->vars[slot->numVars++] = var;
}

/*
 * Optimize stack frame by reusing slots for non-overlapping lifetimes
 * Called after code generation (which computes lifetimes) but before
 * register allocation.
 *
 * Algorithm: Greedy slot packing
 * - For each local variable, try to find an existing slot where its
 *   lifetime doesn't overlap with any variable already in that slot
 * - If found, assign the variable to that slot
 * - Otherwise, create a new slot
 * - Finally, reassign offsets based on slot assignments
 *
 * This reduces frame size by allowing variables with non-overlapping
 * lifetimes to share the same stack location.
 */
void
optFrmLayout()
{
    struct stackSlot *slots = NULL;
    int num_slots = 0;
    int slot_capacity = 0;
    struct local_var *var;
    int newFrameSize;
    int i, j;
    int slot_idx;
    int found_slot;

    if (!fnLocals) return;

    

    /* Build slot assignments for each local variable */
    for (var = fnLocals; var; var = var->next) {
        /* Skip parameters - they have fixed offsets */
        if (var->is_param) continue;

        /* Skip unused variables (never referenced) */
        if (var->first_label == -1) {
            
            continue;
        }

        /* Try to find an existing slot this variable can use */
        found_slot = 0;
        for (slot_idx = 0; slot_idx < num_slots; slot_idx++) {
            if (canUseSlot(&slots[slot_idx], var)) {
                /* Found a compatible slot */
                addVarToSlot(&slots[slot_idx], var);
                found_slot = 1;
                
                break;
            }
        }

        /* If no compatible slot found, create a new one */
        if (!found_slot) {
            /* Grow slots array if needed */
            if (num_slots >= slot_capacity) {
                slot_capacity = slot_capacity ? slot_capacity * 2 : 8;
                slots = realloc(slots, slot_capacity * sizeof(struct stackSlot));
            }

            /* Initialize new slot */
            slots[num_slots].offset = 0;  /* Will assign later */
            slots[num_slots].size = var->size;
            slots[num_slots].vars = NULL;
            slots[num_slots].numVars = 0;
            slots[num_slots].capacity = 0;

            addVarToSlot(&slots[num_slots], var);

            

            num_slots++;
        }
    }

    /* Reassign offsets based on slots */
    newFrameSize = 0;
    for (i = 0; i < num_slots; i++) {
        newFrameSize += slots[i].size;
        slots[i].offset = -newFrameSize;

        /* Assign this offset to all variables in the slot */
        for (j = 0; j < slots[i].numVars; j++) {
            slots[i].vars[j]->offset = slots[i].offset;
        }
    }

    /* Update context frame size */
    
    fnFrmSize = newFrameSize;

    /* Check frame size limit - IY-indexed addressing uses signed 8-bit offsets
     * Local variables use negative offsets from IY, range is -1 to -128
     * Limit to 127 bytes to ensure all locals fit within addressing range */
    if (fnFrmSize > 127) {
        fdprintf(2, "ERROR: Function %s has %d bytes of local variables after optimization (max 127)\n",
                 fnName, fnFrmSize);
        fdprintf(2, "       Reduce number or size of local variables\n");
        exit(1);
    }

    /* Free slot data structures */
    for (i = 0; i < num_slots; i++) {
        free(slots[i].vars);
    }
    free(slots);
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
                while (*p && *p != ',' && *p != ' ' && 
                        i < sizeof(type_buf) - 1) {
                    type_buf[i++] = *p++;
                }
                type_buf[i] = '\0';
            }

            /* Add parameter with positive offset */
            if (name_buf[0]) {
                unsigned char size = type_buf[0] ?
                    getSizeFromTN(type_buf) : 2;
                addParam(name_buf, size, param_offset);
                /* All params take at least 2 bytes on stack (push af/hl) */
                param_offset += (size < 2) ? 2 : size;
            }
        }
    }

    /* Then, assign offsets to local variables (negative offsets below FP) */
    walkLocals(fnBody);
    
}

/*
 * Check if expression is unsigned 16-bit comparison that can be inlined
 * Unsigned comparisons can use sbc hl,de and check carry flag
 */
static int isInlUCmp(struct expr *e)
{
    /* Must be ge or lt comparison */
    if (e->op != 'g' && e->op != '<') return 0;

    /* Must be 16-bit operands */
    if (e->size != 2) return 0;
    if (!e->left || e->left->size != 2) return 0;
    if (!e->right || e->right->size != 2) return 0;

    /* Must be unsigned (either operand unsigned makes it unsigned) */
    return isOpndUnsign(e->left) || isOpndUnsign(e->right);
}

/*
 * Check if expression is signed 16-bit comparison with constant 0
 * Returns: '<' for LT, 'g' for GE, 'L' for LE, '>' for GT, 0 otherwise
 * These can be optimized using bit tests on high byte
 */
static int isSCmpZero(struct expr *e)
{
    /* Must be a comparison op */
    if (e->op != '<' && e->op != 'g' && e->op != 'L' && e->op != '>') return 0;

    /* Must be 16-bit */
    if (e->size != 2) return 0;
    if (!e->left || e->left->size != 2) return 0;
    if (!e->right) return 0;

    /* Right operand must be constant 0 */
    if (e->right->op != 'C' || e->right->value != 0) return 0;

    /* Must be signed (neither operand unsigned) */
    if (isOpndUnsign(e->left) || isOpndUnsign(e->right)) return 0;

    return e->op;
}

/*
 * Generate standard binary operator code
 * Common pattern: move PRIMARY to SECONDARY, call runtime function
 * For unsigned 16-bit comparisons (ge/lt), inline with sbc hl,de
 * For signed 16-bit comparisons with 0, inline with bit 7,h
 */
static void
genBinop(struct expr *e, const char *op_name)
{
    char funcname[32];
    char buf[256];
    char *move_inst;
    int cmp_op;

    /* Inline unsigned 16-bit ge/lt comparisons */
    if (isInlUCmp(e)) {
        /* After pushStack: DE=left, HL=right
         * ex de,hl: HL=left, DE=right
         * or a: clear carry
         * sbc hl,de: HL = left - right, carry set if left < right */
        snprintf(buf, sizeof(buf),
            "\tex de, hl  ; move PRIMARY to SECONDARY\n"
            "\tor a  ; clear carry\n"
            "\tsbc hl, de  ; compare: C if left < right");
        e->asm_block = strdup(buf);
        return;
    }

    /* Inline signed 16-bit comparisons with 0 */
    cmp_op = isSCmpZero(e);
    if (cmp_op) {
        /* Left operand is in HL, right is constant 0 (not emitted)
         * x < 0:  bit 7,h; nz = negative = true
         * x >= 0: bit 7,h; z = non-negative = true
         * x <= 0: test zero first, then sign
         * x > 0:  test sign first, then zero
         */
        /* All sign comparisons use bit 7 test; emit.c handles result logic */
        (void)cmp_op;  /* All cases same: <, >=, <=, > */
        snprintf(buf, sizeof(buf), "\tbit 7, h");
        e->asm_block = strdup(buf);
        /* Mark right operand (constant 0) as not needing emission */
        if (e->right) e->right->asm_block = noasm;
        return;
    }

    mkBinopFnName(funcname, sizeof(funcname), op_name, e);

    move_inst = (e->size == 1) ?
        "\tld e, a  ; move PRIMARY to SECONDARY" :
        "\tex de, hl  ; move PRIMARY to SECONDARY";

    snprintf(buf, sizeof(buf), "%s\n\tcall %s", move_inst, funcname);
    e->asm_block = strdup(buf);
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
    char buf[256];
    struct expr *arg;
    int arg_count;
    int i;

    if (!e) return;
    CHECK_WALK();
    expr_count++;
    if (expr_count > 100000) {
        fdprintf(2, "generateExpr: exceeded 100000 calls, op=%c\n", e->op);
        exit(1);
    }

    /* Special handling for CALL - emitCall handles it directly from AST */
    if (e->op == '@') {
        arg_count = e->value;

        /* Generate code for each argument */
        arg = e->right;
        for (i = 0; i < arg_count && arg; i++) {
            if (arg->left) {
                /* Optimize constant arguments: if constant fits in byte, use byte */
                if (arg->left->op == 'C' &&
                    arg->left->value >= 0 && arg->left->value <= 255) {
                    arg->left->size = 1;
                }
                generateExpr(arg->left);
            }
            arg = arg->right;
        }

        /* Stack cleanup in loops (framefree handles it otherwise) */
        if (arg_count > 0 && g_loop_depth > 0) {
            e->cleanup_block = buildStkCln(arg_count * 2);
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
        e->label = fnLblCnt++;  /* false_label */
        if (e->right) {
            e->right->label = fnLblCnt++;  /* end_label */
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

    /* NOTE: INC/DEC and OREQ/ANDEQ patterns are now handled by specialize() */
    /* Skip nodes that already have asm_block (handled by specialize) */
    if (e->asm_block) {
        /* Still need to recurse for any children that weren't freed */
        if (e->left) generateExpr(e->left);
        if (e->right) generateExpr(e->right);
        return;
    }

    /* INC/DEC and OREQ/ANDEQ are now handled by specialize()
     * If we get here with these ops, specialize didn't handle them - emit fallback */
    if (e->op == 0xcf || e->op == 0xef || e->op == 0xd6 || e->op == 0xf6) {
        /* INC/DEC not handled by specialize - emit TODO comment */
        snprintf(buf, sizeof(buf), "\t; TODO: unhandled inc/dec op=0x%02x", e->op);
        e->asm_block = strdup(buf);
        return;
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
    case 'C':  /* CONST - load immediate value */
        if (e->size == 1) {
            if (e->value == 0) {
                snprintf(buf, sizeof(buf), "\txor a");
            } else {
                snprintf(buf, sizeof(buf), "\tld a, %ld", e->value);
            }
        } else if (e->size == 2) {
            snprintf(buf, sizeof(buf), "\tld hl, %ld", e->value);
        } else {  /* size == 4 - long constant, load into HL',HL */
            unsigned long uval = (unsigned long)e->value;
            unsigned short lower = uval & 0xFFFF;
            unsigned short upper = (uval >> 16) & 0xFFFF;
            snprintf(buf, sizeof(buf),
                "\tld hl, %u  ; load lower 16 bits\n"
                "\texx\n"
                "\tld hl, %u  ; load upper 16 bits into HL'\n"
                "\texx",
                lower, upper);
        }
        e->asm_block = strdup(buf);
        break;

    case 'M':  /* DEREF - load from memory */
        /* Skip if already marked as generated (e.g., by bit test optimization) */
        if (e->flags & E_GENERATED) break;

        /* Simple variable load: (M $var) - use opflags set by setOpFlags() */
        if ((e->opflags & (OP_REGVAR | OP_IYMEM | OP_GLOBAL)) &&
            e->left && e->left->op == '$' && e->left->symbol) {
            const char *sym = e->left->symbol;
            struct local_var *var = findVar(sym);

            if (var && var->reg != REG_NO) {
                /* Register-allocated variable - generate move to PRIMARY */
                if (e->size == 1) {
                    /* Byte in reg -> A */
                    int reg = (var->reg == REG_BC) ? REG_C : var->reg;
                    static const char *byte_load[] = {
                        NULL, "\tld a, b", "\tld a, c",
                        "\texx\n\tld a, b\n\texx", "\texx\n\tld a, c\n\texx"
                    };
                    if (reg >= REG_B && reg <= REG_Cp) {
                        e->asm_block = strdup(byte_load[reg]);
                    } else {
                        e->asm_block = noasm;
                    }
                } else {
                    /* Word in reg -> HL */
                    if (var->reg == REG_BC) {
                        e->asm_block = strdup("\tld h, b\n\tld l, c");
                    } else if (var->reg == REG_IX) {
                        e->asm_block = strdup("\tpush ix\n\tpop hl");
                    } else if (var->reg == REG_BCp) {
                        e->asm_block = strdup("\texx\n\tpush bc\n\texx\n\tpop hl");
                    } else {
                        e->asm_block = noasm;
                    }
                }
            } else if (var) {
                /* Stack variable - IY-indexed */
                int ofs = var->offset;
                char *p = buf;
                if (e->size == 1) {
                    p += sprintf(p, "\tld a, (iy ");
                    fmtIY(p, sizeof(buf) - (p - buf), "%c %d)", ofs);
                } else if (e->size == 2) {
                    p += sprintf(p, "\tld l, (iy ");
                    p += fmtIY(p, sizeof(buf) - (p - buf), "%c %d)", ofs);
                    p += sprintf(p, "\n\tld h, (iy ");
                    fmtIY(p, sizeof(buf) - (p - buf), "%c %d)", ofs + 1);
                } else {
                    sprintf(buf, "\tld a, %d\n\tcall getlong", ofs);
                }
                e->asm_block = strdup(buf);
            } else {
                /* Global variable - defer to emit for cache optimization */
                e->asm_block = noasm;  /* Let emit handle with caching */
            }
            break;
        }

        /* IX-indexed struct member: (M (+ (M:p $var) ofs)) */
        if (e->flags & E_IXDEREF) {
            long ofs = e->value;  /* Offset saved by analyzeExpr */
            const char *sym = e->left->left->left->symbol;
            struct local_var *var = findVar(sym);

            if (var && var->reg == REG_IX) {
                if (e->size == 1) {
                    snprintf(buf, sizeof(buf), "\tld a, (ix + %ld)", ofs);
                } else if (e->size == 2) {
                    snprintf(buf, sizeof(buf), "\tld l, (ix + %ld)\n\tld h, (ix + %ld)", ofs, ofs + 1);
                } else {
                    snprintf(buf, sizeof(buf), "\tld l, (ix + %ld)\n\tld h, (ix + %ld)\n\texx\n\tld l, (ix + %ld)\n\tld h, (ix + %ld)\n\texx",
                             ofs, ofs + 1, ofs + 2, ofs + 3);
                }
                e->asm_block = strdup(buf);
                /* Mark child as handled - don't generate address calculation */
                if (e->left) e->left->asm_block = noasm;
                break;
            }
            /* Fall through to compute address if not IX-allocated */
        }

        /* Indirect byte load through BC pointer: (M:b (M:p $bcvar)) */
        if (e->opflags & OP_BCINDIR) {
            e->asm_block = strdup("BCINDIR");  /* Placeholder for emit-time cache check */
            e->left->asm_block = noasm;   /* Don't load pointer to HL */
            break;
        }

        /* Complex address expression - address will be in PRIMARY (HL) */
        if (e->size == 1) {
            snprintf(buf, sizeof(buf), "\tld a, (hl)");
        } else if (e->size == 2) {
            snprintf(buf, sizeof(buf), "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl");
        } else {
            snprintf(buf, sizeof(buf), "\tcall load32i");
        }
        e->asm_block = strdup(buf);
        break;

    case '=':  /* ASSIGN - store to memory */
        /* Check for set/res bit patterns using helper */
        if (e->size == 1 && e->right) {
            int is_or, bitnum = getBitOpConst(e, &is_or);
            if (bitnum >= 0) {
                const char *inst = is_or ? "set" : "res";
                /* Pattern 1: Simple variable (IY-indexed) */
                if (e->left && e->left->op == '$' && e->left->symbol) {
                    struct local_var *var = findVar(e->left->symbol);
                    if (var && var->reg == REG_NO) {
                        char *p = buf;
                        p += sprintf(p, "\t%s %d, (iy ", inst, bitnum);
                        fmtIY(p, sizeof(buf) - (p - buf), "%c %d)", varIYOfs(var));
                        e->asm_block = strdup(buf);
                        if (e->right->left) e->right->left->asm_block = noasm;
                        e->right->asm_block = noasm;
                        break;
                    }
                }
                /* Pattern 2: IX-indexed struct member */
                if ((e->flags & E_IXASSIGN) && e->left && e->left->left &&
                    e->left->left->left && e->left->left->left->op == '$') {
                    struct local_var *var = findVar(e->left->left->left->symbol);
                    if (var && var->reg == REG_IX) {
                        snprintf(buf, sizeof(buf), "\t%s %d, (ix + %ld)", inst, bitnum, e->value);
                        e->asm_block = strdup(buf);
                        if (e->right->left) e->right->left->asm_block = noasm;
                        e->right->asm_block = noasm;
                        if (e->left) e->left->asm_block = noasm;
                        break;
                    }
                }
            }
        }
        /* Default: defer to emit phase */
        e->asm_block = strdup("\t; ASSIGN_PLACEHOLDER");
        break;

    case '+':  /* ADD */
        {
            /* Check if right operand is a constant for optimization */
            /* Handle both direct constants and type-converted constants */
            struct expr *right_const = NULL;
            long const_val = 0;
            int op_size;
            int i;

            if (e->right && e->right->op == 'C') {
                /* Direct constant */
                right_const = e->right;
                const_val = e->right->value;
            } else if (e->right && e->right->left &&
                       e->right->left->op == 'C') {
                /* Constant wrapped in type conversion (NARROW/WIDEN/SEXT) */
                right_const = e->right->left;
                const_val = e->right->left->value;
            }

            if (right_const) {
                /* Optimized constant addition */
                /* Use left operand's size since that's the register being used */
                op_size = e->left ? e->left->size : e->size;

                if (const_val == 0) {
                    /* Adding 0 is a no-op */
                    e->asm_block = noasm;
                } else if (op_size == 1) {
                    /* Byte add with constant - use immediate add */
                    snprintf(buf, sizeof(buf),
                        "\tadd a, %ld  ; add constant to byte", const_val);
                    e->asm_block = strdup(buf);
                } else if (const_val >= 1 && const_val <= 4) {
                    /* Word add with constant 1-4 - use repeated inc hl */
                    buf[0] = '\0';
                    for (i = 0; i < const_val; i++) {
                        if (i > 0) strcat(buf, "\n");
                        strcat(buf, "\tinc hl");
                    }
                    strcat(buf, "  ; add small constant");
                    e->asm_block = strdup(buf);
                } else {
                    /* Word add with larger constant - load and add */
                    snprintf(buf, sizeof(buf),
                        "\tld de, %ld\n\tadd hl, de", const_val);
                    e->asm_block = strdup(buf);
                }

                /* Free and clear right operand to prevent emission */
                freeExpr(e->right);
                e->right = NULL;
            } else {
                /* Non-constant addition - use general form */
                char *move_inst;
                char *add_inst;

                /* For POSTINC/PREINC/etc, size is now correctly set by specIncDec */
                int rsize = e->right ? e->right->size : 2;

                if (e->size == 1) {
                    /* Byte add - need to call add88 */
                    char funcname[32];
                    mkBinopFnName(funcname, sizeof(funcname), "add", e);
                    move_inst = "\tld e, a  ; move PRIMARY (A) to SECONDARY (E)";
                    snprintf(buf, sizeof(buf), "%s\n\tcall %s",
                        move_inst, funcname);
                } else if (e->right && rsize == 1) {
                    /* Word + byte: after pushStack, DE has left (addr), A has right (byte)
                     * Zero-extend A to HL and add DE. Preserves E for nested saves. */
                    snprintf(buf, sizeof(buf),
                        "\tex de, hl  ; (matches pushStack)\n"
                        "\tld l, a\n"
                        "\tld h, 0\n"
                        "\tadd hl, de");
                } else {
                    /* Word add - use native Z80 add hl,de instruction */
                    move_inst =
                        "\tex de, hl  ; move PRIMARY(HL) to SECONDARY(DE)";
                    add_inst = "\tadd hl, de";
                    snprintf(buf, sizeof(buf), "%s\n%s", move_inst, add_inst);
                }
                e->asm_block = strdup(buf);
            }
        }
        break;

    case '-':  /* SUB */
        genBinop(e, "sub");
        break;

    case '*':  /* MUL */
        genBinop(e, "mul");
        break;

    case '/':  /* DIV */
        genBinop(e, "div");
        break;

    case '%':  /* MOD */
        genBinop(e, "mod");
        break;

    case '&':  /* AND */
        /* Optimize byte AND with small constant to inline instruction */
        if (isByteConstOp(e)) {
            int bitnum = getBitNum(e->right->value);
            /* Check for IX-indexed struct member access */
            if (bitnum >= 0 && (e->opflags & OP_IXMEM)) {
                long ofs = e->left->value;
                snprintf(buf, sizeof(buf), "\tbit %d, (ix + %ld)", bitnum, ofs);
                e->asm_block = strdup(buf);
                e->left->asm_block = noasm;  /* Suppress left operand load */
                break;
            }
            /* Check for simple local variable in memory (IY-indexed) */
            if (bitnum >= 0 && (e->opflags & OP_SIMPLEVAR) && (e->opflags & OP_IYMEM)) {
                const char *sym = e->left->left->symbol;
                struct local_var *var = findVar(sym);
                if (var) {
                    char *p = buf;
                    p += sprintf(p, "\tbit %d, (iy ", bitnum);
                    fmtIY(p, sizeof(buf) - (p - buf), "%c %d)", varIYOfs(var));
                    e->asm_block = strdup(buf);
                    e->left->asm_block = noasm;  /* Suppress left operand load */
                    break;
                }
            }
            if (bitnum >= 0) {
                /* Single bit test: use "bit n, a" which sets Z without modifying A */
                snprintf(buf, sizeof(buf), "\tbit %d, a", bitnum);
            } else {
                /* Byte AND with immediate: use inline "and <imm>" instruction */
                snprintf(buf, sizeof(buf), "\tand %ld", e->right->value & 0xFF);
            }
            e->asm_block = strdup(buf);
        } else {
            genBinop(e, "and");
        }
        break;

    case '|':  /* OR */
        /* Optimize byte OR with small constant to inline instruction */
        if (isByteConstOp(e)) {
            snprintf(buf, sizeof(buf), "\tor %ld", e->right->value & 0xFF);
            e->asm_block = strdup(buf);
        } else {
            genBinop(e, "or");
        }
        break;

    case '^':  /* XOR */
        /* Optimize byte XOR with small constant to inline instruction */
        if (isByteConstOp(e)) {
            snprintf(buf, sizeof(buf), "\txor %ld", e->right->value & 0xFF);
            e->asm_block = strdup(buf);
        } else {
            genBinop(e, "xor");
        }
        break;

    case 'y':  /* LSHIFT */
        {
            /* Emit inline shifts using repeated add instructions */
            /* Right operand should be constant shift amount from strength
             * reduction */
            int shift_amount = 0;
            char asm_buf[256];
            int pos = 0;
            int i;

            if (e->right && e->right->op == 'C') {
                shift_amount = (int)e->right->value;
                /* Suppress code generation for constant - already handled */
                xfree(e->right->asm_block);
                e->right->asm_block = noasm;
            }

            /* Build assembly: repeated add */
            asm_buf[0] = '\0';

            for (i = 0; i < shift_amount && i < 16; i++) {  /* cap at 16 */
                if (e->size == 1) {
                    /* Byte shift: add a,a */
                    pos += snprintf(asm_buf + pos, sizeof(asm_buf) - pos, 
                        "\tadd a,a\n");
                } else {
                    /* Word shift: add hl,hl */
                    pos += snprintf(asm_buf + pos, sizeof(asm_buf) - pos, 
                        "\tadd hl,hl\n");
                }
            }

            /* Remove trailing newline */
            if (pos > 0 && asm_buf[pos-1] == '\n') {
                asm_buf[pos-1] = '\0';
            }

            e->asm_block = strdup(asm_buf);
        }
        break;

    case 'w':  /* RSHIFT */
        genBinop(e, "shr");
        break;

    case '>':  /* GT - greater than comparison */
        genBinop(e, "gt");
        break;

    case '<':  /* LT - less than comparison */
        genBinop(e, "lt");
        break;

    case 'g':  /* GE - greater or equal comparison */
        genBinop(e, "ge");
        break;

    case 'L':  /* LE - less or equal comparison */
        genBinop(e, "le");
        break;

    case 'Q':  /* EQ - equality comparison */
        genBinop(e, "eq");
        break;

    case 'n':  /* NEQ - not equal comparison */
        genBinop(e, "ne");
        break;

    case 0xab:  /* SEXT - sign extend */
        /* Sign extend child expression to target size */
        if (e->size == 2 && e->left && e->left->size == 1) {
            /* Byte to word: extend sign bit from A into H */
            /* Child already in A, extend to HL */
            snprintf(buf, sizeof(buf),
                     "\tld l, a\n"
                     "\trlca\n"
                     "\tsbc a, a\n"
                     "\tld h, a");
        } else {
            /* Other size conversions - placeholder for now */
            snprintf(buf, sizeof(buf), "\t; sign extend from %d to %d",
                     e->left ? e->left->size : 0, e->size);
        }
        e->asm_block = strdup(buf);
        break;

    case 'W':  /* WIDEN - zero extend */
        /* Determine source size from child */
        if (e->left) {
            int src_size = e->left->size;
            if (src_size == 1 && e->size == 2) {
                /* byte to word: A -> HL */
                e->asm_block = strdup("\tld l, a\n\tld h, 0");
            } else if (src_size == 1 && e->size == 4) {
                /* byte to long: A -> HL:HL' */
                e->asm_block = strdup("\tld l, a\n\tld h, 0\n\texx\n\tld hl, 0\n\texx");
            } else if (src_size == 2 && e->size == 4) {
                /* word to long: HL -> HL:HL' */
                e->asm_block = strdup("\texx\n\tld hl, 0\n\texx");
            } else {
                snprintf(buf, sizeof(buf), "\t; zero extend %d to %d", src_size, e->size);
                e->asm_block = strdup(buf);
            }
        } else {
            e->asm_block = noasm;
        }
        break;

    case '$':  /* SYM - symbol reference (address) */
        /* Global symbols as values: load address into HL */
        /* Local symbols as values: handled by parent DEREF */
        if (e->symbol) {
            const char *sym_name = stripVarPfx(e->symbol);
            struct local_var *var = findVar(sym_name);
            if (!var) {
                /* Global (extern or static) - load address into HL */
                if (e->symbol[1] == '_') addRefSym(e->symbol + 1);
                snprintf(buf, sizeof(buf), "\tld hl, %s", sym_name);
                e->asm_block = strdup(buf);
            } else {
                /* Local/param - parent DEREF handles load */
                e->asm_block = noasm;
            }
        } else {
            e->asm_block = noasm;
        }
        break;

    case 0x31:  /* OREQ (|=) - handled by specialize(), fallback here */
        snprintf(buf, sizeof(buf), "\t; OREQ fallback size=%d", e->size);
        e->asm_block = strdup(buf);
        break;

    case 0xc6:  /* ANDEQ (&=) - handled by specialize(), fallback here */
        snprintf(buf, sizeof(buf), "\t; ANDEQ fallback size=%d", e->size);
        e->asm_block = strdup(buf);
        break;

    default:
        /* For now, generate placeholder comment for other operators */
        snprintf(buf, sizeof(buf), "\t; op %c (0x%02x) size=%d%s",
                 e->op >= ' ' && e->op <= '~' ? e->op : '?',
                 e->op, e->size,
                 (e->flags & E_UNSIGNED) ? " unsigned" : "");
        e->asm_block = strdup(buf);
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

    /* Increment current_label for each statement to track program points
     * This provides a monotonically increasing sequence for lifetime analysis */
    fnCurLbl++;

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

    /* Initialize lifetime tracking */
    fnCurLbl = 0;

    /* Reset global loop depth at start of each function */
    g_loop_depth = 0;

    generateStmt(fnBody);
}

/*
 * ============================================================================
 * SCHEDULING PASS - Set location/dest fields on expression nodes
 *
 * This is a new pass that will eventually replace asm_block generation.
 * It walks expressions and tags them with:
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
        } else {
            /* Complex dereference - recurse */
            schedExpr(e->left, R_HL);
            e->loc = LOC_INDIR;
            e->reg = R_HL;
        }
        break;

    case '+':
    case '-':
    case '&':
    case '|':
    case '^':
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
        /* Comparisons: left -> DE, right -> HL */
        if (e->size == 1 || (e->left && e->left->size == 1)) {
            schedExpr(e->left, R_A);
            schedExpr(e->right, R_A);
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
        /* LHS is destination, RHS provides value */
        if (e->size == 1) {
            schedExpr(e->right, R_A);
        } else {
            schedExpr(e->right, R_HL);
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
