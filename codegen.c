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

/* Global loop depth for tracking whether we're inside a loop */
static int g_loop_depth = 0;

/* Forward declaration from parseast.c for symbol tracking */
void addRefSym(const char *name);

/*
 * Helper: Get the original operand size, looking through SEXT/WIDEN conversions
 */
static int
getOrigSize(struct expr *e)
{
    if (!e) return 2;  /* default to 16-bit */

    /* Look through SEXT ('X') and WIDEN ('W') to find original size */
    if ((e->op == 'X' || e->op == 'W') && e->left) {
        return e->left->size;
    }

    /* Also check for NARROW ('N') which truncates to smaller size */
    if (e->op == 'N' && e->left) {
        return e->left->size;
    }

    return e->size;
}

/*
 * Helper: Check if operand is unsigned (look through conversions)
 */
static int
isOpndUnsign(struct expr *e)
{
    if (!e) return 0;

    /* WIDEN ('W') means unsigned (zero extend) */
    if (e->op == 'W') return 1;

    /* Otherwise check the expression's flags */
    return (e->flags & E_UNSIGNED) ? 1 : 0;
}

/*
 * Helper: Look up a variable by symbol name
 * Strips leading $ and A prefixes from symbol name before lookup
 * Returns NULL if not found
 */
struct local_var *
findVar(struct function_ctx *ctx, const char *symbol)
{
    const char *var_name;
    struct local_var *var;
    int count = 0;

    if (!symbol || !ctx) return NULL;

    /* Strip $ prefix if present */
    var_name = symbol;
    if (var_name[0] == '$') {
        var_name++;
    }
    /* Strip A prefix for arguments (e.g., $Ax -> x) */
    if (var_name[0] == 'A') {
        var_name++;
    }

    /* Search locals list */
    if (TRACE(T_VAR)) {
        fdprintf(2, "      findVar(%p): looking for '%s' in locals\n", (void*)ctx, var_name);
    }
    for (var = ctx->locals; var; var = var->next) {
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
 * Helper: Add a parameter to the function context
 * Parameters have positive offsets (above frame pointer)
 *
 * Parameters are eligible for register allocation. If allocated to a register,
 * the function prologue will load the parameter from the stack into the 
 * register.
 *
 * Z80 byte parameter stack layout:
 *   - Byte parameters are pushed using "push AF"
 *   - A register contains the data (high byte of the word)
 *   - F register contains flags (low byte of the word)
 *   - On stack: [flags at offset+0, data at offset+1]
 *   - Data is at the HIGHER address within the pushed word
 */
static void
addParam(struct function_ctx *ctx, const char *name, unsigned char size, 
    int offset)
{
    struct local_var *var = malloc(sizeof(struct local_var));
    if (!var) {
        fdprintf(2, "parseast: out of memory allocating local_var\n");
        exit(1);
    }

    var->name = strdup(name);
    var->size = size;
    var->offset = offset;
    var->is_param = 1;
    var->is_array = 0;      /* Params are scalar (array params are pointers) */
    var->first_label = -1;  /* Not used yet */
    var->last_label = -1;   /* Not used yet */
    var->ref_count = 0;     /* Not referenced yet */
    var->agg_refs = 0;      /* No aggregate member accesses yet */
    var->reg = REG_NO;      /* Not allocated to register yet */
    var->next = ctx->locals;

    ctx->locals = var;

    
}

/*
 * Helper: Add a local variable to the function context
 * Local variables have negative offsets (below frame pointer)
 */
static void
addLocalVar(struct function_ctx *ctx, const char *name, unsigned char size, 
    int is_array)
{
    struct local_var *var = malloc(sizeof(struct local_var));
    if (!var) {
        fdprintf(2, "parseast: out of memory allocating local_var\n");
        exit(1);
    }

    var->name = strdup(name);
    var->size = size;
    /* Stack grows downward - assign negative offset from frame pointer */
    var->offset = -(ctx->frame_size + size);
    var->is_param = 0;
    var->is_array = is_array;  /* Arrays cannot be allocated to registers */
    var->first_label = -1;  /* Not used yet */
    var->last_label = -1;   /* Not used yet */
    var->ref_count = 0;     /* Not referenced yet */
    var->agg_refs = 0;      /* No aggregate member accesses yet */
    var->reg = REG_NO;      /* Not allocated to register yet */
    var->next = ctx->locals;

    ctx->locals = var;
    ctx->frame_size += size;

    
}

/*
 * Helper: Update variable lifetime tracking
 * Called whenever a variable is used, updates first_label and last_label
 */
static void
updVarLife(struct function_ctx *ctx, const char *name)
{
    struct local_var *var;

    if (!ctx || !name) return;

    /* Find the variable in locals list */
    for (var = ctx->locals; var; var = var->next) {
        if (strcmp(var->name, name) == 0) {
            /* Update first use if not set */
            if (var->first_label == -1) {
                var->first_label = ctx->current_label;
            }
            /* Always update last use (high water mark) */
            if (ctx->current_label > var->last_label) {
                var->last_label = ctx->current_label;
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
isParameter(struct function_ctx *ctx, const char *name)
{
    struct local_var *var;
    for (var = ctx->locals; var; var = var->next) {
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
walkForLocals(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* If this is a declaration, add it to locals list (unless it's a param) */
    if (s->type == 'd' && s->symbol) {
        /* Skip parameter declarations - they already have offsets */
        if (!isParameter(ctx, s->symbol)) {
            unsigned char size = getSizeFromTN(s->type_str);
            /* Detect arrays: type_str contains ":array:" */
            int is_array = (s->type_str && 
                strstr(s->type_str, ":array:") != NULL) ? 1 : 0;
            addLocalVar(ctx, s->symbol, size, is_array);
        }
    }

    /* Recursively walk child statements */
    if (s->then_branch) walkForLocals(ctx, s->then_branch);
    if (s->else_branch) walkForLocals(ctx, s->else_branch);
    if (s->next) walkForLocals(ctx, s->next);
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
allocRegs(struct function_ctx *ctx)
{
    struct local_var *var;
    int byteRegsUsed = 0;  /* Count of byte registers allocated */
    int wordRegsUsed = 0;  /* Count of word registers allocated */
    int ix_allocated = 0;    /* IX register allocated flag */

    if (!ctx) return;

    /* First pass: allocate IX to struct pointer with highest agg_refs */
    {
        struct local_var *best_ix_cand = NULL;
        int best_agg_refs = 0;

        for (var = ctx->locals; var; var = var->next) {
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
    for (var = ctx->locals; var; var = var->next) {
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
    for (var = ctx->locals; var; var = var->next) {
        /* Skip if already allocated */
        if (var->reg != REG_NO) continue;

        /* Skip arrays (they must stay on stack) */
        if (var->is_array) continue;

        /* Skip unused or single-use variables */
        if (var->ref_count <= 1) continue;

        /* Allocate byte registers (B, C, B', C')
         * B' and C' work with A (8-bit primary) since exx doesn't swap A
         * Cannot allocate B or C if BC is already allocated (they conflict)
         * B' and C' can always be allocated (no conflicts)
         */
        if (var->size == 1 && byteRegsUsed < 4) {
            enum register_id regs[] = {REG_B, REG_C, REG_Bp, REG_Cp};
            var->reg = regs[byteRegsUsed];
            byteRegsUsed++;
            
        }
    }

    
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
    if (v1->first_label == -1 || v2->first_label == -1) {
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
optFrmLayout(struct function_ctx *ctx)
{
    struct stackSlot *slots = NULL;
    int num_slots = 0;
    int slot_capacity = 0;
    struct local_var *var;
    int newFrameSize;
    int i, j;
    int slot_idx;
    int found_slot;

    if (!ctx) return;

    

    /* Build slot assignments for each local variable */
    for (var = ctx->locals; var; var = var->next) {
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
    
    ctx->frame_size = newFrameSize;

    /* Check frame size limit - IY-indexed addressing uses signed 8-bit offsets
     * Local variables use negative offsets from IY, range is -1 to -128
     * Limit to 127 bytes to ensure all locals fit within addressing range */
    if (ctx->frame_size > 127) {
        fdprintf(2, "ERROR: Function %s has %d bytes of local variables after optimization (max 127)\n",
                 ctx->name, ctx->frame_size);
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
assignFrmOff(struct function_ctx *ctx)
{
    char *p;
    char name_buf[64];
    char type_buf[64];
    int param_offset;
    int i;

    if (!ctx || !ctx->body) return;

    

    /* First, assign offsets to parameters (positive offsets above FP) */
    /* Stack layout: FP+0=saved FP, FP+2=return addr, FP+4=first param */
    param_offset = 4;  /* Skip saved FP (2 bytes) + return address (2 bytes) */

    if (ctx->params && ctx->params[0]) {
        p = ctx->params;
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
                addParam(ctx, name_buf, size, param_offset);
                param_offset += size;
            }
        }
    }

    /* Then, assign offsets to local variables (negative offsets below FP) */
    walkForLocals(ctx, ctx->body);
    
}

/*
 * Generate standard binary operator code
 * Common pattern: move PRIMARY to SECONDARY, call runtime function
 */
static void
genBinop(struct expr *e, const char *op_name)
{
    char funcname[32];
    char buf[256];
    char *move_inst;

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
static void generateExpr(struct function_ctx *ctx, struct expr *e)
{
    char buf[256];
    struct expr *arg;
    struct expr *args[32];
    int arg_count;
    int i;

    if (!e) return;
    expr_count++;
    if (expr_count > 100000) {
        fdprintf(2, "generateExpr: exceeded 100000 calls, op=%c\n", e->op);
        exit(1);
    }

    /* Special handling for CALL - emitCall handles it directly from AST */
    if (e->op == '@') {
        arg_count = e->value;

        /* Optimize constant arguments: if constant fits in byte, use byte */
        arg = e->right;
        for (i = 0; i < arg_count && arg; i++) {
            if (arg->left && arg->left->op == 'C' &&
                arg->left->value >= 0 && arg->left->value <= 255) {
                arg->left->size = 1;
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
        if (e->left) generateExpr(ctx, e->left);  /* condition */
        if (e->right && e->right->left) generateExpr(ctx, e->right->left);   /* true expr */
        if (e->right && e->right->right) generateExpr(ctx, e->right->right); /* false expr */

        /* Allocate labels for branches */
        e->label = ctx->labelCounter++;  /* false_label */
        if (e->right) {
            e->right->label = ctx->labelCounter++;  /* end_label */
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

    /* Check for struct member access patterns BEFORE processing children */
    /* (tree may be modified during processing, e.g., ADD frees constant operands) */

    /* Pattern 1: (= (+ (M:p $var) const) value) - struct member write */
    if (e->op == '=' && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str && strcmp(e->left->left->type_str, ":p") == 0 &&
        e->left->left->left && e->left->left->left->op == '$' &&
        e->left->left->left->symbol &&
        e->left->right && e->left->right->op == 'C') {
        /* Increment agg_refs and save info for emit phase */
        const char *var_symbol = e->left->left->left->symbol;
        long offset = e->left->right->value;
        const char *var_name = var_symbol;
        if (var_name && var_name[0] == '$') {
            var_name++;  /* Skip $ prefix */
        }
        if (var_name && var_name[0] == 'A') {
            var_name++;  /* Skip A prefix for arguments */
        }
        if (var_name) {
            struct local_var *var;
            for (var = ctx->locals; var; var = var->next) {
                if (strcmp(var->name, var_name) == 0) {
                    var->agg_refs++;
                    break;
                }
            }
        }
        /* Mark this ASSIGN for IX-indexed optimization in emit phase */
        e->flags |= 1;  /* Use flags field to mark struct member assignment */
        e->value = offset;  /* Store offset in value field */
    }

    /* Pattern 2: (M (+ (M:p $var) const)) - struct member read */
    if (e->op == 'M' && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str && strcmp(e->left->left->type_str, ":p") == 0 &&
        e->left->left->left && e->left->left->left->op == '$' &&
        e->left->left->left->symbol &&
        e->left->right && e->left->right->op == 'C') {
        /* Increment agg_refs and create placeholder for IX-indexed load */
        const char *var_symbol = e->left->left->left->symbol;
        long offset = e->left->right->value;
        const char *var_name = var_symbol;
        if (var_name && var_name[0] == '$') {
            var_name++;  /* Skip $ prefix */
        }
        if (var_name && var_name[0] == 'A') {
            var_name++;  /* Skip A prefix for arguments */
        }
        if (var_name) {
            struct local_var *var;
            for (var = ctx->locals; var; var = var->next) {
                if (strcmp(var->name, var_name) == 0) {
                    var->agg_refs++;
                    break;
                }
            }
        }
        /* Store placeholder info for emit phase - mark with flags and value */
        e->flags |= 2;  /* Use bit 2 for DEREF struct member access */
        e->value = offset;  /* Store offset */
    }

    /* Handle increment/decrement operators BEFORE recursion */
    /* These operators need custom handling - don't process left child normally */
    if (e->op == 0xcf || e->op == 0xef || e->op == 0xd6 || e->op == 0xf6) {
        char buf[512];
        int is_post = (e->op == 0xef || e->op == 0xf6);
        int is_dec = (e->op == 0xd6 || e->op == 0xf6);
        const char *incdec = is_dec ? "dec" : "inc";
        int unused = (e->flags & E_UNUSED) ? 1 : 0;

        /* Check for simple variable (SYM node) */
        if (e->left && e->left->op == '$' && e->left->symbol) {

        /* Track variable usage for lifetime analysis */
        {
            const char *var_name = e->left->symbol;

            if (var_name && var_name[0] == '$') var_name++;
            if (var_name && var_name[0] == 'A') var_name++;
            updVarLife(ctx, var_name);
        }

        /* Use placeholder to defer code generation to emit phase */
        /* This allows register allocation to happen first, so emit.c can */
        /* generate optimal code based on whether variable is in register */
        /* Placeholder format: INCDEC_PLACEHOLDER:op:size:amount:unused:symbol */
        snprintf(buf, sizeof(buf), "INCDEC_PLACEHOLDER:%d:%d:%ld:%d:%s",
                 (int)e->op, e->size, e->value, unused, e->left->symbol);
        e->asm_block = strdup(buf);

            /* Free children manually since we didn't process them normally */
            /* NULL out pointers so emit phase doesn't try to process freed nodes */
            freeExpr(e->left);
            e->left = NULL;
            freeExpr(e->right);
            e->right = NULL;
            return;  /* Early return - skip normal traversal */
        } else if (e->left && e->left->op == 'M') {
            /* Complex lvalue: dereferenced pointer, array element, struct member */
            /* Pattern: (INC/DEC (DEREF addr)) */
            /* Generate address, load, add/sub amount, store */
            long amount = e->value;  /* Increment amount from AST */

            generateExpr(ctx, e->left->left);  /* Generate address -> PRIMARY (HL) */

            if (e->size == 1) {
                /* Byte increment/decrement */
                if (is_post && !(e->flags & E_UNUSED)) {
                    /* Postfix with used result - need to save old value */
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)  ; load old value\n"
                            "\tld c, a  ; save old value\n"
                            "\t%s a\n"
                            "\tld (de), a  ; store new value\n"
                            "\tld a, c  ; return old value",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)  ; load old value\n"
                            "\tld c, a  ; save old value\n"
                            "\t%s a, %ld\n"
                            "\tld (de), a  ; store new value\n"
                            "\tld a, c  ; return old value",
                            is_dec ? "sub" : "add", amount);
                    }
                } else {
                    /* Prefix, or postfix with unused result - simple increment */
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)  ; load value\n"
                            "\t%s a\n"
                            "\tld (de), a  ; store new value",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)  ; load value\n"
                            "\t%s a, %ld\n"
                            "\tld (de), a  ; store new value",
                            is_dec ? "sub" : "add", amount);
                    }
                }
            } else {
                /* Word increment/decrement */
                if (is_post && !(e->flags & E_UNUSED)) {
                    /* Postfix with used result - need to save old value */
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)\n"
                            "\tld c, a  ; save old low\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a  ; save old high\n"
                            "\tld h, b\n"
                            "\tld l, c  ; old value to HL\n"
                            "\tpush hl  ; save old value\n"
                            "\t%s hl\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a  ; store new low\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a  ; store new high\n"
                            "\tpop hl  ; return old value",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)\n"
                            "\tld c, a  ; save old low\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a  ; save old high\n"
                            "\tld h, b\n"
                            "\tld l, c  ; old value to HL\n"
                            "\tpush hl  ; save old value\n"
                            "\tld bc, %ld\n"
                            "\t%s hl, bc\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a  ; store new low\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a  ; store new high\n"
                            "\tpop hl  ; return old value",
                            amount, is_dec ? "or a\n\tsbc" : "add");
                    }
                } else {
                    /* Prefix, or postfix with unused result - simple increment */
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a\n"
                            "\tld h, b\n"
                            "\tld l, c  ; value to HL\n"
                            "\t%s hl\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a  ; store new low\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a  ; store new high",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h  ; save address\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a\n"
                            "\tld h, b\n"
                            "\tld l, c  ; value to HL\n"
                            "\tld bc, %ld\n"
                            "\t%s hl, bc\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a  ; store new low\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a  ; store new high",
                            amount, is_dec ? "or a\n\tsbc" : "add");
                    }
                }
            }

            e->asm_block = strdup(buf);
            /* Don't free left child - already processed */
            e->left = NULL;
            freeExpr(e->right);
            e->right = NULL;
            return;
        } else if (e->left && e->left->op == '+') {
            /* ADD expression: struct member or array element */
            /* Pattern: (INC/DEC (+ base offset)) */
            /* This is already an address - just load, add/sub amount, store */
            long amount = e->value;  /* Increment amount from AST */

            generateExpr(ctx, e->left);  /* Generate address -> PRIMARY (HL) */

            if (e->size == 1) {
                /* Byte */
                if (is_post) {
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\t%s a\n"
                            "\tld (de), a\n"
                            "\tld a, c",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\t%s a, %ld\n"
                            "\tld (de), a\n"
                            "\tld a, c",
                            is_dec ? "sub" : "add", amount);
                    }
                } else {
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\t%s a\n"
                            "\tld (de), a",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\t%s a, %ld\n"
                            "\tld (de), a",
                            is_dec ? "sub" : "add", amount);
                    }
                }
            } else {
                /* Word */
                if (is_post) {
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a\n"
                            "\tld h, b\n"
                            "\tld l, c\n"
                            "\tpush hl\n"
                            "\t%s hl\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a\n"
                            "\tpop hl",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a\n"
                            "\tld h, b\n"
                            "\tld l, c\n"
                            "\tpush hl\n"
                            "\tld bc, %ld\n"
                            "\t%s hl, bc\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a\n"
                            "\tpop hl",
                            amount, is_dec ? "or a\n\tsbc" : "add");
                    }
                } else {
                    if (amount == 1) {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a\n"
                            "\tld h, b\n"
                            "\tld l, c\n"
                            "\t%s hl\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a",
                            incdec);
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\tld e, l\n"
                            "\tld d, h\n"
                            "\tld a, (hl)\n"
                            "\tld c, a\n"
                            "\tinc hl\n"
                            "\tld a, (hl)\n"
                            "\tld b, a\n"
                            "\tld h, b\n"
                            "\tld l, c\n"
                            "\tld bc, %ld\n"
                            "\t%s hl, bc\n"
                            "\tex de, hl\n"
                            "\tld a, l\n"
                            "\tld (de), a\n"
                            "\tinc de\n"
                            "\tld a, h\n"
                            "\tld (de), a",
                            amount, is_dec ? "or a\n\tsbc" : "add");
                    }
                }
            }

            e->asm_block = strdup(buf);
            e->left = NULL;
            freeExpr(e->right);
            e->right = NULL;
            return;
        } else {
            /* Unsupported lvalue type */
            snprintf(buf, sizeof(buf), "\t; TODO: inc/dec unsupported lvalue");
            e->asm_block = strdup(buf);
            freeExpr(e->left);
            freeExpr(e->right);
            return;
        }
    }

    /* Recursively generate code for children (postorder traversal) */
    if (e->left) generateExpr(ctx, e->left);
    if (e->right) generateExpr(ctx, e->right);

    /* Track variable usage for lifetime analysis */
    if (e->op == '$' && e->symbol) {
        /* Skip the leading $ to match variable names in locals list */
        const char *var_name = e->symbol;
        if (var_name[0] == '$') {
            var_name++;  /* Skip $ prefix */
        }
        /* Also skip 'A' prefix for arguments (e.g., $Ax -> x) */
        if (var_name[0] == 'A') {
            var_name++;  /* Skip A prefix */
        }
        updVarLife(ctx, var_name);
    }

    /* Generate assembly code for this node based on operator */
    switch (e->op) {
    case 'C':  /* CONST - load immediate value */
        if (e->size == 1) {
            snprintf(buf, sizeof(buf), "\tld a, %ld", e->value);
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
        /* Check if this is a struct mem access: (M (+ (M:p <var>) <const>)) */
        {
            char *var_symbol;
            long offset;
            const char *var_name;

            var_symbol = NULL;
            offset = 0;
            if (isStructMem(e, &var_symbol, &offset)) {
                /* Extract variable name (skip $ and A prefixes) */
                var_name = var_symbol;
                if (var_name && var_name[0] == '$') {
                    var_name++;  /* Skip $ prefix */
                }
                if (var_name && var_name[0] == 'A') {
                    var_name++;  /* Skip A prefix for arguments */
                }
                /* Increment aggregate reference count for this variable */
                if (var_name) {
                    struct local_var *var;
                    for (var = ctx->locals; var; var = var->next) {
                        if (strcmp(var->name, var_name) == 0) {
                            var->agg_refs++;
                            break;
                        }
                    }
                }

                /* Create placeholder for emit phase to check IX allocation */
                snprintf(buf, sizeof(buf), "DEREF_IXOFS:%s:%ld", var_symbol, offset);
                e->asm_block = strdup(buf);
                break;
            }
        }

        /* Register allocation happens later, so defer instruction to emit phase */
        /* Check if child is a SYM node for simple variable load */
        if (e->left && e->left->op == '$' && e->left->symbol) {
            /* Placeholder - emit phase will check register allocation */
            snprintf(buf, sizeof(buf), "DEREF_PLACEHOLDER:%s", e->left->symbol);
        }
        /* Check for pattern M:b(M:p($sym)) - indirect byte load through pointer */
        else if (e->size == 1 && e->left && e->left->op == 'M' &&
                 e->left->size == 2 && /* inner is pointer load */
                 e->left->left && e->left->left->op == '$' &&
                 e->left->left->symbol) {
            /* Defer to emit phase - if pointer is register-allocated, use (bc)/(ix) */
            snprintf(buf, sizeof(buf), "DEREF_INDIRECT_B:%s", e->left->left->symbol);
        }
        else {
            /* Complex address expression - address will be in PRIMARY (HL) */
            if (e->size == 1) {
                /* Load byte from (HL) */
                snprintf(buf, sizeof(buf), "\tld a, (hl)");
            } else if (e->size == 2) {
                /* Load word from (HL) - need sequence to preserve address */
                snprintf(buf, sizeof(buf),
                    "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl");
            } else {
                /* Load long from (HL) - call load32i */
                snprintf(buf, sizeof(buf), "\tcall load32i");
            }
        }
        e->asm_block = strdup(buf);
        break;

    case '=':  /* ASSIGN - store to memory */
        /* Register allocation later, so defer instruction to emit phase */
        /* Just mark an assignment - emitExpr will handle register vs stack */
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

                if (op_size == 1) {
                    /* Byte add with constant - use immediate add */
                    snprintf(buf, sizeof(buf),
                        "\tadd a, %ld  ; add constant to byte", const_val);
                } else {
                    /* Word add with constant 1-4 - use repeated inc hl */
                    if (const_val >= 1 && const_val <= 4) {
                        buf[0] = '\0';
                        for (i = 0; i < const_val; i++) {
                            if (i > 0) strcat(buf, "\n");
                            strcat(buf, "\tinc hl");
                        }
                        strcat(buf, "  ; add small constant");
                    } else {
                        /* Word add with larger constant - load and add */
                        snprintf(buf, sizeof(buf),
                            "\tld de, %ld\n\tadd hl, de", const_val);
                    }
                }
                e->asm_block = strdup(buf);

                /* Free and clear right operand to prevent emission */
                freeExpr(e->right);
                e->right = NULL;
            } else {
                /* Non-constant addition - use general form */
                char *move_inst;
                char *add_inst;

                if (e->size == 1) {
                    /* Byte add - need to call add88 */
                    char funcname[32];
                    mkBinopFnName(funcname, sizeof(funcname), "add", e);
                    move_inst = "\tld e, a  ; move PRIMARY (A) to SECONDARY (E)";
                    snprintf(buf, sizeof(buf), "%s\n\tcall %s",
                        move_inst, funcname);
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
        if (e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
            e->right->value >= 0 && e->right->value <= 255) {
            /* Byte AND with immediate: use inline "and <imm>" instruction */
            snprintf(buf, sizeof(buf), "\tand %ld", e->right->value & 0xFF);
            e->asm_block = strdup(buf);
        } else {
            genBinop(e, "and");
        }
        break;

    case '|':  /* OR */
        /* Optimize byte OR with small constant to inline instruction */
        if (e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
            e->right->value >= 0 && e->right->value <= 255) {
            /* Byte OR with immediate: use inline "or <imm>" instruction */
            snprintf(buf, sizeof(buf), "\tor %ld", e->right->value & 0xFF);
            e->asm_block = strdup(buf);
        } else {
            genBinop(e, "or");
        }
        break;

    case '^':  /* XOR */
        /* Optimize byte XOR with small constant to inline instruction */
        if (e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
            e->right->value >= 0 && e->right->value <= 255) {
            /* Byte XOR with immediate: use inline "xor <imm>" instruction */
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
                if (e->right->asm_block) {
                    free(e->right->asm_block);
                }
                e->right->asm_block = strdup("");
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

    case 0xb6:  /* WIDEN - zero extend */
        snprintf(buf, sizeof(buf), "\t; zero extend to size %d", e->size);
        e->asm_block = strdup(buf);
        break;

    case '$':  /* SYM - symbol reference (address) */
        /* SYM nodes don't generate code directly */
        /* Parent operations (M, =, etc.) will use the symbol information */

        /* Track global variable references (for EXTERN declarations) */
        /* Only track if it's a global (starts with $_), not a local (starts with $) */
        if (e->symbol && e->symbol[0] == '$' && e->symbol[1] == '_') {
            const char *sym_name = e->symbol + 1;  /* Strip leading $ */
            addRefSym(sym_name);
        }

        e->asm_block = strdup("");
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
    int len = 0;
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
        if (j->condition) free(j->condition);
        free(j);
        j = next;
    }
}

/*
 * Walk statement tree and generate assembly code blocks
 */
static void generateStmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* Increment current_label for each statement to track program points
     * This provides a monotonically increasing sequence for lifetime analysis */
    ctx->current_label++;

    /* Track loop depth globally for call cleanup optimization */
    if (s->type == 'L' && s->symbol) {
        if (strstr(s->symbol, "_top")) {
            g_loop_depth++;
        } else if (strstr(s->symbol, "_break")) {
            g_loop_depth--;
        }
    }

    /* Recursively generate code for expressions */
    if (s->expr) generateExpr(ctx, s->expr);
    if (s->expr2) generateExpr(ctx, s->expr2);
    if (s->expr3) generateExpr(ctx, s->expr3);

    /* Recursively generate code for child statements */
    if (s->then_branch) generateStmt(ctx, s->then_branch);
    if (s->else_branch) generateStmt(ctx, s->else_branch);
    if (s->next) generateStmt(ctx, s->next);

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
void generateCode(struct function_ctx *ctx)
{
    if (!ctx || !ctx->body) return;

    /* Initialize lifetime tracking */
    ctx->current_label = 0;

    /* Reset global loop depth at start of each function */
    g_loop_depth = 0;

    generateStmt(ctx, ctx->body);
}


/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
