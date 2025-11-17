/*
 * codegen.c - Code generation phase for cc2
 *
 * Walks expression and statement trees, generating assembly code blocks
 * for each node. This phase builds the asm_block strings but does not
 * emit them - that's done by emit.c.
 *
 * Key responsibilities:
 * - assign_frame_offsets(): Assign stack offsets to local variables and parameters
 * - generate_code(): Walk trees and generate assembly code blocks
 * - allocate_registers(): Allocate variables to registers based on usage patterns
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"

/*
 * Helper: Get the original operand size, looking through SEXT/WIDEN conversions
 */
static int
get_original_size(struct expr *e)
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
is_operand_unsigned(struct expr *e)
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
lookup_var(struct function_ctx *ctx, const char *symbol)
{
    const char *var_name;
    struct local_var *var;

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
    for (var = ctx->locals; var; var = var->next) {
        if (strcmp(var->name, var_name) == 0) {
            return var;
        }
    }

    return NULL;
}

/*
 * Helper: Generate function name for binary arithmetic operations
 * Format: [u]<op><leftwidth><rightwidth>
 * Examples: mul88, umul816, div1616, add168
 */
static void
make_binop_funcname(char *buf, size_t bufsize, const char *opname,
                    struct expr *e)
{
    int left_bits = get_original_size(e->left) * 8;
    int right_bits = get_original_size(e->right) * 8;

    /* Operation is unsigned if either operand is unsigned */
    int is_unsigned = is_operand_unsigned(e->left) || 
        is_operand_unsigned(e->right);
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
add_param(struct function_ctx *ctx, const char *name, unsigned char size, 
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

    fdprintf(2, "  Parameter: %s, size=%d, offset=+%d\n", name, size, offset);
}

/*
 * Helper: Add a local variable to the function context
 * Local variables have negative offsets (below frame pointer)
 */
static void
add_local_var(struct function_ctx *ctx, const char *name, unsigned char size, 
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

    fdprintf(2, "  Local var: %s, size=%d, offset=%d%s\n", 
        name, size, var->offset, is_array ? " (array)" : "");
}

/*
 * Helper: Update variable lifetime tracking
 * Called whenever a variable is used, updates first_label and last_label
 */
static void
update_var_lifetime(struct function_ctx *ctx, const char *name)
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
is_parameter(struct function_ctx *ctx, const char *name)
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
walk_for_locals(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* If this is a declaration, add it to locals list (unless it's a param) */
    if (s->type == 'd' && s->symbol) {
        /* Skip parameter declarations - they already have offsets */
        if (!is_parameter(ctx, s->symbol)) {
            unsigned char size = get_size_from_typename(s->type_str);
            /* Detect arrays: type_str contains ":array:" */
            int is_array = (s->type_str && 
                strstr(s->type_str, ":array:") != NULL) ? 1 : 0;
            add_local_var(ctx, s->symbol, size, is_array);
        }
    }

    /* Recursively walk child statements */
    if (s->then_branch) walk_for_locals(ctx, s->then_branch);
    if (s->else_branch) walk_for_locals(ctx, s->else_branch);
    if (s->next) walk_for_locals(ctx, s->next);
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
allocate_registers(struct function_ctx *ctx)
{
    struct local_var *var;
    int byte_regs_used = 0;  /* Count of byte registers allocated */
    int word_regs_used = 0;  /* Count of word registers allocated */
    int ix_allocated = 0;    /* IX register allocated flag */

    if (!ctx) return;

    fdprintf(2, "=== Phase 2.5: Allocating registers ===\n");

    /* First pass: allocate IX to struct pointer with highest agg_refs */
    {
        struct local_var *best_ix_candidate = NULL;
        int best_agg_refs = 0;

        for (var = ctx->locals; var; var = var->next) {
            /* Skip if not a word/pointer variable */
            if (var->size != 2) continue;
            /* Prefer variables with aggregate member accesses */
            if (var->agg_refs > best_agg_refs) {
                best_agg_refs = var->agg_refs;
                best_ix_candidate = var;
            }
        }

        if (best_ix_candidate && best_agg_refs > 0) {
            best_ix_candidate->reg = REG_IX;
            ix_allocated = 1;
            fdprintf(2, "  Allocated IX to %s (agg_refs=%d)\n",
                     best_ix_candidate->name, best_ix_candidate->agg_refs);
        }
    }

    /* Second pass: allocate byte and word registers by reference count */
    for (var = ctx->locals; var; var = var->next) {
        /* Skip if already allocated */
        if (var->reg != REG_NO) continue;

        /* Skip arrays (they must stay on stack) */
        if (var->is_array) continue;

        /* Skip unused or single-use variables */
        if (var->ref_count <= 1) continue;

        /* Allocate byte registers (B, C, B', C') */
        if (var->size == 1 && byte_regs_used < 4) {
            enum register_id regs[] = {REG_B, REG_C, REG_Bp, REG_Cp};
            var->reg = regs[byte_regs_used];
            byte_regs_used++;
            fdprintf(2, "  Allocated byte reg to %s (refs=%d)\n",
                     var->name, var->ref_count);
        }
        /* Allocate word registers (BC and IX only - BC' excluded due to exx complexity) */
        else if (var->size == 2 && word_regs_used < 2) {
            enum register_id regs[] = {REG_BC, REG_IX};
            /* If IX already allocated to struct pointer, skip it */
            if (word_regs_used == 1 && ix_allocated) {
                /* No more registers available */
                continue;
            }
            var->reg = regs[word_regs_used];
            if (var->reg == REG_IX) {
                ix_allocated = 1;
            }
            word_regs_used++;
            fdprintf(2, "  Allocated word reg to %s (refs=%d)\n",
                     var->name, var->ref_count);
        }
    }

    fdprintf(2, "  Register allocation complete: %d byte, %d word, %d IX\n",
             byte_regs_used, word_regs_used, ix_allocated);
}

/*
 * Phase 1.5: Assign stack frame offsets to all local variables and parameters
 */
void
assign_frame_offsets(struct function_ctx *ctx)
{
    char *p;
    char name_buf[64];
    char type_buf[64];
    int param_offset;
    int i;

    if (!ctx || !ctx->body) return;

    fdprintf(2, "  Assigning stack frame offsets:\n");

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
                    get_size_from_typename(type_buf) : 2;
                add_param(ctx, name_buf, size, param_offset);
                param_offset += size;
            }
        }
    }

    /* Then, assign offsets to local variables (negative offsets below FP) */
    walk_for_locals(ctx, ctx->body);
    fdprintf(2, "  Total frame size: %d bytes\n", ctx->frame_size);
}

/*
 * Generate standard binary operator code
 * Common pattern: move PRIMARY to SECONDARY, call runtime function
 */
static void
gen_binop(struct expr *e, const char *op_name)
{
    char funcname[32];
    char buf[256];
    char *move_inst;

    make_binop_funcname(funcname, sizeof(funcname), op_name, e);

    move_inst = (e->size == 1) ?
        "\tld e, a  ; move PRIMARY to SECONDARY" :
        "\tex de, hl  ; move PRIMARY to SECONDARY";

    snprintf(buf, sizeof(buf), "%s\n\tcall %s", move_inst, funcname);
    e->asm_block = strdup(buf);
}

/*
 * Code generation phase (Phase 2)
 * Walk expression tree and generate assembly code blocks
 *
 * Accumulator Management:
 *   - All expressions evaluate to PRIMARY accumulator (HL for word, A for byte)
 *   - Binary operators: left PRIMARY, move to SECONDARY, right PRIMARY, operate
 *   - M (DEREF) operations generate actual load instructions
 */
static void generate_expr(struct function_ctx *ctx, struct expr *e)
{
    char buf[256];
    struct expr *arg;
    struct expr *args[32];
    int arg_count;
    int i;

    if (!e) return;

    /* Special handling for CALL - need custom code generation */
    if (e->op == '@') {
        /* CALL node: left = function, right = argument chain, value = arg count */
        /* NOTE: Don't do normal traversal - we handle it manually here */
        char call_buf[4096];
        int buf_pos = 0;

        arg_count = e->value;

        /* Collect arguments into array (they're chained via right pointers) */
        arg = e->right;
        for (i = 0; i < arg_count && arg; i++) {
            args[i] = arg;
            arg = arg->right;
        }

        /* Generate code for each child expression first */
        for (i = 0; i < arg_count; i++) {
            generate_expr(ctx, args[i]);
        }
        if (e->left) {
            generate_expr(ctx, e->left);
        }

        /* Now build the call sequence:
         * 1. Push arguments in reverse order (right to left)
         * 2. Call function
         * 3. Clean up stack
         */
        call_buf[0] = '\0';

        /* Push arguments in reverse order */
        for (i = arg_count - 1; i >= 0; i--) {
            /* Emit the argument's code (load into PRIMARY) */
            if (args[i]->asm_block && args[i]->asm_block[0]) {
                buf_pos += snprintf(call_buf + buf_pos, sizeof(call_buf) - buf_pos,
                                   "%s%s\n", buf_pos > 0 ? "" : "", args[i]->asm_block);
            }

            /* Push PRIMARY onto stack */
            if (args[i]->size == 1) {
                /* Byte argument - push as word (Z80 only has 16-bit push) */
                buf_pos += snprintf(call_buf + buf_pos, sizeof(call_buf) - buf_pos,
                                   "\tld l, a\n\tld h, 0\n\tpush hl  ; push byte arg %d\n", i);
            } else {
                /* Word argument */
                buf_pos += snprintf(call_buf + buf_pos, sizeof(call_buf) - buf_pos,
                                   "\tpush hl  ; push arg %d\n", i);
            }
        }

        /* Generate the call instruction */
        if (e->left && e->left->op == '$' && e->left->symbol) {
            buf_pos += snprintf(call_buf + buf_pos, sizeof(call_buf) - buf_pos,
                               "\tcall %s", e->left->symbol);
        } else {
            /* Indirect call through register */
            buf_pos += snprintf(call_buf + buf_pos, sizeof(call_buf) - buf_pos,
                               "\t; TODO: indirect call");
        }

        /* Clean up stack after call (arg_count * 2 bytes per arg) */
        if (arg_count > 0) {
            buf_pos += snprintf(call_buf + buf_pos, sizeof(call_buf) - buf_pos,
                               "\n\tld hl, %d\n\tadd hl, sp\n\tld sp, hl  ; pop %d args",
                               arg_count * 2, arg_count);
        }

        e->asm_block = strdup(call_buf);
        return;  /* Early return - custom traversal done */
    }

    /* Recursively generate code for children (postorder traversal) */
    if (e->left) generate_expr(ctx, e->left);
    if (e->right) generate_expr(ctx, e->right);

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
        update_var_lifetime(ctx, var_name);
    }

    /* Generate assembly code for this node based on operator */
    switch (e->op) {
    case 'C':  /* CONST - load immediate value */
        if (e->size == 1) {
            snprintf(buf, sizeof(buf), "\tld a, %ld", e->value);
        } else if (e->size == 2) {
            snprintf(buf, sizeof(buf), "\tld hl, %ld", e->value);
        } else {  /* size == 4 */
            snprintf(buf, sizeof(buf), "\t; TODO: load long %ld", e->value);
        }
        e->asm_block = strdup(buf);
        break;

    case 'M':  /* DEREF - load from memory */
        /* Check if this is a struct mem access: (M (+ (M:p <var>) <const>)) */
        {
            char *var_symbol = NULL;
            long offset = 0;
            if (is_struct_member_access(e, &var_symbol, &offset)) {
                /* Extract variable name (skip $ and A prefixes) */
                const char *var_name = var_symbol;
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
            }
        }

        /* Generate load instruction to PRIMARY accumulator */
        /* Check if child is a SYM node for simple variable load */
        if (e->left && e->left->op == '$' && e->left->symbol) {
            struct local_var *var = lookup_var(ctx, e->left->symbol);
            if (var && var->reg != REG_NO) {
                /* Variable is in a register - move to PRIMARY */
                if (e->size == 1) {
                    /* Byte: move register to A */
                    if (var->reg == REG_B) {
                        snprintf(buf, sizeof(buf), "\tld a, b");
                    } else if (var->reg == REG_C) {
                        snprintf(buf, sizeof(buf), "\tld a, c");
                    } else {
                        snprintf(buf, sizeof(buf), 
                            "\t; TODO: load byte from reg %d to A", var->reg);
                    }
                } else {
                    /* Word: move register pair to HL */
                    if (var->reg == REG_BC) {
                        snprintf(buf, sizeof(buf), "\tld h, b\n\tld l, c");
                    } else if (var->reg == REG_IX) {
                        snprintf(buf, sizeof(buf), "\tpush ix\n\tpop hl");
                    } else {
                        snprintf(buf, sizeof(buf),
                            "\t; TODO: load word from reg %d to HL", var->reg);
                    }
                }
            } else if (var) {
                /* Variable is on stack - load from (iy + offset) */
                if (e->size == 1) {
                    /* Byte load */
                    if (var->offset >= 0) {
                        snprintf(buf, sizeof(buf), "\tld a, (iy + %d)", 
                            var->offset);
                    } else {
                        snprintf(buf, sizeof(buf), "\tld a, (iy - %d)", 
                            -var->offset);
                    }
                } else {
                    /* Word load */
                    if (var->offset >= 0) {
                        snprintf(buf, sizeof(buf), "\tld hl, (iy + %d)", 
                            var->offset);
                    } else {
                        snprintf(buf, sizeof(buf), "\tld hl, (iy - %d)", 
                            -var->offset);
                    }
                }
            } else {
                /* Variable not found - fallback to placeholder */
                if (e->size == 1) {
                    snprintf(buf, sizeof(buf), 
                        "\t; load byte from %s (not found)", e->left->symbol);
                } else {
                    snprintf(buf, sizeof(buf), 
                        "\t; load word from %s (not found)", e->left->symbol);
                }
            }
        } else {
            /* Complex address expression - placeholder for now */
            if (e->size == 1) {
                snprintf(buf, sizeof(buf), "\t; load byte from address");
            } else if (e->size == 2) {
                snprintf(buf, sizeof(buf), "\t; load word from address");
            } else {
                snprintf(buf, sizeof(buf), "\t; load long from address");
            }
        }
        e->asm_block = strdup(buf);
        break;

    case '=':  /* ASSIGN - store to memory */
        /* Register allocation later, so defer instruction to emit phase */
        /* Just mark an assignment - emit_expr will handle register vs stack */
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
                free_expr(e->right);
                e->right = NULL;
            } else {
                /* Non-constant addition - use general form */
                char *move_inst;
                char *add_inst;

                if (e->size == 1) {
                    /* Byte add - need to call add88 */
                    char funcname[32];
                    make_binop_funcname(funcname, sizeof(funcname), "add", e);
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
        gen_binop(e, "sub");
        break;

    case '*':  /* MUL */
        gen_binop(e, "mul");
        break;

    case '/':  /* DIV */
        gen_binop(e, "div");
        break;

    case '%':  /* MOD */
        gen_binop(e, "mod");
        break;

    case '&':  /* AND */
        gen_binop(e, "and");
        break;

    case '|':  /* OR */
        gen_binop(e, "or");
        break;

    case '^':  /* XOR */
        gen_binop(e, "xor");
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
        gen_binop(e, "shr");
        break;

    case '>':  /* GT - greater than comparison */
        gen_binop(e, "gt");
        break;

    case '<':  /* LT - less than comparison */
        gen_binop(e, "lt");
        break;

    case 'g':  /* GE - greater or equal comparison */
        gen_binop(e, "ge");
        break;

    case 'L':  /* LE - less or equal comparison */
        gen_binop(e, "le");
        break;

    case 'Q':  /* EQ - equality comparison */
        gen_binop(e, "eq");
        break;

    case 'n':  /* NEQ - not equal comparison */
        gen_binop(e, "ne");
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
 * Walk statement tree and generate assembly code blocks
 */
static void generate_stmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* Update current_label for statements that have labels */
    /* This tracks the current position in the control flow for life analysis */
    if (s->label > 0) {
        ctx->current_label = s->label;
    }

    /* Recursively generate code for expressions */
    if (s->expr) generate_expr(ctx, s->expr);
    if (s->expr2) generate_expr(ctx, s->expr2);
    if (s->expr3) generate_expr(ctx, s->expr3);

    /* Recursively generate code for child statements */
    if (s->then_branch) generate_stmt(ctx, s->then_branch);
    if (s->else_branch) generate_stmt(ctx, s->else_branch);
    if (s->next) generate_stmt(ctx, s->next);

    /* TODO: Generate assembly code for this statement */
    /* s->asm_block = malloc(...); strcpy(s->asm_block, "..."); */
}

/*
 * Generate assembly code for entire function
 */
void generate_code(struct function_ctx *ctx)
{
    if (!ctx || !ctx->body) return;

    fdprintf(2, "=== Phase 2: Generating assembly code blocks ===\n");

    /* Initialize lifetime tracking */
    ctx->current_label = 0;

    generate_stmt(ctx, ctx->body);
}


/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
