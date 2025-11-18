/*
 * emit.c - Code emission phase for cc2
 *
 * Walks expression and statement trees, emitting assembly code and freeing nodes.
 * This phase outputs the actual assembly code that was generated in codegen.c.
 *
 * Key responsibilities:
 * - emit_assembly(): Main entry point - emit function assembly and free tree
 * - emitExpr()/emitStmt(): Walk trees, emit assembly, free nodes
 * - emitFunctionPrologue(): Output function header and variable metadata
 *
 * Register Architecture (Stack Machine Model):
 * ============================================
 * The code generator implements a stack machine where the top two stack elements
 * are kept in registers:
 * - PRIMARY accumulator (HL for words, A for bytes) - top of stack
 * - SECONDARY accumulator (DE for words, E for bytes) - second element
 *
 * For binary operations (a + b):
 *   1. Evaluate left operand -> PRIMARY
 *   2. Move PRIMARY to SECONDARY (spill to register)
 *   3. Evaluate right operand -> PRIMARY
 *   4. Operate on SECONDARY and PRIMARY
 *
 * Nested Binary Operations - Spilling Strategy:
 * ----------------------------------------------
 * For nested expressions like (a + b) + c, the right operand (c) evaluation may
 * itself contain binary operations that would clobber SECONDARY. We implement
 * different spilling strategies for byte vs word operations:
 *
 * BYTE OPERATIONS (8-bit):
 *   SECONDARY is E register. Spilling hierarchy:
 *   1. First level: E -> D (use D as temp storage)
 *   2. Second level: DE -> stack (if D already in use)
 *   Track: ctx->d_in_use flag
 *
 * WORD OPERATIONS (16-bit):
 *   SECONDARY is DE register pair. Spilling:
 *   - Always: DE -> stack (can't split the pair)
 *   Track: ctx->de_save_count counter
 *
 * Example (word): (a + b) + (c + d)
 *   Left child (a + b):
 *     eval a -> PRIMARY (HL)
 *     move PRIMARY to SECONDARY (DE)
 *     eval b -> PRIMARY (HL)
 *     add HL, DE -> PRIMARY (HL)
 *   Main operation:
 *     move PRIMARY to SECONDARY (DE)
 *     push DE (save result of a+b to stack)
 *     Right child (c + d):
 *       eval c -> PRIMARY (HL)
 *       move PRIMARY to SECONDARY (DE, would clobber a+b)
 *       eval d -> PRIMARY (HL)
 *       add HL, DE -> PRIMARY (HL)
 *     pop DE (restore result of a+b from stack)
 *     add HL, DE -> PRIMARY (HL)
 *
 * Example (byte): (a + b) + (c + d)
 *   Left child (a + b):
 *     eval a -> PRIMARY (A)
 *     move PRIMARY to SECONDARY (E)
 *     eval b -> PRIMARY (A)
 *     add A, E -> PRIMARY (A)
 *   Main operation:
 *     move PRIMARY to SECONDARY (E)
 *     ld D, E (save result of a+b to D register)
 *     Right child (c + d):
 *       eval c -> PRIMARY (A)
 *       move PRIMARY to SECONDARY (E, would clobber a+b)
 *       eval d -> PRIMARY (A)
 *       add A, E -> PRIMARY (A)
 *     ld E, D (restore result of a+b from D register)
 *     add A, E -> PRIMARY (A)
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"

static const char *
getRegisterName(enum register_id reg)
{
    switch (reg) {
        case REG_NO:  return NULL;
        case REG_B:   return "B";
        case REG_C:   return "C";
        case REG_Bp:  return "B'";
        case REG_Cp:  return "C'";
        case REG_BC:  return "BC";
        case REG_BCp: return "BC'";
        case REG_IX:  return "IX";
        default:      return "???";
    }
}

/*
 * Strip leading $ from symbol name for assembly output
 * Returns pointer into the string, skipping the $ if present
 */
static const char *
stripDollar(const char *symbol)
{
    if (symbol && symbol[0] == '$') {
        return symbol + 1;
    }
    return symbol;
}

/*
 * Emit function prologue
 * Outputs assembly comment describing function and function label
 */
static void
emitFunctionPrologue(char *name, char *params, char *rettype, int frame_size,
                       struct local_var *locals)
{
    int has_params = (params && params[0]);
    struct local_var *var;

    /* Assembly comment with function signature */
    fdprintf(outFd, "; Function: %s", name);

    if (has_params) {
        fdprintf(outFd, "(%s)", params);
    } else {
        fdprintf(outFd, "()");
    }

    if (rettype && rettype[0]) {
        fdprintf(outFd, " -> %s", rettype);
    }

    fdprintf(outFd, "\n");

    /* Output local variable information as assembly comments */
    if (locals) {
        fdprintf(outFd, "; Local variables:\n");
        for (var = locals; var; var = var->next) {
            const char *regname = getRegisterName(var->reg);

            /* Build offset string */
            char offset_str[32];
            if (var->offset >= 0) {
                snprintf(offset_str, sizeof(offset_str), "(iy+%d)", 
                    var->offset);
            } else {
                snprintf(offset_str, sizeof(offset_str), "(iy%d)", 
                    var->offset);
            }

            fdprintf(outFd, ";   %s: ", var->name);
            if (var->first_label == -1) {
                if (regname) {
                    fdprintf(outFd, 
                        "unused (0 refs, 0 agg_refs, %s, reg=%s)\n",
                        offset_str, regname);
                } else {
                    fdprintf(outFd, 
                        "unused (0 refs, 0 agg_refs, %s)\n", offset_str);
                }
            } else {
                if (regname) {
                    fdprintf(outFd, 
                        "labels %d-%d (%d refs, %d agg_refs, %s, reg=%s)\n",
                        var->first_label, var->last_label, var->ref_count, 
                        var->agg_refs, offset_str, regname);
                } else {
                    fdprintf(outFd, 
                        "labels %d-%d (%d refs, %d agg_refs, %s)\n",
                        var->first_label, var->last_label, var->ref_count, 
                        var->agg_refs, offset_str);
                }
            }
        }
    }

    /* Function label (standard C naming with underscore prefix) */
    fdprintf(outFd, "_%s:\n", name);

    /* Emit frame allocation call if we have locals or parameters */
    if (frame_size > 0 || has_params) {
        fdprintf(outFd, "\tld a, %d\n", frame_size);
        fdprintf(outFd, "\tcall framealloc\n");
    }

    /* Load register-allocated parameters from stack into registers
     *
     * After framealloc, parameters are accessible via IY-indexed addressing.
     * Parameters that were allocated to registers during register allocation
     * must be loaded from their stack locations into those registers.
     *
     * This is done after frame setup because:
     * - Parameters are initially passed on stack
     * - Register allocation decides which params get registers
     * - We load them here so rest of function can use register access
     *
     * Note: Z80 has no direct "ld IX, (IY+offset)" instruction, so we must
     * load through HL first using push/pop to transfer to IX.
     */
    for (var = locals; var; var = var->next) {
        if (var->is_param && var->reg != REG_NO) {
            /* This parameter has been allocated to a register */
            if (var->size == 2) {
                /* Word parameter - load from (iy + offset) */
                if (var->reg == REG_BC) {
                    fdprintf(outFd, "\tld l, (iy + %d)\n", var->offset);
                    fdprintf(outFd, "\tld h, (iy + %d)\n", var->offset + 1);
                    fdprintf(outFd, "\tld b, h\n\tld c, l\n");
                } else if (var->reg == REG_IX) {
                    /* No direct ld ix, (iy+offset) - must go through HL */
                    fdprintf(outFd, "\tld l, (iy + %d)\n", var->offset);
                    fdprintf(outFd, "\tld h, (iy + %d)\n", var->offset + 1);
                    fdprintf(outFd, "\tpush hl\n\tpop ix\n");
                }
            } else if (var->size == 1) {
                /* Byte parameter */
                if (var->reg == REG_B || var->reg == REG_C) {
                    fdprintf(outFd, "\tld a, (iy + %d)\n", var->offset);
                    if (var->reg == REG_B) {
                        fdprintf(outFd, "\tld b, a\n");
                    } else {
                        fdprintf(outFd, "\tld c, a\n");
                    }
                }
            }
        }
    }
}

/*
 * Helper: Check if operator is a binary operator that needs accumulator
 * management
 */
static int
isBinopWithAccum(unsigned char op)
{
    switch (op) {
    case '+': case '-': case '*': case '/': case '%':  /* Arithmetic */
    case '&': case '|': case '^': case 'w':            /* Bitwise (w=RSHIFT) */
    case '>': case '<': case 'g': case 'L':            /* Comparisons */
    case 'Q': case 'n':                                /* Equality */
        return 1;
    default:
        return 0;
    }
}

/*
 * Helper: Check if expression tree contains any binary operations
 * Used to determine if we need to save secondary register (DE)
 */
static int
contains_binop(struct expr *e)
{
    if (!e) return 0;

    /* Check if this node is a binop */
    if (isBinopWithAccum(e->op)) return 1;

    /* Recursively check children */
    if (contains_binop(e->left)) return 1;
    if (contains_binop(e->right)) return 1;

    return 0;
}

/*
 * Emission phase (Phase 3)
 * Walk expression tree, emit assembly, and free nodes
 *
 * Binary operators need special handling to emit accumulator move between
 * children:
 *   1. Emit left child (result in PRIMARY)
 *   2. Emit move instruction (PRIMARY to SECONDARY)
 *   3. Emit right child (result in PRIMARY)
 *   4. Emit call instruction (operates on SECONDARY and PRIMARY)
 *
 * Assignment operators need register information from context.
 */
static void emitExpr(struct function_ctx *ctx, struct expr *e)
{
    if (!e) return;

    /* Handle DEREF specially - need to check register allocation */
    if (e->op == 'M' && e->asm_block &&
            strstr(e->asm_block, "DEREF_PLACEHOLDER")) {
        /* Look up variable BEFORE emitting child (which frees it) */
        struct local_var *var = NULL;
        if (e->left && e->left->op == '$' && e->left->symbol) {
            var = findVar(ctx, e->left->symbol);
        }

        /* Emit child expression first (SYM nodes emit nothing and are freed) */
        emitExpr(ctx, e->left);

        /* Now emit the load inst based on current register allocation */
        if (var) {
            if (var->reg != REG_NO) {
                /* Variable is in a register - move to PRIMARY */
                if (e->size == 1) {
                    /* Byte: move register to A */
                    if (var->reg == REG_B) {
                        fdprintf(outFd, "\tld a, b\n");
                    } else if (var->reg == REG_C) {
                        fdprintf(outFd, "\tld a, c\n");
                    } else if (var->reg == REG_Bp) {
                        fdprintf(outFd, "\texx\n\tld a, b\n\texx\n");
                    } else if (var->reg == REG_Cp) {
                        fdprintf(outFd, "\texx\n\tld a, c\n\texx\n");
                    }
                } else {
                    /* Word: move register pair to HL */
                    if (var->reg == REG_BC) {
                        fdprintf(outFd, "\tld h, b\n\tld l, c\n");
                    } else if (var->reg == REG_IX) {
                        fdprintf(outFd, "\tpush ix\n\tpop hl\n");
                    }
                }
            } else {
                /* Variable is on stack - load from (iy + offset) */
                if (e->size == 1) {
                    if (var->offset >= 0) {
                        fdprintf(outFd, "\tld a, (iy + %d)\n", var->offset);
                    } else {
                        fdprintf(outFd, "\tld a, (iy - %d)\n", -var->offset);
                    }
                } else if (e->size == 2) {
                    if (var->offset >= 0) {
                        fdprintf(outFd, "\tld l, (iy + %d)\n", var->offset);
                        fdprintf(outFd, "\tld h, (iy + %d)\n", var->offset + 1);
                    } else {
                        /* Negative offset: high byte is at less negative offset */
                        fdprintf(outFd, "\tld l, (iy - %d)\n", -var->offset);
                        fdprintf(outFd, "\tld h, (iy - %d)\n", -var->offset - 1);
                    }
                } else {
                    /* Long (4 bytes) - use getlong function */
                    fdprintf(outFd, "\tld a, %d\n", var->offset);
                    fdprintf(outFd, "\tcall getlong\n");
                }
            }
        } else if (e->left && e->left->symbol) {
            /* Global variable - direct memory access */
            const char *sym = stripDollar(e->left->symbol);
            if (e->size == 1) {
                fdprintf(outFd, "\tld a, (%s)\n", sym);
            } else if (e->size == 2) {
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
            } else if (e->size == 4) {
                /* Long - load HL'HL from global */
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
                fdprintf(outFd, "\texx\n");
                fdprintf(outFd, "\tld hl, (%s+2)\n", sym);
                fdprintf(outFd, "\texx\n");
            }
        }

        /* Free this node and return */
        if (e->asm_block) free(e->asm_block);
        if (e->cleanup_block) free(e->cleanup_block);
        free(e);
        return;
    }
    /* Handle DEREF with struct member access (marked by flags in codegen) */
    else if (e->op == 'M' && (e->flags & 2) && e->left && e->left->left &&
             e->left->left->left && e->left->left->left->op == '$' &&
             e->left->left->left->symbol) {
        /* Pattern: (M (+ (M:p $var) const)) where $var is in IX */
        const char *var_symbol;
        long offset;
        const char *var_name;
        struct local_var *var;

        var_symbol = e->left->left->left->symbol;
        offset = e->value;  /* Saved by codegen phase */
        var_name = var_symbol;

        /* Strip $ and A prefixes from symbol */
        if (var_name[0] == '$') var_name++;
        if (var_name[0] == 'A') var_name++;

        /* Look up the variable */
        var = findVar(ctx, var_name);

        if (var && var->reg == REG_IX) {
            /* Variable is in IX - use IX-indexed addressing */
            if (e->size == 1) {
                /* Byte: ld a, (ix+offset) */
                fdprintf(outFd, "\tld a, (ix + %ld)\n", offset);
            } else if (e->size == 2) {
                /* Word: ld l, (ix+offset); ld h, (ix+offset+1) */
                fdprintf(outFd, "\tld l, (ix + %ld)\n", offset);
                fdprintf(outFd, "\tld h, (ix + %ld)\n", offset + 1);
            } else if (e->size == 4) {
                /* Long (4 bytes) - use IX indexed for all 4 bytes */
                fdprintf(outFd, "\tld l, (ix + %ld)\n", offset);
                fdprintf(outFd, "\tld h, (ix + %ld)\n", offset + 1);
                fdprintf(outFd, "\texx\n");
                fdprintf(outFd, "\tld l, (ix + %ld)\n", offset + 2);
                fdprintf(outFd, "\tld h, (ix + %ld)\n", offset + 3);
                fdprintf(outFd, "\texx\n");
            }

            /* Free child expression tree without emitting code */
            freeExpr(e->left);

            /* Free this node */
            if (e->asm_block) free(e->asm_block);
            if (e->cleanup_block) free(e->cleanup_block);
            free(e);
            return;
        } else {
            /* Not IX-allocated - fall back to computing address and dereferencing */
            /* Emit child expression (computes address to HL) */
            emitExpr(ctx, e->left);

            /* Emit load from (HL) */
            if (e->size == 1) {
                fdprintf(outFd, "\tld a, (hl)\n");
            } else if (e->size == 2) {
                fdprintf(outFd, "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl\n");
            } else if (e->size == 4) {
                fdprintf(outFd, "\tcall load32i\n");
            }

            /* Free this node */
            if (e->asm_block) free(e->asm_block);
            if (e->cleanup_block) free(e->cleanup_block);
            free(e);
            return;
        }
    }
    /* Handle DEREF_INDIRECT_B - byte load through pointer variable */
    else if (e->op == 'M' && e->asm_block &&
            strstr(e->asm_block, "DEREF_INDIRECT_B:")) {
        /* Extract symbol name from placeholder */
        const char *sym_start;
        char symbol[256];
        int i;
        const char *var_name;
        struct local_var *var;

        sym_start = strstr(e->asm_block, "DEREF_INDIRECT_B:") + 17;
        i = 0;
        while (sym_start[i] && sym_start[i] != '\n' && i < 255) {
            symbol[i] = sym_start[i];
            i++;
        }
        symbol[i] = '\0';

        /* Strip $ and A prefixes from symbol */
        var_name = symbol;
        if (var_name[0] == '$') var_name++;
        if (var_name[0] == 'A') var_name++;

        /* Look up the pointer variable */
        var = findVar(ctx, var_name);

        if (var && var->reg != REG_NO) {
            /* Pointer is register-allocated - use indirect addressing */
            if (var->reg == REG_BC) {
                fdprintf(outFd, "\tld a, (bc)\n");
            } else if (var->reg == REG_IX) {
                fdprintf(outFd, "\tld a, (ix+0)\n");
            } else {
                /* Register type can't do indirect addressing - fall back */
                /* Load pointer to HL first, then indirect load */
                if (var->reg == REG_B || var->reg == REG_C ||
                    var->reg == REG_Bp || var->reg == REG_Cp) {
                    /* Single byte register shouldn't hold pointer, but handle it */
                    fdprintf(outFd, "\t; WARNING: byte reg holds pointer?\n");
                }
                fdprintf(outFd, "\t; fall back to normal indirect\n");
                /* Emit the inner DEREF to load pointer */
                emitExpr(ctx, e->left);
                fdprintf(outFd, "\tld a, (hl)\n");
            }
        } else {
            /* Pointer not register-allocated - load to HL then indirect */
            emitExpr(ctx, e->left);
            fdprintf(outFd, "\tld a, (hl)\n");
        }

        /* Free outer node (don't free e->left, already freed above if emitted) */
        if (!var || var->reg == REG_NO ||
            (var->reg != REG_BC && var->reg != REG_IX)) {
            /* e->left was emitted and freed, just free this node */
        } else {
            /* e->left was not emitted, need to free it manually */
            freeExpr(e->left);
        }
        if (e->asm_block) free(e->asm_block);
        if (e->cleanup_block) free(e->cleanup_block);
        free(e);
        return;
    }
    /* Handle ASSIGN specially - need to check register allocation */
    else if (e->op == '=' && e->asm_block &&
            strstr(e->asm_block, "ASSIGN_PLACEHOLDER")) {
        /* Emit right child first (value goes to PRIMARY) */
        emitExpr(ctx, e->right);

        /* Check for IX-indexed store: marked by flags in codegen */
        /* Pattern: (= (+ (M:p $var) const) value) where $var is in IX */
        /* Note: e->left->right may be NULL (freed by codegen ADD optimization) */
        if ((e->flags & 1) && e->left && e->left->op == '+' &&
            e->left->left && e->left->left->op == 'M' &&
            e->left->left->type_str && strcmp(e->left->left->type_str, ":p") == 0 &&
            e->left->left->left && e->left->left->left->op == '$' &&
            e->left->left->left->symbol) {
            /* Extract variable and offset (saved in e->value by codegen) */
            const char *var_symbol;
            long offset;
            const char *var_name;
            struct local_var *var;

            var_symbol = e->left->left->left->symbol;
            offset = e->value;
            var_name = var_symbol;

            /* Strip $ and A prefixes */
            if (var_name[0] == '$') var_name++;
            if (var_name[0] == 'A') var_name++;

            var = findVar(ctx, var_name);

            if (var && var->reg == REG_IX) {
                /* Variable is in IX - use indexed addressing */
                if (e->size == 1) {
                    /* Byte: ld (ix+offset), a */
                    fdprintf(outFd, "\tld (ix + %ld), a\n", offset);
                } else if (e->size == 2) {
                    /* Word: ld (ix+offset), l; ld (ix+offset+1), h */
                    fdprintf(outFd, "\tld (ix + %ld), l\n", offset);
                    fdprintf(outFd, "\tld (ix + %ld), h\n", offset + 1);
                } else if (e->size == 4) {
                    /* Long (4 bytes) - use IX indexed for all 4 bytes */
                    fdprintf(outFd, "\tld (ix + %ld), l\n", offset);
                    fdprintf(outFd, "\tld (ix + %ld), h\n", offset + 1);
                    fdprintf(outFd, "\texx\n");
                    fdprintf(outFd, "\tld (ix + %ld), l\n", offset + 2);
                    fdprintf(outFd, "\tld (ix + %ld), h\n", offset + 3);
                    fdprintf(outFd, "\texx\n");
                }
                /* Don't emit left child - we handled the store directly */
                freeExpr(e->left);
                if (e->asm_block) free(e->asm_block);
                if (e->cleanup_block) free(e->cleanup_block);
                free(e);
                return;
            }
        }

        /* Now emit the store inst based on current register allocation */
        if (e->left && e->left->op == '$' && e->left->symbol) {
            struct local_var *var = findVar(ctx, e->left->symbol);

            if (var && var->reg != REG_NO) {
                /* Variable is in a register - move from PRIMARY to register */
                if (e->size == 1) {
                    /* Byte: move A to register */
                    if (var->reg == REG_B) {
                        fdprintf(outFd, "\tld b, a\n");
                    } else if (var->reg == REG_C) {
                        fdprintf(outFd, "\tld c, a\n");
                    } else if (var->reg == REG_Bp) {
                        fdprintf(outFd, "\texx\n\tld b, a\n\texx\n");
                    } else if (var->reg == REG_Cp) {
                        fdprintf(outFd, "\texx\n\tld c, a\n\texx\n");
                    }
                } else {
                    /* Word: move HL to register pair */
                    if (var->reg == REG_BC) {
                        fdprintf(outFd, "\tld b, h\n\tld c, l\n");
                    } else if (var->reg == REG_IX) {
                        fdprintf(outFd, "\tpush hl\n\tpop ix\n");
                    }
                }
            } else if (var) {
                /* Variable is on stack - store to (iy + offset) */
                if (e->size == 1) {
                    if (var->offset >= 0) {
                        fdprintf(outFd, "\tld (iy + %d), a\n", var->offset);
                    } else {
                        fdprintf(outFd, "\tld (iy - %d), a\n", -var->offset);
                    }
                } else if (e->size == 2) {
                    if (var->offset >= 0) {
                        fdprintf(outFd, "\tld (iy + %d), l\n", var->offset);
                        fdprintf(outFd, "\tld (iy + %d), h\n", var->offset + 1);
                    } else {
                        /* Negative offset: high byte is at less negative offset */
                        fdprintf(outFd, "\tld (iy - %d), l\n", -var->offset);
                        fdprintf(outFd, "\tld (iy - %d), h\n", -var->offset - 1);
                    }
                } else {
                    /* Long (4 bytes) - use putlong function */
                    fdprintf(outFd, "\tld a, %d\n", var->offset);
                    fdprintf(outFd, "\tcall putlong\n");
                }
            } else {
                /* Global variable - direct memory access */
                const char *sym = stripDollar(e->left->symbol);
                if (e->size == 1) {
                    fdprintf(outFd, "\tld (%s), a\n", sym);
                } else if (e->size == 2) {
                    fdprintf(outFd, "\tld (%s), hl\n", sym);
                } else if (e->size == 4) {
                    /* Long - store HL'HL to global */
                    fdprintf(outFd, "\tld (%s), hl\n", sym);
                    fdprintf(outFd, "\texx\n");
                    fdprintf(outFd, "\tld (%s+2), hl\n", sym);
                    fdprintf(outFd, "\texx\n");
                }
            }
        }

        /* Handle complex lvalue: compute address, then store through it */
        else if (e->left && e->left->op == 'M') {
            /* Pointer dereference: (M $var) - store through pointer */
            /* Value is already in PRIMARY (HL or A) */
            if (e->size == 1) {
                /* Byte: value in A, need address in HL */
                fdprintf(outFd, "\tld e, a  ; save byte value\n");
                emitExpr(ctx, e->left->left);  /* Load pointer to HL */
                fdprintf(outFd, "\tld (hl), e\n");
            } else if (e->size == 2) {
                /* Word: value in HL, need address in HL - save value first */
                fdprintf(outFd, "\tex de, hl  ; save word value in DE\n");
                emitExpr(ctx, e->left->left);  /* Load pointer to HL */
                fdprintf(outFd, "\tld (hl), e\n");
                fdprintf(outFd, "\tinc hl\n");
                fdprintf(outFd, "\tld (hl), d\n");
            }
        }
        else if (e->left && e->left->op == '+') {
            /* Complex lvalue like (+ (M:p $var) offset) */
            /* Value is already in PRIMARY (HL or A) */
            /* Need to: save value, compute address, store through address */

            if (e->size == 1) {
                /* Byte: value in A */
                /* Save value to E, compute address to HL, store (HL) <- E */
                fdprintf(outFd, "\tld e, a  ; save byte value\n");
                emitExpr(ctx, e->left);  /* Compute address to HL */
                fdprintf(outFd, "\tld (hl), e\n");
            } else if (e->size == 2) {
                /* Word: value in HL */
                /* Save value to DE, compute address to HL, store */
                fdprintf(outFd, "\tex de, hl  ; save word value in DE\n");
                emitExpr(ctx, e->left);  /* Compute address to HL */
                fdprintf(outFd, "\tld (hl), e\n");
                fdprintf(outFd, "\tinc hl\n");
                fdprintf(outFd, "\tld (hl), d\n");
            } else if (e->size == 4) {
                /* Long: value in HL'HL */
                /* This is complex - need to save 4 bytes */
                fdprintf(outFd, "\tpush hl  ; save lower word\n");
                fdprintf(outFd, "\texx\n");
                fdprintf(outFd, "\tpush hl  ; save upper word\n");
                fdprintf(outFd, "\texx\n");
                emitExpr(ctx, e->left);  /* Compute address to HL */
                fdprintf(outFd, "\tex de, hl  ; DE = address\n");
                fdprintf(outFd, "\tpop hl  ; restore upper word\n");
                fdprintf(outFd, "\tpush de  ; save address\n");
                fdprintf(outFd, "\texx\n");
                fdprintf(outFd, "\tpop de  ; DE = address\n");
                fdprintf(outFd, "\tpop hl  ; restore lower word\n");
                /* Store 4 bytes: (DE) <- HL, (DE+2) <- HL' */
                fdprintf(outFd, "\tld a, l\n");
                fdprintf(outFd, "\tld (de), a\n");
                fdprintf(outFd, "\tinc de\n");
                fdprintf(outFd, "\tld a, h\n");
                fdprintf(outFd, "\tld (de), a\n");
                fdprintf(outFd, "\tinc de\n");
                fdprintf(outFd, "\texx\n");
                fdprintf(outFd, "\tld a, l\n");
                fdprintf(outFd, "\tld (de), a\n");
                fdprintf(outFd, "\tinc de\n");
                fdprintf(outFd, "\tld a, h\n");
                fdprintf(outFd, "\tld (de), a\n");
                fdprintf(outFd, "\texx\n");
            }
        }
        /* Else: left child is neither simple var nor handled pattern */
        /* This shouldn't happen in well-formed code */
    }
    /* Optimize ADD with constant where left is register-allocated variable */
    else if (e->op == '+' && e->left && e->left->op == 'M' && e->size == 2 &&
             e->left->left && e->left->left->op == '$' && e->left->left->symbol &&
             !e->right) {
        /* Pattern: (+ (M:p $var) constant) where right was already freed by codegen */
        /* This happens when codegen optimized constant addition */
        /* Check if the variable is register-allocated */
        const char *var_name = e->left->left->symbol;
        struct local_var *var;

        if (var_name[0] == '$') var_name++;
        if (var_name[0] == 'A') var_name++;
        var = findVar(ctx, var_name);

        if (var && (var->reg == REG_BC || var->reg == REG_BCp || var->reg == REG_IX)) {
            /* Variable is in register - emit optimized add */
            /* The constant is already in asm_block as "ld de, N; add hl, de" or "inc hl" */
            /* Replace with: ld hl, N; add hl, bc/ix */

            /* Parse the constant from asm_block */
            long const_val = 0;
            int is_small = 0;

            if (strstr(e->asm_block, "inc hl")) {
                /* Small constant - count inc hl instructions */
                const char *p = e->asm_block;
                while ((p = strstr(p, "inc hl")) != NULL) {
                    const_val++;
                    p += 6;
                }
                is_small = 1;
            } else if (strstr(e->asm_block, "ld de, ")) {
                /* Extract constant from "ld de, N" */
                sscanf(strstr(e->asm_block, "ld de, ") + 7, "%ld", &const_val);
            }

            /* Don't emit left child - access register directly */
            freeExpr(e->left);

            /* Emit optimized sequence */
            if (is_small && const_val <= 4) {
                /* For small constants, can still use inc but on BC first */
                if (var->reg == REG_BC) {
                    fdprintf(outFd, "\tld h, b\n\tld l, c\n");
                } else if (var->reg == REG_BCp) {
                    fdprintf(outFd, "\texx\n\tld h, b\n\tld l, c\n\texx\n");
                } else {  /* REG_IX */
                    fdprintf(outFd, "\tpush ix\n\tpop hl\n");
                }
                /* Now do the inc hl sequence */
                fdprintf(outFd, "%s\n", e->asm_block);
            } else {
                /* Use add hl, reg for larger constants */
                fdprintf(outFd, "\tld hl, %ld\n", const_val);
                if (var->reg == REG_BC) {
                    fdprintf(outFd, "\tadd hl, bc\n");
                } else if (var->reg == REG_BCp) {
                    fdprintf(outFd, "\texx\n\tadd hl, bc\n\texx\n");
                } else {  /* REG_IX */
                    fdprintf(outFd, "\tadd hl, ix\n");
                }
            }
        } else {
            /* Not register-allocated - emit normally */
            emitExpr(ctx, e->left);
            fdprintf(outFd, "%s\n", e->asm_block);
        }

        /* Free this node */
        if (e->asm_block) free(e->asm_block);
        if (e->cleanup_block) free(e->cleanup_block);
        free(e);
        return;
    }
    /* Binary operators with accumulator management need special handling */
    else if (isBinopWithAccum(e->op) && e->left && e->right &&
            e->asm_block) {
        /* Check for inline byte operations with immediate (and/or/xor) */
        int is_inline_immediate = 0;
        if (!strchr(e->asm_block, '\n') &&
            (e->op == '&' || e->op == '|' || e->op == '^') &&
            e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
            e->right->value >= 0 && e->right->value <= 255) {
            /* Single-line asm_block for byte bitwise op with constant */
            is_inline_immediate = 1;
        }

        if (is_inline_immediate) {
            /* Inline immediate: just emit left to A, then inline instruction */
            emitExpr(ctx, e->left);
            /* Don't emit right child - constant is baked into instruction */
            freeExpr(e->right);
            fdprintf(outFd, "%s\n", e->asm_block);
        } else {
            /* Standard binop with accumulator management */
            char *move_inst = NULL;
            char *call_inst = NULL;
            char *newline = strchr(e->asm_block, '\n');
            int saved_de = 0;

            if (newline) {
                /* Extract move instruction (before newline) */
                size_t move_len = newline - e->asm_block;
                move_inst = malloc(move_len + 1);
                if (move_inst) {
                    memcpy(move_inst, e->asm_block, move_len);
                    move_inst[move_len] = '\0';
                }

                /* Extract call instruction (after newline) */
                call_inst = strdup(newline + 1);
            }

            /* Emit in correct order for accumulator management */
            /* 1. Left operand to PRIMARY */
            emitExpr(ctx, e->left);
            if (move_inst) {
                /* 2. Move PRIMARY to SECONDARY */
                fdprintf(outFd, "%s\n", move_inst);
                free(move_inst);
            }

            /* If right child contains binops, save SECONDARY before evaluation */
            if (contains_binop(e->right)) {
                if (e->size == 1) {
                    /* Byte operation: SECONDARY is E, spill to D or stack */
                    if (!ctx->d_in_use) {
                        /* D is free - use it to save E */
                        fdprintf(outFd, "\tld d, e  ; save SECONDARY (E) to D\n");
                        ctx->d_in_use = 1;
                        saved_de = 1;
                    } else {
                        /* D already in use - spill to stack */
                        fdprintf(outFd, "\tpush de  ; save D and E to stack\n");
                        ctx->de_save_count++;
                        saved_de = 2;
                    }
                } else {
                    /* Word operation: SECONDARY is DE, spill to stack */
                    fdprintf(outFd, "\tpush de  ; save SECONDARY (DE) for nested binop\n");
                    ctx->de_save_count++;
                    saved_de = 2;
                }
            }

            /* 3. Right operand to PRIMARY */
            emitExpr(ctx, e->right);

            /* Restore SECONDARY if we saved it */
            if (saved_de == 1) {
                /* Restore E from D (byte operation) */
                fdprintf(outFd, "\tld e, d  ; restore SECONDARY (E) from D\n");
                ctx->d_in_use = 0;
            } else if (saved_de == 2) {
                /* Restore from stack */
                fdprintf(outFd, "\tpop de  ; restore SECONDARY from stack\n");
                ctx->de_save_count--;
            }

            if (call_inst) {
                /* 4. Call binary operation */
                fdprintf(outFd, "%s\n", call_inst);
                free(call_inst);
            }
        }  /* End of else (standard binop) */
    }
    /* CALL operator - don't emit children, they're in the asm_block */
    else if (e->op == '@') {
        /* Process asm_block line by line to handle placeholders */
        if (e->asm_block) {
            char *line_start = e->asm_block;
            char *line_end;
            char line_buf[512];
            int len;

            while (*line_start) {
                /* Find end of line */
                line_end = strchr(line_start, '\n');
                if (line_end) {
                    len = line_end - line_start;
                    if (len >= sizeof(line_buf)) len = sizeof(line_buf) - 1;
                    memcpy(line_buf, line_start, len);
                    line_buf[len] = '\0';
                    line_start = line_end + 1;
                } else {
                    /* Last line without newline */
                    strncpy(line_buf, line_start, sizeof(line_buf) - 1);
                    line_buf[sizeof(line_buf) - 1] = '\0';
                    line_start += strlen(line_buf);
                }

                /* Check if line is a DEREF_PLACEHOLDER or load arg comment */
                if (strstr(line_buf, "DEREF_PLACEHOLDER:") ||
                    strstr(line_buf, "; load arg")) {
                    /* Extract symbol name */
                    char *sym_start;
                    char symbol[256];
                    const char *var_name;
                    struct local_var *var;
                    int j = 0;

                    if (strstr(line_buf, "DEREF_PLACEHOLDER:")) {
                        sym_start = strstr(line_buf, "DEREF_PLACEHOLDER:") + 18;
                    } else {
                        /* Format: "; load arg N: $symbol" */
                        sym_start = strchr(line_buf, ':');
                        if (sym_start) {
                            sym_start++;  /* Skip : */
                            while (*sym_start == ' ') sym_start++;  /* Skip spaces */
                        }
                    }

                    if (sym_start) {
                        while (sym_start[j] && sym_start[j] != ' ' &&
                               sym_start[j] != '\t' && sym_start[j] != '\n' && j < 255) {
                            symbol[j] = sym_start[j];
                            j++;
                        }
                    }
                    symbol[j] = '\0';

                    /* Strip $ and A prefixes */
                    var_name = symbol;
                    if (var_name[0] == '$') var_name++;
                    if (var_name[0] == 'A') var_name++;

                    /* Look up variable and check if we can optimize */
                    var = findVar(ctx, var_name);

                    /* Look ahead to see if next line is "push hl" */
                    if (var && var->reg != REG_NO && var->size == 2 && line_end) {
                        /* Check if next line is push hl */
                        char *next_line_start = line_end + 1;
                        char next_buf[512];
                        char *next_end = strchr(next_line_start, '\n');
                        int next_len;

                        if (next_end) {
                            next_len = next_end - next_line_start;
                            if (next_len >= sizeof(next_buf)) next_len = sizeof(next_buf) - 1;
                            memcpy(next_buf, next_line_start, next_len);
                            next_buf[next_len] = '\0';
                        } else {
                            strncpy(next_buf, next_line_start, sizeof(next_buf) - 1);
                            next_buf[sizeof(next_buf) - 1] = '\0';
                            next_len = strlen(next_buf);
                        }

                        /* Check if it's a push hl instruction */
                        if (strstr(next_buf, "push hl")) {
                            /* Optimize: push register directly */
                            if (var->reg == REG_BC) {
                                fdprintf(outFd, "\tpush bc\n");
                            } else if (var->reg == REG_BCp) {
                                fdprintf(outFd, "\texx\n\tpush bc\n\texx\n");
                            } else if (var->reg == REG_IX) {
                                fdprintf(outFd, "\tpush ix\n");
                            }
                            /* Skip the push hl line */
                            line_start = next_end ? next_end + 1 : next_line_start + next_len;
                            continue;
                        }
                    }

                    /* Standard path: load to HL */
                    if (var && var->reg != REG_NO) {
                        /* Variable in register - move to HL */
                        if (var->reg == REG_BC) {
                            fdprintf(outFd, "\tld h, b\n\tld l, c\n");
                        } else if (var->reg == REG_BCp) {
                            fdprintf(outFd, "\texx\n\tld h, b\n\tld l, c\n\texx\n");
                        } else if (var->reg == REG_IX) {
                            fdprintf(outFd, "\tpush ix\n\tpop hl\n");
                        }
                    } else if (var) {
                        /* Variable on stack */
                        if (var->offset >= 0) {
                            fdprintf(outFd, "\tld l, (iy + %d)\n", var->offset);
                            fdprintf(outFd, "\tld h, (iy + %d)\n", var->offset + 1);
                        } else {
                            /* Negative offset: high byte is at less negative offset */
                            fdprintf(outFd, "\tld l, (iy - %d)\n", -var->offset);
                            fdprintf(outFd, "\tld h, (iy - %d)\n", -var->offset - 1);
                        }
                    }
                } else {
                    /* Normal line - emit it */
                    fdprintf(outFd, "%s\n", line_buf);
                }
            }
        }
        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
        /* Free children manually since we didn't call emitExpr on them */
        freeExpr(e->left);
        freeExpr(e->right);
    }
    /* Handle SYM - load variable value to PRIMARY */
    else if (e->op == '$' && e->symbol) {
        struct local_var *var = findVar(ctx, e->symbol);

        if (var && var->reg != REG_NO) {
            /* Variable in register - move to PRIMARY (HL) */
            if (var->reg == REG_BC) {
                fdprintf(outFd, "\tld h, b\n\tld l, c\n");
            } else if (var->reg == REG_BCp) {
                fdprintf(outFd, "\texx\n\tld h, b\n\tld l, c\n\texx\n");
            } else if (var->reg == REG_IX) {
                fdprintf(outFd, "\tpush ix\n\tpop hl\n");
            }
        } else if (var) {
            /* Variable on stack - load to HL */
            if (var->offset >= 0) {
                fdprintf(outFd, "\tld l, (iy + %d)\n", var->offset);
                fdprintf(outFd, "\tld h, (iy + %d)\n", var->offset + 1);
            } else {
                fdprintf(outFd, "\tld l, (iy - %d)\n", -var->offset);
                fdprintf(outFd, "\tld h, (iy - %d)\n", -var->offset - 1);
            }
        } else {
            /* Global variable */
            const char *sym = stripDollar(e->symbol);
            fdprintf(outFd, "\tld hl, (%s)\n", sym);
        }
    }
    else {
        /* Normal postorder traversal for other operators */
        if (e->left) emitExpr(ctx, e->left);
        if (e->right) emitExpr(ctx, e->right);

        if (e->asm_block) {
            fdprintf(outFd, "%s\n", e->asm_block);
        }

        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    if (e->asm_block) free(e->asm_block);
    if (e->cleanup_block) free(e->cleanup_block);
    free(e);
}

/*
 * Walk statement tree, emit assembly, and free nodes
 */
static void emitStmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* For ASM nodes, emit the assembly block directly */
    if (s->type == 'A' && s->asm_block) {
        fdprintf(outFd, "%s\n", s->asm_block);
    }

    /* Handle IF statements specially */
    if (s->type == 'I') {
        int invert_condition = 0;
        int use_direct_jump = 0;
        struct expr *cond = s->expr;

        /* Check if condition has ! wrapper */
        if (cond && cond->op == '!') {
            invert_condition = 1;
            cond = cond->left;  /* unwrap ! */
        }

        /* Check if this is a byte operation that sets Z flag */
        /* Byte bitwise ops with constants emit inline instructions that set Z */
        if (cond && (cond->op == '&' || cond->op == '|' || cond->op == '^') &&
            cond->left && cond->left->size == 1 &&
            cond->right && cond->right->op == 'C' &&
            cond->right->value >= 0 && cond->right->value <= 255) {
            use_direct_jump = 1;
        }
        /* Other byte-sized operations */
        else if (cond && cond->size == 1) {
            use_direct_jump = 1;
        }

        if (use_direct_jump) {
            /* Emit unwrapped condition expression (leaves Z flag set) */
            emitExpr(ctx, cond);

            /* If we had a ! wrapper, manually free it (child already freed) */
            if (invert_condition && s->expr != cond) {
                free(s->expr);
            }

            /* Jump to fail label if condition is false */
            if (s->label2 > 0) {
                /* Has else (or had empty else): jump to else on false, fall through to then on true */
                if (invert_condition) {
                    /* ! wrapper: jump if nonzero (true) */
                    fdprintf(outFd, "\tjp nz, _if_%d\n", s->label);
                } else {
                    /* No !: jump if zero (false) */
                    fdprintf(outFd, "\tjp z, _if_%d\n", s->label);
                }
            } else {
                /* No else: jump to end on false */
                if (invert_condition) {
                    /* ! wrapper: jump if nonzero (true) */
                    fdprintf(outFd, "\tjp nz, _if_end_%d\n", s->label);
                } else {
                    /* No !: jump if zero (false) */
                    fdprintf(outFd, "\tjp z, _if_end_%d\n", s->label);
                }
            }
        } else {
            /* Non-byte or complex expression */
            /* Check for word variable in register - can test directly */
            struct local_var *var = NULL;
            if (cond && cond->op == 'M' && cond->size == 2 &&
                cond->left && cond->left->op == '$' && cond->left->symbol) {
                /* Strip $ and A prefixes */
                const char *var_name = cond->left->symbol;
                if (var_name[0] == '$') var_name++;
                if (var_name[0] == 'A') var_name++;
                var = findVar(ctx, var_name);
            }

            if (var && (var->reg == REG_BC || var->reg == REG_BCp)) {
                /* Word variable in BC or BC' - test directly */
                if (var->reg == REG_BC) {
                    fdprintf(outFd, "\tld a, b\n\tor c\n");
                } else {  /* REG_BCp */
                    fdprintf(outFd, "\texx\n\tld a, b\n\tor c\n\texx\n");
                }
                /* Free the condition expression without emitting it */
                freeExpr(cond);
                /* If we had ! wrapper, free it too */
                if (invert_condition && s->expr != cond) {
                    free(s->expr);
                }
            } else {
                /* Evaluate to HL and test */
                emitExpr(ctx, s->expr);
                /* Test if HL is zero */
                fdprintf(outFd, "\tld a, h\n\tor l\n");
            }

            /* Jump based on test result */
            if (s->label2 > 0) {
                if (invert_condition) {
                    fdprintf(outFd, "\tjp nz, _if_%d\n", s->label);
                } else {
                    fdprintf(outFd, "\tjp z, _if_%d\n", s->label);
                }
            } else {
                if (invert_condition) {
                    fdprintf(outFd, "\tjp nz, _if_end_%d\n", s->label);
                } else {
                    fdprintf(outFd, "\tjp z, _if_end_%d\n", s->label);
                }
            }
        }

        /* Emit then branch */
        if (s->then_branch) emitStmt(ctx, s->then_branch);

        if (s->label2 > 0) {
            /* Jump over else if we took the then branch */
            fdprintf(outFd, "\tjp _if_end_%d\n", s->label2);

            /* Emit else branch (if it exists) */
            if (s->else_branch) {
                fdprintf(outFd, "_if_%d:\n", s->label);
                emitStmt(ctx, s->else_branch);
            } else {
                /* Empty else branch - just emit the label */
                fdprintf(outFd, "_if_%d:\n", s->label);
            }

            /* End label is emitted by ASM node in s->next inserted by parseast.c */
        } else {
            /* End label is emitted by ASM node in s->next inserted by parseast.c */
        }

        /* Emit next statement if any (includes end label ASM node) */
        if (s->next) emitStmt(ctx, s->next);

        /* Free this statement node */
        if (s->asm_block) free(s->asm_block);
        free(s);
        return;
    }
    /* Handle RETURN statements specially */
    else if (s->type == 'R') {
        /* Emit expression to load return value into HL */
        if (s->expr) {
            emitExpr(ctx, s->expr);
            /* If function returns long but expression is short, zero-extend to long */
            if (strcmp(ctx->rettype, "_long_") == 0 && s->expr->size == 2) {
                fdprintf(outFd, "\t; zero-extend short to long\n");
                fdprintf(outFd, "\texx\n");
                fdprintf(outFd, "\tld hl, 0  ; upper 16 bits = 0\n");
                fdprintf(outFd, "\texx\n");
            }
        }
        /* Jump to function exit label */
        fdprintf(outFd, "\tjp _%s_exit\n", ctx->name);
    } else {
        /* Emit expressions (this frees them) */
        if (s->expr) emitExpr(ctx, s->expr);
        if (s->expr2) emitExpr(ctx, s->expr2);
        if (s->expr3) emitExpr(ctx, s->expr3);
    }

    /* Emit child statements (this frees them) */
    if (s->then_branch) emitStmt(ctx, s->then_branch);
    if (s->else_branch) emitStmt(ctx, s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (s->next) emitStmt(ctx, s->next);

    /* Free this node only (children already freed by recursive emit calls) */
    if (s->asm_block) free(s->asm_block);
    free(s);
}

/*
 * Emit assembly for entire function and free tree
 */
void emit_assembly(struct function_ctx *ctx, int fd)
{
    struct local_var *var, *next;
    int has_params;

    if (!ctx || !ctx->body) return;

    /* Check if function has parameters */
    has_params = (ctx->params && ctx->params[0]);

    /* Emit function prologue with frame allocation and lifetime info */
    emitFunctionPrologue(ctx->name, ctx->params, ctx->rettype, 
        ctx->frame_size, ctx->locals);

    /* Emit function body */
    emitStmt(ctx, ctx->body);

    /* Emit function exit label (for return statements to jump to) */
    fdprintf(outFd, "_%s_exit:\n", ctx->name);

    /* Emit function epilogue with frame deallocation */
    /* Jump to framefree if we have locals or parameters (tail call optimization) */
    if (ctx->frame_size > 0 || has_params) {
        fdprintf(outFd, "\tjp framefree\n");
    } else {
        /* No frame to free, just return */
        fdprintf(outFd, "\tret\n");
    }

    /* Free local variables list */
    var = ctx->locals;
    while (var) {
        next = var->next;
        free(var->name);
        free(var);
        var = next;
    }
}


/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
