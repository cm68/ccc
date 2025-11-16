/*
 * emit.c - Code emission phase for cc2
 *
 * Walks expression and statement trees, emitting assembly code and freeing nodes.
 * This phase outputs the actual assembly code that was generated in codegen.c.
 *
 * Key responsibilities:
 * - emit_assembly(): Main entry point - emit function assembly and free tree
 * - emit_expr()/emit_stmt(): Walk trees, emit assembly, free nodes
 * - emit_function_prologue(): Output function header and variable metadata
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
 * Nested Binary Operations:
 * -------------------------
 * For nested expressions like (a + b) + c, the right operand (c) evaluation may
 * itself contain binary operations that would clobber SECONDARY (DE). To handle
 * this, we implement a spilling mechanism:
 * - Before evaluating right child: if it contains binops, push DE (spill to stack)
 * - After evaluating right child: pop DE (restore from stack)
 * - Track spill depth with ctx->de_save_count
 *
 * Example: (a + b) + (c + d)
 *   Left child (a + b):
 *     eval a -> PRIMARY (HL)
 *     move PRIMARY to SECONDARY (DE)
 *     eval b -> PRIMARY (HL)
 *     add HL, DE -> PRIMARY (HL)
 *   Main operation:
 *     move PRIMARY to SECONDARY (DE)
 *     push DE (save result of a+b)
 *     Right child (c + d):
 *       eval c -> PRIMARY (HL)
 *       move PRIMARY to SECONDARY (DE, clobbering saved a+b)
 *       eval d -> PRIMARY (HL)
 *       add HL, DE -> PRIMARY (HL)
 *     pop DE (restore result of a+b)
 *     add HL, DE -> PRIMARY (HL)
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"

static const char *
get_register_name(enum register_id reg)
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
 * Emit function prologue
 * Outputs assembly comment describing function and function label
 */
static void
emit_function_prologue(char *name, char *params, char *rettype, int frame_size,
                       struct local_var *locals)
{
    int has_params = (params && params[0]);
    struct local_var *var;

    /* Assembly comment with function signature */
    fdprintf(out_fd, "; Function: %s", name);

    if (has_params) {
        fdprintf(out_fd, "(%s)", params);
    } else {
        fdprintf(out_fd, "()");
    }

    if (rettype && rettype[0]) {
        fdprintf(out_fd, " -> %s", rettype);
    }

    fdprintf(out_fd, "\n");

    /* Output local variable information as assembly comments */
    if (locals) {
        fdprintf(out_fd, "; Local variables:\n");
        for (var = locals; var; var = var->next) {
            const char *regname = get_register_name(var->reg);

            /* Build offset string */
            char offset_str[32];
            if (var->offset >= 0) {
                snprintf(offset_str, sizeof(offset_str), "(iy+%d)", 
                    var->offset);
            } else {
                snprintf(offset_str, sizeof(offset_str), "(iy%d)", 
                    var->offset);
            }

            fdprintf(out_fd, ";   %s: ", var->name);
            if (var->first_label == -1) {
                if (regname) {
                    fdprintf(out_fd, 
                        "unused (0 refs, 0 agg_refs, %s, reg=%s)\n",
                        offset_str, regname);
                } else {
                    fdprintf(out_fd, 
                        "unused (0 refs, 0 agg_refs, %s)\n", offset_str);
                }
            } else {
                if (regname) {
                    fdprintf(out_fd, 
                        "labels %d-%d (%d refs, %d agg_refs, %s, reg=%s)\n",
                        var->first_label, var->last_label, var->ref_count, 
                        var->agg_refs, offset_str, regname);
                } else {
                    fdprintf(out_fd, 
                        "labels %d-%d (%d refs, %d agg_refs, %s)\n",
                        var->first_label, var->last_label, var->ref_count, 
                        var->agg_refs, offset_str);
                }
            }
        }
    }

    /* Function label (standard C naming with underscore prefix) */
    fdprintf(out_fd, "_%s:\n", name);

    /* Emit frame allocation call if we have locals or parameters */
    if (frame_size > 0 || has_params) {
        fdprintf(out_fd, "\tld a, %d\n", frame_size);
        fdprintf(out_fd, "\tcall framealloc\n");
    }
}

/*
 * Helper: Check if operator is a binary operator that needs accumulator
 * management
 */
static int
is_binop_with_accum(unsigned char op)
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
    if (is_binop_with_accum(e->op)) return 1;

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
static void emit_expr(struct function_ctx *ctx, struct expr *e)
{
    if (!e) return;

    /* Handle ASSIGN specially - need to check register allocation */
    if (e->op == '=' && e->asm_block && 
            strstr(e->asm_block, "ASSIGN_PLACEHOLDER")) {
        /* Emit right child first (value goes to PRIMARY) */
        emit_expr(ctx, e->right);

        /* Now emit the store inst based on current register allocation */
        if (e->left && e->left->op == '$' && e->left->symbol) {
            struct local_var *var = lookup_var(ctx, e->left->symbol);
            char buf[256];

            if (var && var->reg != REG_NO) {
                /* Variable is in a register - move from PRIMARY to register */
                if (e->size == 1) {
                    /* Byte: move A to register */
                    if (var->reg == REG_B) {
                        fdprintf(out_fd, "\tld b, a\n");
                    } else if (var->reg == REG_C) {
                        fdprintf(out_fd, "\tld c, a\n");
                    } else if (var->reg == REG_Bp) {
                        fdprintf(out_fd, "\texx\n\tld b, a\n\texx\n");
                    } else if (var->reg == REG_Cp) {
                        fdprintf(out_fd, "\texx\n\tld c, a\n\texx\n");
                    }
                } else {
                    /* Word: move HL to register pair */
                    if (var->reg == REG_BC) {
                        fdprintf(out_fd, "\tld b, h\n\tld c, l\n");
                    } else if (var->reg == REG_BCp) {
                        fdprintf(out_fd, 
                            "\texx\n\tld b, h\n\tld c, l\n\texx\n");
                    } else if (var->reg == REG_IX) {
                        fdprintf(out_fd, "\tpush hl\n\tpop ix\n");
                    }
                }
            } else if (var) {
                /* Variable is on stack - store to (iy + offset) */
                if (e->size == 1) {
                    if (var->offset >= 0) {
                        fdprintf(out_fd, "\tld (iy + %d), a\n", var->offset);
                    } else {
                        fdprintf(out_fd, "\tld (iy - %d), a\n", -var->offset);
                    }
                } else {
                    if (var->offset >= 0) {
                        fdprintf(out_fd, "\tld (iy + %d), hl\n", var->offset);
                    } else {
                        fdprintf(out_fd, "\tld (iy - %d), hl\n", -var->offset);
                    }
                }
            }
        }

        /* Emit left child last (address not needed for simple assignments) */
        emit_expr(ctx, e->left);
    }
    /* Binary operators with accumulator management need special handling */
    else if (is_binop_with_accum(e->op) && e->left && e->right &&
            e->asm_block) {
        /* Split asm_block into move instruction and call instruction */
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
        emit_expr(ctx, e->left);
        if (move_inst) {
            /* 2. Move PRIMARY to SECONDARY */
            fdprintf(out_fd, "%s\n", move_inst);
            free(move_inst);
        }

        /* If right child contains binops, save SECONDARY (DE) before evaluation */
        if (contains_binop(e->right)) {
            fdprintf(out_fd, "\tpush de  ; save SECONDARY for nested binop\n");
            ctx->de_save_count++;
            saved_de = 1;
        }

        /* 3. Right operand to PRIMARY */
        emit_expr(ctx, e->right);

        /* Restore SECONDARY (DE) if we saved it */
        if (saved_de) {
            fdprintf(out_fd, "\tpop de  ; restore SECONDARY\n");
            ctx->de_save_count--;
        }

        if (call_inst) {
            /* 4. Call binary operation */
            fdprintf(out_fd, "%s\n", call_inst);
            free(call_inst);
        }
    } else {
        /* Normal postorder traversal for other operators */
        if (e->left) emit_expr(ctx, e->left);
        if (e->right) emit_expr(ctx, e->right);

        if (e->asm_block) {
            fdprintf(out_fd, "%s\n", e->asm_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    if (e->asm_block) free(e->asm_block);
    free(e);
}

/*
 * Walk statement tree, emit assembly, and free nodes
 */
static void emit_stmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* For ASM nodes, emit the assembly block directly */
    if (s->type == 'A' && s->asm_block) {
        fdprintf(out_fd, "%s\n", s->asm_block);
    }

    /* Emit expressions (this frees them) */
    if (s->expr) emit_expr(ctx, s->expr);
    if (s->expr2) emit_expr(ctx, s->expr2);
    if (s->expr3) emit_expr(ctx, s->expr3);

    /* Emit child statements (this frees them) */
    if (s->then_branch) emit_stmt(ctx, s->then_branch);
    if (s->else_branch) emit_stmt(ctx, s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (s->next) emit_stmt(ctx, s->next);

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

    fdprintf(2, "=== Phase 3: Emitting assembly and freeing tree ===\n");

    /* Check if function has parameters */
    has_params = (ctx->params && ctx->params[0]);

    /* Emit function prologue with frame allocation and lifetime info */
    emit_function_prologue(ctx->name, ctx->params, ctx->rettype, 
        ctx->frame_size, ctx->locals);

    /* Emit function body */
    emit_stmt(ctx, ctx->body);

    /* Emit function epilogue with frame deallocation */
    /* Emit framefree if we have locals or parameters */
    if (ctx->frame_size > 0 || has_params) {
        fdprintf(out_fd, "\tcall framefree\n");
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
