/*
 * emit.c - Code emission phase for cc2
 *
 * Walks expression and statement trees, emitting assembly code and freeing nodes.
 * This phase outputs the actual assembly code that was generated in codegen.c.
 *
 * Key responsibilities:
 * - emitAssembly(): Main entry point - emit function assembly and free tree
 * - emitExpr()/emitStmt(): Walk trees, emit assembly, free nodes
 * - emitFnProlog(): Output function header and variable metadata
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

/* Common assembly strings - indexed for compact emission */
#define S_EXX 0
#define S_IXHL 1
#define S_BCHL 2
#define S_HLDE 3
#define S_DEA 4
#define S_SBCHLDE 5
#define S_AHL 6
#define S_INCDE 7
#define S_EXXBCHL 8
#define S_ADDHLDE 9
#define S_HLPIX 10
#define S_ORA 11
#define S_HLD 12
#define S_ESAVE 13
#define S_CA 14
#define S_BCHLX 15
#define S_BA 16
#define S_AL 17
#define S_AHORL 18
#define S_AH 19
#define S_JRNZ3 20
#define S_INCHL 21
#define S_DESAVE 22
#define S_EXBCHL 23
#define S_EXXBC 24
#define S_LDABC 25
#define S_LDAIXZ 26
#define S_LDAB 27
#define S_LDAC 28
#define S_LDBA 29
#define S_LDCA 30
#define S_EXXLDAB 31
#define S_EXXLDAC 32
#define S_EXXLDBA 33
#define S_EXXLDCA 34
#define S_ADDHLBC 35
#define S_CALLFA 36
#define S_CALLGL 37
#define S_CALLL32I 38
#define S_CALLPL 39
#define S_ERRPARS 40
#define S_CACHESWP 41
#define S_DEADR 42
#define S_PUSHTOS 43
#define S_EXXABCORC 44
#define S_EXXBCPOPHL 45
#define S_FBKNORM 46
#define S_JPFF 47
#define S_ABCORC 48
#define S_LDEDHLSWP 49
#define S_HLZERO 50
#define S_POPAFRET 51
#define S_POPDEADR 52
#define S_POPDERES 53
#define S_POPHPOST 54
#define S_POPHLLOW 55
#define S_POPHLUPP 56
#define S_POPHLRET 57
#define S_PUSHAFSV 58
#define S_PUSHBC 59
#define S_PUSHDESV 60
#define S_PUSHDESP 61
#define S_PUSHHLLOW 62
#define S_PUSHHLOV 63
#define S_PUSHHLUPP 64
#define S_PUSHIX 65
#define S_IXSWPHL 66
#define S_RET 67
#define S_WARNBPTR 68
#define S_ZEXTSL 69
#define S_LOCVAR 70
#define S_EMPTY 71
#define S_NEWLINE 72

static const char *asmstr[] = {
    "\texx\n",                          /* S_EXX */
    "\tpush ix\n\tpop hl\n",            /* S_IXHL */
    "\tld h, b\n\tld l, c\n",           /* S_BCHL */
    "\tld (hl), e\n",                   /* S_HLDE */
    "\tld (de), a\n",                   /* S_DEA */
    "\tor a\n\tsbc hl, de\n",           /* S_SBCHLDE */
    "\tld a, (hl)\n",                   /* S_AHL */
    "\tinc de\n",                       /* S_INCDE */
    "\texx\n\tpush bc\n\texx\n\tpop hl\n", /* S_EXXBCHL */
    "\tadd hl, de\n",                   /* S_ADDHLDE */
    "\tpush hl\n\tpop ix\n",            /* S_HLPIX */
    "\tor a\n",                         /* S_ORA */
    "\tld (hl), d\n",                   /* S_HLD */
    "\tld e, a  ; save byte value\n",  /* S_ESAVE */
    "\tld c, a\n",                      /* S_CA */
    "\tld b, h\n\tld c, l\n",           /* S_BCHLX */
    "\tld b, a\n",                      /* S_BA */
    "\tld a, l\n",                      /* S_AL */
    "\tld a, h\n\tor l\n",              /* S_AHORL */
    "\tld a, h\n",                      /* S_AH */
    "\tjr nz, $+3\n",                   /* S_JRNZ3 */
    "\tinc hl\n",                       /* S_INCHL */
    "\tex de, hl  ; save word value in DE\n", /* S_DESAVE */
    "\tex de, hl\n",                    /* S_EXBCHL */
    "\texx\n\tpush bc\n\texx\n",        /* S_EXXBC */
    "\tld a, (bc)\n",                   /* S_LDABC */
    "\tld a, (ix+0)\n",                 /* S_LDAIXZ */
    "\tld a, b\n",                      /* S_LDAB */
    "\tld a, c\n",                      /* S_LDAC */
    "\tld b, a\n",                      /* S_LDBA */
    "\tld c, a\n",                      /* S_LDCA */
    "\texx\n\tld a, b\n\texx\n",        /* S_EXXLDAB */
    "\texx\n\tld a, c\n\texx\n",        /* S_EXXLDAC */
    "\texx\n\tld b, a\n\texx\n",        /* S_EXXLDBA */
    "\texx\n\tld c, a\n\texx\n",        /* S_EXXLDCA */
    "\tadd hl, bc\n",                   /* S_ADDHLBC */
    "\tcall framealloc\n",              /* S_CALLFA */
    "\tcall getlong\n",                 /* S_CALLGL */
    "\tcall load32i\n",                 /* S_CALLL32I */
    "\tcall putlong\n",                 /* S_CALLPL */
    "\t; ERROR: failed to parse INCDEC_PLACEHOLDER\n", /* S_ERRPARS */
    "\tex de, hl  ; cached value in DE, swap to HL\n", /* S_CACHESWP */
    "\tex de, hl  ; DE = address\n",    /* S_DEADR */
    "\tex de, hl  ; push TOS to 2nd entry\n", /* S_PUSHTOS */
    "\texx\n\tld a, b\n\tor c\n\texx\n", /* S_EXXABCORC */
    "\texx\n\tpush bc\n\texx\n\tpop de\n\tadd hl, de\n", /* S_EXXBCPOPHL */
    "\t; fall back to normal indirect\n", /* S_FBKNORM */
    "\tjp framefree\n",                 /* S_JPFF */
    "\tld a, b\n\tor c\n",              /* S_ABCORC */
    "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl\n", /* S_LDEDHLSWP */
    "\tld hl, 0  ; upper 16 bits = 0\n", /* S_HLZERO */
    "\tpop af  ; return old value\n",  /* S_POPAFRET */
    "\tpop de  ; DE = address\n",      /* S_POPDEADR */
    "\tpop de  ; restore from nested op\n", /* S_POPDERES */
    "\tpop hl  ; old value for postfix\n", /* S_POPHPOST */
    "\tpop hl  ; restore lower word\n", /* S_POPHLLOW */
    "\tpop hl  ; restore upper word\n", /* S_POPHLUPP */
    "\tpop hl  ; return old value\n",  /* S_POPHLRET */
    "\tpush af  ; save old value\n",   /* S_PUSHAFSV */
    "\tpush bc\n",                      /* S_PUSHBC */
    "\tpush de  ; save address\n",     /* S_PUSHDESV */
    "\tpush de  ; spill 2nd stack entry\n", /* S_PUSHDESP */
    "\tpush hl  ; save lower word\n",  /* S_PUSHHLLOW */
    "\tpush hl  ; save old value\n",   /* S_PUSHHLOV */
    "\tpush hl  ; save upper word\n",  /* S_PUSHHLUPP */
    "\tpush ix\n",                      /* S_PUSHIX */
    "\tpush ix\n\tpop de\n\tadd hl, de\n", /* S_IXSWPHL */
    "\tret\n",                          /* S_RET */
    "\t; WARNING: byte reg holds pointer?\n", /* S_WARNBPTR */
    "\t; zero-extend short to long\n", /* S_ZEXTSL */
    "; Local variables:\n",             /* S_LOCVAR */
    "",                                 /* S_EMPTY */
    "\n"                                /* S_NEWLINE */
};

static void emit(char idx) {
    fdprintf(outFd, "%s", asmstr[idx]);
}

static const char *
getRegName(enum register_id reg)
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

/*
 * Label map for jump optimization
 * Tracks which labels are pure jumps and what they jump to
 */
#define MAX_LABELS 1000
static struct labelMap labelMap[MAX_LABELS];
static int lblMapCnt = 0;

/*
 * Add an entry to the label map
 */
static void
addLabelMap(int label, int target, enum jump_type jtype)
{
    if (lblMapCnt < MAX_LABELS) {
        labelMap[lblMapCnt].label = label;
        labelMap[lblMapCnt].target = target;
        labelMap[lblMapCnt].jump_type = jtype;
        lblMapCnt++;
    }
}

/*
 * Resolve a label transitively through the label map
 * Returns the final target label, avoiding jump-to-jump chains
 */
static int
resolveLabel(int label)
{
    int i;
    int visited[100];  /* Prevent infinite loops */
    int visited_count = 0;
    int current = label;

    while (visited_count < 100) {
        /* Check if we've already visited this label (cycle detection) */
        for (i = 0; i < visited_count; i++) {
            if (visited[i] == current) {
                return current;  /* Cycle detected, stop here */
            }
        }
        visited[visited_count++] = current;

        /* Look up current label in map */
        for (i = 0; i < lblMapCnt; i++) {
            if (labelMap[i].label == current &&
                labelMap[i].jump_type == JMP_UNCOND &&
                labelMap[i].target != -1) {
                /* Found a pure unconditional jump, follow it */
                current = labelMap[i].target;
                break;
            }
        }

        /* If we didn't find a mapping, we're done */
        if (i == lblMapCnt) {
            return current;
        }
    }

    /* Too many hops, give up */
    return label;
}

/*
 * Extract label number from ASM label string
 * Returns -1 if not a recognized label format
 * Handles formats: _if_N:, _if_end_N:, _while_N:, _while_end_N:, _tern_false_N:, _tern_end_N:, etc.
 */
static int
extLabelNum(const char *asm_text)
{
    const char *p;
    int num;

    if (!asm_text) return -1;

    /* Look for _tern_false_, _tern_end_ */
    if ((p = strstr(asm_text, "_tern_false_")) != NULL) {
        if (sscanf(p + 12, "%d:", &num) == 1) {
            return num;
        }
    }
    else if ((p = strstr(asm_text, "_tern_end_")) != NULL) {
        if (sscanf(p + 10, "%d:", &num) == 1) {
            return num;
        }
    }
    /* Look for _if_end_, _while_end_, etc. */
    else if ((p = strstr(asm_text, "_end_")) != NULL) {
        if (sscanf(p + 5, "%d:", &num) == 1) {
            return num;
        }
    }
    /* Look for _if_, _while_, etc. (but not _end_) */
    else if ((p = strstr(asm_text, "_if_")) != NULL) {
        if (sscanf(p + 4, "%d:", &num) == 1) {
            return num;
        }
    }
    else if ((p = strstr(asm_text, "_while_")) != NULL && strstr(asm_text, "_end_") == NULL) {
        if (sscanf(p + 7, "%d:", &num) == 1) {
            return num;
        }
    }

    return -1;
}

/*
 * Extract jump target from asm instruction
 * Returns label number if this is a jp/call to a known label, -1 otherwise
 */
static int
extJumpTarget(const char *asm_text, enum jump_type *jtype)
{
    const char *p;
    int num;

    if (!asm_text) return -1;

    /* Check for unconditional jump: jp _xxx_N */
    if ((p = strstr(asm_text, "jp ")) != NULL) {
        *jtype = JMP_UNCOND;
        /* Try various label formats */
        if (sscanf(p + 3, "_tern_end_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_tern_false_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_if_end_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_if_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_while_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_while_end_%d", &num) == 1) return num;
    }
    /* Check for conditional jumps */
    else if ((p = strstr(asm_text, "jp z,")) != NULL) {
        *jtype = JUMP_IF_ZERO;
        if (sscanf(p + 6, "_tern_false_%d", &num) == 1) return num;
        if (sscanf(p + 6, "_tern_end_%d", &num) == 1) return num;
        if (sscanf(p + 6, "_if_end_%d", &num) == 1) return num;
        if (sscanf(p + 6, "_if_%d", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "jp nz,")) != NULL) {
        *jtype = JMP_IF_NOT_Z;
        if (sscanf(p + 7, "_tern_false_%d", &num) == 1) return num;
        if (sscanf(p + 7, "_tern_end_%d", &num) == 1) return num;
        if (sscanf(p + 7, "_if_end_%d", &num) == 1) return num;
        if (sscanf(p + 7, "_if_%d", &num) == 1) return num;
    }

    return -1;
}

/*
 * Scan expression tree for jump nodes
 */
static void
scanExprJumps(struct expr *e)
{
    if (!e) return;

    /* Check if this expression has a jump node */
    if (e->jump && e->label > 0) {
        addLabelMap(e->label, e->jump->target_label, e->jump->type);
    }

    /* Recursively scan children */
    if (e->left) scanExprJumps(e->left);
    if (e->right) scanExprJumps(e->right);
}

/*
 * Scan statement tree to build label map
 * Identifies which labels are pure jumps to other labels
 */
static void
scanLabJumps(struct stmt *s)
{
    if (!s) return;

    /* Check if this is an ASM node with a label */
    if (s->type == 'A' && s->asm_block) {
        int label_num = extLabelNum(s->asm_block);

        if (label_num >= 0) {
            /* This is a label - check if next statement is a jump */
            if (s->next && s->next->type == 'A' && s->next->asm_block) {
                enum jump_type jtype;
                int target = extJumpTarget(s->next->asm_block, &jtype);
                if (target >= 0) {
                    /* This label is followed by a jump - record the mapping */
                    addLabelMap(label_num, target, jtype);
                }
            }
            /* Also check if next is a RETURN statement (jump to exit) */
            else if (s->next && s->next->type == 'R') {
                /* Label followed by return - could optimize jumps to this label */
                /* For now, we don't optimize this case */
            }
        }
    }

    /* Check if this statement has an explicit jump node */
    if (s->jump) {
        /* Record this label as jumping to target */
        if (s->label > 0) {
            addLabelMap(s->label, s->jump->target_label, s->jump->type);
        }
    }

    /* Scan expressions for jump nodes (e.g., ternary operators) */
    if (s->expr) scanExprJumps(s->expr);
    if (s->expr2) scanExprJumps(s->expr2);
    if (s->expr3) scanExprJumps(s->expr3);

    /* Recursively scan all branches */
    if (s->then_branch) scanLabJumps(s->then_branch);
    if (s->else_branch) scanLabJumps(s->else_branch);
    if (s->next) scanLabJumps(s->next);
}

/*
 * Emit a jump instruction with label resolution
 * Resolves the target label transitively to avoid jump-to-jump chains
 */
static void
emitJump(const char *instr, const char *prefix, int label)
{
    int resolved = resolveLabel(label);
    fdprintf(outFd, "\t%s %s%d\n", instr, prefix, resolved);
}

/*
 * Emit function prologue
 * Outputs assembly comment describing function and function label
 */
static void
emitFnProlog(char *name, char *params, char *rettype, int frame_size,
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

    emit(S_NEWLINE);

    /* Output local variable information as assembly comments */
    if (locals) {
        emit(S_LOCVAR);
        for (var = locals; var; var = var->next) {
            const char *regname = getRegName(var->reg);

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

    /* Function label - static functions (mangled names) have no prefix */
    if (isMangledName(name)) {
        fdprintf(outFd, "%s:\n", name);
    } else {
        fdprintf(outFd, "_%s:\n", name);
    }

    /* Emit frame allocation call if we have locals or parameters */
    if (frame_size > 0 || has_params) {
        fdprintf(outFd, "\tld a, %d\n", frame_size);
        emit(S_CALLFA);
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
                    emit(S_BCHLX);
                } else if (var->reg == REG_IX) {
                    /* No direct ld ix, (iy+offset) - must go through HL */
                    fdprintf(outFd, "\tld l, (iy + %d)\n", var->offset);
                    fdprintf(outFd, "\tld h, (iy + %d)\n", var->offset + 1);
                    emit(S_HLPIX);
                }
            } else if (var->size == 1) {
                /* Byte parameter - data is at offset+1 (pushed as AF) */
                if (var->reg == REG_B || var->reg == REG_C) {
                    fdprintf(outFd, "\tld a, (iy + %d)\n", var->offset + 1);
                    if (var->reg == REG_B) {
                        emit(S_BA);
                    } else {
                        emit(S_CA);
                    }
                }
            }
        }
    }
}

/*
 * Check if function name is a comparison that sets Z flag
 * These functions return 0/1 in HL and set Z flag to match
 */
static int isCmpFunc(const char *fname) {
    if (!fname) return 0;

    /* Check for comparison function patterns */
    if (strstr(fname, "eq16") || strstr(fname, "eq32") || strstr(fname, "eq88"))
        return 1;
    if (strstr(fname, "ne16") || strstr(fname, "ne32") || strstr(fname, "ne88"))
        return 1;
    if (strstr(fname, "lt16") || strstr(fname, "lt32") || strstr(fname, "lt88"))
        return 1;
    if (strstr(fname, "gt16") || strstr(fname, "gt32") || strstr(fname, "gt88"))
        return 1;
    if (strstr(fname, "le16") || strstr(fname, "le32") || strstr(fname, "le88"))
        return 1;
    if (strstr(fname, "ge16") || strstr(fname, "ge32") || strstr(fname, "ge88"))
        return 1;
    if (strstr(fname, "and16") || strstr(fname, "and32") || strstr(fname, "and88"))
        return 1;
    if (strstr(fname, "or16") || strstr(fname, "or32") || strstr(fname, "or88"))
        return 1;

    return 0;
}

/*
 * Expression cache helpers
 * Create shallow copy of expression for caching (no asm_block, no children)
 */
static struct expr *shallowCopy(struct expr *e) {
    struct expr *copy;
    if (!e) return NULL;

    copy = malloc(sizeof(struct expr));
    if (!copy) return NULL;

    memcpy(copy, e, sizeof(struct expr));

    /* Clear pointers - this is a shallow copy for caching */
    copy->left = NULL;
    copy->right = NULL;
    copy->asm_block = NULL;
    copy->cleanup_block = NULL;
    copy->jump = NULL;

    return copy;
}

/*
 * Create cache node for variable: DEREF(SYM(symbol))
 * Returns DEREF node on success, NULL on failure
 */
static struct expr *mkVarCache(const char *symbol, unsigned char size) {
    struct expr *sym_node;
    struct expr *deref_node;

    sym_node = malloc(sizeof(struct expr));
    if (!sym_node) return NULL;

    sym_node->op = '$';
    sym_node->symbol = (char *)symbol;  /* Cast: we don't modify the string */
    sym_node->size = size;
    sym_node->left = NULL;
    sym_node->right = NULL;
    sym_node->asm_block = NULL;
    sym_node->cleanup_block = NULL;
    sym_node->jump = NULL;
    sym_node->flags = 0;

    deref_node = malloc(sizeof(struct expr));
    if (!deref_node) {
        free(sym_node);
        return NULL;
    }

    deref_node->op = 'M';
    deref_node->size = size;
    deref_node->left = sym_node;
    deref_node->right = NULL;
    deref_node->symbol = NULL;
    deref_node->asm_block = NULL;
    deref_node->cleanup_block = NULL;
    deref_node->jump = NULL;
    deref_node->flags = 0;

    return deref_node;
}

/*
 * Check if two expressions match (for cache lookup)
 * Only checks SYM and DEREF patterns for now
 */
static int matchesCache(struct expr *e1, struct expr *e2) {
    if (!e1 || !e2) return 0;
    if (e1->op != e2->op) return 0;
    if (e1->size != e2->size) return 0;

    if (e1->op == '$') {
        /* Symbol: compare names */
        if (!e1->symbol || !e2->symbol) return 0;
        return strcmp(e1->symbol, e2->symbol) == 0;
    }

    if (e1->op == 'M') {
        /* DEREF: recursively check child */
        return matchesCache(e1->left, e2->left);
    }

    return 0;
}

/*
 * Emit: load word from (iy+offset) into HL
 */
static void loadWordIY(int offset) {
    if (offset >= 0) {
        fdprintf(outFd, "\tld l, (iy + %d)\n", offset);
        fdprintf(outFd, "\tld h, (iy + %d)\n", offset + 1);
    } else {
        fdprintf(outFd, "\tld l, (iy - %d)\n", -offset);
        fdprintf(outFd, "\tld h, (iy - %d)\n", -offset - 1);
    }
}

/*
 * Emit: store word from HL to (iy+offset)
 */
static void storeWordIY(int offset) {
    if (offset >= 0) {
        fdprintf(outFd, "\tld (iy + %d), l\n", offset);
        fdprintf(outFd, "\tld (iy + %d), h\n", offset + 1);
    } else {
        fdprintf(outFd, "\tld (iy - %d), l\n", -offset);
        fdprintf(outFd, "\tld (iy - %d), h\n", -offset - 1);
    }
}

/*
 * Clear HL cache
 */
static void clearHL(struct function_ctx *ctx) {
    if (!ctx) return;
    if (ctx->hl_cache) {
        freeExpr(ctx->hl_cache);
        ctx->hl_cache = NULL;
    }
}

/*
 * Clear DE cache
 */
static void clearDE(struct function_ctx *ctx) {
    if (!ctx) return;
    if (ctx->de_cache) {
        freeExpr(ctx->de_cache);
        ctx->de_cache = NULL;
    }
}

/*
 * Stack machine model: HL = TOS (top of stack), DE = 2nd entry
 * Push HL onto stack (move to DE, spilling DE if needed)
 */
static void pushStack(struct function_ctx *ctx) {
    if (!ctx) return;

    if (ctx->de_valid) {
        /* DE already holds a value - spill it to real stack */
        emit(S_PUSHDESP);
        ctx->de_save_count++;
        clearDE(ctx);  /* Cache is now invalid */
    }

    /* Move HL (TOS) to DE (2nd entry) */
    emit(S_PUSHTOS);
    ctx->de_valid = 1;

    /* Copy HL cache to DE cache */
    if (ctx->de_cache) {
        freeExpr(ctx->de_cache);
    }
    ctx->de_cache = ctx->hl_cache;
    ctx->hl_cache = NULL;

    /* HL is now free for new TOS */
    /* Most operations that modify HL also clobber Z flag */
    ctx->zflag_valid = 0;
}

/*
 * Pop stack: mark DE as consumed (no actual code gen needed)
 */
static void popStack(struct function_ctx *ctx) {
    if (!ctx) return;
    ctx->de_valid = 0;
    ctx->zflag_valid = 0;
    clearDE(ctx);
}

/*
 * Invalidate stack state (for operations that clobber registers)
 */
static void invalStack(struct function_ctx *ctx) {
    if (!ctx) return;
    ctx->de_valid = 0;
    ctx->zflag_valid = 0;
    clearHL(ctx);
    clearDE(ctx);
}

/*
 * Helper: Check if operator is a binary operator that needs accumulator
 * management
 */
static int
isBinopWAccum(unsigned char op)
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
        struct local_var *var = NULL;
        const char *global_sym = NULL;
        struct expr *temp;

        /* Check cache first - if value already in HL, no code needed */
        if (ctx->hl_cache && matchesCache(e, ctx->hl_cache)) {
            /* Value already in HL - mark as generated and return */
            e->flags |= E_GENERATED;
            freeExpr(e->left);
            if (e->asm_block) free(e->asm_block);
            if (e->cleanup_block) free(e->cleanup_block);
            free(e);
            return;
        }

        /* Check if value is in DE - swap if so */
        if (ctx->de_cache && matchesCache(e, ctx->de_cache)) {
            /* Value in DE - swap DE and HL */
            emit(S_CACHESWP);

            /* Swap caches */
            temp = ctx->hl_cache;
            ctx->hl_cache = ctx->de_cache;
            ctx->de_cache = temp;

            /* Mark as generated and return */
            e->flags |= E_GENERATED;
            freeExpr(e->left);
            if (e->asm_block) free(e->asm_block);
            if (e->cleanup_block) free(e->cleanup_block);
            free(e);
            return;
        }

        /* Look up variable BEFORE emitting child (which frees it) */

        if (e->left && e->left->op == '$' && e->left->symbol) {
            var = findVar(ctx, e->left->symbol);
            /* If not found as local var, it's a global - save symbol name */
            if (!var) {
                global_sym = e->left->symbol;
            }
        }

        /* Free child without emitting (we'll emit the load ourselves) */
        freeExpr(e->left);

        /* Now emit the load inst based on current register allocation */
        if (var) {
            if (var->reg != REG_NO) {
                /* Variable is in a register - move to PRIMARY */
                if (e->size == 1) {
                    /* Byte: move register to A */
                    if (var->reg == REG_B) {
                        emit(S_LDAB);
                    } else if (var->reg == REG_C) {
                        emit(S_LDAC);
                    } else if (var->reg == REG_Bp) {
                        emit(S_EXXLDAB);
                    } else if (var->reg == REG_Cp) {
                        emit(S_EXXLDAC);
                    }
                } else {
                    /* Word: move register pair to HL */
                    if (var->reg == REG_BC) {
                        emit(S_BCHL);
                    } else if (var->reg == REG_IX) {
                        emit(S_IXHL);
                    }
                }
            } else {
                /* Variable is on stack - load from (iy + offset) */
                if (e->size == 1) {
                    if (var->offset >= 0) {
                        /* Parameter: byte is at offset+1 (pushed as AF) */
                        fdprintf(outFd, "\tld a, (iy + %d)\n", var->offset + 1);
                    } else {
                        /* Local variable: byte is at offset */
                        fdprintf(outFd, "\tld a, (iy - %d)\n", -var->offset);
                    }
                } else if (e->size == 2) {
                    loadWordIY(var->offset);
                } else {
                    /* Long (4 bytes) - use getlong function */
                    fdprintf(outFd, "\tld a, %d\n", var->offset);
                    emit(S_CALLGL);
                }
            }
        } else if (global_sym) {
            /* Global variable - direct memory access */
            const char *sym = stripDollar(global_sym);
            if (e->size == 1) {
                fdprintf(outFd, "\tld a, (%s)\n", sym);
            } else if (e->size == 2) {
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
            } else if (e->size == 4) {
                /* Long - load HL'HL from global */
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
                emit(S_EXX);
                fdprintf(outFd, "\tld hl, (%s+2)\n", sym);
                emit(S_EXX);
            }
        }

        /* Value now in HL (TOS) - Z flag may be invalid */
        ctx->zflag_valid = 0;

        /* Save expression to cache */
        clearHL(ctx);
        ctx->hl_cache = shallowCopy(e);

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
                emit(S_EXX);
                fdprintf(outFd, "\tld l, (ix + %ld)\n", offset + 2);
                fdprintf(outFd, "\tld h, (ix + %ld)\n", offset + 3);
                emit(S_EXX);
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
                emit(S_AHL);
            } else if (e->size == 2) {
                emit(S_LDEDHLSWP);
            } else if (e->size == 4) {
                emit(S_CALLL32I);
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
                emit(S_LDABC);
            } else if (var->reg == REG_IX) {
                emit(S_LDAIXZ);
            } else {
                /* Register type can't do indirect addressing - fall back */
                /* Load pointer to HL first, then indirect load */
                if (var->reg == REG_B || var->reg == REG_C ||
                    var->reg == REG_Bp || var->reg == REG_Cp) {
                    /* Single byte register shouldn't hold pointer, but handle it */
                    emit(S_WARNBPTR);
                }
                emit(S_FBKNORM);
                /* Emit the inner DEREF to load pointer */
                emitExpr(ctx, e->left);
                emit(S_AHL);
            }
        } else {
            /* Pointer not register-allocated - load to HL then indirect */
            emitExpr(ctx, e->left);
            emit(S_AHL);
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
    /* Handle increment/decrement placeholders - need to check register allocation */
    else if (e->asm_block && strstr(e->asm_block, "INCDEC_PLACEHOLDER:")) {
        /* Parse placeholder format: INCDEC_PLACEHOLDER:op:size:amount:unused:symbol */
        int op, size, unused;
        long amount;
        char symbol[256];
        const char *p;
        const char *var_name;
        struct local_var *var;
        int is_post, is_dec;

        p = strstr(e->asm_block, "INCDEC_PLACEHOLDER:") + 19;
        if (sscanf(p, "%d:%d:%ld:%d:%255s", &op, &size, &amount, &unused, symbol) != 5) {
            /* Parse error - emit comment and skip */
            emit(S_ERRPARS);
            if (e->asm_block) free(e->asm_block);
            if (e->cleanup_block) free(e->cleanup_block);
            free(e);
            return;
        }

        is_post = (op == 0xef || op == 0xf6);
        is_dec = (op == 0xd6 || op == 0xf6);

        /* If result is unused, treat postfix like prefix (simpler code) */
        if (unused && is_post) is_post = 0;

        /* Strip $ and A prefixes from symbol */
        var_name = symbol;
        if (var_name[0] == '$') var_name++;
        if (var_name[0] == 'A') var_name++;

        /* Look up variable */
        var = findVar(ctx, var_name);

        if (var && var->reg != REG_NO) {
            /* Variable is register-allocated */
            if (size == 1) {
                /* Byte register */
                const char *reg_name = (var->reg == REG_B || var->reg == REG_Bp) ? "b" : "c";
                int use_alt = (var->reg == REG_Bp || var->reg == REG_Cp);

                if (use_alt) emit(S_EXX);
                if (is_post) fdprintf(outFd, "\tld a, %s\n", reg_name);
                if (amount == 1) {
                    fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", reg_name);
                } else {
                    fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                    fdprintf(outFd, "\tld %s, a\n", reg_name);
                }
                if (!is_post) fdprintf(outFd, "\tld a, %s\n", reg_name);
                if (use_alt) emit(S_EXX);
            } else {
                /* Word register */
                const char *reg_pair = (var->reg == REG_IX) ? "ix" : "bc";
                int use_alt = (var->reg == REG_BCp);

                if (use_alt) emit(S_EXX);
                if (is_post) {
                    if (var->reg == REG_IX) emit(S_IXHL);
                    else emit(S_BCHL);
                }
                if (amount == 1) {
                    fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", reg_pair);
                } else {
                    fdprintf(outFd, "\tpush %s\n", reg_pair);
                    fdprintf(outFd, "\tld de, %ld\n", amount);
                    if (is_dec) {
                        fdprintf(outFd, "\tor a\n\tsbc %s, de\n", reg_pair);
                    } else {
                        fdprintf(outFd, "\tadd %s, de\n", reg_pair);
                    }
                    emit(S_POPHPOST);
                }
                if (!is_post && amount != 1) {
                    if (var->reg == REG_IX) emit(S_IXHL);
                    else emit(S_BCHL);
                } else if (!is_post) {
                    if (var->reg == REG_IX) emit(S_IXHL);
                    else emit(S_BCHL);
                }
                if (use_alt) emit(S_EXX);
            }
        } else if (var) {
            /* Variable is on stack */
            char sign = (var->offset >= 0) ? '+' : '-';
            int abs_offset = (var->offset >= 0) ? var->offset : -var->offset;
            /* For byte parameters (offset >= 0), data is at offset+1 (pushed as AF) */
            int byte_adj = (size == 1 && var->offset >= 0) ? 1 : 0;

            if (size == 1) {
                /* Byte on stack */
                if (is_post) fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset + byte_adj);
                if (amount == 1) {
                    fdprintf(outFd, "\t%s (iy %c %d)\n", is_dec ? "dec" : "inc", sign, abs_offset + byte_adj);
                } else {
                    fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset + byte_adj);
                    fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                    fdprintf(outFd, "\tld (iy %c %d), a\n", sign, abs_offset + byte_adj);
                }
                if (!is_post) fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset + byte_adj);
            } else {
                /* Word on stack */
                if (is_post) {
                    fdprintf(outFd, "\tld l, (iy %c %d)\n", sign, abs_offset);
                    fdprintf(outFd, "\tld h, (iy %c %d)\n", sign, abs_offset + 1);
                }

                if (amount == 1) {
                    if (is_dec) {
                        fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset);
                        fdprintf(outFd, "\tdec (iy %c %d)\n", sign, abs_offset);
                        emit(S_ORA);
                        fdprintf(outFd, "\tjr nz, $+3\n");
                        fdprintf(outFd, "\tdec (iy %c %d)\n", sign, abs_offset + 1);
                    } else {
                        fdprintf(outFd, "\tinc (iy %c %d)\n", sign, abs_offset);
                        fdprintf(outFd, "\tjr nz, $+3\n");
                        fdprintf(outFd, "\tinc (iy %c %d)\n", sign, abs_offset + 1);
                    }
                } else {
                    fdprintf(outFd, "\tld l, (iy %c %d)\n", sign, abs_offset);
                    fdprintf(outFd, "\tld h, (iy %c %d)\n", sign, abs_offset + 1);
                    fdprintf(outFd, "\tld de, %ld\n", amount);
                    if (is_dec) {
                        emit(S_SBCHLDE);
                    } else {
                        emit(S_ADDHLDE);
                    }
                    fdprintf(outFd, "\tld (iy %c %d), l\n", sign, abs_offset);
                    fdprintf(outFd, "\tld (iy %c %d), h\n", sign, abs_offset + 1);
                }

                if (!is_post) {
                    fdprintf(outFd, "\tld l, (iy %c %d)\n", sign, abs_offset);
                    fdprintf(outFd, "\tld h, (iy %c %d)\n", sign, abs_offset + 1);
                }
            }
        } else {
            /* Global variable */
            const char *sym = stripDollar(symbol);

            if (size == 1) {
                /* Byte global */
                if (is_post) {
                    fdprintf(outFd, "\tld a, (%s)\n", sym);
                    emit(S_PUSHAFSV);
                    if (amount == 1) {
                        fdprintf(outFd, "\t%s a\n", is_dec ? "dec" : "inc");
                    } else {
                        fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                    }
                    fdprintf(outFd, "\tld (%s), a\n", sym);
                    emit(S_POPAFRET);
                } else {
                    fdprintf(outFd, "\tld a, (%s)\n", sym);
                    if (amount == 1) {
                        fdprintf(outFd, "\t%s a\n", is_dec ? "dec" : "inc");
                    } else {
                        fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                    }
                    fdprintf(outFd, "\tld (%s), a\n", sym);
                }
            } else {
                /* Word global */
                if (is_post) {
                    fdprintf(outFd, "\tld hl, (%s)\n", sym);
                    emit(S_PUSHHLOV);
                    if (amount == 1) {
                        fdprintf(outFd, "\t%s hl\n", is_dec ? "dec" : "inc");
                    } else {
                        fdprintf(outFd, "\tld de, %ld\n", amount);
                        if (is_dec) {
                            emit(S_SBCHLDE);
                        } else {
                            emit(S_ADDHLDE);
                        }
                    }
                    fdprintf(outFd, "\tld (%s), hl\n", sym);
                    emit(S_POPHLRET);
                } else {
                    fdprintf(outFd, "\tld hl, (%s)\n", sym);
                    if (amount == 1) {
                        fdprintf(outFd, "\t%s hl\n", is_dec ? "dec" : "inc");
                    } else {
                        fdprintf(outFd, "\tld de, %ld\n", amount);
                        if (is_dec) {
                            emit(S_SBCHLDE);
                        } else {
                            emit(S_ADDHLDE);
                        }
                    }
                    fdprintf(outFd, "\tld (%s), hl\n", sym);
                }
            }
        }

        /* Free this node */
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
                    emit(S_EXX);
                    fdprintf(outFd, "\tld (ix + %ld), l\n", offset + 2);
                    fdprintf(outFd, "\tld (ix + %ld), h\n", offset + 3);
                    emit(S_EXX);
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
                        emit(S_BA);
                    } else if (var->reg == REG_C) {
                        emit(S_CA);
                    } else if (var->reg == REG_Bp) {
                        emit(S_EXXLDBA);
                    } else if (var->reg == REG_Cp) {
                        emit(S_EXXLDCA);
                    }
                } else {
                    /* Word: move HL to register pair */
                    if (var->reg == REG_BC || var->reg == REG_IX) {
                        if (var->reg == REG_BC) {
                            emit(S_BCHLX);
                        } else {
                            emit(S_HLPIX);
                        }

                        /* After ld b,h; ld c,l OR push hl; pop ix,
                         * HL still contains the value
                         * Update cache to reflect HL now represents the variable */
                        clearHL(ctx);
                        ctx->hl_cache = mkVarCache(e->left->symbol, e->size);
                    }
                }
            } else if (var) {
                /* Variable is on stack - store to (iy + offset) */
                if (e->size == 1) {
                    if (var->offset >= 0) {
                        /* Parameter: byte is at offset+1 (pushed as AF) */
                        fdprintf(outFd, "\tld (iy + %d), a\n", var->offset + 1);
                    } else {
                        /* Local variable: byte is at offset */
                        fdprintf(outFd, "\tld (iy - %d), a\n", -var->offset);
                    }
                } else if (e->size == 2) {
                    storeWordIY(var->offset);
                } else {
                    /* Long (4 bytes) - use putlong function */
                    fdprintf(outFd, "\tld a, %d\n", var->offset);
                    emit(S_CALLPL);
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
                    emit(S_EXX);
                    fdprintf(outFd, "\tld (%s+2), hl\n", sym);
                    emit(S_EXX);
                }
            }
        }

        /* Handle complex lvalue: compute address, then store through it */
        else if (e->left && e->left->op == 'M') {
            /* Pointer dereference: (M $var) - store through pointer */
            /* Value is already in PRIMARY (HL or A) */
            if (e->size == 1) {
                /* Byte: value in A, need address in HL */
                emit(S_ESAVE);
                emitExpr(ctx, e->left->left);  /* Load pointer to HL */
                emit(S_HLDE);
            } else if (e->size == 2) {
                /* Word: value in HL, need address in HL - save value first */
                emit(S_DESAVE);
                emitExpr(ctx, e->left->left);  /* Load pointer to HL */
                emit(S_HLDE);
                emit(S_INCHL);
                emit(S_HLD);
            }
        }
        else if (e->left && e->left->op == '+') {
            /* Complex lvalue like (+ (M:p $var) offset) */
            /* Value is already in PRIMARY (HL or A) */
            /* Need to: save value, compute address, store through address */

            if (e->size == 1) {
                /* Byte: value in A */
                /* Save value to E, compute address to HL, store (HL) <- E */
                emit(S_ESAVE);
                emitExpr(ctx, e->left);  /* Compute address to HL */
                emit(S_HLDE);
            } else if (e->size == 2) {
                /* Word: value in HL */
                /* Save value to DE, compute address to HL, store */
                emit(S_DESAVE);
                emitExpr(ctx, e->left);  /* Compute address to HL */
                emit(S_HLDE);
                emit(S_INCHL);
                emit(S_HLD);
            } else if (e->size == 4) {
                /* Long: value in HL'HL */
                /* This is complex - need to save 4 bytes */
                emit(S_PUSHHLLOW);
                emit(S_EXX);
                emit(S_PUSHHLUPP);
                emit(S_EXX);
                emitExpr(ctx, e->left);  /* Compute address to HL */
                emit(S_DEADR);
                emit(S_POPHLUPP);
                emit(S_PUSHDESV);
                emit(S_EXX);
                emit(S_POPDEADR);
                emit(S_POPHLLOW);
                /* Store 4 bytes: (DE) <- HL, (DE+2) <- HL' */
                emit(S_AL);
                emit(S_DEA);
                emit(S_INCDE);
                emit(S_AH);
                emit(S_DEA);
                emit(S_INCDE);
                emit(S_EXX);
                emit(S_AL);
                emit(S_DEA);
                emit(S_INCDE);
                emit(S_AH);
                emit(S_DEA);
                emit(S_EXX);
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
                    emit(S_BCHL);
                } else if (var->reg == REG_BCp) {
                    /* Can't use exx - it switches HL too, making result inaccessible */
                    /* Stage BC' through stack into HL */
                    emit(S_EXXBCHL);
                } else {  /* REG_IX */
                    emit(S_IXHL);
                }
                /* Now do the inc hl sequence */
                fdprintf(outFd, "%s\n", e->asm_block);
            } else {
                /* Use add hl, reg for larger constants */
                fdprintf(outFd, "\tld hl, %ld\n", const_val);
                if (var->reg == REG_BC) {
                    emit(S_ADDHLBC);
                } else if (var->reg == REG_BCp) {
                    /* Can't use exx - it switches HL too */
                    /* Stage BC' into DE, then add */
                    emit(S_EXXBCPOPHL);
                } else {  /* REG_IX */
                    /* Z80 doesn't have 'add hl, ix' - stage through DE */
                    emit(S_IXSWPHL);
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
    else if (isBinopWAccum(e->op) && e->left && e->right &&
            e->asm_block) {
        /* Check for inline byte operations with immediate (and/or/xor) */
        int isInlineImm = 0;
        if (!strchr(e->asm_block, '\n') &&
            (e->op == '&' || e->op == '|' || e->op == '^') &&
            e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
            e->right->value >= 0 && e->right->value <= 255) {
            /* Single-line asm_block for byte bitwise op with constant */
            isInlineImm = 1;
        }

        if (isInlineImm) {
            /* Inline immediate: just emit left to A, then inline instruction */
            emitExpr(ctx, e->left);
            /* Don't emit right child - constant is baked into instruction */
            freeExpr(e->right);
            fdprintf(outFd, "%s\n", e->asm_block);
        } else {
            /* Stack machine binop: left → TOS, push, right → TOS, operate */
            char *call_inst = NULL;
            char *newline = strchr(e->asm_block, '\n');
            int init_saves = ctx->de_save_count;

            /* Extract call instruction (skip move instruction - we do it explicitly) */
            if (newline) {
                call_inst = strdup(newline + 1);
            }

            /* 1. Evaluate left operand → HL (TOS) */
            emitExpr(ctx, e->left);

            /* 2. Push HL onto stack (move to DE, spilling if needed) */
            pushStack(ctx);

            /* 3. Evaluate right operand → HL (TOS) */
            /* Right's evaluation may itself use the stack machine */
            emitExpr(ctx, e->right);

            /* 4. Restore saved DE values from nested operations */
            while (ctx->de_save_count > init_saves) {
                emit(S_POPDERES);
                ctx->de_save_count--;
            }

            /* 5. Call binary operation (DE, HL) → HL */
            if (call_inst) {
                fdprintf(outFd, "%s\n", call_inst);

                /* Check if this is a comparison function that sets Z flag */
                if (strstr(call_inst, "call")) {
                    char *call_pos = strstr(call_inst, "call");
                    call_pos += 4;  /* Skip "call" */
                    while (*call_pos && (*call_pos == ' ' || *call_pos == '\t')) call_pos++;
                    if (*call_pos && isCmpFunc(call_pos)) {
                        ctx->zflag_valid = 1;
                    }
                }

                free(call_inst);
            }
        }  /* End of else (standard binop) */

        /* Binary op consumed DE and produced result in HL - pop stack, preserve zflag if set */
        {
            int zflag_saved = ctx->zflag_valid;
            popStack(ctx);
            ctx->zflag_valid = zflag_saved;
        }
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
                        char *next_line_st = line_end + 1;
                        char next_buf[512];
                        char *next_end = strchr(next_line_st, '\n');
                        int next_len;

                        if (next_end) {
                            next_len = next_end - next_line_st;
                            if (next_len >= sizeof(next_buf)) next_len = sizeof(next_buf) - 1;
                            memcpy(next_buf, next_line_st, next_len);
                            next_buf[next_len] = '\0';
                        } else {
                            strncpy(next_buf, next_line_st, sizeof(next_buf) - 1);
                            next_buf[sizeof(next_buf) - 1] = '\0';
                            next_len = strlen(next_buf);
                        }

                        /* Check if it's a push hl instruction */
                        if (strstr(next_buf, "push hl")) {
                            /* Optimize: push register directly */
                            if (var->reg == REG_BC) {
                                emit(S_PUSHBC);
                            } else if (var->reg == REG_BCp) {
                                emit(S_EXXBC);
                            } else if (var->reg == REG_IX) {
                                emit(S_PUSHIX);
                            }
                            /* Skip the push hl line */
                            line_start = next_end ? next_end + 1 : next_line_st + next_len;
                            continue;
                        }
                    }

                    /* Standard path: load to HL */
                    if (var && var->reg != REG_NO) {
                        /* Variable in register - move to HL */
                        if (var->reg == REG_BC) {
                            emit(S_BCHL);
                        } else if (var->reg == REG_BCp) {
                            /* Can't use exx - it switches HL too */
                            emit(S_EXXBCHL);
                        } else if (var->reg == REG_IX) {
                            emit(S_IXHL);
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

                    /* Check if this is a call to a comparison function that sets Z flag */
                    if (strstr(line_buf, "call")) {
                        char *call_pos = strstr(line_buf, "call");
                        /* Skip "call " and any whitespace to get function name */
                        call_pos += 4;  /* Skip "call" */
                        while (*call_pos == ' ' || *call_pos == '\t') call_pos++;
                        if (isCmpFunc(call_pos)) {
                            ctx->zflag_valid = 1;
                        }
                    }
                }
            }
        }
        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }

        /* Save zflag_valid before invalidating stack */
        {
            int zflag_saved = ctx->zflag_valid;
            /* Function call clobbers registers - invalidate stack state */
            invalStack(ctx);
            /* Restore zflag_valid for comparison functions */
            ctx->zflag_valid = zflag_saved;
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
                emit(S_BCHL);
            } else if (var->reg == REG_BCp) {
                /* Can't use exx - it switches HL too */
                emit(S_EXXBCHL);
            } else if (var->reg == REG_IX) {
                emit(S_IXHL);
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
    /* Handle ternary operator (? :) */
    else if (e->op == '?') {
        /* Ternary: condition ? true_expr : false_expr */
        /* Tree: '?' has condition in left, ':' node in right */
        /* ':' has true_expr in left, false_expr in right */
        unsigned char cond_size;

        /* Save condition size before emitting (emission frees the node) */
        cond_size = e->left ? e->left->size : 2;

        /* Emit condition evaluation */
        if (e->left) emitExpr(ctx, e->left);

        /* Test condition result (in PRIMARY - HL for words, A for bytes) */
        if (cond_size == 1) {
            /* Byte condition - test A */
            emit(S_ORA);  /* Set Z flag based on A */
        } else {
            /* Word condition - test HL */
            if (!ctx->zflag_valid) {
                emit(S_AHORL);  /* Set Z flag if HL==0 */
            }
        }
        ctx->zflag_valid = 0;  /* Z flag consumed */

        /* Jump to false branch if condition is zero (using resolved label) */
        if (e->jump) {
            emitJump("jp z,", "_tern_false_", e->label);
        }

        /* Emit true branch */
        if (e->right && e->right->left) {
            emitExpr(ctx, e->right->left);
        }

        /* Jump over false branch (using resolved label) */
        if (e->right && e->right->jump) {
            emitJump("jp", "_tern_end_", e->right->label);
        }

        /* Emit false label */
        fdprintf(outFd, "_tern_false_%d:\n", e->label);

        /* Emit false branch */
        if (e->right && e->right->right) {
            emitExpr(ctx, e->right->right);
        }

        /* Emit end label */
        if (e->right) {
            fdprintf(outFd, "_tern_end_%d:\n", e->right->label);
        }

        /* Result is in PRIMARY (either from true or false branch) */

        /* Free jump nodes */
        if (e->jump) freeJump(e->jump);
        if (e->right && e->right->jump) freeJump(e->right->jump);
        if (e->right) free(e->right);  /* Free COLON node */

        /* Free this node and return */
        if (e->asm_block) free(e->asm_block);
        if (e->cleanup_block) free(e->cleanup_block);
        free(e);
        return;
    }
    else {
        /* Normal postorder traversal for other operators */
        if (e->left) emitExpr(ctx, e->left);
        if (e->right) emitExpr(ctx, e->right);

        if (e->asm_block) {
            if (e->asm_block[0]) {  /* Only if non-empty */
                fdprintf(outFd, "%s\n", e->asm_block);
            }
            /* Empty asm_block - don't emit anything */

            /* Check if this is a call to a comparison function that sets Z flag */
            if (strstr(e->asm_block, "call")) {
                char *call_pos;
                call_pos = strstr(e->asm_block, "call");
                /* Skip "call" and any whitespace/newlines to get function name */
                call_pos += 4;  /* Skip "call" */
                while (*call_pos && (*call_pos == ' ' || *call_pos == '\t' || *call_pos == '\n' || *call_pos == '\r')) {
                    call_pos++;
                }
                if (*call_pos && isCmpFunc(call_pos)) {
                    ctx->zflag_valid = 1;
                }
            }
        }

        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    if (e->asm_block) free(e->asm_block);
    if (e->cleanup_block) free(e->cleanup_block);
    if (e->jump) freeJump(e->jump);
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
        int invertCond = 0;
        int use_dir_jump = 0;
        struct expr *cond = s->expr;

        /* Check if condition has ! wrapper */
        if (cond && cond->op == '!') {
            invertCond = 1;
            cond = cond->left;  /* unwrap ! */
        }

        /* Check if this is a byte operation that sets Z flag */
        /* Byte bitwise ops with constants emit inline instructions that set Z */
        if (cond && (cond->op == '&' || cond->op == '|' || cond->op == '^') &&
            cond->left && cond->left->size == 1 &&
            cond->right && cond->right->op == 'C' &&
            cond->right->value >= 0 && cond->right->value <= 255) {
            use_dir_jump = 1;
        }
        /* Other byte-sized operations */
        else if (cond && cond->size == 1) {
            use_dir_jump = 1;
        }

        if (use_dir_jump) {
            /* Emit unwrapped condition expression (leaves Z flag set) */
            emitExpr(ctx, cond);

            /* If we had a ! wrapper, manually free it (child already freed) */
            if (invertCond && s->expr != cond) {
                free(s->expr);
            }

            /* Jump to fail label if condition is false (use optimized jumps) */
            if (s->label2 > 0) {
                /* Has else (or had empty else): jump to else on false, fall through to then on true */
                if (invertCond) {
                    /* ! wrapper: jump if nonzero (true) */
                    emitJump("jp nz,", "_if_", s->label);
                } else {
                    /* No !: jump if zero (false) */
                    emitJump("jp z,", "_if_", s->label);
                }
            } else {
                /* No else: jump to end on false */
                if (invertCond) {
                    /* ! wrapper: jump if nonzero (true) */
                    emitJump("jp nz,", "_if_end_", s->label);
                } else {
                    /* No !: jump if zero (false) */
                    emitJump("jp z,", "_if_end_", s->label);
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
                    emit(S_ABCORC);
                } else {  /* REG_BCp */
                    emit(S_EXXABCORC);
                }
                /* Free the condition expression without emitting it */
                freeExpr(cond);
                /* If we had ! wrapper, free it too */
                if (invertCond && s->expr != cond) {
                    free(s->expr);
                }
            } else {
                /* Evaluate to HL and test */
                emitExpr(ctx, s->expr);
                /* Test if HL is zero */
                if (!ctx->zflag_valid) {
                    emit(S_AHORL);
                }
                ctx->zflag_valid = 0;  /* Z flag consumed */
            }

            /* Jump based on test result (use resolved labels to avoid jump-to-jump) */
            if (s->label2 > 0) {
                int target = resolveLabel(s->label);
                if (invertCond) {
                    fdprintf(outFd, "\tjp nz, _if_%d\n", target);
                } else {
                    fdprintf(outFd, "\tjp z, _if_%d\n", target);
                }
            } else {
                int target = resolveLabel(s->label);
                if (invertCond) {
                    fdprintf(outFd, "\tjp nz, _if_end_%d\n", target);
                } else {
                    fdprintf(outFd, "\tjp z, _if_end_%d\n", target);
                }
            }
        }

        /* Emit then branch */
        if (s->then_branch) emitStmt(ctx, s->then_branch);

        if (s->label2 > 0) {
            /* Jump over else if we took the then branch (use resolved label) */
            int target = resolveLabel(s->label2);
            fdprintf(outFd, "\tjp _if_end_%d\n", target);

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
        if (s->jump) freeJump(s->jump);
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
                emit(S_ZEXTSL);
                emit(S_EXX);
                emit(S_HLZERO);
                emit(S_EXX);
            }
        }
        /* Jump to function exit label */
        fdprintf(outFd, "\tjp %sX\n", ctx->name);
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
    if (s->jump) freeJump(s->jump);
    free(s);
}

/*
 * Emit assembly for entire function and free tree
 */
void emitAssembly(struct function_ctx *ctx, int fd)
{
    struct local_var *var, *next;
    int has_params;

    if (!ctx || !ctx->body) return;

    /* Initialize label map for jump optimization */
    lblMapCnt = 0;

    /* Scan statement tree to build label map
     * This identifies which labels are pure jumps, allowing us to
     * transitively resolve jump-to-jump chains */
    scanLabJumps(ctx->body);

    /* Check if function has parameters */
    has_params = (ctx->params && ctx->params[0]);

    /* Emit function prologue with frame allocation and lifetime info */
    emitFnProlog(ctx->name, ctx->params, ctx->rettype,
        ctx->frame_size, ctx->locals);

    /* Emit function body */
    emitStmt(ctx, ctx->body);

    /* Emit function exit label (for return statements to jump to) */
    fdprintf(outFd, "%sX:\n", ctx->name);

    /* Emit function epilogue with frame deallocation */
    /* Jump to framefree if we have locals or parameters (tail call optimization) */
    if (ctx->frame_size > 0 || has_params) {
        emit(S_JPFF);
    } else {
        /* No frame to free, just return */
        emit(S_RET);
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
