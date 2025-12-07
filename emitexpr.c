/*
 * emitexpr.c - Expression emission for cc2
 *
 * Walks expression trees, emitting assembly code and freeing nodes.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"
#include "regcache.h"

/*
 * Emit expression tree, emit assembly, and free nodes
 *
 * Binary operators need special handling to emit accumulator move between
 * children:
 *   1. Emit left child (result in PRIMARY)
 *   2. Emit move instruction (PRIMARY to SECONDARY)
 *   3. Emit right child (result in PRIMARY)
 *   4. Emit call instruction (operates on SECONDARY and PRIMARY)
 */
static int exprCount = 0;
void emitExpr(struct expr *e)
{
    if (!e) return;
    exprCount++;
    if (TRACE(T_EXPR)) {
        fdprintf(2, "emitExpr: %d calls, op=%c (0x%x)\n", exprCount, e->op, e->op);
    }
    if (exprCount > 100000) {
        fdprintf(2, "emitExpr: exceeded 100000 calls, op=%c\n", e->op);
        exit(1);
    }

    /* Handle BC indirect load with caching - use opflags */
    if (e->op == 'M' && (e->opflags & OP_BCINDIR)) {
        emitBCIndir();
        freeNode(e);
        return;
    }
    /* Handle increment/decrement - Pattern 1 (e->symbol) or Pattern 2/3 (e->left) */
    else if ((e->op == AST_PREINC || e->op == AST_POSTINC || e->op == AST_PREDEC || e->op == AST_POSTDEC) &&
             (e->symbol || e->left)) {
        emitIncDec(e);
        return;
    }
    /* Handle ASSIGN - use op check */
    else if (e->op == '=') {
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  calling emitAssign\n");
        }
        emitAssign(e);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  emitAssign returned, about to return from emitExpr\n");
        }
        return;
    }
    /* Binary operators with accumulator management need special handling */
    else if (isBinopWAccum(e->op) && e->left && e->right) {
        emitBinop(e);
        return;
    }
    /* CALL operator - children handled by emitCall */
    else if (e->op == '@') {
        emitCall(e);
        return;
    }
    /* Handle ternary operator (? :) */
    else if (e->op == '?') {
        emitTernary(e);
        return;
    }
    /* Handle DEREF of register variable with caching - use opflags */
    else if (e->op == 'M' && (e->opflags & OP_REGVAR) &&
             e->left && e->left->op == '$') {
        emitRegVarDrf(e);
        return;
    }
    /* Handle DEREF of global with caching */
    else if (e->op == 'M' && (e->opflags & OP_GLOBAL) &&
             e->left && e->left->op == '$') {
        emitGlobDrf(e);
        return;
    }
    /* Handle DEREF of stack variable (IY-indexed) */
    else if (e->op == 'M' && (e->opflags & OP_IYMEM) &&
             e->left && e->left->op == '$') {
        emitStackDrf(e);
        return;
    }
    /* Handle DEREF with indirect addressing (loc=LOC_INDIR) */
    else if (e->op == 'M' && e->loc == LOC_INDIR) {
        /* Emit address calculation first */
        emitExpr(e->left);
        /* Then load through HL */
        if (e->size == 1) {
            fdprintf(outFd, "\tld a, (hl)\n");
            clearA();
        } else if (e->size == 2) {
            fdprintf(outFd, "\tld a, (hl)\n\tinc hl\n\tld h, (hl)\n\tld l, a\n");
            clearHL();
        }
        freeNode(e);
        return;
    }
    /* Handle symbol address - check if global or local */
    else if (e->op == '$' && e->symbol) {
        const char *sym_name = stripVarPfx(e->symbol);
        struct local_var *var = findVar(sym_name);

        if (!var) {
            /* Global symbol - load address */
            addRefSym(sym_name);  /* Track for extern declaration */
            if (e->dest == R_DE) {
                fdprintf(outFd, "\tld de, %s\n", sym_name);
                fnDEValid = 1;
            } else {
                int cached = cacheFindWord(e);
                if (cached == 'H') {
                    /* HL already has this address - skip load */
                } else {
                    fdprintf(outFd, "\tld hl, %s\n", sym_name);
                    cacheSetHL(e);
                }
            }
        } else {
            /* Local variable - compute address (IY + offset) */
            int ofs = var->offset;
            fdprintf(outFd, "\tpush iy\n\tpop hl\n");
            if (ofs != 0) {
                fdprintf(outFd, "\tld de, %d\n\tadd hl, de\n", ofs);
            }
            clearHL();
        }
        free(e);
        return;
    }
    /* Handle constants with scheduler */
    else if (e->op == 'C' && e->loc == LOC_CONST) {
        if (e->size == 1) {
            if ((e->value & 0xff) == 0) {
                if (!fnAZero) {
                    fdprintf(outFd, "\txor a\n");
                    fnAZero = 1;
                }
            } else if (cacheFindByte(e) == 'A') {
                /* A already has this value */
            } else {
                fdprintf(outFd, "\tld a, %ld\n", e->value & 0xff);
                fnAZero = 0;
                cacheSetA(e);
            }
        } else {
            fdprintf(outFd, "\tld hl, %ld\n", e->value);
            clearHL();
        }
        freeNode(e);
        return;
    }
    /* Handle specialized BYTE shift ops (LSHIFTEQ/RSHIFTEQ with register var) */
    else if ((e->op == '0' || e->op == '6') && e->size == 1 &&
             (e->opflags & OP_REGVAR) && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        const char *inst = (e->op == '0') ? "sla" : "srl";
        int count = e->value, i;
        if (var->reg == REG_BC) rn = "c";
        for (i = 0; i < count; i++)
            fdprintf(outFd, "\t%s %s\n", inst, rn);
        freeNode(e);
        return;
    }
    /* Handle specialized bit ops (OREQ=set, ANDEQ=res) */
    else if ((e->op == '1' || e->op == AST_ANDEQ) && !e->left && !e->right && e->cached_var &&
             e->value >= 0 && e->value <= 7) {
        /* Simple variable patterns - kids were freed, bitnum stored in e->value */
        struct local_var *var = e->cached_var;
        const char *inst = (e->op == '1') ? "set" : "res";
        int bitnum = e->value;
        if (e->opflags & OP_REGVAR) {
            const char *rn = byteRegName(var->reg);
            if (var->reg == REG_BC) rn = "c";
            if (rn)
                fdprintf(outFd, "\t%s %d, %s\n", inst, bitnum, rn);
        } else if (e->opflags & OP_IYMEM) {
            int ofs = varIYOfs(var);
            fdprintf(outFd, "\t%s %d, (iy %c %d)\n", inst, bitnum,
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
        }
        freeNode(e);
        return;
    }
    /* Handle specialized bit ops with IX-indexed or (hl) addressing */
    else if ((e->op == '1' || e->op == AST_ANDEQ) && (e->opflags & OP_IXMEM) && !e->left) {
        const char *inst = (e->op == '1') ? "set" : "res";
        int bitnum = (e->value >> 8) & 0xff;
        int ofs = (char)(e->value & 0xff);
        fdprintf(outFd, "\t%s %d, (ix %c %d)\n", inst, bitnum,
                 ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
        freeNode(e);
        return;
    }
    else if ((e->op == '1' || e->op == AST_ANDEQ) && e->left && !e->right) {
        /* (hl) addressing - emit left for address, then bit op */
        const char *inst = (e->op == '1') ? "set" : "res";
        int bitnum = e->value;
        emitExpr(e->left);
        fdprintf(outFd, "\t%s %d, (hl)\n", inst, bitnum);
        freeNode(e);
        return;
    }
    /* Handle specialized OREQ/ANDEQ/XOREQ for byte register variables */
    else if ((e->op == '1' || e->op == AST_ANDEQ || e->op == 'X') && (e->opflags & OP_REGVAR) &&
             !e->left && e->right && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        const char *inst = (e->op == '1') ? "or" : (e->op == AST_ANDEQ) ? "and" : "xor";
        if (var->reg == REG_BC) rn = "c";
        emitExpr(e->right);
        fdprintf(outFd, "\t%s %s\n\tld %s, a\n", inst, rn, rn);
        freeNode(e);
        return;
    }
    /* Handle specialized PLUSEQ/SUBEQ for byte register variables */
    else if ((e->op == 'P' || e->op == AST_SUBEQ) && (e->opflags & OP_REGVAR) &&
             !e->left && e->right && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        if (var->reg == REG_BC) rn = "c";
        emitExpr(e->right);
        if (e->op == 'P')
            fdprintf(outFd, "\tadd a, %s\n\tld %s, a\n", rn, rn);
        else
            fdprintf(outFd, "\tld e, a\n\tld a, %s\n\tsub e\n\tld %s, a\n", rn, rn);
        freeNode(e);
        return;
    }
    /* Handle word LSHIFTEQ for BC register variable with constant shift */
    else if (e->op == '0' && e->size == 2 && (e->opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC &&
             e->right && e->right->op == 'C' &&
             e->right->value >= 1 && e->right->value <= 8) {
        int i, cnt = e->right->value;
        fdprintf(outFd, "\tld h, b\n\tld l, c\n");
        for (i = 0; i < cnt; i++)
            fdprintf(outFd, "\tadd hl, hl\n");
        fdprintf(outFd, "\tld b, h\n\tld c, l\n");
        freeExpr(e->left);
        freeExpr(e->right);
        freeNode(e);
        return;
    }
    /* Handle word OREQ for BC register variable */
    else if (e->op == '1' && e->size == 2 && (e->opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC && e->right) {
        int rhs_byte = (e->right->op == 'W');  /* WIDEN means high byte is 0 */
        freeExpr(e->left);
        emitExpr(e->right);  /* Result in HL */
        fdprintf(outFd, "\tld a, l\n\tor c\n\tld c, a\n");
        if (!rhs_byte)
            fdprintf(outFd, "\tld a, h\n\tor b\n\tld b, a\n");
        freeNode(e);
        return;
    }
    /* Handle word PLUSEQ for BC register variable */
    else if (e->op == 'P' && e->size == 2 && (e->opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC && e->right) {
        freeExpr(e->left);
        emitExpr(e->right);  /* Result in HL */
        fdprintf(outFd, "\tadd hl, bc\n\tld b, h\n\tld c, l\n");
        freeNode(e);
        return;
    }
    else {
        /* Normal postorder traversal for other operators */
        if (e->left) emitExpr(e->left);
        if (e->right) emitExpr(e->right);

        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    xfree(e->cleanup_block);
    if (e->jump) freeJump(e->jump);
    free(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
