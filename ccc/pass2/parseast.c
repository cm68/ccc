/*
 * parseast.c - AST parsing and statement handling
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cc2.h"
#include "../cpp/lexeme.h"

/*
 * Static buffers to reduce stack usage (Z80 IY-indexed limit is 127)
 */
static char ps_line[80];       /* parseString line buffer */
static char ps_strbuf[64];     /* parseString string accumulator */
static unsigned char ps_bytes[256]; /* parseString decoded bytes */
static char pga_line[256];     /* parseGlobAsm line buffer */

/*
 * Expression allocation
 */
static struct expr sentinel = { 0 };  /* invalid op=0 marks unused */
#ifdef DEBUG
int exprAlloc = 0;
int exprFree = 0;
#endif

struct expr *
newExpr(unsigned char op, char type)
{
    struct expr *e = malloc(sizeof(struct expr));
    e->op = op;
    e->type = type;
    e->size = TSIZE(type);
    e->left = e->right = &sentinel;
    e->v.l = 0;
    e->sym = 0;
    e->aux = e->aux2 = 0;
    e->demand = 0;
    e->dest = 0;
    e->unused = 0;
    e->cond = 0;
    e->special = 0;
    e->offset = 0;
    e->incr = 0;
#ifdef DEBUG
    exprAlloc++;
#endif
    return e;
}

void
freeExpr(struct expr *e)
{
    if (!e || e == &sentinel) return;
    freeExpr(e->left);
    freeExpr(e->right);
    if (e->sym) free(e->sym);
    free(e);
#ifdef DEBUG
    exprFree++;
#endif
}

/*
 * Propagate cond flag and label through condition tree.
 * aux2 encodes the jump target:
 *   positive: emit FALSE jump to no{aux2}
 *   negative: emit TRUE jump to ht{-aux2}
 * inOr indicates we're inside a || chain.
 */
static void
setCondLbl2(struct expr *e, int lbl, int inOr)
{
    if (!e || e == &sentinel) return;
    e->cond = 1;
    if (e->op == LOR) {
        /* || children emit TRUE jumps to merge label */
        e->aux2 = lbl;
        e->aux = inOr;  /* save context: 1 if nested inside another || */
        setCondLbl2(e->left, lbl, 1);   /* inside || */
        setCondLbl2(e->right, lbl, 1);
    } else if (e->op == LAND) {
        /* && children emit FALSE jumps */
        e->aux2 = lbl;
        e->aux = inOr;  /* save context: 1 if inside ||, 0 if not */
        setCondLbl2(e->left, lbl, 0);
        setCondLbl2(e->right, lbl, 0);
    } else {
        /* Leaf comparison */
        if (inOr)
            e->aux2 = -(lbl + 1);  /* TRUE jump to ht{lbl+1} */
        else
            e->aux2 = lbl;         /* FALSE jump to no{lbl} */
    }
}

static void
setCondLbl(struct expr *e, int lbl)
{
    if (!e || e == &sentinel) return;
    if (e->op == LOR)
        setCondLbl2(e, lbl, 0);  /* || at root: not nested (aux=0) */
    else if (e->op == LAND)
        setCondLbl2(e, lbl, 0);  /* && at root: not inside || */
    else {
        e->cond = 1;
        e->aux2 = lbl;  /* simple comparison: FALSE jump */
    }
}

/*
 * Emit expression: either as pattern comment (-p mode) or as code
 */
static void
emitExprOrPat(struct expr *e)
{
    if (patternOnly) {
        struct pattern p;
        patFromExpr(&p, e);
        patEmitComment(&p);
    } else {
        emitExpr(e);
    }
}

/*
 * Convert SYM node with aux (indexed local) to LOCALVAR node.
 * For compound ops, the left operand is an address but we load/modify/store,
 * so it's effectively a LOCALVAR node. Unifies IX/IY handling in emit code.
 */
static struct expr *
dollarToV(struct expr *e)
{
    if (e->op == SYM && e->aux) {
        e->op = LOCALVAR;
        /* keep aux, offset, sym - they transfer directly */
    }
    return e;
}

/*
 * Complexity score for normalization (lower = simpler, goes on right)
 * 0 = constant, 1 = regvar, 2 = local/global, 3 = simple deref, 4+ = complex
 */
static int
exprCmplx(struct expr *e)
{
    switch (e->op) {
    case AST_CONST: return 0;  /* constant - simplest */
    case REGVAR: return 1;     /* register variable */
    case LOCALVAR: return 2;   /* local variable */
    case SYM: return 2;        /* global variable */
    case DEREF:                /* dereference */
        if (e->left->op == SYM || e->left->op == LOCALVAR || e->left->op == REGVAR)
            return 3;          /* simple deref */
        return 5;              /* complex deref */
    default:  return 5;        /* complex expression */
    }
}

/*
 * Normalize binary op: put simpler operand on right
 * For commutative ops, just swap. For comparisons, swap and flip operator.
 */
static void
normBinop(struct expr *e)
{
    struct expr *tmp;
    int lc = exprCmplx(e->left);
    int rc = exprCmplx(e->right);

    if (lc >= rc)
        return;  /* already normalized or equal */

    /* Swap operands */
    tmp = e->left;
    e->left = e->right;
    e->right = tmp;

    /* Flip operator for non-commutative ops.
     * Note: comparisons are now always (expr) op 0, so no swap needed.
     * Pass1 normalizes >, <=, >= to < with rearranged operands. */
}

/*
 * Parse expression from AST, build tree
 */
struct expr *
parseExpr(void)
{
    unsigned char c;
    char type;
    char name[14];
    struct expr *e, *arg;
    unsigned char nargs, i;

    /* Check for EOF before processing */
    if (curchar == ASTEOF)
        return 0;

    /* binary format - no whitespace */
    c = curchar;
#ifdef DEBUG
    fprintf(stderr, "parseExpr: op='%c' (0x%02x)\n", c >= ' ' && c < 127 ? c : '?', c);
#endif
    advance();

    switch (c) {
    case AST_CONST:  /* constant */
        type = curchar;
        advance();
        e = newExpr(AST_CONST, type);
        e->v.l = read4();
        return e;

    case SYM:  /* global symbol */
        readName(name);
        e = newExpr(SYM, 0);
        e->sym = strdup(name);
        return e;

    case LOCALVAR:  /* local variable (IY-indexed) */
        type = curchar;
        advance();
        e = newExpr(LOCALVAR, type);
        e->aux = R_IY;
        e->offset = read1();
        return e;

    case REGVAR:  /* register variable */
        type = curchar;
        advance();
        e = newExpr(REGVAR, type);
        e->aux = read1();
        return e;

    case DEREF:  /* deref */
        type = curchar;
        advance();
        e = newExpr(DEREF, type);
        e->left = parseExpr();
        /* Collapse DEREF[LOCALVAR] and DEREF[REGVAR] when sizes match */
        if ((e->left->op == LOCALVAR || e->left->op == REGVAR) &&
            e->size == e->left->size) {
            struct expr *child = e->left;
            free(e);
            return child;
        }
        /* Collapse DEREF[PLUS DEREF[REGVAR(IX)] AST_CONST] to LOCALVAR */
        if (e->left->op == PLUS && e->left->size == 2 &&
            e->left->left->op == DEREF && e->left->left->size == 2 &&
            e->left->left->left->op == REGVAR &&
            e->left->left->left->aux == R_IX &&
            e->left->right->op == AST_CONST) {
            struct expr *v = newExpr(LOCALVAR, type);
            v->aux = R_IX;
            v->offset = e->left->right->v.s;
            freeExpr(e);
            return v;
        }
        return e;

    case ASSIGN:  /* assign */
        type = curchar;
        advance();
        e = newExpr(ASSIGN, type);
        e->left = parseExpr();
        e->right = parseExpr();
        return e;

    case CALL:  /* call */
        type = curchar;
        advance();
        nargs = read1();
        e = newExpr(CALL, type);
        e->aux = nargs;
        e->left = parseExpr();  /* func */
        /* chain args in reverse order for C calling convention (right-to-left) */
        for (i = 0; i < nargs; i++) {
            arg = newExpr(ARGNODE, 0);  /* wrapper node */
            arg->left = parseExpr();    /* actual argument */
            arg->right = e->right;      /* link to previous head */
            e->right = arg;             /* new head */
        }
        return e;

    case WIDEN:
    case NARROW:
    case SEXT:
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = parseExpr();
        return e;

    case PREINC:
    case POSTINC:
    case PREDEC:
    case POSTDEC:
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = dollarToV(parseExpr());
        e->aux2 = read2();  /* increment amount */
        return e;

    case BANG:  /* logical not */
    case TWIDDLE:  /* bitwise not */
    case NEG:  /* unary minus/negation */
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = parseExpr();
        return e;

    case PLUS: case MINUS: case STAR: case DIV: case MOD:
    case LT: case LE:  /* comparisons */
    case EQ: case NEQ:  /* equality */
    case OR: case XOR: case AND: case LSHIFT: case RSHIFT:
    case LAND: case LOR:  /* logical and/or */
    case SUBEQ: case ANDEQ: case MODEQ:  /* compound assign -=, &=, %= */
    case PLUSEQ: case OREQ: case XOREQ:  /* +=, |=, ^= */
    case MULTEQ: case DIVEQ: case RSHIFTEQ: case LSHIFTEQ:  /* *=, /=, >>=, <<= */
    case COMMA:  /* comma */
        /* binary operators */
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = parseExpr();
        /* Collapse $local to V for compound ops - unifies IX/IY handling */
        if (c == SUBEQ || c == ANDEQ || c == MODEQ || c == PLUSEQ || c == OREQ ||
            c == XOREQ || c == MULTEQ || c == DIVEQ || c == RSHIFTEQ || c == LSHIFTEQ)
            e->left = dollarToV(e->left);
        e->right = parseExpr();
        /* Normalize: put simpler operand on right for commutative/comparison ops */
        if (c == PLUS || c == STAR || c == AND || c == OR || c == XOR ||
            c == LT || c == LE ||
            c == EQ || c == NEQ || c == LAND || c == LOR)
            normBinop(e);
        /* Transform <= n to < n+1 when safe */
        if (e->op == LE && e->right->op == AST_CONST) {
            long val = e->right->v.l;
            if (e->size == 1 && val < 127) {
                e->op = LT;
                e->right->v.l = val + 1;
            } else if (e->size == 2 && val < 32767) {
                e->op = LT;
                e->right->v.l = val + 1;
            }
        }
        /* Collapse PLUS REGVAR(IX/IY) AST_CONST to LOCALVAR node */
        if (c == PLUS && e->size == 2 &&
            e->left->op == REGVAR && (e->left->aux == R_IX || e->left->aux == R_IY) &&
            e->right->op == AST_CONST) {
            struct expr *v = newExpr(LOCALVAR, type);
            v->aux = e->left->aux;
            v->offset = e->right->v.s;
            freeExpr(e);
            return v;
        }
        return e;

    case QUES:  /* ternary: cond in left, then/else as left/right of right */
        type = curchar;
        advance();
        e = newExpr(QUES, type);
        e->aux = read1();  /* nlabels */
        e->left = parseExpr();  /* cond */
        e->right = newExpr(TERNBRANCH, type);
        e->right->left = parseExpr();   /* then */
        e->right->right = parseExpr();  /* else */
        return e;

    case BFEXTRACT:  /* bitfield extract */
        type = curchar;
        advance();
        e = newExpr(BFEXTRACT, type);
        e->aux = read1();   /* offset */
        e->aux2 = read1();  /* width */
        e->left = parseExpr();
        return e;

    case BFASSIGN:  /* bitfield assign */
        type = curchar;
        advance();
        e = newExpr(BFASSIGN, type);
        e->aux = read1();   /* offset */
        e->aux2 = read1();  /* width */
        e->left = parseExpr();
        e->right = parseExpr();
        return e;

    case 'U':  /* inline string - emit definition, parse actual expr */
        parseString();
        return parseExpr();

    default:
        e = newExpr('?', 0);
        e->v.c = c;  /* save bad char for debugging */
        return e;
    }
}

/*
 * Emit initializer (recursive)
 */
void
emitInit(void)
{
    char ftype;
    long lval;
    unsigned char count, i;
    char symname[14];

    /* binary format - no whitespace */
    if (curchar == LBRACK) {
        advance();
        advance();
        count = read1();
        for (i = 0; i < count; i++)
            emitInit();
    } else if (curchar == BEGIN) {
        advance();
        count = read1();
        for (i = 0; i < count; i++)
            emitInit();
        /* binary format - no whitespace */
        if (curchar == END) advance();
    } else if (curchar == AST_CONST) {
        advance();
        ftype = curchar;
        advance();
        lval = read4();
        if (ftype == 'b' || ftype == 'B')
            emit("\t.db %d", (int)(lval & 0xff));
        else if (ftype == 'l' || ftype == 'f') {
            emit("\t.dw %d", (int)(lval & 0xffff));
            emit("\t.dw %d", (int)((lval >> 16) & 0xffff));
        } else
            emit("\t.dw %d", (int)(lval & 0xffff));
    } else if (curchar == SYM) {
        advance();
        readName(symname);
        emit("\t.dw %s", symname);
    } else if (curchar == WIDEN || curchar == NARROW || curchar == SEXT) {
        advance();
        advance();
        emitInit();
    } else {
        advance();
    }
}

/*
 * Dump statement (recursive)
 */
void
dumpStmt(void)
{
    char c, name[14];
    unsigned char ndecls, nstmts, hasElse, nlabels;
    unsigned char ncases, nstmt, i;

    /* Check for EOF before processing */
    if (curchar == ASTEOF)
        return;

    /* binary format - no whitespace */
    c = curchar;
#ifdef DEBUG
    fprintf(stderr, "dumpStmt: stmt='%c' (0x%02x)\n", c >= ' ' && c < 127 ? c : '?', (unsigned char)c);
#endif
    advance();

    switch (c) {
    case 'B':  /* block */
        ndecls = read1();
        nstmts = read1();
        comment("BLOCK %d decls=%d stmts=%d {", blockCnt++, ndecls, nstmts);
        indent += 2;
        /* parse decls: 'd' type name reg off */
        for (i = 0; i < ndecls; i++) {
            char dtype, dname[14];
            unsigned char dreg, doff;
            /* binary format - no whitespace */
            if (curchar == 'd') {
                advance();
                dtype = curchar;
                advance();
                readName(dname);
                dreg = read1();
                doff = read1();
                comment("decl %c %s reg=%d off=%d", dtype, dname, dreg, (char)(doff));
            }
        }
        for (i = 0; i < nstmts; i++)
            dumpStmt();
        indent -= 2;
        comment("}");
        break;

    case 'I':  /* if - format: I nlabels cond then has_else [else] */
        {
            int lbl, lbl2 = 0;
            struct expr *cond;
            /* New format: nlabels first, hasElse comes after then-body */
            nlabels = read1();
            lbl = labelCnt++;
            labelCnt += nlabels;  /* reserve intermediate labels */
            comment("IF labels=%d lbl=%d [", nlabels, lbl);
            indent += 2;
            cond = parseExpr();
            if (patternOnly) {
                emitExprOrPat(cond);
                freeExpr(cond);
            } else {
                unsigned char special;
                setCondLbl(cond, lbl);  /* propagate cond flag and label */
                annotate(cond);
                emitExpr(cond);
                special = cond->special;
                {
                    char cop = cond->op;
                    char csz = cond->size;
                    char ccond = cond->cond;  /* save cond flag */
                    freeExpr(cond);
                    /* emit conditional jump to skip then block */
                    /* If cond had cond=1, it already emitted its own jump */
                    if (ccond && (special == SP_CMPHL || special == SP_CMPV ||
                                  special == SP_CMPR || cop == LAND || cop == LOR ||
                                  cop == LT || cop == EQ || cop == NEQ)) {
                        /* cond node already emitted jumps, nothing to do */
                    } else if (special == SP_BITTEST) {
                        /* bit n,(ix+ofs): Z=1 if bit is 0; skip then if Z (false) */
                        emit("jp z,no%d_%d", lbl, fnIndex);
                    } else if (special == SP_SIGN || special == SP_SIGNREG) {
                        /* >= 0: Z=true, NZ=false; jump to no on NZ */
                        emit("jp nz,no%d_%d", lbl, fnIndex);
                    } else if (special == SP_CMPEQ) {
                        /* Word equality: HL==0 means equal; test HL for zero */
                        emit("ld a,h");
                        emit("or l");
                        if (cop == EQ)  /* == : skip when NOT equal (NZ) */
                            emit("jp nz,no%d_%d", lbl, fnIndex);
                        else            /* != : skip when equal (Z) */
                            emit("jp z,no%d_%d", lbl, fnIndex);
                    } else if (cop == LT || cop == GT || cop == LE || cop == GE ||
                               cop == EQ || cop == NEQ) {
                        /* General comparison: emit FALSE jump to no{lbl} */
                        emitCondJmp(cop, lbl);
                    } else if (cop == LOR) {
                        /* || chain: emit merge label, then FALSE jump */
                        /* All TRUE paths jumped to ht, FALSE falls through */
                        emit("ht%d_%d:", lbl + 1, fnIndex);
                        /* Last comparison's flags: skip then if FALSE (NZ for ==) */
                        emit("jp nz,no%d_%d", lbl, fnIndex);
                    } else if (cop == BANG) {
                        /* !expr: test child for zero, jump if NON-zero */
                        if (csz == 1) {
                            emit("or a");
                        } else {
                            emit("ld a,h");
                            emit("or l");
                        }
                        emit("jp nz,no%d_%d", lbl, fnIndex);
                    } else {
                        /* general: test HL/A for zero, jump if zero */
                        if (csz == 1) {
                            emit("or a");
                        } else {
                            emit("ld a,h");
                            emit("or l");
                        }
                        emit("jp z,no%d_%d", lbl, fnIndex);
                    }
                }
            }
            dumpStmt();  /* then */
            /* hasElse now comes AFTER the then-body */
            hasElse = read1();
            if (hasElse) {
                lbl2 = labelCnt++;
                if (!patternOnly) {
                    emit("\tjp no%d_%d", lbl2, fnIndex);
                    emit("no%d_%d:", lbl, fnIndex);
                    emit("el%d_%d:", lbl, fnIndex);
                }
                dumpStmt();  /* else */
                if (!patternOnly)
                    emit("no%d_%d:", lbl2, fnIndex);
            } else {
                if (!patternOnly)
                    emit("no%d_%d:", lbl, fnIndex);
            }
            indent -= 2;
            comment("]");
        }
        break;

    case 'E':  /* expression statement */
        {
            struct expr *e = parseExpr();
            e->unused = 1;  /* result not used */
            comment("EXPR [");
            indent += 2;
            if (patternOnly) {
                emitExprOrPat(e);
            } else {
                annotate(e);
                emitExpr(e);
            }
            indent -= 2;
            comment("]");
            freeExpr(e);
        }
        break;

    case 'R':  /* return */
        {
            unsigned char hasVal = read1();
            if (hasVal) {
                struct expr *e = parseExpr();
                comment("RETURN [");
                indent += 2;
                if (patternOnly) {
                    emitExprOrPat(e);
                } else {
                    annotate(e);
                    emitExpr(e);
                }
                indent -= 2;
                comment("]");
                freeExpr(e);
            } else {
                comment("RETURN (void)");
            }
            if (!patternOnly) {
                if (hasFrame)
                    emit("jp framefree");
                else
                    emit("ret");
            }
        }
        break;

    case 'L':  /* label */
        readName(name);
        if (!patternOnly)
            emitLabel(name);
        else
            comment("LABEL %s", name);
        break;

    case 'G':  /* goto */
        readName(name);
        comment("GOTO %s", name);
        if (!patternOnly)
            emit("jp %s", name);
        break;

    case 'S':  /* switch */
        {
            struct expr *e;
            struct swctx *sw;
            unsigned char hasLabel = read1();
            if (hasLabel) readName(name);
            ncases = read1();
            if (swdepth >= MAXSWDEPTH) {
                comment("??? switch nesting too deep");
                break;
            }
            comment("SWITCH label=%s cases=%d [", hasLabel ? name : "(none)", ncases);
            indent += 2;
            /* Push switch context */
            sw = &swstack[swdepth++];
            sw->ncases = 0;
            sw->hasdef = 0;
            sw->tblLabel = labelCnt++;
            sw->endLabel = labelCnt++;
            /* Emit switch expression to A */
            e = parseExpr();
            if (patternOnly) {
                emitExprOrPat(e);
            } else {
                annotate(e);
                emitExpr(e);
                if (e->size != 1) {
                    /* Word expression - get low byte to A */
                    emit("ld a,l");
                }
            }
            freeExpr(e);
            if (!patternOnly) {
                /* Jump to switch table */
                emit("ld hl,sw%d_%d", sw->tblLabel, fnIndex);
                emit("jp switch");
            }
            /* Process all cases - they record themselves in sw */
            for (i = 0; i < ncases; i++)
                dumpStmt();
            /* Emit switch end label */
            if (!patternOnly)
                emit("swe%d_%d:", sw->endLabel, fnIndex);
            if (!patternOnly) {
                if (hasLabel)
                    emit("%s_break:", name);  /* alias for break statements */
                /* Emit jump table inline in text segment */
                emit("sw%d_%d:", sw->tblLabel, fnIndex);
                emit("\t.db %d", sw->ncases);
                for (i = 0; i < sw->ncases; i++) {
                    emit("\t.db %d", sw->vals[i]);
                    emit("\t.dw swc%d_%d", sw->labels[i], fnIndex);
                }
                if (sw->hasdef)
                    emit("\t.dw swd%d_%d", sw->defLabel, fnIndex);
                else
                    emit("\t.dw swe%d_%d", sw->endLabel, fnIndex);
            }
            /* Pop switch context */
            swdepth--;
            indent -= 2;
            comment("]");
        }
        break;

    case 'C':  /* case */
        {
            struct expr *e;
            struct swctx *sw;
            int lbl;
            if (swdepth == 0) {
                comment("??? case outside switch");
                break;
            }
            sw = &swstack[swdepth - 1];
            lbl = labelCnt++;
            nstmt = read1();
            /* Parse case value (must be constant) */
            e = parseExpr();
            /* Record case in switch context */
            if (sw->ncases < MAXCASES) {
                sw->vals[sw->ncases] = e->v.c;
                sw->labels[sw->ncases] = lbl;
                sw->ncases++;
            }
            comment("CASE %d [", e->v.c);
            freeExpr(e);
            indent += 2;
            /* Emit case label */
            if (!patternOnly)
                emit("swc%d_%d:", lbl, fnIndex);
            for (i = 0; i < nstmt; i++)
                dumpStmt();
            indent -= 2;
            comment("]");
        }
        break;

    case 'O':  /* default */
        {
            struct swctx *sw;
            int lbl;
            if (swdepth == 0) {
                comment("??? default outside switch");
                break;
            }
            sw = &swstack[swdepth - 1];
            lbl = labelCnt++;
            nstmt = read1();
            sw->hasdef = 1;
            sw->defLabel = lbl;
            comment("DEFAULT [");
            indent += 2;
            if (!patternOnly)
                emit("swd%d_%d:", lbl, fnIndex);
            for (i = 0; i < nstmt; i++)
                dumpStmt();
            indent -= 2;
            comment("]");
        }
        break;

    case ';':  /* empty */
        comment(";");
        break;

    case 'A':  /* inline asm */
        {
            unsigned len = read2();
            comment("ASM len=%d", len);
            while (len-- > 0) {
                advance();
                advance();
            }
        }
        break;

    case 'U':  /* inline string literal */
        parseString();
        dumpStmt();  /* continue with next stmt */
        break;

    default:
        comment("??? stmt '%c' (0x%02x)", c, c);
        break;
    }
}

/*
 * Parse global variable
 */
void
parseGlobal(void)
{
    char name[14];
    char type;
    unsigned char hasInit;
    unsigned count, elemsize, size;

    /* binary format - no whitespace */
    if (curchar != SYM) {
        fprintf(stderr, "cc2: expected SYM in global\n");
        return;
    }
    advance();

    readName(name);

    /* binary format - no whitespace */
    type = curchar;
    advance();

    if (type == 'a') {
        count = read2();
        elemsize = read2();
        size = count * elemsize;
        hasInit = read1();
        emit("\t.globl %s", name);
        if (hasInit) {
            emitLabel(name);
            emitInit();
        } else {
            emit("\t.bss");
            emitLabel(name);
            emit("\t.ds %d", size);
            emit("\t.text");
        }
        return;
    }

    if (type == 'r') {
        size = read2();
        hasInit = read1();
        emit("\t.globl %s", name);
        if (hasInit) {
            emitLabel(name);
            emitInit();
        } else {
            emit("\t.bss");
            emitLabel(name);
            emit("\t.ds %d", size);
            emit("\t.text");
        }
        return;
    }

    hasInit = read1();
    emit("\t.globl %s", name);
    if (hasInit) {
        emitLabel(name);
        emitInit();
    } else {
        emit("\t.bss");
        emitLabel(name);
        emit("\t.ds %d", TSIZE(type));
        emit("\t.text");
    }
}

/*
 * Parse string literal - emit as .db with quoted strings and hex values
 * Format: .db 0x4, "this is ", 0x8f, "a\ttest\n", 0x0
 */
void
parseString(void)
{
    char name[14];
    char item[70];
    unsigned char len, i, col, strpos;
    unsigned char val, isprintable, itemlen;

    readName(name);
    len = read1();

    /* First decode all bytes into static buffer */
    for (i = 0; i < len; i++) {
        ps_bytes[i] = read1();
    }

    emit("\t.data");
    emitLabel(name);
    col = sprintf(ps_line, "\t.db ");
    strpos = 0;
    ps_strbuf[0] = 0;

    for (i = 0; i <= len; i++) {  /* <= to include null terminator */
        val = (i < len) ? ps_bytes[i] : 0;
        isprintable = (val >= 0x20 && val <= 0x7e && val != '"' && val != '\\') ||
                          val == '\t' || val == '\n' || val == '\r';

        if (isprintable && i < len) {
            /* Accumulate into string buffer */
            if (val == '\t') {
                ps_strbuf[strpos++] = '\\';
                ps_strbuf[strpos++] = 't';
            } else if (val == '\n') {
                ps_strbuf[strpos++] = '\\';
                ps_strbuf[strpos++] = 'n';
            } else if (val == '\r') {
                ps_strbuf[strpos++] = '\\';
                ps_strbuf[strpos++] = 'r';
            } else {
                ps_strbuf[strpos++] = val;
            }
            ps_strbuf[strpos] = 0;
        } else {
            /* Flush string buffer if any */
            if (strpos > 0) {
                itemlen = sprintf(item, "\"%s\"", ps_strbuf);
                if (col + itemlen + 1 > 70 && col > 6) {
                    ps_line[col] = 0;
                    emit("%s", ps_line);
                    col = sprintf(ps_line, "\t.db ");
                }
                if (col > 6)
                    ps_line[col++] = ',';
                strcpy(ps_line + col, item);
                col += itemlen;
                strpos = 0;
                ps_strbuf[0] = 0;
            }
            /* Emit hex value */
            itemlen = sprintf(item, "0x%x", val);
            if (col + itemlen + 1 > 70 && col > 6) {
                ps_line[col] = 0;
                emit("%s", ps_line);
                col = sprintf(ps_line, "\t.db ");
            }
            if (col > 6)
                ps_line[col++] = ',';
            strcpy(ps_line + col, item);
            col += itemlen;
        }
    }

    /* Emit final line */
    if (col > 6) {
        ps_line[col] = 0;
        emit("%s", ps_line);
    }
    emit("\t.text");
}

/*
 * Parse global asm
 */
void
parseGlobAsm(void)
{
    unsigned len = read2();
    unsigned i;
    unsigned char asmpos = 0;
    unsigned char ch;

    for (i = 0; i < len; i++) {
        ch = read1();
        if (ch == '\n' || asmpos >= 250) {
            pga_line[asmpos] = 0;
            if (asmpos > 0) emit("%s", pga_line);
            asmpos = 0;
        } else {
            pga_line[asmpos++] = ch;
        }
    }
    if (asmpos > 0) {
        pga_line[asmpos] = 0;
        emit("%s", pga_line);
    }
}

/*
 * Parse function - emit label, then dump body as comments
 */
void
parseFunc(void)
{
    char name[14];
    unsigned char nparams, nlocals, fsize;
    unsigned char i;
    char type;

    labelCnt = 0;
    fnIndex++;

    type = curchar;
    fnRetType = type;
    advance();
    readName(name);

    nparams = read1();
    nlocals = read1();
    fsize = read1();

    emit("\t.globl %s", name);
    emitLabel(name);
    comment("FUNC %s ret=%c params=%d locals=%d frame=%d",
            name, type, nparams, nlocals, fsize);

    indent = 2;

    /* Dump parameters */
    for (i = 0; i < nparams; i++) {
        char ptype, pname[14];
        unsigned char preg, poff;
        /* binary format - no whitespace */
        if (curchar == 'd') {
            advance();
            ptype = curchar;
            advance();
            readName(pname);
            preg = read1();
            poff = read1();
            comment("param %c %s %s off=%d", ptype, pname, regnames[preg] ? regnames[preg] : "-", (char)(poff));
        }
    }

    /* Dump locals */
    for (i = 0; i < nlocals; i++) {
        char ltype, lname[14];
        unsigned char lreg, loff;
        /* binary format - no whitespace */
        if (curchar == 'd') {
            advance();
            ltype = curchar;
            advance();
            readName(lname);
            lreg = read1();
            loff = read1();
            comment("local %c %s %s off=%d", ltype, lname, regnames[lreg] ? regnames[lreg] : "-", (char)(loff));
        }
    }

    /* Emit frame allocation if needed */
    hasFrame = fsize > 0;
    if (hasFrame) {
        emit("ld a,%d", fsize);
        emit("call framealloc");
    }

    /* Dump body */
    dumpStmt();

    indent = 0;
    emit("");
}

/*
 * Top-level AST loop
 */
void
parseAst(void)
{
    while (curchar != ASTEOF) {
        /* binary format - no whitespace */
        if (curchar == ASTEOF) break;

        switch (curchar) {
        case 'F':
            advance();
            parseFunc();
            break;
        case 'Z':
            advance();
            parseGlobal();
            break;
        case 'U':
            advance();
            parseString();
            break;
        case 'A':
            advance();
            parseGlobAsm();
            break;
        default:
            fprintf(stderr, "cc2: unknown top-level '%c'\n", curchar);
            advance();
            break;
        }
    }
}
/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
