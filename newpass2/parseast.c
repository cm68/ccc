/*
 * parseast.c - AST parsing and statement handling
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cc2.h"

/*
 * Expression allocation
 */
static struct expr sentinel = { 0 };  /* invalid op=0 marks unused */

struct expr *
newExpr(char op, char type)
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
}

/*
 * Convert $ node with aux (indexed local) to V node.
 * For compound ops, the left operand is an address but we load/modify/store,
 * so it's effectively a V node. Unifies IX/IY handling in emit code.
 */
static struct expr *
dollarToV(struct expr *e)
{
    if (e->op == '$' && e->aux) {
        e->op = 'V';
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
    case '#': return 0;  /* constant - simplest */
    case 'R': return 1;  /* register variable */
    case 'V': return 2;  /* local variable */
    case '$': return 2;  /* global variable */
    case 'M':            /* dereference */
        if (e->left->op == '$' || e->left->op == 'V' || e->left->op == 'R')
            return 3;    /* simple deref */
        return 5;        /* complex deref */
    default:  return 5;  /* complex expression */
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

    /* Flip operator for non-commutative comparisons */
    switch (e->op) {
    case '<': e->op = '>'; break;
    case '>': e->op = '<'; break;
    case 'L': e->op = 'g'; break;  /* <= becomes >= */
    case 'g': e->op = 'L'; break;  /* >= becomes <= */
    /* These are commutative, no op change needed:
     * + * & | ^ == != && || */
    }
}

/*
 * Parse expression from AST, build tree
 */
struct expr *
parseExpr(void)
{
    char c, type;
    char name[14];
    struct expr *e, *arg;
    unsigned char nargs, i;

    skipWs();
    c = curchar;
    advance();

    switch (c) {
    case '#':  /* constant */
        type = curchar;
        advance();
        e = newExpr('#', type);
        e->v.l = hex8();
        return e;

    case '$':  /* symbol ref - mark locals with aux for later M[] collapse */
        readName(name);
        {
            struct sym *s = findLocal(name);
            if (s && s->reg) {
                e = newExpr('R', s->type);  /* register var */
                e->aux = s->reg;
            } else if (s) {
                /* Local address - mark with aux/offset for M[] to collapse */
                e = newExpr('$', T_USHORT);  /* address type */
                e->aux = R_IY;               /* IY-relative marker */
                e->offset = s->off;
            } else {
                e = newExpr('$', 0);        /* global */
            }
            e->sym = strdup(name);
        }
        return e;

    case 'M':  /* deref */
        type = curchar;
        advance();
        e = newExpr('M', type);
        e->left = parseExpr();
        /* Collapse M[$local] to V node - indexed load from IY */
        if (e->left->op == '$' && e->left->aux == R_IY) {
            struct expr *v = newExpr('V', type);
            v->aux = R_IY;
            v->offset = e->left->offset;
            v->sym = e->left->sym;
            e->left->sym = 0;  /* prevent double-free */
            freeExpr(e);
            return v;
        }
        /* Collapse M[V] and M[R] when sizes match - V/R already load value */
        if ((e->left->op == 'V' || e->left->op == 'R') &&
            e->size == e->left->size) {
            struct expr *child = e->left;
            free(e);
            return child;
        }
        /* Collapse M[+p Mp[Rp(ix)] #ofs] to V with reg=IX */
        if (e->left->op == '+' && e->left->size == 2 &&
            e->left->left->op == 'M' && e->left->left->size == 2 &&
            e->left->left->left->op == 'R' &&
            e->left->left->left->aux == R_IX &&
            e->left->right->op == '#') {
            struct expr *v = newExpr('V', type);
            v->aux = R_IX;
            v->offset = e->left->right->v.s;
            freeExpr(e);
            return v;
        }
        return e;

    case '=':  /* assign */
        type = curchar;
        advance();
        e = newExpr('=', type);
        e->left = parseExpr();
        e->right = parseExpr();
        return e;

    case '@':  /* call */
        type = curchar;
        advance();
        nargs = hex2();
        e = newExpr('@', type);
        e->aux = nargs;
        e->left = parseExpr();  /* func */
        /* chain args in reverse order for C calling convention (right-to-left) */
        for (i = 0; i < nargs; i++) {
            arg = newExpr('A', 0);  /* wrapper node */
            arg->left = parseExpr();  /* actual argument */
            arg->right = e->right;    /* link to previous head */
            e->right = arg;           /* new head */
        }
        return e;

    case 'W':  /* widen */
    case 'N':  /* narrow */
    case 'x':  /* sign extend */
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = parseExpr();
        return e;

    case '(':  /* pre-inc */
    case ')':  /* post-inc */
    case '{':  /* pre-dec */
    case '}':  /* post-dec */
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = dollarToV(parseExpr());
        e->aux2 = hex4();  /* increment amount */
        return e;

    case '!':  /* logical not */
    case '~':  /* bitwise not */
    case '_':  /* unary minus */
    case '\\': /* negation (NEG) */
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = parseExpr();
        return e;

    case '+': case '-': case '*': case '/': case '%':
    case '<': case '>': case 'Q': case 'n': case 'L': case 'g':
    case '|': case '^': case '&': case 'y': case 'w':
    case 'j': case 'h':  /* logical and/or */
    case 'D': case 'O': case 'z':  /* unsigned ops */
    case 'o': case 'a': case 'm':  /* compound assign -=, &=, %= */
    case 'P': case '1': case 'X':  /* +=, |=, ^= */
    case 'T': case '2': case '6': case '0':  /* *=, /=, >>=, <<= */
    case ',':  /* comma */
        /* binary operators */
        type = curchar;
        advance();
        e = newExpr(c, type);
        e->left = parseExpr();
        /* Collapse $local to V for compound ops - unifies IX/IY handling */
        if (c == 'o' || c == 'a' || c == 'm' || c == 'P' || c == '1' ||
            c == 'X' || c == 'T' || c == '2' || c == '6' || c == '0')
            e->left = dollarToV(e->left);
        e->right = parseExpr();
        /* Normalize: put simpler operand on right for commutative/comparison ops */
        if (c == '+' || c == '*' || c == '&' || c == '|' || c == '^' ||
            c == '<' || c == '>' || c == 'L' || c == 'g' ||
            c == 'Q' || c == 'n' || c == 'j' || c == 'h')
            normBinop(e);
        /* Transform <= n to < n+1, >= n to > n-1 when safe */
        if ((e->op == 'L' || e->op == 'g') && e->right->op == '#') {
            long val = e->right->v.l;
            if (e->op == 'L' && e->size == 1 && val < 127) {
                e->op = '<';
                e->right->v.l = val + 1;
            } else if (e->op == 'L' && e->size == 2 && val < 32767) {
                e->op = '<';
                e->right->v.l = val + 1;
            } else if (e->op == 'g' && e->size == 1 && val > -128) {
                e->op = '>';
                e->right->v.l = val - 1;
            } else if (e->op == 'g' && e->size == 2 && val > -32768) {
                e->op = '>';
                e->right->v.l = val - 1;
            }
        }
        /* Collapse +p [R(ix/iy) #ofs] to V node (constant always on right after normalize) */
        if (c == '+' && e->size == 2 &&
            e->left->op == 'R' && (e->left->aux == R_IX || e->left->aux == R_IY) &&
            e->right->op == '#') {
            struct expr *v = newExpr('V', type);
            v->aux = e->left->aux;
            v->offset = e->right->v.s;
            freeExpr(e);
            return v;
        }
        return e;

    case '?':  /* ternary: cond in left, then/else as left/right of right */
        type = curchar;
        advance();
        e = newExpr('?', type);
        e->aux = hex2();  /* nlabels */
        e->left = parseExpr();  /* cond */
        e->right = newExpr(':', type);
        e->right->left = parseExpr();   /* then */
        e->right->right = parseExpr();  /* else */
        return e;

    case 'F':  /* bitfield */
        type = curchar;
        advance();
        e = newExpr('F', type);
        e->aux = hex2();   /* offset */
        e->aux2 = hex2();  /* width */
        e->left = parseExpr();
        return e;

    case 'Y':  /* memory copy */
        e = newExpr('Y', 0);
        e->aux = hex4();  /* length */
        e->left = parseExpr();   /* dest */
        e->right = parseExpr();  /* src */
        return e;

    case 'U':  /* inline string - skip definition, parse actual expr */
        readName(name);
        {
            unsigned char len = hex2();
            while (len-- > 0) {
                advance();
                advance();
            }
        }
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

    skipWs();
    if (curchar == '[') {
        advance();
        advance();
        count = hex2();
        for (i = 0; i < count; i++)
            emitInit();
    } else if (curchar == '{') {
        advance();
        count = hex2();
        for (i = 0; i < count; i++)
            emitInit();
        skipWs();
        if (curchar == '}') advance();
    } else if (curchar == '#') {
        advance();
        ftype = curchar;
        advance();
        lval = hex8();
        if (ftype == 'b' || ftype == 'B')
            emit("\t.db %d", (int)(lval & 0xff));
        else if (ftype == 'l' || ftype == 'f') {
            emit("\t.dw %d", (int)(lval & 0xffff));
            emit("\t.dw %d", (int)((lval >> 16) & 0xffff));
        } else
            emit("\t.dw %d", (int)(lval & 0xffff));
    } else if (curchar == '$') {
        advance();
        readName(symname);
        emit("\t.dw %s", symname);
    } else if (curchar == 'W' || curchar == 'N' || curchar == 'x') {
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

    skipWs();
    c = curchar;
    advance();

    switch (c) {
    case 'B':  /* block */
        ndecls = hex2();
        nstmts = hex2();
        comment("BLOCK %d decls=%d stmts=%d {", blockCnt++, ndecls, nstmts);
        indent += 2;
        /* parse decls: 'd' type name reg off */
        for (i = 0; i < ndecls; i++) {
            char dtype, dname[14];
            unsigned char dreg, doff;
            skipWs();
            if (curchar == 'd') {
                advance();
                dtype = curchar;
                advance();
                readName(dname);
                dreg = hex2();
                doff = hex2();
                addLocal(dname, dtype, dreg, doff);
                comment("decl %c %s reg=%d off=%d", dtype, dname, dreg, (char)(doff));
            }
        }
        for (i = 0; i < nstmts; i++)
            dumpStmt();
        indent -= 2;
        comment("}");
        break;

    case 'I':  /* if */
        {
            int lbl, lbl2;
            struct expr *cond;
            unsigned char special;
            hasElse = hex2();
            nlabels = hex2();
            lbl = labelCnt++;
            labelCnt += nlabels;  /* reserve intermediate labels */
            if (hasElse)
                lbl2 = labelCnt++;
            comment("IF else=%d labels=%d lbl=%d [", hasElse, nlabels, lbl);
            indent += 2;
            cond = parseExpr();
            cond->cond = 1;
            calcDemand(cond);
            assignDest(cond, cond->size == 1 ? R_A : R_HL);
            emitExpr(cond);
            special = cond->special;
            {
                char cop = cond->op;
                freeExpr(cond);
                /* emit conditional jump to skip then block */
                if (special == SP_BITTEST) {
                    /* bit n,(ix+ofs): Z=1 if bit is 0; skip then if Z (false) */
                    emit("jp z,no%d_%d", lbl, fnIndex);
                } else if (special == SP_SIGN || special == SP_SIGNREG) {
                    /* >= 0: Z=true, NZ=false; jump to no on NZ */
                    emit("jp nz,no%d_%d", lbl, fnIndex);
                } else if (special == SP_CMPEQ) {
                    /* Word equality: HL==0 means equal; test HL for zero */
                    emit("ld a,h");
                    emit("or l");
                    if (cop == 'Q')  /* == : skip when NOT equal (NZ) */
                        emit("jp nz,no%d_%d", lbl, fnIndex);
                    else             /* != : skip when equal (Z) */
                        emit("jp z,no%d_%d", lbl, fnIndex);
                } else if (special == SP_CMPHL ||
                           cop == '<' || cop == '>' || cop == 'L' || cop == 'g') {
                    /* cp/sbc sets flags: C if A < operand, Z if A == operand
                     * Skip then block when condition is FALSE */
                    switch (cop) {
                    case 'Q':  /* == : skip on NZ */
                        emit("jp nz,no%d_%d", lbl, fnIndex);
                        break;
                    case 'n':  /* != : skip on Z */
                        emit("jp z,no%d_%d", lbl, fnIndex);
                        break;
                    case '<':  /* skip when left >= right: NC */
                        emit("jp nc,no%d_%d", lbl, fnIndex);
                        break;
                    case 'L':  /* <= : skip when left > right: NC and NZ */
                        emit("jp z,$+5");
                        emit("jp nc,no%d_%d", lbl, fnIndex);
                        break;
                    case '>':  /* skip when left <= right: C or Z */
                        emit("jp c,no%d_%d", lbl, fnIndex);
                        emit("jp z,no%d_%d", lbl, fnIndex);
                        break;
                    case 'g':  /* >= : skip when left < right: C */
                        emit("jp c,no%d_%d", lbl, fnIndex);
                        break;
                    }
                } else if (cop == '!') {
                    /* !expr: test child for zero, jump if NON-zero */
                    emit("ld a,h");
                    emit("or l");
                    emit("jp nz,no%d_%d", lbl, fnIndex);
                } else {
                    /* general: test HL/A for zero, jump if zero */
                    emit("ld a,h");
                    emit("or l");
                    emit("jp z,no%d_%d", lbl, fnIndex);
                }
            }
            dumpStmt();  /* then */
            if (hasElse) {
                emit("\tjp no%d_%d", lbl2, fnIndex);
                emit("el%d_%d:", lbl, fnIndex);
                dumpStmt();  /* else */
                emit("no%d_%d:", lbl2, fnIndex);
            } else {
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
            calcDemand(e);
            assignDest(e, e->size == 1 ? R_A : R_HL);
            comment("EXPR [");
            indent += 2;
            emitExpr(e);
            indent -= 2;
            comment("]");
            freeExpr(e);
        }
        break;

    case 'R':  /* return */
        {
            unsigned char hasVal = hex2();
            if (hasVal) {
                struct expr *e = parseExpr();
                calcDemand(e);
                assignDest(e, e->size == 1 ? R_A : R_HL);
                comment("RETURN [");
                indent += 2;
                emitExpr(e);
                indent -= 2;
                comment("]");
                freeExpr(e);
            } else {
                comment("RETURN (void)");
            }
            /* TODO: epilogue for locals/frame */
            emit("ret");
        }
        break;

    case 'L':  /* label */
        readName(name);
        emitLabel(name);
        break;

    case 'G':  /* goto */
        readName(name);
        comment("GOTO %s", name);
        break;

    case 'S':  /* switch */
        {
            struct expr *e;
            struct swctx *sw;
            unsigned char hasLabel = hex2();
            if (hasLabel) readName(name);
            ncases = hex2();
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
            calcDemand(e);
            if (e->size == 1) {
                assignDest(e, R_A);
                emitExpr(e);
            } else {
                /* Word expression - load to HL, then get low byte to A */
                assignDest(e, R_HL);
                emitExpr(e);
                emit("ld a,l");
            }
            freeExpr(e);
            /* Jump to switch table */
            emit("ld hl,sw%d_%d", sw->tblLabel, fnIndex);
            emit("jp switch");
            /* Process all cases - they record themselves in sw */
            for (i = 0; i < ncases; i++)
                dumpStmt();
            /* Emit switch end label */
            emit("swe%d_%d:", sw->endLabel, fnIndex);
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
            /* Pop switch context */
            swdepth--;
            indent -= 2;
            comment("]");
        }
        break;

    case 'C':  /* case */
        {
            struct expr *e;
            struct swctx *sw = &swstack[swdepth - 1];
            int lbl = labelCnt++;
            nstmt = hex2();
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
            emit("swc%d_%d:", lbl, fnIndex);
            for (i = 0; i < nstmt; i++)
                dumpStmt();
            indent -= 2;
            comment("]");
        }
        break;

    case 'O':  /* default */
        {
            struct swctx *sw = &swstack[swdepth - 1];
            int lbl = labelCnt++;
            nstmt = hex2();
            sw->hasdef = 1;
            sw->defLabel = lbl;
            comment("DEFAULT [");
            indent += 2;
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
            unsigned len = hex4();
            comment("ASM len=%d", len);
            while (len-- > 0) {
                advance();
                advance();
            }
        }
        break;

    case 'U':  /* inline string literal */
        readName(name);
        comment("STRING %s", name);
        {
            unsigned char len = hex2();
            while (len-- > 0) {
                advance();
                advance();
            }
        }
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

    skipWs();
    if (curchar != '$') {
        fprintf(stderr, "cc2: expected $ in global\n");
        return;
    }
    advance();

    readName(name);

    skipWs();
    type = curchar;
    advance();

    if (type == 'a') {
        count = hex4();
        elemsize = hex4();
        size = count * elemsize;
        hasInit = hex2();
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
        size = hex4();
        hasInit = hex2();
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

    hasInit = hex2();
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
    char line[80];
    char strbuf[64];
    unsigned char len, i, col, strpos;
    unsigned char bytes[256];  /* store decoded bytes */

    readName(name);
    len = hex2();

    /* First decode all bytes */
    for (i = 0; i < len; i++) {
        bytes[i] = hex2();
    }

    emitLabel(name);
    col = sprintf(line, "\t.db ");
    strpos = 0;
    strbuf[0] = 0;

    for (i = 0; i <= len; i++) {  /* <= to include null terminator */
        unsigned char val = (i < len) ? bytes[i] : 0;
        unsigned char isprintable = (val >= 0x20 && val <= 0x7e && val != '"' && val != '\\') ||
                          val == '\t' || val == '\n' || val == '\r';

        if (isprintable && i < len) {
            /* Accumulate into string buffer */
            if (val == '\t') {
                strbuf[strpos++] = '\\';
                strbuf[strpos++] = 't';
            } else if (val == '\n') {
                strbuf[strpos++] = '\\';
                strbuf[strpos++] = 'n';
            } else if (val == '\r') {
                strbuf[strpos++] = '\\';
                strbuf[strpos++] = 'r';
            } else {
                strbuf[strpos++] = val;
            }
            strbuf[strpos] = 0;
        } else {
            /* Flush string buffer if any */
            if (strpos > 0) {
                char item[70];
                unsigned char itemlen = sprintf(item, "\"%s\"", strbuf);
                if (col + itemlen + 1 > 70 && col > 6) {
                    line[col] = 0;
                    emit("%s", line);
                    col = sprintf(line, "\t.db ");
                }
                if (col > 6)
                    line[col++] = ',';
                strcpy(line + col, item);
                col += itemlen;
                strpos = 0;
                strbuf[0] = 0;
            }
            /* Emit hex value */
            {
                char item[8];
                unsigned char itemlen = sprintf(item, "0x%x", val);
                if (col + itemlen + 1 > 70 && col > 6) {
                    line[col] = 0;
                    emit("%s", line);
                    col = sprintf(line, "\t.db ");
                }
                if (col > 6)
                    line[col++] = ',';
                strcpy(line + col, item);
                col += itemlen;
            }
        }
    }

    /* Emit final line */
    if (col > 6) {
        line[col] = 0;
        emit("%s", line);
    }
}

/*
 * Parse global asm
 */
void
parseGlobAsm(void)
{
    unsigned len = hex4();
    unsigned i;
    char asmline[256];
    unsigned char asmpos = 0;

    for (i = 0; i < len; i++) {
	unsigned char ch;
	ch = hex2();
        if (ch == '\n' || asmpos >= 250) {
            asmline[asmpos] = 0;
            if (asmpos > 0) emit("%s", asmline);
            asmpos = 0;
        } else {
            asmline[asmpos++] = ch;
        }
    }
    if (asmpos > 0) {
        asmline[asmpos] = 0;
        emit("%s", asmline);
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
    clearLocals();

    type = curchar;
    advance();
    readName(name);

    nparams = hex2();
    nlocals = hex2();
    fsize = hex2();

    emit("\t.globl %s", name);
    emitLabel(name);
    comment("FUNC %s ret=%c params=%d locals=%d frame=%d",
            name, type, nparams, nlocals, fsize);

    indent = 2;

    /* Dump parameters */
    for (i = 0; i < nparams; i++) {
        char ptype, pname[14];
        unsigned char preg, poff;
        skipWs();
        if (curchar == 'd') {
            advance();
            ptype = curchar;
            advance();
            readName(pname);
            preg = hex2();
            poff = hex2();
            /* Byte params pushed via AF have value in high byte of stack word */
            if (ISBYTE(ptype) && preg == 0 && poff > 0)
                poff++;
            addLocal(pname, ptype, preg, poff);
            comment("param %c %s %s off=%d", ptype, pname, regnames[preg] ? regnames[preg] : "-", (char)(poff));
        }
    }

    /* Dump locals */
    for (i = 0; i < nlocals; i++) {
        char ltype, lname[14];
        unsigned char lreg, loff;
        skipWs();
        if (curchar == 'd') {
            advance();
            ltype = curchar;
            advance();
            readName(lname);
            lreg = hex2();
            loff = hex2();
            addLocal(lname, ltype, lreg, loff);
            comment("local %c %s %s off=%d", ltype, lname, regnames[lreg] ? regnames[lreg] : "-", (char)(loff));
        }
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
        skipWs();
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
