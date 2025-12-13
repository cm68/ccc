/*
 * cc2.c - AST dumper (newpass2)
 *
 * Emits labels with formatted AST dumps as comments
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdarg.h>

#include "cc2.h"

/* Global state */
int infd;
int outfd;
int curchar;
int lineno = 1;

/* Register names indexed by R_* constants */
char *regnames[] = { 0, "b", "c", "bc", "ix", "de", "hl", "a", "iy",
                     "(ix)", "(iy)", "(ix%+d)", "(iy%+d)" };

static char outbuf[256];
static int indent;
static int labelCnt;   /* label counter within function */
static int fnIndex;    /* function index for unique labels */

/* Symbol table for locals/register vars */
struct sym locals[MAXLOCALS];
int nlocals;

void
clearLocals(void)
{
    nlocals = 0;
}

void
addLocal(char *name, char type, int reg, int off)
{
    if (nlocals < MAXLOCALS) {
        strncpy(locals[nlocals].name, name, 13);
        locals[nlocals].name[13] = 0;
        locals[nlocals].type = type;
        locals[nlocals].reg = reg;
        locals[nlocals].off = off;
        nlocals++;
    }
}

struct sym *
findLocal(char *name)
{
    int i;
    for (i = 0; i < nlocals; i++) {
        if (strcmp(locals[i].name, name) == 0)
            return &locals[i];
    }
    return 0;
}

/*
 * AST I/O
 */
void
advance(void)
{
    char c;
    if (read(infd, &c, 1) == 1) {
        curchar = c;
        if (c == '\n') lineno++;
    } else {
        curchar = -1;
    }
}

void
skipWs(void)
{
    while (curchar == ' ' || curchar == '\t' ||
           curchar == '\n' || curchar == '\r') {
        advance();
    }
}

/* Read 2 hex digits */
int
hex2(void)
{
    int v = 0;
    int i;
    char c;

    skipWs();
    for (i = 0; i < 2; i++) {
        c = curchar;
        advance();
        if (c >= '0' && c <= '9') v = (v << 4) | (c - '0');
        else if (c >= 'a' && c <= 'f') v = (v << 4) | (c - 'a' + 10);
        else if (c >= 'A' && c <= 'F') v = (v << 4) | (c - 'A' + 10);
    }
    return v;
}

/* Read 4 hex digits */
int
hex4(void)
{
    return (hex2() << 8) | hex2();
}

/* Read 8 hex digits */
long
hex8(void)
{
    long v = 0;
    int i, neg = 0;
    char c;

    skipWs();
    if (curchar == '-') {
        neg = 1;
        advance();
    }
    for (i = 0; i < 8; i++) {
        c = curchar;
        advance();
        if (c >= '0' && c <= '9') v = (v << 4) | (c - '0');
        else if (c >= 'a' && c <= 'f') v = (v << 4) | (c - 'a' + 10);
        else if (c >= 'A' && c <= 'F') v = (v << 4) | (c - 'A' + 10);
    }
    return neg ? -v : v;
}

/* Read length-prefixed name */
void
readName(char *buf)
{
    int len, i;

    len = hex2();
    for (i = 0; i < len && i < 13; i++) {
        buf[i] = curchar;
        advance();
    }
    buf[i] = 0;
    while (i < len) {
        advance();
        i++;
    }
}

/*
 * Output
 */
void
emit(char *fmt, ...)
{
    va_list ap;
    char *p = outbuf;
    char *s;
    int n;

    va_start(ap, fmt);
    while (*fmt) {
        if (*fmt == '%') {
            fmt++;
            switch (*fmt++) {
            case 's':
                s = va_arg(ap, char *);
                while (*s) *p++ = *s++;
                break;
            case 'r':  /* register - may have %d for offset */
                s = va_arg(ap, char *);
                n = va_arg(ap, int);
                if (n > 127) n -= 256;  /* sign extend byte offset */
                p += sprintf(p, s, n);
                break;
            case 'o':  /* signed offset */
                n = va_arg(ap, int);
                p += sprintf(p, "%+d", n);
                break;
            case 'd':
                n = va_arg(ap, int);
                p += sprintf(p, "%d", n);
                break;
            case 'c':
                n = va_arg(ap, int);
                *p++ = n;
                break;
            case '%':
                *p++ = '%';
                break;
            }
        } else {
            *p++ = *fmt++;
        }
    }
    va_end(ap);
    *p++ = '\n';
    write(outfd, outbuf, p - outbuf);
}

void
emitLabel(char *name)
{
    int n;
    n = sprintf(outbuf, "%s:", name);
    outbuf[n++] = '\n';
    write(outfd, outbuf, n);
}

/* Emit comment with current indent */
void
comment(char *fmt, ...)
{
    va_list ap;
    int n, i;

    n = sprintf(outbuf, "; ");
    for (i = 0; i < indent; i++)
        outbuf[n++] = ' ';

    va_start(ap, fmt);
    n += vsprintf(outbuf + n, fmt, ap);
    va_end(ap);
    outbuf[n++] = '\n';
    write(outfd, outbuf, n);
}

/*
 * Forward declarations
 */
void emitExpr(struct expr *e);
void dumpStmt(void);

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
 * Parse expression from AST, build tree
 * With PARSEDUMP defined, emits "P:" comments showing parse-time info
 */
struct expr *
parseExpr(void)
{
    char c, type;
    char name[14];
    struct expr *e, *arg, *last;
    int nargs, i;

    skipWs();
    c = curchar;
    advance();

    switch (c) {
    case '#':  /* constant */
        type = curchar;
        advance();
        e = newExpr('#', type);
        e->v.l = hex8();
#ifdef PARSEDUMP
        comment("P: #%c %ld", type, e->v.l);
#endif
        return e;

    case '$':  /* symbol ref - swizzle to R/V for locals */
        readName(name);
        {
            struct sym *s = findLocal(name);
            if (s && s->reg) {
                e = newExpr('R', s->type);  /* register var */
                e->aux = s->reg;
            } else if (s) {
                e = newExpr('V', s->type);  /* local var */
                e->aux2 = s->off;
            } else {
                e = newExpr('$', 0);        /* global */
            }
            e->sym = strdup(name);
        }
#ifdef PARSEDUMP
        comment("P: $%s", name);
#endif
        return e;

    case 'M':  /* deref */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: M%c [", type);
        indent += 2;
#endif
        e = newExpr('M', type);
        e->left = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case '=':  /* assign */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: =%c [", type);
        indent += 2;
#endif
        e = newExpr('=', type);
        e->left = parseExpr();
        e->right = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case '@':  /* call */
        type = curchar;
        advance();
        nargs = hex2();
#ifdef PARSEDUMP
        comment("P: @%c nargs=%d [", type, nargs);
        indent += 2;
#endif
        e = newExpr('@', type);
        e->aux = nargs;
        e->left = parseExpr();  /* func */
        /* chain args as right-linked list */
        last = 0;
        for (i = 0; i < nargs; i++) {
            arg = parseExpr();
            if (!last)
                e->right = arg;
            else
                last->right = arg;
            last = arg;
        }
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case 'W':  /* widen */
    case 'N':  /* narrow */
    case 'x':  /* sign extend */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: %c%c [", c, type);
        indent += 2;
#endif
        e = newExpr(c, type);
        e->left = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case '(':  /* pre-inc */
    case ')':  /* post-inc */
    case '{':  /* pre-dec */
    case '}':  /* post-dec */
        type = curchar;
        advance();
        e = newExpr(c, type);
#ifdef PARSEDUMP
        comment("P: %c%c [", c, type);
        indent += 2;
#endif
        e->left = parseExpr();
        e->aux2 = hex4();  /* increment amount */
#ifdef PARSEDUMP
        comment("P: incr=%d", e->aux2);
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case '!':  /* logical not */
    case '~':  /* bitwise not */
    case '_':  /* unary minus */
    case '\\': /* negation (NEG) */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: %c%c [", c, type);
        indent += 2;
#endif
        e = newExpr(c, type);
        e->left = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case '+': case '-': case '*': case '/': case '%':
    case '<': case '>': case 'Q': case 'n': case 'L': case 'g':
    case '|': case '^': case '&': case 'y': case 'w':
    case 'j': case 'h':  /* logical and/or */
    case 'D': case 'O': case 'z':  /* unsigned ops */
        /* binary operators */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: %c%c [", c, type);
        indent += 2;
#endif
        e = newExpr(c, type);
        e->left = parseExpr();
        e->right = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case '?':  /* ternary: cond in left, then/else as left/right of right */
        type = curchar;
        advance();
        e = newExpr('?', type);
        e->aux = hex2();  /* nlabels */
#ifdef PARSEDUMP
        comment("P: ?%c nlabels=%d [", type, e->aux);
        indent += 2;
#endif
        e->left = parseExpr();  /* cond */
        e->right = newExpr(':', type);
        e->right->left = parseExpr();   /* then */
        e->right->right = parseExpr();  /* else */
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case 'o': case 'a': case 'm':  /* compound assign -=, &=, %= */
    case 'P': case '1': case 'X':  /* +=, |=, ^= */
    case 'T': case '2': case '6': case '0':  /* *=, /=, >>=, <<= */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: %c%c (compound) [", c, type);
        indent += 2;
#endif
        e = newExpr(c, type);
        e->left = parseExpr();
        e->right = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case ',':  /* comma */
        type = curchar;
        advance();
#ifdef PARSEDUMP
        comment("P: ,%c [", type);
        indent += 2;
#endif
        e = newExpr(',', type);
        e->left = parseExpr();
        e->right = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case 'F':  /* bitfield */
        type = curchar;
        advance();
        e = newExpr('F', type);
        e->aux = hex2();   /* offset */
        e->aux2 = hex2();  /* width */
#ifdef PARSEDUMP
        comment("P: F%c off=%d wid=%d [", type, e->aux, e->aux2);
        indent += 2;
#endif
        e->left = parseExpr();
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case 'Y':  /* memory copy */
        e = newExpr('Y', 0);
        e->aux = hex4();  /* length */
#ifdef PARSEDUMP
        comment("P: Y len=%d [", e->aux);
        indent += 2;
#endif
        e->left = parseExpr();   /* dest */
        e->right = parseExpr();  /* src */
#ifdef PARSEDUMP
        indent -= 2;
        comment("P: ]");
#endif
        return e;

    case 'U':  /* inline string - skip definition, parse actual expr */
        readName(name);
#ifdef PARSEDUMP
        comment("P: U (inline str %s)", name);
#endif
        {
            int len = hex2();
            while (len-- > 0) {
                advance();
                advance();
            }
        }
        return parseExpr();

    default:
#ifdef PARSEDUMP
        comment("P: ??? '%c' (0x%02x)", c, c);
#endif
        e = newExpr('?', 0);
        e->v.c = c;  /* save bad char for debugging */
        return e;
    }
}

/*
 * Calculate temporary demand for expression tree
 * Binop: sum of children, Unop/primary: 1
 * Returns demand, also stores in e->demand
 */
unsigned short
calcDemand(struct expr *e)
{
    unsigned char demand;
    unsigned char op = e->op;

    unsigned char size = TSIZE(e->type);
    unsigned char count = e->aux2;

    if (!op) return 0;  /* sentinel */
	
    /* special: =[BS] $var - assign to byte/short var is demand 1 */
    if (op == '=' && size <= 2 && e->left->op == '$') {
        calcDemand(e->right);  /* still calc children for display */
        demand = 1;
        goto done;
    }

    /* (s Rs reg -> inc reg; pre-inc/dec of regvar, incr <= 4 */
    if ((op == '(' || op == '{') && e->left->op == 'R' && count <= 4) {
        e->special = (op == '(') ? SP_INCR : SP_DECR;
        e->incr = e->aux2;
        e->dest = e->left->aux;     /* R_B, R_C, R_BC, or R_IX */
        demand = 0;
        goto done;
    }

    /* (b Vb ofs -> inc (iy+ofs); pre-inc/dec of byte local, incr <= 4 */
    if ((op == '(' || op == '{') && e->left->op == 'V' && size == 1 && count <= 4) {
        e->special = (op == '(') ? SP_INCR : SP_DECR;
        e->incr = count;
        e->offset = e->left->aux2;  /* IY offset from V node */
        e->dest = R_IYO;            /* (iy+ofs) addressing */
        demand = 0;
        goto done;
    }

    /* +p $sym #const -> ld hl,sym+offset */
    if (op == '+' && e->type == T_PTR &&
        e->left->op == '$' && e->right->op == '#' && e->left->sym) {
        e->special = SP_SYMOFS;
        e->sym = e->left->sym;          /* steal symbol from child */
        e->left->sym = 0;               /* prevent double-free */
        e->offset = e->right->v.s;
        demand = 1;
        goto done;
    }

    /* M[+p Mp[Rp(ix)] #ofs] -> (ix+ofs); IX-relative deref */
    if (op == 'M' && e->left->op == '+' && e->left->type == T_PTR &&
        e->left->left->op == 'M' && e->left->left->type == T_PTR &&
        e->left->left->left->op == 'R' && e->left->left->left->aux == R_IX &&
        e->left->right->op == '#') {
        e->special = SP_IXOD;
        e->offset = e->left->right->v.s;
        e->dest = R_IXO;
        demand = 0;
        goto done;
    }

    /* M[+p $sym #const] -> ld hl,(sym+offset) */
    if (op == 'M' && e->left->op == '+' && e->left->type == T_PTR &&
        e->left->left->op == '$' && e->left->right->op == '#' &&
        e->left->left->sym) {
        e->special = SP_SYMOFD;
        e->sym = e->left->left->sym;    /* steal symbol from grandchild */
        e->left->left->sym = 0;         /* prevent double-free */
        e->offset = e->left->right->v.s;
        demand = 1;
        goto done;
    }

    /* Ms $sym -> ld hl,(sym); direct deref of global */
    if (op == 'M' && e->left->op == '$' && e->left->sym) {
        e->special = SP_MSYM;
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        demand = 1;
        goto done;
    }

    /* *s expr #s 4 -> add hl,hl; multiply by power of 2 */
    if (op == '*' && e->right->op == '#') {
        long n = e->right->v.l;
        if (n > 0 && (n & (n - 1)) == 0) {
            int shifts = 0;
            while (n > 1) { shifts++; n >>= 1; }
            e->special = SP_MUL2;
            e->incr = shifts;
            demand = calcDemand(e->left);
            goto done;
        }
    }

    /* gB Ms $sym #B 0 -> bit 7,(sym+ofs); sign test x >= 0 */
    if (op == 'g' && e->left->op == 'M' && e->left->left->op == '$' &&
        e->right->op == '#' && e->right->v.l == 0 && e->left->left->sym) {
        e->special = SP_SIGN;
        e->sym = e->left->left->sym;    /* steal symbol */
        e->left->left->sym = 0;
        e->offset = TSIZE(e->left->type) - 1;  /* offset to high byte */
        demand = 1;
        goto done;
    }

    /* cmpB left M[+p Mp[Rp(ix)] #ofs] -> cp (ix+d); byte cmp with (ix+d) */
    if ((op == '<' || op == '>' || op == 'Q' || op == 'n' ||
         op == 'L' || op == 'g') && ISBYTE(e->type)) {
        struct expr *l = e->left, *r = e->right;
        /* check if right is the (ix+d) pattern */
        if (r->op == 'M' && r->left->op == '+' && r->left->type == T_PTR &&
            r->left->left->op == 'M' && r->left->left->type == T_PTR &&
            r->left->left->left->op == 'R' && r->left->left->left->aux == R_IX &&
            r->left->right->op == '#') {
            e->special = SP_CMPIX;
            e->offset = r->left->right->v.s;
            e->aux2 = 0;  /* right is (ix+d), normal sense */
            demand = calcDemand(l);  /* only need to emit left */
            goto done;
        }
        /* check if left is the (ix+d) pattern */
        if (l->op == 'M' && l->left->op == '+' && l->left->type == T_PTR &&
            l->left->left->op == 'M' && l->left->left->type == T_PTR &&
            l->left->left->left->op == 'R' && l->left->left->left->aux == R_IX &&
            l->left->right->op == '#') {
            e->special = SP_CMPIX;
            e->offset = l->left->right->v.s;
            e->aux2 = 1;  /* left is (ix+d), flipped sense */
            demand = calcDemand(r);  /* only need to emit right */
            goto done;
        }
    }

    /* R in IX (reg=4) or BC (reg=3) is demand 0 - already in register */
    if (op == 'R' && (e->aux == 3 || e->aux == 4)) {
        demand = 0;
        goto done;
    }

    if (op == '#' || op == '$' || op == 'R' || op == 'V') {
        demand = 1;
        goto done;
    }

    /* function call: max of all arg demands */
    /* direct call ($funcname) has demand 0 for the func ref */
    if (op == '@') {
        struct expr *arg;
        int d;
        demand = (e->left->op == '$') ? 0 : calcDemand(e->left);
        for (arg = e->right; arg && arg->op; arg = arg->right) {
            d = calcDemand(arg);
            if (d > demand) demand = d;
        }
        goto done;
    }

    demand = calcDemand(e->right) + calcDemand(e->left);

done:
    e->demand  = demand;
    return demand;
}

/*
 * Calculate tree depth
 */
static int
treeDepth(struct expr *e)
{
    int ld, rd;
    if (!e || !e->op) return 0;
    ld = treeDepth(e->left);
    rd = treeDepth(e->right);
    return 1 + (ld > rd ? ld : rd);
}

/*
 * Assign destination registers to expression nodes
 * dest: 'H'=HL, 'D'=DE, 'A'=A, 'E'=E
 * Only nodes with demand > 0 get a destination
 */
void
assignDest(struct expr *e, char dest)
{
    char op;

    if (!e || !e->op) return;

    op = e->op;

    /* no destination for demand 0 nodes */
    if (e->demand == 0) {
        e->dest = 0;
        return;
    }

    /* assign this node's destination */
    e->dest = dest;

    /* SP_CMPIX: only non-(ix+d) operand needs A */
    if (e->special == SP_CMPIX) {
        if (e->aux2 == 0)
            assignDest(e->left, R_A);   /* right is (ix+d), emit left */
        else
            assignDest(e->right, R_A);  /* left is (ix+d), emit right */
        return;
    }

    /* determine child destinations based on operator */
    if (op == '=' || op == '+' || op == '-' || op == '*' || op == '/' ||
        op == '%' || op == '&' || op == '|' || op == '^' ||
        op == 'Q' || op == 'q' || op == 'G' || op == 'g' ||  /* comparisons */
        op == 'L' || op == 'R' || op == 'l' || op == 'r') {  /* shifts */
        /* binop: deeper child gets HL, other gets DE */
        char hlDest = ISBYTE(e->type) ? R_A : R_HL;
        char deDest = ISBYTE(e->type) ? R_A : R_DE;
        if (treeDepth(e->left) >= treeDepth(e->right)) {
            assignDest(e->left, hlDest);
            assignDest(e->right, deDest);
        } else {
            assignDest(e->right, hlDest);
            assignDest(e->left, deDest);
        }
    } else if (op == 'M' || op == '~' || op == '!' || op == '_' ||
               op == 'W' || op == 'N' || op == 'x') {  /* widen/narrow/sext */
        /* unop: child to same dest */
        assignDest(e->left, dest);
    } else if (op == '(' || op == '{' || op == ')' || op == '}') {  /* inc/dec */
        /* inc/dec: child is lvalue (address), always needs HL */
        assignDest(e->left, R_HL);
    } else if (op == '@') {
        /* call: recurse into args with HL/A dest */
        struct expr *arg;
        char adest = ISBYTE(e->type) ? R_A : R_HL;
        for (arg = e->right; arg && arg->op; arg = arg->right) {
            assignDest(arg, adest);
        }
        /* indirect call needs func ptr in HL */
        if (e->left->op != '$') {
            assignDest(e->left, R_HL);
        }
    } else if (op == '?') {
        /* ternary: both branches to same dest */
        assignDest(e->left, dest);  /* condition */
        if (e->right) {
            assignDest(e->right->left, dest);   /* then */
            assignDest(e->right->right, dest);  /* else */
        }
    } else if (op == ',') {
        /* comma: left discarded, right to dest */
        assignDest(e->left, dest);
        assignDest(e->right, dest);
    }
}

/*
 * Emit expression tree
 */
void
emitExpr(struct expr *e)
{
    if (!e || !e->op) return;  /* null or sentinel */

    if (e->special)
        comment("SPECIAL");

    switch (e->op) {
    case '#':
        comment("#%c %ld d=%d %s", e->type, e->v.l, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        if (e->demand == 0)
            break;
        if (!regnames[e->dest]) {
            emit("XXXXXXXXX #dest");
        } else if (ISBYTE(e->type)) {
            emit("ld %s,%d", regnames[e->dest], e->v.c & 0xff);
        } else if (ISWORD(e->type)) {
            emit("ld %s,%d", regnames[e->dest], e->v.s & 0xffff);
        } else if (ISLONG(e->type)) {
            emit("XXXXXXXXX #long");
        } else {
            emit("XXXXXXXXX #?");
        }
        break;
    case '$':
        comment("$%s d=%d %s", e->sym ? e->sym : "?", e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        if (regnames[e->dest])
            emit("ld %s,%s", regnames[e->dest], e->sym);
        else if (e->demand > 0)
            emit("XXXXXXXXX $");
        break;
    case 'R':  /* register var */
        {
            static char *rnames[] = { "?", "b", "c", "bc", "ix" };
            comment("R%c %s %s d=%d %s", e->type, e->sym ? e->sym : "?",
                rnames[e->aux < 5 ? e->aux : 0], e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            if (e->demand > 0)
                emit("XXXXXXXXX R");
        }
        break;
    case 'V':  /* local var */
        comment("V%c %s off=%d d=%d %s", e->type, e->sym ? e->sym : "?", (signed char)e->aux2, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        if (e->demand > 0)
            emit("XXXXXXXXX V");
        break;
    case 'M':
        comment("M%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->special == SP_IXOD) {
            /* M[+p Rp(ix) #ofs] -> (ix+ofs) addressing, demand 0 */
            comment("(ix%+d)", e->offset);
            /* no code - addressing mode used by parent */
        } else if (e->special == SP_MSYM) {
            /* M $sym -> ld reg,(sym) */
            comment("$%s", e->sym);
            if (regnames[e->dest])
                emit("ld %s,(%s)", regnames[e->dest], e->sym);
            else
                emit("XXXXXXXXX M$dest");
        } else if (e->special == SP_SYMOFD) {
            /* M[+p $sym #ofs] -> ld reg,(sym+ofs) */
            comment("+p $%s #%d", e->sym, e->offset);
            if (regnames[e->dest])
                emit("ld %s,(%s+%d)", regnames[e->dest], e->sym, e->offset);
            else
                emit("XXXXXXXXX M$+dest");
        } else {
            /* indirect: emit address, then deref */
            emitExpr(e->left);
            if (ISBYTE(e->type)) {
                emit("ld a,(hl)");
            } else if (ISWORD(e->type) && e->dest == R_HL) {
                emit("ld a,(hl)");
                emit("inc hl");
                emit("ld h,(hl)");
                emit("ld l,a");
            } else if (e->demand > 0) {
                emit("XXXXXXXXX M");
            }
        }
        indent -= 2;
        comment("]");
        break;
    case '=':
        comment("=%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
        indent += 2;
        /* assign to IX or BC register var */
        if (e->left->op == 'R' && e->left->aux == 4) {
            comment("Rp %s ix d=0 -", e->left->sym ? e->left->sym : "?");
            emitExpr(e->right);
            emit("push hl");
            emit("pop ix");
        } else if (e->left->op == 'R' && e->left->aux == 3) {
            comment("R%c %s bc d=0 -", e->left->type, e->left->sym ? e->left->sym : "?");
            emitExpr(e->right);
            emit("ld b,h");
            emit("ld c,l");
        } else {
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX =");
        }
        indent -= 2;
        comment("]");
        break;
    case '@':
        comment("@%c nargs=%d d=%d %s%s [", e->type, e->aux, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
        indent += 2;
        /* evaluate and push args */
        {
            struct expr *a = e->right;
            while (a && a->op) {
                /* check for M[R] in BC or IX - push directly */
                if (a->op == 'M' && a->left->op == 'R' && a->left->aux == 3) {
                    comment("M%c [R%c %s reg=%d] d=%d", a->type,
                        a->left->type, a->left->sym ? a->left->sym : "?",
                        a->left->aux, a->demand);
                    emit("push bc");
                } else if (a->op == 'M' && a->left->op == 'R' && a->left->aux == 4) {
                    comment("M%c [R%c %s reg=%d] d=%d", a->type,
                        a->left->type, a->left->sym ? a->left->sym : "?",
                        a->left->aux, a->demand);
                    emit("push ix");
                } else {
                    emitExpr(a);
                    /* push based on size */
                    if (ISBYTE(a->type))
                        emit("push af");
                    else
                        emit("push hl");
                }
                a = a->right;
            }
        }
        /* call */
        if (e->left->op == '$') {
            comment("$%s d=%d", e->left->sym, e->left->demand);
            emit("call _%s", e->left->sym);
        } else {
            emitExpr(e->left);
            emit("call _callhl");
        }
        indent -= 2;
        comment("]");
        break;
    case '?':
        comment("?%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        if (e->right) {
            emitExpr(e->right->left);
            emitExpr(e->right->right);
        }
        emit("XXXXXXXXX ?");
        indent -= 2;
        comment("]");
        break;
    case '(':  /* pre-inc */
    case '{':  /* pre-dec */
        if (e->special == SP_INCR || e->special == SP_DECR) {
            char *ins = (e->special == SP_INCR) ? "inc" : "dec";
            int i;
            for (i = 0; i < e->incr; i++)
                emit("\t%s %r", ins, regnames[e->dest], e->offset);
        } else {
            comment("%c%c d=%d %s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
            indent += 2;
            emitExpr(e->left);
            comment("incr=%d", e->aux2);
            /* unused byte inc/dec with small incr: use inc/dec (hl) */
            if (e->unused && ISBYTE(e->type) && e->aux2 <= 4) {
                char *ins = (e->op == '(') ? "inc" : "dec";
                int i;
                for (i = 0; i < e->aux2; i++)
                    emit("%s (hl)", ins);
            } else {
                emit("XXXXXXXXX %c", e->op);
            }
            indent -= 2;
            comment("]");
        }
        break;
    case ')':  /* post-inc */
    case '}':  /* post-dec */
        comment("%c%c d=%d %s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
        indent += 2;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        /* unused byte inc/dec with small incr: use inc/dec (hl) */
        if (e->unused && ISBYTE(e->type) && e->aux2 <= 4) {
            char *ins = (e->op == ')') ? "inc" : "dec";
            int i;
            for (i = 0; i < e->aux2; i++)
                emit("%s (hl)", ins);
        } else {
            emit("XXXXXXXXX %c", e->op);
        }
        indent -= 2;
        comment("]");
        break;
    case 'F':  /* bitfield */
        comment("F%c off=%d wid=%d d=%d %s [", e->type, e->aux, e->aux2, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        emit("XXXXXXXXX F");
        indent -= 2;
        comment("]");
        break;
    case 'Y':  /* memory copy */
        comment("Y len=%d d=%d %s [", e->aux, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        emitExpr(e->right);
        emit("XXXXXXXXX Y");
        indent -= 2;
        comment("]");
        break;
    case '+':  /* add */
        comment("+%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->special == SP_SYMOFS) {
            /* +p $sym #const -> ld hl,sym+ofs */
            comment("$%s #%d", e->sym ? e->sym : "?", e->offset);
            if (e->offset > 0)
                emit("ld hl,%s+%d", e->sym, e->offset);
            else if (e->offset < 0)
                emit("ld hl,%s%d", e->sym, e->offset);
            else
                emit("ld hl,%s", e->sym);
        } else {
            emitExpr(e->left);
            emitExpr(e->right);
            if (e->type == T_PTR)
                emit("add hl,de");
            else
                emit("XXXXXXXXX +");
        }
        indent -= 2;
        comment("]");
        break;
    case '*':  /* multiply */
        comment("*%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->special == SP_MUL2) {
            int i;
            emitExpr(e->left);
            comment("#%c %ld d=%d -", e->right->type, e->right->v.l, e->right->demand);
            for (i = 0; i < e->incr; i++)
                emit("add hl,hl");
        } else {
            /* non-power-of-2: normal binop */
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX *");
        }
        indent -= 2;
        comment("]");
        break;
    case '<': case '>': case 'Q': case 'n': case 'L':  /* comparisons */
        comment("%c%c d=%d %s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
        indent += 2;
        if (e->special == SP_CMPIX) {
            /* byte cmp with (ix+d): aux2=0 right is (ix+d), aux2=1 left is */
            comment("CMPIX ofs=%d side=%d", e->offset, e->aux2);
            if (e->aux2 == 0)
                emitExpr(e->left);   /* left to A */
            else
                emitExpr(e->right);  /* right to A */
            emit("cp (ix%o)", e->offset);
            /* aux2=0: A - (ix+d) = left - right, flags normal */
            /* aux2=1: A - (ix+d) = right - left, flags flipped */
        } else {
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX %c", e->op);
        }
        indent -= 2;
        comment("]");
        break;
    case 'g':  /* >= comparison */
        comment("g%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
        indent += 2;
        if (e->special == SP_CMPIX) {
            comment("CMPIX ofs=%d side=%d", e->offset, e->aux2);
            if (e->aux2 == 0)
                emitExpr(e->left);
            else
                emitExpr(e->right);
            emit("cp (ix%o)", e->offset);
        } else if (e->special == SP_SIGN) {
            /* sign test: bit 7 of high byte */
            comment("SIGN $%s ofs=%d", e->sym ? e->sym : "?", e->offset);
            if (e->offset > 0)
                emit("ld hl,%s+%d", e->sym, e->offset);
            else
                emit("ld hl,%s", e->sym);
            emit("bit 7,(hl)");
            if (!e->cond) {
                /* not used as condition - need 0/1 value */
                /* Z=non-negative(1), NZ=negative(0) */
                int lbl = labelCnt++;
                emit("ld a,0");
                emit("jr nz,sg%d_%d", lbl, fnIndex);
                emit("inc a");
                emit("sg%d_%d:", lbl, fnIndex);
            }
            /* else: result in flags Z=true, NZ=false */
        } else {
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX g");
        }
        indent -= 2;
        comment("]");
        break;
    default:
        if (e->left && e->right) {
            comment("%c%c d=%d %s%s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-",
                e->unused ? " U" : "", e->cond ? " C" : "");
            indent += 2;
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX %c", e->op);
            indent -= 2;
            comment("]");
        } else if (e->left) {
            comment("%c%c d=%d %s%s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-",
                e->unused ? " U" : "", e->cond ? " C" : "");
            indent += 2;
            emitExpr(e->left);
            emit("XXXXXXXXX %c", e->op);
            indent -= 2;
            comment("]");
        } else {
            comment("%c%c d=%d %s%s%s", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-",
                e->unused ? " U" : "", e->cond ? " C" : "");
            emit("XXXXXXXXX %c", e->op);
        }
        break;
    }
}

/*
 * Dump statement (recursive)
 */
void
dumpStmt(void)
{
    char c, name[14];
    int ndecls, nstmts, hasElse, nlabels;
    int ncases, nstmt, i;

    skipWs();
    c = curchar;
    advance();

    switch (c) {
    case 'B':  /* block */
        ndecls = hex2();
        nstmts = hex2();
        comment("BLOCK decls=%d stmts=%d {", ndecls, nstmts);
        indent += 2;
        /* parse decls: 'd' type name reg off */
        for (i = 0; i < ndecls; i++) {
            char dtype, dname[14];
            int dreg, doff;
            skipWs();
            if (curchar == 'd') {
                advance();
                dtype = curchar;
                advance();
                readName(dname);
                dreg = hex2();
                doff = hex2();
                addLocal(dname, dtype, dreg, doff);
                comment("decl %c %s reg=%d off=%d", dtype, dname, dreg, (signed char)doff);
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
            int special;
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
            assignDest(cond, ISBYTE(cond->type) ? R_A : R_HL);
            emitExpr(cond);
            special = cond->special;
            {
                char cop = cond->op;
                int side = cond->aux2;
                freeExpr(cond);
                /* emit conditional jump to skip then block */
                if (special == SP_SIGN) {
                    /* >= 0: Z=true, NZ=false; jump to no on NZ */
                    emit("jr nz,no%d_%d", lbl, fnIndex);
                } else if (special == SP_CMPIX) {
                    /* cp sets flags: C if A < operand, Z if A == operand */
                    /* side=0: A=left, (ix+d)=right -> A-right = left-right */
                    /* side=1: A=right, (ix+d)=left -> A-left = right-left */
                    /* We skip the then block when condition is FALSE */
                    char *jmp = "XXXXXXXXX";
                    switch (cop) {
                    case 'Q':  /* == : Z, skip on NZ */
                        jmp = "jr nz";
                        break;
                    case 'n':  /* != : NZ, skip on Z */
                        jmp = "jr z";
                        break;
                    case '<':
                        /* side=0: left<right means C, skip on NC */
                        /* side=1: left<right means NC&NZ (right>left), need 2 jumps */
                        jmp = side ? "jr c" : "jr nc";  /* side=1: skip on C */
                        break;
                    case 'L':  /* <= */
                        /* side=0: left<=right means C or Z, skip on NC&NZ (tricky) */
                        /* side=1: left<=right means NC (right>=left), skip on C */
                        jmp = side ? "jr c" : "jr nc";
                        break;
                    case '>':
                        /* side=0: left>right means NC&NZ, skip on C or Z */
                        /* side=1: left>right means C, skip on NC */
                        jmp = side ? "jr nc" : "jr c";
                        break;
                    case 'g':  /* >= */
                        /* side=0: left>=right means NC, skip on C */
                        /* side=1: left>=right means C or Z (right<=left), tricky */
                        jmp = side ? "jr nc" : "jr c";
                        break;
                    }
                    emit("%s,no%d_%d", jmp, lbl, fnIndex);
                } else {
                    /* general: test HL/A for zero, jump if zero */
                    emit("ld a,h");
                    emit("or l");
                    emit("jr z,no%d_%d", lbl, fnIndex);
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
            assignDest(e, ISBYTE(e->type) ? R_A : R_HL);
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
            int hasVal = hex2();
            if (hasVal) {
                struct expr *e = parseExpr();
                calcDemand(e);
                assignDest(e, ISBYTE(e->type) ? R_A : R_HL);
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
            int hasLabel = hex2();
            if (hasLabel) readName(name);
            ncases = hex2();
            comment("SWITCH label=%s cases=%d [", hasLabel ? name : "(none)", ncases);
            indent += 2;
            e = parseExpr();
            calcDemand(e);
            assignDest(e, ISBYTE(e->type) ? R_A : R_HL);
            emitExpr(e);
            freeExpr(e);
            /* Cases are statements, parsed via dumpStmt */
            for (i = 0; i < ncases; i++)
                dumpStmt();
            indent -= 2;
            comment("]");
        }
        break;

    case 'C':  /* case */
        {
            struct expr *e;
            nstmt = hex2();
            comment("CASE nstmt=%d [", nstmt);
            indent += 2;
            e = parseExpr();
            calcDemand(e);
            assignDest(e, ISBYTE(e->type) ? R_A : R_HL);
            emitExpr(e);
            freeExpr(e);
            for (i = 0; i < nstmt; i++)
                dumpStmt();
            indent -= 2;
            comment("]");
        }
        break;

    case 'O':  /* default */
        nstmt = hex2();
        comment("DEFAULT [");
        indent += 2;
        for (i = 0; i < nstmt; i++)
            dumpStmt();
        indent -= 2;
        comment("]");
        break;

    case ';':  /* empty */
        comment(";");
        break;

    case 'A':  /* inline asm */
        {
            int len = hex4();
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
            int len = hex2();
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
 * Emit initializer (recursive)
 */
static void
emitInit(void)
{
    char ftype;
    long lval;
    int count, i;
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
            emit("\t.byte %ld", lval & 0xff);
        else if (ftype == 'l' || ftype == 'f')
            emit("\t.long 0x%lx", lval);
        else
            emit("\t.word %ld", lval & 0xffff);
    } else if (curchar == '$') {
        advance();
        readName(symname);
        emit("\t.word %s", symname);
    } else if (curchar == 'W' || curchar == 'N' || curchar == 'x') {
        advance();
        advance();
        emitInit();
    } else {
        advance();
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
    int hasInit;
    int count, elemsize, size;

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
            emit("\t.comm %s,%d", name, size);
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
            emit("\t.comm %s,%d", name, size);
        }
        return;
    }

    hasInit = hex2();
    emit("\t.globl %s", name);
    if (hasInit) {
        emitLabel(name);
        emitInit();
    } else {
        emit("\t.comm %s,%d", name, TSIZE(type));
    }
}

/*
 * Parse string literal
 */
void
parseString(void)
{
    char name[14];
    int len, i;

    readName(name);
    len = hex2();

    emitLabel(name);
    for (i = 0; i < len; i++) {
        int hi, lo, val;
        hi = curchar;
        advance();
        lo = curchar;
        advance();

        if (hi >= '0' && hi <= '9') hi -= '0';
        else if (hi >= 'a' && hi <= 'f') hi = hi - 'a' + 10;
        else if (hi >= 'A' && hi <= 'F') hi = hi - 'A' + 10;

        if (lo >= '0' && lo <= '9') lo -= '0';
        else if (lo >= 'a' && lo <= 'f') lo = lo - 'a' + 10;
        else if (lo >= 'A' && lo <= 'F') lo = lo - 'A' + 10;

        val = (hi << 4) | lo;
        emit("\t.byte %d", val);
    }
}

/*
 * Parse global asm
 */
void
parseGlobAsm(void)
{
    int len = hex4();
    int i;
    char asmline[256];
    int asmpos = 0;

    for (i = 0; i < len; i++) {
        int hi, lo, ch;
        hi = curchar;
        advance();
        lo = curchar;
        advance();

        if (hi >= '0' && hi <= '9') hi -= '0';
        else if (hi >= 'a' && hi <= 'f') hi = hi - 'a' + 10;
        else if (hi >= 'A' && hi <= 'F') hi = hi - 'A' + 10;

        if (lo >= '0' && lo <= '9') lo -= '0';
        else if (lo >= 'a' && lo <= 'f') lo = lo - 'a' + 10;
        else if (lo >= 'A' && lo <= 'F') lo = lo - 'A' + 10;

        ch = (hi << 4) | lo;
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
    int nparams, nlocals, fsize;
    int i;
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
        int preg, poff;
        skipWs();
        if (curchar == 'd') {
            advance();
            ptype = curchar;
            advance();
            readName(pname);
            preg = hex2();
            poff = hex2();
            addLocal(pname, ptype, preg, poff);
            comment("param %c %s reg=%d off=%d", ptype, pname, preg, (signed char)poff);
        }
    }

    /* Dump locals */
    for (i = 0; i < nlocals; i++) {
        char ltype, lname[14];
        int lreg, loff;
        skipWs();
        if (curchar == 'd') {
            advance();
            ltype = curchar;
            advance();
            readName(lname);
            lreg = hex2();
            loff = hex2();
            addLocal(lname, ltype, lreg, loff);
            comment("local %c %s %s off=%d", ltype, lname, regnames[lreg] ? regnames[lreg] : "-", (signed char)loff);
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
    while (curchar != -1) {
        skipWs();
        if (curchar == -1) break;

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
 * Main
 */
int
main(int argc, char **argv)
{
    char *infile = 0;
    char *outfile = 0;

    argc--;
    argv++;

    while (argc > 0) {
        if (strcmp(argv[0], "-o") == 0) {
            argc--;
            argv++;
            if (argc > 0) {
                outfile = argv[0];
                argc--;
                argv++;
            }
        } else {
            infile = argv[0];
            argc--;
            argv++;
        }
    }

    if (!infile) {
        fprintf(stderr, "usage: cc2 [-o out] input.ast\n");
        return 1;
    }

    infd = open(infile, O_RDONLY);
    if (infd < 0) {
        perror(infile);
        return 1;
    }

    if (outfile) {
        outfd = open(outfile, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (outfd < 0) {
            perror(outfile);
            return 1;
        }
    } else {
        outfd = 1;
    }

    advance();

    emit("; Generated by newpass2 (AST dump mode)");
    emit("");

    parseAst();

    close(infd);
    if (outfd != 1) close(outfd);

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

