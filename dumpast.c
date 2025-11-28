/*
 * dumpast.c - AST dump to assembly comments
 *
 * Outputs function AST as assembly comments before function prolog.
 * Called after setOpFlags() so register allocation and opflags are visible.
 */

#include <stdio.h>
#include "cc2.h"

#ifdef DEBUG

/* Output state */
static int dumpFd;
static int dumpCol;

/* Forward declarations */
static int exprLen(struct expr *e);
static void dumpExpr(struct expr *e);
static void dumpStmt(struct stmt *s);

/* Start new comment line */
static void
newLine(void)
{
    fdprintf(dumpFd, "\n; ");
    dumpCol = 2;
}

/* Output string, tracking column */
static void
out(const char *s)
{
    while (*s) {
        fdprintf(dumpFd, "%c", *s);
        dumpCol++;
        s++;
    }
}

/* Output formatted, tracking column */
static void
outf(const char *fmt, long val)
{
    char buf[32];
    snprintf(buf, sizeof(buf), fmt, val);
    out(buf);
}

/* Get operator name */
static const char *
opName(unsigned char op)
{
    switch (op) {
    case 'C': return "C";
    case '$': return "$";
    case 'M': return "M";
    case '=': return "=";
    case '+': return "+";
    case '-': return "-";
    case '*': return "*";
    case '/': return "/";
    case '%': return "%";
    case '&': return "&";
    case '|': return "|";
    case '^': return "^";
    case '~': return "~";
    case '!': return "!";
    case '<': return "<";
    case '>': return ">";
    case 'L': return "<=";
    case 'g': return ">=";
    case 'Q': return "==";
    case 'n': return "!=";
    case 'y': return "<<";
    case 'z': return ">>";
    case 'j': return "&&";
    case 'h': return "||";
    case '@': return "@";
    case 'A': return "A";
    case 'W': return "W";
    case 'X': return "X";
    case 'N': return "N";
    case ',': return ",";
    case '?': return "?";
    case ':': return ":";
    case 0xcf: return "++p";
    case 0xef: return "p++";
    case 0xd6: return "--p";
    case 0xf6: return "p--";
    case 0x31: return "|=";
    case 0xc6: return "&=";
    case 'P': return "+=";
    case 0xdf: return "-=";
    default: return NULL;
    }
}

/* Calculate length of opflags string */
static int
opflagsLen(unsigned char flags)
{
    int len = 0;
    if (!flags) return 0;
    len = 2; /* [] */
    if (flags & OP_CONST) len += 1;
    if (flags & OP_SIMPLEVAR) len += 1;
    if (flags & OP_REGVAR) len += 1;
    if (flags & OP_IXMEM) len += 2;
    if (flags & OP_IYMEM) len += 2;
    if (flags & OP_GLOBAL) len += 1;
    if (flags & OP_INDIR) len += 1;
    return len;
}

/* Calculate length of expression when printed */
static int
exprLen(struct expr *e)
{
    const char *name;
    int len;
    if (!e) return 0;

    len = 2; /* () */
    name = opName(e->op);
    if (name) len += strlen(name);
    else len += 4; /* 0xNN */

    /* Size suffix */
    if (e->size == 1 || e->size == 4 || e->type_str == 'p') len += 2;

    /* Opflags */
    len += opflagsLen(e->opflags);

    /* Value or symbol */
    if (e->op == 'C') {
        char buf[20];
        snprintf(buf, sizeof(buf), "%ld", e->value);
        len += 1 + strlen(buf); /* space + number */
    } else if (e->op == '$' && e->symbol) {
        len += 1 + strlen(e->symbol); /* space + symbol */
    }

    /* Children */
    if (e->left) len += 1 + exprLen(e->left);
    if (e->right) len += 1 + exprLen(e->right);

    return len;
}

/* Output opflags */
static void
dumpOpflags(unsigned char flags)
{
    if (!flags) return;
    out("[");
    if (flags & OP_CONST) out("C");
    if (flags & OP_SIMPLEVAR) out("V");
    if (flags & OP_REGVAR) out("R");
    if (flags & OP_IXMEM) out("IX");
    if (flags & OP_IYMEM) out("IY");
    if (flags & OP_GLOBAL) out("G");
    if (flags & OP_INDIR) out("I");
    out("]");
}

/* Dump expression */
static void
dumpExpr(struct expr *e)
{
    const char *name;
    if (!e) return;

    /* Check if expression fits on current line */
    if (dumpCol + exprLen(e) > 78) newLine();

    out("(");
    name = opName(e->op);
    if (name) {
        out(name);
    } else {
        outf("0x%02x", e->op);
    }

    /* Size suffix */
    if (e->size == 1) out(":b");
    else if (e->size == 4) out(":l");
    else if (e->type_str == 'p') out(":p");

    /* Opflags */
    dumpOpflags(e->opflags);

    /* Value or symbol */
    if (e->op == 'C') {
        outf(" %ld", e->value);
    } else if (e->op == '$' && e->symbol) {
        out(" ");
        out(e->symbol);
    }

    /* Children */
    if (e->left) { out(" "); dumpExpr(e->left); }
    if (e->right) { out(" "); dumpExpr(e->right); }

    out(")");
}

/* Dump single statement */
static void
dumpStmt(struct stmt *s)
{
    struct stmt *child;
    if (!s) return;

    switch (s->type) {
    case 'B': /* Block */
        out("(B");
        for (child = s->then_branch; child; child = child->next) {
            newLine();
            dumpStmt(child);
        }
        out(")");
        break;

    case 'E': /* Expression statement */
        out("(E ");
        dumpExpr(s->expr);
        out(")");
        break;

    case 'I': /* If */
        out("(I ");
        dumpExpr(s->expr);
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        if (s->else_branch) { newLine(); dumpStmt(s->else_branch); }
        out(")");
        break;

    case 'R': /* Return */
        out("(R");
        if (s->expr) { out(" "); dumpExpr(s->expr); }
        out(")");
        break;

    case 'L': /* Label */
        out("(L ");
        out(s->symbol ? s->symbol : "?");
        out(")");
        break;

    case 'G': /* Goto */
        out("(G ");
        out(s->symbol ? s->symbol : "?");
        out(")");
        break;

    case 'd': /* Declaration */
        out("(d");
        if (s->type_str) outf(":%c", s->type_str);
        out(" ");
        out(s->symbol ? s->symbol : "?");
        out(")");
        break;

    case 'S': /* Switch */
        out("(S ");
        dumpExpr(s->expr);
        for (child = s->then_branch; child; child = child->next) {
            newLine();
            dumpStmt(child);
        }
        out(")");
        break;

    case 'C': /* Case */
        outf("(C %ld", s->expr ? s->expr->value : 0);
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(")");
        break;

    case 'O': /* Default */
        out("(O");
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(")");
        break;

    case 'W': /* While */
        out("(W ");
        dumpExpr(s->expr);
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(")");
        break;

    case 'D': /* Do-while */
        out("(D");
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(" ");
        dumpExpr(s->expr);
        out(")");
        break;

    case 'F': /* For */
        out("(F ");
        if (s->expr) dumpExpr(s->expr);
        out("; ");
        if (s->expr2) dumpExpr(s->expr2);
        out("; ");
        if (s->expr3) dumpExpr(s->expr3);
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(")");
        break;

    case 'b': /* Break */
        out("(b)");
        break;

    case 'c': /* Continue */
        out("(c)");
        break;

    default:
        outf("(%c", s->type);
        if (s->symbol) { out(" "); out(s->symbol); }
        if (s->expr) { out(" "); dumpExpr(s->expr); }
        out(")");
        break;
    }
}

/* Entry point: dump function AST as comments */
void
dumpFnAst(int fd)
{
    struct stmt *s;
    if (!fnBody) return;

    dumpFd = fd;
    fdprintf(fd, "; AST %s:\n; ", fnName ? fnName : "?");
    dumpCol = 2;

    for (s = fnBody; s; s = s->next) {
        dumpStmt(s);
        if (s->next) newLine();
    }
    fdprintf(fd, "\n");
}

#else

/* Stub when DEBUG not defined */
void
dumpFnAst(int fd)
{
    (void)fd;
}

#endif /* DEBUG */
