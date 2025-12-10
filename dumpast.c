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
    case AST_PREINC: return "++p";
    case AST_POSTINC: return "p++";
    case AST_PREDEC: return "--p";
    case AST_POSTDEC: return "p--";
    case '1': return "|=";
    case AST_ANDEQ: return "&=";
    case 'P': return "+=";
    case AST_SUBEQ: return "-=";
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
    if (flags & OP_BCINDIR) len += 2;
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
    if (flags & OP_BCINDIR) out("BC");
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
        outf("(I@%d ", s->label);
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
        outf("(W@%d ", s->label);
        dumpExpr(s->expr);
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(")");
        break;

    case 'D': /* Do-while */
        outf("(D@%d", s->label);
        if (s->then_branch) { newLine(); dumpStmt(s->then_branch); }
        out(" ");
        dumpExpr(s->expr);
        out(")");
        break;

    case 'F': /* For */
        outf("(F@%d ", s->label);
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
dumpFnAst(char fd)
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

/* Location type names */
static const char *
locName(int loc)
{
    switch (loc) {
    case LOC_NONE:  return "-";
    case LOC_CONST: return "const";
    case LOC_REG:   return "reg";
    case LOC_MEM:   return "mem";
    case LOC_STACK: return "stk";
    case LOC_IX:    return "ix";
    case LOC_INDIR: return "ind";
    default:        return "?";
    }
}

/* Register names */
static const char *
regName(int reg)
{
    switch (reg) {
    case R_NONE: return "-";
    case R_A:    return "A";
    case R_HL:   return "HL";
    case R_DE:   return "DE";
    case R_BC:   return "BC";
    case R_IX:   return "IX";
    case R_IY:   return "IY";
    case R_SP:   return "SP";
    default:     return "?";
    }
}

/* Instruction names for scheduled ops */
static const char *
insName(int ins)
{
    switch (ins) {
    case EO_NOP:       return "NOP";
    case EO_HL_BC:     return "HL<-BC";
    case EO_HL_DE:     return "HL<->DE";
    case EO_DE_HL:     return "DE<->HL";
    case EO_BC_HL:     return "BC<-HL";
    case EO_A_L:       return "A<-L";
    case EO_A_B:       return "A<-B";
    case EO_A_C:       return "A<-C";
    case EO_L_A:       return "L<-A";
    case EO_HL_IX:     return "HL<-IX";
    case EO_IX_HL:     return "IX<-HL";
    case EO_HL_IYW:    return "HL<-(IY+n)";
    case EO_HL_IXW:    return "HL<-(IX+n)";
    case EO_HL_MEM:    return "HL<-(mem)";
    case EO_HL_CONST:  return "HL<-const";
    case EO_HLHL_IYL:  return "HLHL'<-(IY+n:4)";
    case EO_HLHL_IXL:  return "HLHL'<-(IX+n:4)";
    case EO_DE_IYW:    return "DE<-(IY+n)";
    case EO_DE_IXW:    return "DE<-(IX+n)";
    case EO_DE_MEM:    return "DE<-(mem)";
    case EO_DE_CONST:  return "DE<-const";
    case EO_A_IY:      return "A<-(IY+n)";
    case EO_A_IX:      return "A<-(IX+n)";
    case EO_A_MEM:     return "A<-(mem)";
    case EO_A_CONST:   return "A<-const";
    case EO_A_BC_IND:  return "A<-(BC)";
    case EO_A_HL_IND:  return "A<-(HL)";
    case EO_IYW_HL:    return "(IY+n)<-HL";
    case EO_IXW_HL:    return "(IX+n)<-HL";
    case EO_MEM_HL:    return "(mem)<-HL";
    case EO_MEM_BC:    return "(mem)<-BC";
    case EO_MEM_DE:    return "(mem)<-DE";
    case EO_IY_A:      return "(IY+n)<-A";
    case EO_IX_A:      return "(IX+n)<-A";
    case EO_MEM_A:     return "(mem)<-A";
    case EO_BC_IND_A:  return "(BC)<-A";
    case EO_HL_IND_A:  return "(HL)<-A";
    case EO_ADD_HL_DE: return "HL+=DE";
    case EO_ADD_HL_BC: return "HL+=BC";
    case EO_SBC_HL_DE: return "HL-=DE";
    default:           return "?";
    }
}

/* Dump expression with scheduling info */
static void
dumpSchedExpr(struct expr *e, int indent)
{
    int i;
    const char *oname;
    if (!e) return;

    /* Comment prefix and indent */
    fdprintf(dumpFd, "; ");
    for (i = 0; i < indent; i++) fdprintf(dumpFd, "  ");

    /* Op name or code */
    oname = opName(e->op);
    if (oname) {
        fdprintf(dumpFd, "%s", oname);
    } else {
        fdprintf(dumpFd, "%c", e->op);
    }
    fdprintf(dumpFd, ":%d", e->size);

    /* Opflags (from setExprFlags) */
    if (e->opflags) {
        fdprintf(dumpFd, " flags=[");
        if (e->opflags & OP_CONST) fdprintf(dumpFd, "C");
        if (e->opflags & OP_SIMPLEVAR) fdprintf(dumpFd, "V");
        if (e->opflags & OP_REGVAR) fdprintf(dumpFd, "R");
        if (e->opflags & OP_IXMEM) fdprintf(dumpFd, "IX");
        if (e->opflags & OP_IYMEM) fdprintf(dumpFd, "IY");
        if (e->opflags & OP_GLOBAL) fdprintf(dumpFd, "G");
        if (e->opflags & OP_INDIR) fdprintf(dumpFd, "I");
        if (e->opflags & OP_BCINDIR) fdprintf(dumpFd, "BC");
        fdprintf(dumpFd, "]");
    }

    /* Location info (from old scheduler) */
    if (e->loc != LOC_NONE) {
        fdprintf(dumpFd, " loc=%s", locName(e->loc));
        if (e->loc == LOC_REG || e->loc == LOC_INDIR) {
            fdprintf(dumpFd, "(%s)", regName(e->reg));
        }
        if (e->loc == LOC_STACK || e->loc == LOC_IX) {
            fdprintf(dumpFd, "(%d)", (int)e->offset);
        }
    }

    /* Destination (from sched2) */
    if (e->dest != R_NONE) {
        fdprintf(dumpFd, " dest=%s", regName(e->dest));
    }

    /* Scheduled instructions */
    if (e->nins > 0) {
        fdprintf(dumpFd, " emit={");
        for (i = 0; i < e->nins; i++) {
            if (i > 0) fdprintf(dumpFd, ",");
            fdprintf(dumpFd, "%s", insName(e->ins[i]));
        }
        fdprintf(dumpFd, "}");
    }

    /* Value/symbol */
    if (e->op == 'C') {
        fdprintf(dumpFd, " val=%ld", e->value);
    } else if (e->op == '$' && e->symbol) {
        fdprintf(dumpFd, " sym=%s", e->symbol);
    }

    fdprintf(dumpFd, "\n");

    /* Children */
    if (e->left)  dumpSchedExpr(e->left, indent + 1);
    if (e->right) dumpSchedExpr(e->right, indent + 1);
}

/* Dump scheduled statement tree */
static void
dumpSchedStmt(struct stmt *s, int indent)
{
    int i;
    if (!s) return;

    /* Comment prefix and indent */
    fdprintf(dumpFd, "; ");
    for (i = 0; i < indent; i++) fdprintf(dumpFd, "  ");

    /* Statement type */
    fdprintf(dumpFd, "%c", s->type);
    if (s->label) fdprintf(dumpFd, " L%d", s->label);
    if (s->label2) fdprintf(dumpFd, " L2=%d", s->label2);
    fdprintf(dumpFd, "\n");

    /* Expression */
    if (s->expr) dumpSchedExpr(s->expr, indent + 1);
    if (s->expr2) dumpSchedExpr(s->expr2, indent + 1);
    if (s->expr3) dumpSchedExpr(s->expr3, indent + 1);

    /* Branches */
    if (s->then_branch) dumpSchedStmt(s->then_branch, indent + 1);
    if (s->else_branch) dumpSchedStmt(s->else_branch, indent + 1);

    /* Next */
    if (s->next) dumpSchedStmt(s->next, indent);
}

/* Entry point: dump scheduled tree */
void
dumpScheduled(char fd)
{
    struct local_var *v;

    if (!fnBody) return;

    dumpFd = fd;
    fdprintf(fd, "; Scheduled tree for %s (ret=%s):\n", fnName ? fnName : "?",
        fnRettype ? fnRettype : "?");

    /* Dump variable stats */
    for (v = fnLocals; v; v = v->next) {
        fdprintf(fd, ";   %s: ofs=%d sz=%d ref=%d agg=%d reg=%d%s\n",
            v->name, v->offset, v->size, v->ref_count, v->agg_refs,
            v->reg, v->is_param ? " (param)" : "");
    }

    dumpSchedStmt(fnBody, 0);
}

#else

/* Stub when DEBUG not defined */
void
dumpFnAst(char fd)
{
    (void)fd;
}

void
dumpScheduled(char fd)
{
    (void)fd;
}

#endif /* DEBUG */
