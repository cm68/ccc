/*
 * AST Pretty Printer for ccc compiler
 * Rewritten from astpp.py
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *data;
static int pos, len;
static int indent;
static int blockCnt;

static int cur(void) { return pos < len ? data[pos] : -1; }
static void advance(void) { if (pos < len) pos++; }

static void skipWS(void) {
    int c;
    while ((c = cur()) == ' ' || c == '\t' || c == '\n' || c == '\r')
        advance();
}

static int hval(int c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + c - 'a';
    if (c >= 'A' && c <= 'F') return 10 + c - 'A';
    return 0;
}

static int readHex2(void) {
    int h = hval(cur()); advance();
    int l = hval(cur()); advance();
    return h * 16 + l;
}

static int readHex4(void) {
    int i, v = 0;
    for (i = 0; i < 4; i++) { v = v * 16 + hval(cur()); advance(); }
    return v;
}

static long readHex8(void) {
    int i, neg = 0;
    long v = 0;
    if (cur() == '-') { neg = 1; advance(); }
    for (i = 0; i < 8; i++) { v = v * 16 + hval(cur()); advance(); }
    return neg ? -v : v;
}

static char nameBuf[256];
static char *readName(void) {
    int i, n = readHex2();
    for (i = 0; i < n && i < 255; i++) { nameBuf[i] = cur(); advance(); }
    nameBuf[n < 255 ? n : 255] = 0;
    return nameBuf;
}

static void prIndent(void) {
    int i;
    for (i = 0; i < indent; i++) printf("  ");
}

static void prln(const char *s) { prIndent(); puts(s); }

static const char *widthName(int c) {
    switch (c) {
    case 'b': return "byte";
    case 'B': return "ubyte";
    case 's': return "short";
    case 'S': return "ushort";
    case 'l': return "long";
    case 'L': return "ulong";
    case 'p': return "ptr";
    case 'f': return "float";
    case 'd': return "double";
    case 'v': return "void";
    default: { static char buf[2]; buf[0] = c; buf[1] = 0; return buf; }
    }
}

static const char *regName(int r) {
    switch (r) {
    case 0: return "-";
    case 1: return "B";
    case 2: return "C";
    case 3: return "BC";
    case 4: return "IX";
    default: return "?";
    }
}

static const char *opName(int c) {
    switch (c) {
    case 'M': return "DEREF";
    case '=': return "ASSIGN";
    case '+': return "ADD";
    case '-': return "SUB";
    case '*': return "MUL";
    case '/': return "DIV";
    case '%': return "MOD";
    case '&': return "AND";
    case '|': return "OR";
    case '^': return "XOR";
    case '~': return "NOT";
    case 'y': return "LSHIFT";
    case 'w': return "RSHIFT";
    case '<': return "LT";
    case '>': return "GT";
    case 'L': return "LE";
    case 'g': return "GE";
    case 'Q': return "EQ";
    case 'n': return "NE";
    case 'j': return "LAND";
    case 'h': return "LOR";
    case '!': return "LNOT";
    case 'N': return "NARROW";
    case 'W': return "WIDEN";
    case 'X': return "SEXT";
    case '?': return "TERNARY";
    case '@': return "CALL";
    case 'Y': return "COPY";
    case '\\': return "NEG";
    case '\'': return "ADDR";
    case 'P': return "+=";
    case 'T': return "*=";
    case '0': return "<<=";
    case '1': return "|=";
    case '2': return "/=";
    case '6': return ">>=";
    case ':': return "COLON";
    case '(': return "++p";
    case ')': return "p++";
    case '{': return "--p";
    case '}': return "p--";
    case 'o': return "-=";
    case 'm': return "%=";
    case 'a': return "&=";
    case 'e': return "BFEXT";
    case 'x': return "SEXT";
    default: { static char buf[2]; buf[0] = c; buf[1] = 0; return buf; }
    }
}

static int opArity(int c) {
    if (c == '(' || c == ')' || c == '{' || c == '}') return -1; /* special */
    if (c == 'e' || c == 'f') return -1; /* bitfield */
    if (c == 'x') return 1;
    if (c == 'M' || c == 'N' || c == 'W' || c == '!' || c == '~' || c == '\\' || c == '\'') return 1;
    if (c == '@' || c == '?' || c == 'Y') return -1;
    return 2;
}

#define EXPR_BUF_SIZE 4096
static char exprBuf[EXPR_BUF_SIZE];
static int exprPos;

static void exprReset(void) { exprPos = 0; exprBuf[0] = 0; }
static void exprApp(const char *s) {
    while (*s && exprPos < EXPR_BUF_SIZE - 1) exprBuf[exprPos++] = *s++;
    exprBuf[exprPos] = 0;
}
static void exprAppC(int c) {
    if (exprPos < EXPR_BUF_SIZE - 1) exprBuf[exprPos++] = c;
    exprBuf[exprPos] = 0;
}
static void exprAppNum(long n) {
    char buf[24];
    sprintf(buf, "%ld", n);
    exprApp(buf);
}

static void parseExpr(void);

static void parseExpr(void) {
    skipWS();
    int c = cur();

    if (c == -1) { exprApp("(EOF)"); return; }
    if (c == '_') { advance(); exprApp("()"); return; }

    /* Symbol reference */
    if (c == '$') {
        advance();
        exprApp("$");
        exprApp(readName());
        return;
    }

    /* Stack offset */
    if (c == 'S') {
        advance();
        int off = readHex4();
        char buf[32];
        sprintf(buf, "SP[%d]", off);
        exprApp(buf);
        return;
    }

    /* Inline string literal */
    if (c == 'U') {
        int i, n;
        advance();
        readName(); /* skip name */
        n = readHex2();
        for (i = 0; i < n; i++) { advance(); advance(); }
        parseExpr();
        return;
    }

    /* Numeric constant */
    if (c == '#') {
        advance();
        int w = cur(); advance();
        long v = readHex8();
        exprAppNum(v);
        exprApp(":");
        exprApp(widthName(w));
        return;
    }

    /* Inc/Dec */
    if (c == '(' || c == ')' || c == '{' || c == '}') {
        const char *opn = (c == '(') ? "PREINC" : (c == ')') ? "POSTINC" : (c == '{') ? "PREDEC" : "POSTDEC";
        advance();
        int w = cur(); advance();
        exprApp("(");
        exprApp(opn);
        exprApp(":");
        exprApp(widthName(w));
        exprApp(" ");
        parseExpr();
        int delta = readHex4();
        char buf[16];
        sprintf(buf, " %d)", delta);
        exprApp(buf);
        return;
    }

    /* Bitfield extract */
    if (c == 'e') {
        advance();
        int off = readHex2();
        int wid = readHex2();
        exprApp("(BFEXT ");
        exprAppNum(off);
        exprApp(":");
        exprAppNum(wid);
        exprApp(" ");
        parseExpr();
        exprApp(")");
        return;
    }

    /* Bitfield assign */
    if (c == 'f') {
        advance();
        int off = readHex2();
        int wid = readHex2();
        exprApp("(BFSET ");
        exprAppNum(off);
        exprApp(":");
        exprAppNum(wid);
        exprApp(" ");
        parseExpr();
        exprApp(" ");
        parseExpr();
        exprApp(")");
        return;
    }

    /* Sign extend */
    if (c == 'x') {
        advance();
        int w = cur(); advance();
        exprApp("(SEXT:");
        exprApp(widthName(w));
        exprApp(" ");
        parseExpr();
        exprApp(")");
        return;
    }

    /* Regular operator */
    int opChar = c;
    advance();

    /* Call */
    if (opChar == '@') {
        int i, retType = cur(); advance();
        int argc = readHex2();
        exprApp("(CALL:");
        exprAppC(retType);
        exprApp(" ");
        parseExpr();
        for (i = 0; i < argc; i++) {
            exprApp(" ");
            parseExpr();
        }
        exprApp(")");
        return;
    }

    /* Ternary */
    if (opChar == '?') {
        int w = cur(); advance();
        readHex2(); /* skip nlabels */
        exprApp("(?:");
        exprApp(widthName(w));
        exprApp(" ");
        parseExpr();
        exprApp(" ");
        parseExpr();
        exprApp(" ");
        parseExpr();
        exprApp(")");
        return;
    }

    /* Copy */
    if (opChar == 'Y') {
        int sz = readHex4();
        exprApp("(COPY:");
        exprAppNum(sz);
        exprApp(" ");
        parseExpr();
        exprApp(" ");
        parseExpr();
        exprApp(")");
        return;
    }

    /* Regular operator with width */
    int w = cur(); advance();
    int arity = opArity(opChar);

    exprApp("(");
    exprApp(opName(opChar));
    exprApp(":");
    exprApp(widthName(w));
    exprApp(" ");
    parseExpr();
    if (arity == 2) {
        exprApp(" ");
        parseExpr();
    }
    exprApp(")");
}

static char *getExpr(void) {
    exprReset();
    parseExpr();
    return exprBuf;
}

static void parseStmt(void);

static void parseDecl(void) {
    skipWS();
    if (cur() != 'd') return;
    advance();
    int typeChar = cur(); advance();
    char *name = readName();
    char nameCopy[256];
    strcpy(nameCopy, name);
    int reg = readHex2();
    int off = readHex2();
    if (off > 127) off -= 256;

    prIndent();
    printf("DECL %s : %s reg=%s off=IY%+d\n", nameCopy, widthName(typeChar), regName(reg), off);
}

static void parseStmt(void) {
    skipWS();
    int c = cur();
    if (c == -1) return;
    advance();

    char lineBuf[512];

    /* Block */
    if (c == 'B') {
        int i, declCnt = readHex2();
        int stmtCnt = readHex2();
        int blockNum = blockCnt++;
        sprintf(lineBuf, "BLOCK %d {", blockNum);
        prln(lineBuf);
        indent++;
        for (i = 0; i < declCnt; i++) parseDecl();
        for (i = 0; i < stmtCnt; i++) parseStmt();
        indent--;
        prln("}");
        return;
    }

    /* If */
    if (c == 'I') {
        int hasElse = readHex2();
        int nlabels = readHex2();
        char *cond = getExpr();
        sprintf(lineBuf, "IF [nlabels=%d] (%s)", nlabels, cond);
        prln(lineBuf);
        indent++;
        parseStmt();
        indent--;
        if (hasElse == 1) {
            prln("ELSE");
            indent++;
            parseStmt();
            indent--;
        }
        return;
    }

    /* Expression statement */
    if (c == 'E') {
        char *e = getExpr();
        sprintf(lineBuf, "EXPR %s", e);
        prln(lineBuf);
        return;
    }

    /* Return */
    if (c == 'R') {
        int hasVal = readHex2();
        if (hasVal == 1) {
            char *e = getExpr();
            sprintf(lineBuf, "RETURN %s", e);
            prln(lineBuf);
        } else {
            prln("RETURN");
        }
        return;
    }

    /* Label */
    if (c == 'L') {
        char *name = readName();
        sprintf(lineBuf, "LABEL %s:", name);
        prln(lineBuf);
        return;
    }

    /* Goto */
    if (c == 'G') {
        char *name = readName();
        sprintf(lineBuf, "GOTO %s", name);
        prln(lineBuf);
        return;
    }

    /* Switch */
    if (c == 'S') {
        int i, hasLabel = readHex2();
        char labelBuf[256] = "";
        int caseCnt;
        char *expr;
        if (hasLabel == 1) strcpy(labelBuf, readName());
        caseCnt = readHex2();
        expr = getExpr();
        if (labelBuf[0])
            sprintf(lineBuf, "SWITCH [%s] (%s) {", labelBuf, expr);
        else
            sprintf(lineBuf, "SWITCH (%s) {", expr);
        prln(lineBuf);
        indent++;
        for (i = 0; i < caseCnt; i++) parseStmt();
        indent--;
        prln("}");
        return;
    }

    /* Case */
    if (c == 'C') {
        int i, stmtCnt = readHex2();
        char *val = getExpr();
        sprintf(lineBuf, "CASE %s:", val);
        prln(lineBuf);
        indent++;
        for (i = 0; i < stmtCnt; i++) parseStmt();
        indent--;
        return;
    }

    /* Default */
    if (c == 'O') {
        int i, stmtCnt = readHex2();
        prln("DEFAULT:");
        indent++;
        for (i = 0; i < stmtCnt; i++) parseStmt();
        indent--;
        return;
    }

    /* Asm */
    if (c == 'A') {
        int i, asmLen = readHex4();
        int hasNL = 0;
        char *asmBuf = malloc(asmLen + 1);
        for (i = 0; i < asmLen; i++) {
            int hi = hval(cur()); advance();
            int lo = hval(cur()); advance();
            asmBuf[i] = hi * 16 + lo;
        }
        asmBuf[asmLen] = 0;

        /* Check for newlines */
        for (i = 0; i < asmLen; i++) if (asmBuf[i] == '\n') { hasNL = 1; break; }

        if (!hasNL) {
            sprintf(lineBuf, "ASM { %s }", asmBuf);
            prln(lineBuf);
        } else {
            prln("ASM {");
            indent++;
            char *p = asmBuf, *q;
            while ((q = strchr(p, '\n')) != NULL) {
                *q = 0;
                prln(p);
                p = q + 1;
            }
            if (*p) prln(p);
            indent--;
            prln("}");
        }
        free(asmBuf);
        return;
    }

    /* Empty statement */
    if (c == ';') { prln(";"); return; }

    /* Break */
    if (c == 'K') { prln("BREAK"); return; }

    /* Continue */
    if (c == 'N') { prln("CONTINUE"); return; }

    /* While */
    if (c == 'W') {
        readHex2(); /* nlabels */
        char *cond = getExpr();
        sprintf(lineBuf, "WHILE (%s)", cond);
        prln(lineBuf);
        indent++;
        parseStmt();
        indent--;
        return;
    }

    /* Do-while */
    if (c == 'D') {
        readHex2(); /* nlabels */
        prln("DO");
        indent++;
        parseStmt();
        indent--;
        char *cond = getExpr();
        sprintf(lineBuf, "WHILE (%s)", cond);
        prln(lineBuf);
        return;
    }

    /* For */
    if (c == 'F') {
        readHex2(); /* nlabels */
        char initBuf[EXPR_BUF_SIZE], condBuf[EXPR_BUF_SIZE], incrBuf[EXPR_BUF_SIZE];
        strcpy(initBuf, getExpr());
        strcpy(condBuf, getExpr());
        strcpy(incrBuf, getExpr());
        prIndent();
        printf("FOR (%s; %s; %s)\n", initBuf, condBuf, incrBuf);
        indent++;
        parseStmt();
        indent--;
        return;
    }

    sprintf(lineBuf, "??? stmt '%c'", c);
    prln(lineBuf);
}

static void parseInit(void);
static char initBuf[4096];
static int initPos;

static void initReset(void) { initPos = 0; initBuf[0] = 0; }
static void initApp(const char *s) {
    while (*s && initPos < 4095) initBuf[initPos++] = *s++;
    initBuf[initPos] = 0;
}
static void initAppNum(long n) {
    char buf[24];
    sprintf(buf, "%ld", n);
    initApp(buf);
}

static void parseInit(void) {
    skipWS();
    int c = cur();

    /* Array */
    if (c == '[') {
        int i, count;
        advance();
        advance(); /* elem type */
        count = readHex2();
        initApp("[");
        for (i = 0; i < count; i++) {
            if (i > 0) initApp(", ");
            parseInit();
        }
        if (cur() == ']') advance();
        initApp("]");
        return;
    }

    /* Aggregate */
    if (c == '{') {
        int i, count;
        advance();
        count = readHex2();
        initApp("{");
        for (i = 0; i < count; i++) {
            if (i > 0) initApp(", ");
            parseInit();
        }
        if (cur() == '}') advance();
        initApp("}");
        return;
    }

    /* Scalar */
    if (c == '#') {
        advance();
        int w = cur(); advance();
        long val = readHex8();
        initAppNum(val);
        initApp(":");
        initApp(widthName(w));
        return;
    }

    /* Symbol */
    if (c == '$') {
        advance();
        initApp("$");
        initApp(readName());
        return;
    }

    /* Widened */
    if (c == 'W') {
        advance();
        advance(); /* skip target type */
        initApp("(W ");
        parseInit();
        initApp(")");
        return;
    }

    /* ADD expression (for pointer arithmetic in initializers) */
    if (c == '+') {
        advance();
        advance(); /* skip type suffix */
        initApp("(");
        parseInit();
        initApp(" + ");
        parseInit();
        initApp(")");
        return;
    }

    /* Unknown */
    advance();
    initApp("?");
}

static char *getInit(void) {
    initReset();
    parseInit();
    return initBuf;
}

static void parseFunction(void) {
    int retType = cur(); advance();
    char *name = readName();
    char nameCopy[256];
    strcpy(nameCopy, name);

    int paramCnt = readHex2();
    int localCnt = readHex2();
    int frmSize = readHex2();

    /* Params */
    char paramsBuf[1024] = "";
    int i, plen = 0;
    for (i = 0; i < paramCnt; i++) {
        int ptype, preg, poff;
        char *pname;
        char pnameCopy[256];
        skipWS();
        if (cur() != 'd') continue;
        advance();
        ptype = cur(); advance();
        pname = readName();
        strcpy(pnameCopy, pname);
        preg = readHex2();
        poff = readHex2();
        if (poff > 127) poff -= 256;

        if (plen > 0) { paramsBuf[plen++] = ','; paramsBuf[plen++] = ' '; }
        plen += sprintf(paramsBuf + plen, "%s:%s@", pnameCopy, widthName(ptype));
        if (preg)
            plen += sprintf(paramsBuf + plen, "%s", regName(preg));
        else
            plen += sprintf(paramsBuf + plen, "IY%+d", poff);
    }

    /* Locals */
    char localsBuf[1024] = "";
    int llen = 0;
    for (i = 0; i < localCnt; i++) {
        int ltype, lreg, loff;
        char *lname;
        char lnameCopy[256];
        skipWS();
        if (cur() != 'd') continue;
        advance();
        ltype = cur(); advance();
        lname = readName();
        strcpy(lnameCopy, lname);
        lreg = readHex2();
        loff = readHex2();
        if (loff > 127) loff -= 256;

        if (llen > 0) { localsBuf[llen++] = ','; localsBuf[llen++] = ' '; }
        llen += sprintf(localsBuf + llen, "%s:%s@", lnameCopy, widthName(ltype));
        if (lreg)
            llen += sprintf(localsBuf + llen, "%s", regName(lreg));
        else
            llen += sprintf(localsBuf + llen, "IY%+d", loff);
    }

    printf("\nFUNCTION %s(%s) -> %s [frame=%d]\n", nameCopy, paramsBuf, widthName(retType), frmSize);
    if (localsBuf[0]) printf("  LOCALS: %s\n", localsBuf);
    puts("{");
    indent = 1;
    skipWS();
    parseStmt();
    puts("}");
}

static void parseGlobal(void) {
    skipWS();
    if (cur() == '$') advance();
    char *name = readName();
    char nameCopy[256];
    strcpy(nameCopy, name);
    int typeChar = cur(); advance();

    /* Array */
    if (typeChar == 'a') {
        int count = readHex4();
        int elemSize = readHex4();
        int hasInit = readHex2();
        printf("GLOBAL %s : array[%d] of %d-byte", nameCopy, count, elemSize);
        if (hasInit == 1) {
            char *init = getInit();
            printf(" = %s\n", init);
        } else {
            puts("");
        }
        return;
    }

    /* Struct */
    if (typeChar == 'r') {
        int size = readHex4();
        int hasInit = readHex2();
        printf("GLOBAL %s : struct[%d]", nameCopy, size);
        if (hasInit == 1) {
            char *init = getInit();
            printf(" = %s\n", init);
        } else {
            puts("");
        }
        return;
    }

    /* Pointer or primitive */
    int hasInit = readHex2();
    printf("GLOBAL %s : %s", nameCopy, widthName(typeChar));
    if (hasInit == 1) {
        char *init = getInit();
        printf(" = %s\n", init);
    } else {
        puts("");
    }
}

static void parseString(void) {
    int i, n;
    char *name = readName();
    char nameCopy[256];
    strcpy(nameCopy, name);
    n = readHex2();
    printf("STRING _%s = \"", nameCopy);
    for (i = 0; i < n; i++) {
        int b = readHex2();
        if (b >= 32 && b < 127 && b != '"' && b != '\\')
            putchar(b);
        else
            printf("\\x%02x", b);
    }
    puts("\"");
}

static int parseTopLevel(void) {
    skipWS();
    int c = cur();
    if (c == -1) return 0;
    advance();

    if (c == 'F') parseFunction();
    else if (c == 'Z') parseGlobal();
    else if (c == 'U') parseString();
    else if (c == '\n') ; /* skip */
    else printf("??? top-level: '%c'\n", c);

    return 1;
}

int main(int argc, char **argv) {
    FILE *f;
    if (argc > 1) {
        f = fopen(argv[1], "rb");
        if (!f) { perror(argv[1]); return 1; }
    } else {
        f = stdin;
    }

    /* Read entire file */
    fseek(f, 0, SEEK_END);
    len = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (len < 0) {
        /* stdin - read in chunks */
        int cap = 4096;
        data = malloc(cap);
        len = 0;
        int n;
        while ((n = fread(data + len, 1, cap - len, f)) > 0) {
            len += n;
            if (len == cap) {
                cap *= 2;
                data = realloc(data, cap);
            }
        }
    } else {
        data = malloc(len + 1);
        fread(data, 1, len, f);
    }
    if (f != stdin) fclose(f);

    puts("========================================");
    puts("AST Pretty Printer Output");
    puts("========================================");

    while (parseTopLevel())
        ;

    puts("");
    puts("========================================");

    free(data);
    return 0;
}
