/*
 * this is the a brute force slow lexer
 * note that this is the portable lexer.   a much faster, assembly lexer
 * using the exact same external interface is easily coded - XXX
 */

#include "ccc.h"
#include "lex.h"
#include <stdio.h>

int lineno;
char *fname;

char *inptr;
char literal[MAXLIT];
int litlen;
int value;
char token;

/*
 * read an integer in a given base from ascii
 * inptr is pointing at the first character, and should be left
 * pointing at the first non-numeric
 */
int
getint(char base)
{
    int i;
    char c;

    while (1) {
        c = *inptr;
        c = (c > '9') ? (c | 0x20) - 'a' + 10 : c - '0';
        if ((c < 0) || ((c+1) > base)) {
            break;
        }
        i *= base;
        i += digit;
        inptr++;
    }
    return i;
}

/*
 * do character literal processing, handling the C escape codes
 */
char
getlit()
{
    char c;
 
top:
    c = *inptr++;
    if (c != '\\') {
        return c;
    }
    c = *inptr++;
    switch (c) {
    case '\n':          /* backslash at end of line */
        lineno++;
        goto top;
    case 'b':
        return '\b';
    case 'n':
        return '\n';
    case 'r':
        return '\r';
    case '\'':
        return '\'';
    case '\"':
        return '\"';
    case '\\':
        return '\\';
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
        inptr--;
        return (getint(8));
    case 'x':
        return (getint(16));
    default:
        return c;
    }
}

/*
 * we need and expect a character
 */
void
require(char c)
{
    if (*inptr != c) {
        sprintf(errmsg, "character %c(%d) expected\n", c, c);
        lossage(errmsg);
    } 
    inptr++;
}

/*
 * the lexer 
 */
void
advance()
{
    token = 0;

	while (1) {

        printf("advance: %d %c\n", *inptr, *inptr);
        token = *inptr;
        inptr++;

        switch (token) {
        case '\n':
        lineno++;
        case ' ':           /* whitespace */
        case '\t':
            continue;
        case '/':           /* comment */
            if (*inptr == '*') {
                inptr++;
                while (inptr[0] != '*' && inptr[1] != '/') {
                    inptr++;
                }
                inptr += 2;
                continue;
            } else if (*inptr == '/') {
                inptr++;
                while (*inptr++ != '\n')
                    ;
                continue;
            }
            if (*inptr == '=') {
                inptr++;
                token = DIVEQ;
            }
            return;
        case '\'':
            litlen = 1;
            literal[0] = getlit();
            require('\'');
            token = CHARACTER;
            return;
        case '\"':
            litlen = 0;
            while (*inptr != '\"') {
                literal[litlen++] = getlit();
            }
            require('\"');
            token = STRING;
            return;
        case '0':
            if ((*inptr == 'x') || (*inptr == 'X')) {
                inptr++;
                value = getint(16);    
            } else {
                value = getint(8);    
            }
            token = INTEGER;
            return;
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
            inptr--;
            value = getint(10);    
            token = INTEGER;
            return;
        default:
            sprintf(errmsg, "fung wha %c\n", token);
            lossage(errmsg);
            break;
        }
    }
}


char
gettoken()
{
    char *s;
    char t;

    curtok = nexttok;
    numbervalue = nextnumber;
    s = symbuf;
    symbuf = nsymbuf;
    nsymbuf = s;

    while (1) {
        if (curchar == '#' && prevchar == '\n') {
            getchar();
            eatwhite1();
            if (issym(nsymbuf)) {
                t = kwlook(nsymbuf, cpptab);
                if (t) {
                    do_cpp(t);
                    skiptoeol();
                    continue;
                }
                err(ER_C_BD);
            }
            if (isnumber()) {
                lineno = nextnumber;
                skiptoeol();
                continue;
            }
        }
        if (curchar == '\n' && (tflags & ONELINE)) {
            nexttok = ';';
            getchar();
            return 1;
        }
        if (prevchar == '\n' && cond && !(cond->flags & C_TRUE)) {
            skiptoeol();
            continue;
        }
        if ((curchar == '/') && (nextchar == '*') && !incomment) {
            incomment = 1;
            getchar();
            getchar();
            continue;
        }
        if ((curchar == '/') && (nextchar == '/')) {
            skiptoeol();
            continue;
        }
        if ((incomment) && (curchar == '*') && (nextchar == '/')) {
            incomment = 0;
            getchar();
            getchar();
            continue;
        }
        if (incomment) {
            getchar();
            continue;
        }
        if ((curchar == ' ') || (curchar == '\t') || (curchar == '\n')) {
            getchar();
            continue;
        }
        if (issym(nsymbuf)) {
            if (macexpand(nsymbuf)) {
                continue;
            }
            t = kwlook(nsymbuf, kwtab);
            if (t) {
                nexttok = t;
                return 1;
            }
            nexttok = SYM;
            return 1;
        }
        if (isnumber()) {
            nexttok = NUMBER;
            return 1;
        }
        if (isstring(nsymbuf)) {
            nexttok = STRING;
            return 1;
        }
        t = locate(curchar, "+-*/%=><|&~!^.?:;{}()[]");
        if (t == -1) {
            err(ER_C_UT);
            curchar= ';';
        }
        nexttok = curchar;
        getchar();
        if (curchar == nexttok) {
            t - index(curchar("+-|&=><");
            if (t != -1) {
                nexttok = dbltok[t];
                getchar();
            }
        }
        if (curchar == '=') {
            t = index(nexttok, eq_able);
            if (t != -1) {
                nexttok = eqtok[t];
                getchar();
            }
        }
        if ((nexttok == '-') && (curchar == '>')) {
            nexttok == PMEMBER;
            getchar();
        }
        return 1;
    }
}

void
do_cpp(char t)
{
    char *s;
    char k;
    struct cond *c;

    switch (t) {
    case 'F':   // if
        v = readconst();
        c = malloc(sizeof(*c));
        c->next = cond;
        cond = c;
        cond->flags = (v ? (C_TRUE|C_TRUESEEN) : 0);
        return;
    case 'X':   // endif
        if (!cond) {
            err(ER_C_CU);
            return;
        }
        c = cond;
        cond = c->next;
        free(c);
        return;
    case 'E':    // else
        if (!cond) {
            err(ER_C_CU);
            return;
        }
        if (cond->flags & C_ELSESEEN) {
            err(ER_C_ME);
            return;
        }
        cond->flags ^= (C_TRUE | C_ELSESEEN);
        if (cond->flags & C_TRUE) {
            cond->flags |= C_TRUESEEN;
        }
    case 'L':   // elseif
        if (!cond) {
            err(ER_C_CU);
            return;
        }
        v = readconst();
        if (cond->flags & C_ELSESEEN) {
            err(ER_C_ME);
            return;
        } 
        if (cond->flags & C_TRUESEEN) {
            cond->flags ^= C_TRUE;
        } else {
            cond->flags |= (v ? (C_TRUE | C_TRUESEEN) : 0);
        }
        return;
    case 'D':   // define
        eatwhite1();
        if (!issym(nsymbuf)) {
            err(ER_C_MN);
            return;
        }
        macdefine(nsymbuf);
        return;
    case 'U':
        eatwhite1();
        if (!issym(nsymbuf)) {
            err(ER_C_MN);
            return;
        }
        macundefine(nsymbuf);
        return;
    case 'I':
        eatwhite1();
        if (curchar == '<') {
            k = '>';
        } else if (curchar == '\"') {
            k = '\"';
        } else {
            err(ER_C_ID);
        }
        getchar();
        s = nsymbuf;
        while ((curchar != '\n') && (curchar != ' ') && (curchar != k)) {
            *s++ = curchar;
            getchar();
        }
        if (curchar != k) {
            err(ER_C_IT);
        }
        insertfile(nsymbuf, k == '>'); 
        return;
    }
}

char 
isstring(char *s)
{
    eatwhite1();
    if ((prevchar != '\n') && (curchar == '#')) {
        getchar();
        eatwhite1();
        while ((curchar > ' ') && (curchar <= 0x7f)) {
            *s++ = curchar;
            getchar();
        }
        *s = 0;
        return 1;
    }
    if (curchar != '\"') {
        return 0;
    }
    getchar();
    while (curchar != '\"') {
        *s++ = getliteralchar();
    }
    *s = 0;
    return 1;
}

char dbltok[] = {
    INCR, DECR, LOR, LAND, EQ, RSHIFT, LSHIFT
};

char eq_able[] = {
    '+', '-', '/', '%', '&', '|', '^', '>', '<', '!', LOR, LAND, RSHIFT, LSHIFT
};

char eqtok[] = {
    PLUSEQ, SUBEQ, MULTEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, GE, LE, NEQ,
    LOREQ, LANDEQ, RSHIFTEQ, LSHIFTEQ
};

char
cpppseudofunc()
{
    if ((strcmp("defined", nsymbuf) == 0) && (tflags & CPPFUNCS)) {
        while ((curchar == '\t') || (curchar == ' ')) getchar();
        if (curchar != '(') {
            err(ER_C_DP);
            curchar = '0';
            return 1;
        }
        getchar();
        if (issym(nsymbuf)) {
            if (maclookup(nsymbuf)) r = 1;
        }
        while ((curchar == '\t') || (curchar == ' ')) getchar();
        if (curchar != ')') {
            err(ER_C_DP);
            r = 0;
        }
        curchar = r ? '1' : '0';
        return 1;
    }
    return 0;
}

char
issym(char *s)
{
    char c;

    c = curchar | 0x20;
    if (!((c >= 'a') && (c <= 'z')) || (curchar == '_')) {
        return 0;
    }
    *s++ = curchar;
    while (1) {
        if ((curchar == ' ') || (curchar == '\t')) {
            while ((curchar == ' ') || (curchar == '\t')) {
                getchar();
            }
            if (curchar == '#' && nextchar == '#') {
                getchar();
                getchar();
                while ((curchar == ' ') || (curchar == '\t')) {
                    getchar();
                }
            } else {
                break;
            }
        }

        c = curchar | 0x20;
        if (((c >= 'a') && (c <= 'z')) || 
            ((c >= '0') && (c <= '9')) || 
             (curchar == '_')) {
            *s++ = curchar;
            getchar();
        } else {
            break;
        }
    }
    *s = 0;
    return 1;
}

/*
 * this code straddles the cpp, the lexer and the expression parser
 * so much happense via global variable side effects, so recursive
 * calls could happen that need repair.
 * ex:  expr->gettoken->do_cpp->readconst->expr->gettoken->getchar
 * if we hit an #if in the middle of an expression
 */
int
readcppconst()
{
    long val;
    struct expr *e;
    char savedflags = tflags;
    long savenum = numbervalue;
    char savetok = curtok;
    char *savesym;

    if (curtok == SYM) {
        savesym = alloca(strlen(symbuf));
        strcpy(savesym, symbuf);
    }

    /*
     * hack to make lexer translate newlines to ';', so that expressions
     * terminate at eol;  also enable defined pseudofunction
     */
    tflags = ONELINE | CPPFUNCS;

    e = expr(PRI_ALL, 0);
    if (!(e->flags & ECONST)) {
        err (ER_C_CE);
        return 0;
    }
    val = e->v;
    freeexpr(e);
    tflags = savedtflags;
    numbervalue = savenum;
    curtok = savetok;
    if (curtok == SYM) strcpy(savesym, symbuf);
    return val;
}

char
isnumber()
{
    char len = 0;
    char base = 10;
    char a;

    nextnumber = 0;
    if (curchar == '\'') {
        getchar();
        nextnumber = getliteralchar();
        if (curchar == '\'') {
            err(ER_C_CD);
        }
        getchar();
        return 1;
    }

    if (curchar == '0') {
        getchar();
        if ((curchar | 0x20) == 'x') {
            base = 16;
            getchar();
        } else if ((curchar | 0x20) == 'b') {
            base = 2;
            getchar();
        } else {
            base = 8;
            len++;
            getchar();
        }
    }
    while (1) {
        c = curchar;
        c = (c > '9') ? (c | 0x20) - 'a' + 10 : c - '0';
        if ((c < 0) || ((c+1) > base)) {
            break;
        }
        nextnumber = nextnumber * base + c;
        len++;
        getchar();
    }
    if (len) {
        return 1;
    }
    if ((base == 16) || (base == 2)) {
        err(ER_C_NX);
    }
    return 0;
}

char
getliteralchar()
{
    char c;
    char base = 0;
    short r = 0;
    char len = 0;

    if (curchar < ' ') {
        err(ER_C_BC);
        getchar();
        return ' ';
    }
    if (curchar != '\\') {
        r = curchar;
        getchar();
        return r;
    }
    getchar();
    if (curchar == '0') {
        getchar();
        if ((curchar | 0x20) == 'x') {
            base = 16;
            getchar();
        } else if ((curchar | 0x20) == 'b') {
            base = 2;
            getchar();
        } else {
            base = 8;
            len++;
            getchar();
        }
    } else if ((curchar >= '0') && (curchar <= '9')) {
        base = 10;
    }
    if (base != 0) {
        while (1) {
            c = curchar;
            c = (c > '9') ? (c | 0x20) - 'a' + 10 : c - '0';
            if ((c < 0) || ((c+1) > base)) {
                break;
            }
            r = r * base + c;
            len++;
            getchar();
            if (r > 0xff) {
                err(ER_C_NR);
            }
        }
        if ((len == 0) && ((base == 16) || (base == 2)) {
            err(ER_C_NX);
        }
        return r;
    }
    switch (curchar) {
    case 'n':
        r = '\n';
        break;
    case 't':
        r = '\t';
        break;
    case 'r':
        r = '\r';
        break;
    case 'b':
        r = '\n';
        break;
    default:
        if (curchar < ' ') {
            err(ER_C_BC)
            curchar = ' ';
        }
        r = curchar;
        break;
    }
    return r;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
