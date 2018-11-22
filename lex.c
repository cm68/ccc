/*
 * this is a brute force lexer that uses a tight keyword lookup in kw.c
 * we do cpp conditionals in here
 * it handles different keyword tables for cpp, c, and asm
 */
#include "ccc.h"
#include "expr.h"

#include <stdio.h>

token_t curtok;
token_t nexttok;

long curval;        /* numeric data */
long nextval;
char *curstr;       /* name or string data */
char *nextstr;

/*
 * cpp conditional
 */
struct cond {
    int flags;
#define C_TRUE      0x01
#define C_ELSESEEN  0x02
#define C_TRUESEEN  0x04
    struct cond *next;
};
struct cond *cond;

char tflags;
#define ONELINE     0x01
#define CPPFUNCS    0x02

/*
 * return true if the current token matches
 * also, consume the token.  
 * this is a code size optimization, since this path is common
 */
char
match(token_t t)
{
    if (curtok == t) {
        gettoken();
        return 1;
    }
    return 0;
}

/*
 * skip over any whitespace
 */
void
skipwhite()
{
    while ((curchar == ' ') || (curchar == '\n')) {
        getnext();
    }
}

/*
 * skip over any whitespace other than newline
 */
void
skipwhite1()
{
    while (curchar == ' ') {
        getnext();
    }
}

/*
 * skip to end of line
 */
void
skiptoeol()
{
    while (curchar && (curchar != '\n')) {
        getnext();
    }
}

/*
 * read an integer in a given base from the input stream
 * the base marker [bxd] has been consumed
 */
static
int
getint(char base)
{
    int i;
    char c;
    int len = 0;

    while (1) {
        c = curchar;
        c = (c > '9') ? (c | 0x20) - 'a' + 10 : c - '0';
        if ((c < 0) || ((c+1) > base)) {
            break;
        }
        i *= base;
        i += c;
        getnext();
        len++;
    }
    /* if no characters are consumed, note the error if base 2 or 16 */
    if ((len == 0) && ((base == 2) || (base == 16))) {
        err(ER_C_NX);
    }
    return i;
}

/*
 * do character literal processing, handling the C escape codes
 * extended with decimal and binary constants
 */
static char
getlit()
{
top:
    if (curchar != '\\') {
        if ((curchar < 0x20) || (curchar > 0x7e)) {
            err(ER_C_BC);
            curchar = ' ';
        }
        return curchar;
    }
    getnext();          // eat the backslash
    switch (curchar) {
    case '\n':          /* backslash at end of line */
        lineno++;
        goto top;
    case 'b':
        return '\b';
    case 'e':
        return '\x1b';
    case 'f':
        return '\f';
    case 'n':
        return '\n';
    case 'r':
        return '\r';
    case 't':
        return '\t';
    case 'v':
        return '\v';
    case '0': case '1': case '2': case '3':     // octal
    case '4': case '5': case '6': case '7':
        return (getint(8));
    case 'x': case 'X':                         // hex
        getnext();
        return (getint(16));
    case 'B':                                   // binary
        getnext();
        return (getint(2));
    case 'D':                                   // decimal
        getnext();
        return (getint(10));
    default:
        return curchar;                         // literal next
    }
}

/*
 * if we have a constant number, then return 1 and assign nextval
 */
char
isnumber()
{
    char base;

    if (curchar == '\'') {
        getnext();
        nextval = getlit();
        if (curchar == '\'') {
            err(ER_C_CD);
        }
        getnext();
        return 1;
    }

    if ((curchar < '0') || (curchar > '9')) {
        return 0;
    }

    base = 10;
    if (curchar == '0') {
        getnext();
        if ((curchar | 0x20) == 'x') {
            base = 16;
            getnext();
        } else if ((curchar | 0x20) == 'b') {
            base = 2;
            getnext();
        } else if ((curchar | 0x20) == 'd') {
            base = 10;
        } else {
            base = 8;
        }
    }
    nextval = getint(base);
    return 1;
}

char strbuf[128];

/*
 * does the next hunk of characters look like a C symbol or keyword
 * specifically, does it look like [A-Za-z_][A-Za-z0-9_]*
 * if it does, copy it to the string buffer
 *
 * there is a little hair here, since the cpp has the ## operator,
 * which does glomming:  _ ## xx ## yy  is identical to _xxyy
 */
char
issym()
{
    char *s;
    char c;

    s = strbuf;

    c = curchar | 0x20;
    if (!((c >= 'a') && (c <= 'z')) || (curchar == '_')) {
        return 0;
    }
    *s++ = curchar;
    *s = 0;
    getnext();
    while (1) {
        /* handle glommer operator */
        if (curchar == ' ') {
            while (curchar == ' ') {
                getnext();
            }
            if (curchar == '#' && nextchar == '#') {
                getnext();
                getnext();
                while (curchar == ' ') {
                    getnext();
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
            *s = 0;
            getnext();
        } else {
            break;
        }
    }
    return 1;
}

void
do_cpp(char t)
{
    char *s;
    char k;
    struct cond *c;
    int v;

    switch (t) {
    case IF:
        v = readcppconst();
        c = malloc(sizeof(*c));
        c->next = cond;
        cond = c;
        cond->flags = (v ? (C_TRUE|C_TRUESEEN) : 0);
        skiptoeol();
        return;
    case ENDIF:
        skiptoeol();
        if (!cond) {
            err(ER_C_CU);
            return;
        }
        c = cond;
        cond = c->next;
        free(c);
        return;
    case ELSE:
        skiptoeol();
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
    case ELIF:
        if (!cond) {
            skiptoeol();
            err(ER_C_CU);
            return;
        }
        v = readcppconst();
        if (cond->flags & C_ELSESEEN) {
            err(ER_C_ME);
            return;
        } 
        if (cond->flags & C_TRUESEEN) {
            cond->flags ^= C_TRUE;
        } else {
            cond->flags |= (v ? (C_TRUE | C_TRUESEEN) : 0);
        }
        skiptoeol();
        return;
    case DEFINE:
        skipwhite1();
        if (!issym()) {
            err(ER_C_MN);
            return;
        }
        macdefine(strbuf);
        return;
    case UNDEF:
        skipwhite1();
        if (!issym()) {
            err(ER_C_MN);
            return;
        }
        macundefine(strbuf);
        return;
    case INCLUDE:
        skipwhite1();
        if (curchar == '<') {
            k = '>';
        } else if (curchar == '\"') {
            k = '\"';
        } else {
            err(ER_C_ID);
        }
        getnext();
        s = strbuf;
        while ((curchar != '\n') && (curchar != ' ') && (curchar != k)) {
            *s++ = curchar;
            getnext();
        }
        if (curchar != k) {
            err(ER_C_ID);
        }
        skiptoeol();
        insertfile(strbuf, k == '>'); 
        return;
    }
}

/*
 * check if we have a literal string
 * this needs to handle both the stringify operator and the string concat:
 *   #FOO   -> "FOO":q
 *
 *   "foo" <whitespace> "bar"   -> "foobar"
 */
char 
isstring(char *s)
{
#ifdef notdef
    XXX - fixme
    /*
     * stringify #xxx into "<value of xxx>" if it's a cpp macro
     * this is quite gnarly - if there is a literal string inside, then
     * the quotes and backslashes in the string are escaped. as this
     * can get ugly recursive, and it does not interact well with the
     * method I use to do macros, this might not work.
     */
    if ((prevchar != '\n') && (curchar == '#')) {
        getnext();
        while ((curchar > ' ') && (curchar <= 0x7f)) {
            *s++ = curchar;
            getnext();
        }
        *s = 0;
        return 1;
    }
#endif
    if (curchar != '\"') {
        return 0;
    }
    getnext();
    while (curchar != '\"') {
        *s++ = getlit();
        getnext();
    }
    *s = 0;
    getnext();
    return 1;
}

/*
 * character to token translation for single char tokens
 * the enum for these is the actual character seen.  
 * no need to translate, just recognize.
 */
char simple[] = {
    BEGIN, END, LBRACK, RBRACK, LPAR, RPAR, SEMI, COMMA,
    ASSIGN, DOT, PLUS, MINUS, DIV, MOD, AND, OR, XOR,
    LT, GT, BANG, TWIDDLE, QUES, OTHER, STAR, 0
};

/*
 * list of tokens that can be doubled, and the resulting token
 */
char dbl_able[] = {
    PLUS, MINUS, OR, AND, ASSIGN, GT, LT, 0
};
char dbltok[] = {
    INC, DEC, LOR, LAND, EQ, RSHIFT, LSHIFT, 0
};

/*
 * list of tokens that can have '=' appended 
 * and then, what token that turns them into
 */ 
char eq_able[] = {
    PLUS, MINUS, STAR, DIV, MOD, AND, OR, XOR, 
    GT, LT, BANG, LOR, LAND, RSHIFT, LSHIFT, 0
};
char eqtok[] = {
    PLUSEQ, SUBEQ, MULTEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ, 
    GE, LE, NEQ, LOREQ, LANDEQ, RSHIFTEQ, LSHIFTEQ, 0
};

/*
 * string memory allocation
 */
char *
stralloc(char *s)
{
    return strdup(s);
}

void
strfree(char *s)
{
    free(s);
}

/*
 * we want a stream of lexemes to be placed into 
 * curtok and nexttok respectively.  
 * we need 1 token of lookahead to do a recursive descent parse of C
 *
 * all the comment and preprocessor stuff is invisible above here
 * as is string, character escaping, and number bases
 */
void
gettoken()
{
    char *s;
    char t;
    int incomment = 0;

    /* advance */
    if (curstr) {
        strfree(curstr);
    }
    curtok = nexttok;
    curval = nextval;
    curstr = nextstr;
    nextstr = 0;

    while (1) {
        if (curchar == 0) {
            nexttok = E_O_F;
            return;
        }
        if (curchar == '#' && prevchar == '\n') {   // cpp directive
            getnext();
            skipwhite1();
            if (issym()) {
                t = kwlook(strbuf, cppkw);
                if (t) {
                    do_cpp(t);
                    continue;
                }
                err(ER_C_BD);
            }
            if (isnumber()) {
                lineno = nextval;
                skiptoeol();
                continue;
            }
        }
        if (curchar == '\n' && (tflags & ONELINE)) {
            nexttok = ';';
            getnext();
            return;
        }
        if (prevchar == '\n' && cond && !(cond->flags & C_TRUE)) {
            skiptoeol();
            continue;
        }
        if ((curchar == '/') && (nextchar == '*') && !incomment) {
            incomment = 1;
            getnext();
            getnext();
            continue;
        }
        if ((curchar == '/') && (nextchar == '/')) {
            skiptoeol();
            continue;
        }
        if ((incomment) && (curchar == '*') && (nextchar == '/')) {
            incomment = 0;
            getnext();
            getnext();
            continue;
        }
        if (incomment) {
            getnext();
            continue;
        }
        if ((curchar == ' ') || (curchar == '\n')) {
            getnext();
            continue;
        }
        if (issym()) {
            if (macexpand(strbuf)) {
                continue;
            }
            t = kwlook(strbuf, ckw);
            if (t) {
                nexttok = t;
                return;
            }
            nexttok = SYM;
            nextstr = stralloc(strbuf);
            return;
        }
        if (isnumber()) {
            nexttok = NUMBER;
            return;
        }
        if (isstring(strbuf)) {
            nexttok = STRING;
            nextstr = stralloc(strbuf);
            return;
        }
        t = lookupc(simple, curchar);
        if (t == -1) {
            err(ER_C_UT);
            curchar= ';';
        }
        nexttok = curchar;
        getnext();
        if (curchar == nexttok) {
            t = lookupc(dbl_able, curchar);
            if (t != -1) {
                nexttok = dbltok[t];
                getnext();
            }
        }
        if (curchar == '=') {
            t = lookupc(eq_able, nextchar);
            if (t != -1) {
                nexttok = eqtok[t];
                getnext();
            }
        }
        if ((nexttok == '-') && (curchar == '>')) {
            nexttok = DEREF;
            getnext();
        }
        return;
    }
}

char
cpppseudofunc()
{
    int r = 0;

    if ((strcmp("defined", strbuf) == 0) && (tflags & CPPFUNCS)) {
        while ((curchar == '\t') || (curchar == ' ')) getnext();
        if (curchar != '(') {
            err(ER_C_DP);
            curchar = '0';
            return 1;
        }
        getnext();
        if (issym()) {
            if (maclookup(strbuf)) r = 1;
        }
        while ((curchar == '\t') || (curchar == ' ')) getnext();
        if (curchar != ')') {
            err(ER_C_DP);
            r = 0;
        }
        curchar = r ? '1' : '0';
        return 1;
    }
    return 0;
}

/*
 * this code straddles the cpp, the lexer and the expression parser
 * so much happens via global variable side effects, so recursive
 * calls could happen that need repair.
 * ex:  expr->gettoken->do_cpp->readcppconst->expr->gettoken->getnext
 * if we hit an #if in the middle of an expression
 */
int
readcppconst()
{
    long val;
    struct expr *e;
    char savedtflags = tflags;
    long savenum = curval;
    char savetok = curtok;
    char *savesym;

    if (curtok == SYM) {
        savesym = alloca(strlen(strbuf));
        strcpy(savesym, strbuf);
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
    curval = savenum;
    curtok = savetok;
    if (curtok == SYM) strcpy(savesym, strbuf);
    return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
