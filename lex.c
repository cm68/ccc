/*
 * this is a brute force lexer that uses a tight keyword lookup in kw.c
 * we do cpp conditionals in here
 * it handles different keyword tables for cpp, c, and asm
 *
 * XXX - maybe use character type bitmask
 */
#include "ccc.h"

#include <stdio.h>

token_t curtok;
token_t nexttok;

long curval;        /* numeric data */
long nextval;
char *curstr;       /* name or string data */
char *nextstr;

int readcppconst();

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
 * this happens a fair amount too, and we want to save code
 * if curchar is this, then true and eat the character
 */
int
charmatch(int c)
{
    if (curchar == c) {
        advance();
        return 1;
    } else {
        return 0;
    }
}

/*
 * skip over any whitespace
 */
void
skipwhite()
{
    while ((curchar == ' ') || (curchar == '\n')) {
        advance();
    }
}

/*
 * skip over any whitespace other than newline
 */
void
skipwhite1()
{
    while (curchar == ' ') {
        advance();
    }
}

/*
 * skip to end of line
 */
void
skiptoeol()
{
    while (curchar && (curchar != '\n')) {
        advance();
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
    int i = 0;
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
        advance();
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
    advance();          // eat the backslash
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
        advance();
        return (getint(16));
    case 'B':                                   // binary
        advance();
        return (getint(2));
    case 'D':                                   // decimal
        advance();
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

    if (charmatch('\'')) {
        nextval = getlit();
        if (curchar == '\'') {
            err(ER_C_CD);
        }
        advance();
        return 1;
    }

    if ((curchar < '0') || (curchar > '9')) {
        return 0;
    }

    base = 10;
    if (charmatch('0')) {
        if ((curchar | 0x20) == 'x') {
            base = 16;
            advance();
        } else if ((curchar | 0x20) == 'b') {
            base = 2;
            advance();
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
 * important: don't advance. leave curchar being the last character of sym
 * and nextchar being the character that detected the end of the name
 */
char
issym()
{
    char *s = strbuf;

    /* if not a symbol starter */
    if (!(((curchar >= 'a') && (curchar <= 'z')) || 
          ((curchar >= 'A') && (curchar <= 'Z')) ||
          (curchar == '_'))) {
        return 0;
    }

    while (1) {
        *s++ = curchar;
        *s = 0;

        if (!((nextchar >= 'A' && nextchar <= 'Z') ||
            (nextchar >= 'a' && nextchar <= 'z') ||
            (nextchar >= '0' && nextchar <= '9') ||
            (nextchar == '_'))) {
            break;
        }
        advance();
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
        advance();
        macdefine(strbuf);
        return;
    case UNDEF:
        skipwhite1();
        if (!issym()) {
            err(ER_C_MN);
            return;
        }
        advance();
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
        advance();
        s = strbuf;
        while ((curchar != '\n') && (curchar != ' ') && (curchar != k)) {
            *s++ = curchar;
            advance();
        }
        *s = 0;
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
 */
char 
isstring(char *s)
{
    if (!charmatch('\"')) {
        return 0;
    }
    while (!charmatch('\"')) {
        *s++ = getlit();
        advance();
    }
    *s = 0;
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

void
outcpp()
{
    char nbuf[20];
    if (curtok == SYM) {
        cpp_out(curstr);
    } else if (curtok == NUMBER) {
        sprintf(nbuf, "%d", curval);
        cpp_out(nbuf);
    } else if (curtok == STRING) {
        sprintf(nbuf, "\"%s\"", curstr);
        cpp_out(nbuf);
    } else {
        if (detoken[curtok]) {
            cpp_out(detoken[curtok]);
        } else {
            cpp_out(tokenname[curtok]);
        }
    }
}

#ifdef notdef
char *
detail()
{
    switch (curtok) {
    case SYM:
        return curstr;
    case STRING:
        return curstr;
    default:
        return ""; 
    }
}
#endif

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

#ifdef notdef
    printf("gettoken: 0x%02x %d %c %s %s\n", curtok, curtok, curtok, detoken[curtok], detail());
#endif

top:
    while (1) {
        if (curchar == 0) {
            nexttok = E_O_F;
            return;
        }
        if (column == 0 && charmatch('#')) {   // cpp directive
            skipwhite1();
            if (issym()) {
                t = kwlook(strbuf, cppkw);
                if (t) {
                    advance();
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
        if (curchar == '\n') {
            cpp_out("\n");
        }
        if ((tflags & ONELINE) && charmatch('\n')) {
            nexttok = ';';
            return;
        }
        if ((column == 0) && cond && !(cond->flags & C_TRUE)) {
            skiptoeol();
            continue;
        }
        if ((curchar == '/') && (nextchar == '*') && !incomment) {
            incomment = 1;
            advance();
            advance();
            continue;
        }
        if ((curchar == '/') && (nextchar == '/')) {
            skiptoeol();
            continue;
        }
        if ((incomment) && (curchar == '*') && (nextchar == '/')) {
            incomment = 0;
            advance();
            advance();
            continue;
        }
        if (incomment) {
            advance();
            continue;
        }
        if ((curchar == ' ') || (curchar == '\n')) {
            advance();
            continue;
        }
        if (issym()) {
            if (macexpand(strbuf)) {
                goto top;
            }
            advance();
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
        /* see if it is an operator character */
        t = lookupc(simple, curchar);
        if (t == -1) {
            err(ER_C_UT);
            curchar= ';';
        }
        nexttok = curchar;
        advance();
        /* see if the character is doubled.  this can be an operator */
        if (curchar == nexttok) {
            t = lookupc(dbl_able, curchar);
            if (t != -1) {
                nexttok = dbltok[t];
                advance();
            }
        }
        /* see if the character has an '=' appended.  this can be an operator */
        if (curchar == '=') {
            t = lookupc(eq_able, nextchar);
            if (t != -1) {
                nexttok = eqtok[t];
                advance();
            }
        }
        if ((nexttok == '-') && (curchar == '>')) {
            nexttok = DEREF;
            advance();
        }
        return;
    }
}

/*
 * it's not k&r, but #if defined(foo) should return 1 or 0.
 */
char
cpppseudofunc()
{
    int r = 0;

    if ((strcmp("defined", strbuf) == 0) && (tflags & CPPFUNCS)) {
        while ((curchar == '\t') || (curchar == ' ')) advance();
        if (curchar != '(') {
            err(ER_C_DP);
            curchar = '0';
            return 1;
        }
        advance();
        if (issym()) {
            if (maclookup(strbuf)) r = 1;
        }
        while ((curchar == '\t') || (curchar == ' ')) advance();
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
 * ex:  expr->gettoken->do_cpp->readcppconst->expr->gettoken->advance
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
