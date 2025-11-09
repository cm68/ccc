/*
 * this is a brute force lexer that uses a tight keyword lookup in kw.c
 * we do cpp conditionals in here
 * it handles different keyword tables for cpp, c, and asm
 *
 * XXX - maybe use character type bitmask
 */
#include "cc1.h"

int write_cpp_file = 0;
char *cpp_file_name;
int cpp_file;

struct token cur, next;

/*
 * this is the place we build filenames, symbols and literal strings
 * no overflow checking or anything, we ain't got time for that shit
 */
char strbuf[STRBUFSIZE];

int readcppconst();
char cpppseudofunc();

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

unsigned char tflags;
#define ONELINE     0x01
#define CPPFUNCS    0x02

void
lexinit()
{
	cur.type = next.type = NONE;
}

/*
 * return true if the current token matches
 * also, consume the token.  
 * this is a code size optimization, since this path is common
 */
char
match(token_t t)
{
    if (cur.type == t) {
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
getint(unsigned char base)
{
    int i = 0;
    unsigned char c;
    int len = 0;

    while (1) {
        c = curchar;
        if (c < '0') break;
        if (c > '9') {
            c |= 0x20;
            if (c >= 'a' && c <= 'f') {
                c = 10 + c - 'a';
            } else {
                break;
            }
        } else {
            c -= '0';
        }
        if ((c+1) > base) {
            break;
        }
        i *= base;
        i += c;
        advance();
        len++;
    }
    /* if no characters are consumed, note the error if base 2 or 16 */
    if ((len == 0) && ((base == 2) || (base == 16))) {
        lose(ER_C_NX);
    }
    return i;
}

/*
 * do character literal processing, handling the C escape codes
 * extended with decimal and binary constants
 * this function consumes the input, returning the character value
 */
static unsigned char
getlit()
{
    unsigned char c;
top:
    if (curchar != '\\') {
        if ((curchar < 0x20) || (curchar > 0x7e)) {
            lose(ER_C_BC);
            curchar = ' ';
        }
        c = curchar;
    } else {
        advance();          // eat the backslash
        switch (curchar) {
        case '\n':          /* backslash at end of line */
            lineno++;
            goto top;
        case 'b':
            c = '\b';
            break;
        case 'e':
            c = '\x1b';
            break;
        case 'f':
            c = '\f';
            break;
        case 'n':
            c = '\n';
            break;
        case 'r':
            c = '\r';
            break;
        case 't':
            c = '\t';
            break;
        case 'v':
            c = '\v';
            break;
        case '0': case '1': case '2': case '3':     // octal
        case '4': case '5': case '6': case '7':
            return (getint(8));
        case 'x': case 'X':                         // hex
            advance();
            return (getint(16));
        /* extension */
        case 'B':                                   // binary
            advance();
            return (getint(2));
        case 'D':                                   // decimal
            advance();
            return (getint(10));
        default:
            c = curchar;
            break;                                   // literal next
        }
    }
    advance();
    return c;
}

/*
 * if we have a constant number, then return 1 and assign nextval
 */
char
isnumber()
{
    unsigned char base;

    if (charmatch('\'')) {
        next.v.numeric = getlit();
        if (curchar != '\'') {
            lose(ER_C_CD);
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
    next.v.numeric = getint(base);
    return 1;
}

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
#ifdef DEBUG
    if (VERBOSE(V_SYM)) {
        printf("issym = %s curchar = %c nextchar = %c\n",
            strbuf, curchar, nextchar);
    }
#endif
    return 1;
}

void
do_cpp(unsigned char t)
{
    char *s;
    unsigned char k;
    struct cond *c;
    int v;

    switch (t) {
    case IF:
        v = readcppconst();
        c = malloc(sizeof(*c));
        c->next = cond;
        cond = c;
        cond->flags = (v ? (C_TRUE|C_TRUESEEN) : 0);
        // Don't call skiptoeol() - readcppconst() already consumed the line
        return;
    case IFDEF:
        skipwhite1();
        if (!issym()) {
            lose(ER_C_MN);
            skiptoeol();
            return;
        }
        advance();
        v = (maclookup(strbuf) != 0);  // true if macro is defined
        c = malloc(sizeof(*c));
        c->next = cond;
        cond = c;
        cond->flags = (v ? (C_TRUE|C_TRUESEEN) : 0);
        skiptoeol();
        return;
    case IFNDEF:
        skipwhite1();
        if (!issym()) {
            lose(ER_C_MN);
            skiptoeol();
            return;
        }
        advance();
        v = (maclookup(strbuf) == 0);  // true if macro is NOT defined
        c = malloc(sizeof(*c));
        c->next = cond;
        cond = c;
        cond->flags = (v ? (C_TRUE|C_TRUESEEN) : 0);
        skiptoeol();
        return;
    case ENDIF:
        skiptoeol();
        if (!cond) {
            lose(ER_C_CU);
            return;
        }
        c = cond;
        cond = c->next;
        free(c);
        return;
    case ELSE:
        skiptoeol();
        if (!cond) {
            lose(ER_C_CU);
            return;
        }
        if (cond->flags & C_ELSESEEN) {
            lose(ER_C_ME);
            return;
        }
        cond->flags ^= (C_TRUE | C_ELSESEEN);
        if (cond->flags & C_TRUE) {
            cond->flags |= C_TRUESEEN;
        }
        return;
    case ELIF:
        if (!cond) {
            skiptoeol();
            lose(ER_C_CU);
            return;
        }
        v = readcppconst();
        if (cond->flags & C_ELSESEEN) {
            lose(ER_C_ME);
            return;
        } 
        if (cond->flags & C_TRUESEEN) {
            cond->flags ^= C_TRUE;
        } else {
            cond->flags |= (v ? (C_TRUE | C_TRUESEEN) : 0);
        }
        // Don't call skiptoeol() - readcppconst() already consumed the line
        return;
    case DEFINE:
        skipwhite1();
        if (!issym()) {
            lose(ER_C_MN);
            return;
        }
        advance();
        macdefine(strbuf);
        return;
    case UNDEF:
        skipwhite1();
        if (!issym()) {
            lose(ER_C_MN);
            return;
        }
        advance();
        macundefine(strbuf);
        return;
    case INCLUDE:
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            printf("Processing INCLUDE directive\n");
        }
#endif
        skipwhite1();
        if (curchar == '<') {
            k = '>';
        } else if (curchar == '\"') {
            k = '\"';
        } else {
            lose(ER_C_ID);
            return;
        }
        advance();
        s = strbuf;
        while ((curchar != '\n') && (curchar != ' ') && (curchar != k)) {
            *s++ = curchar;
            advance();
        }
        *s = 0;
        if (curchar != k) {
            lose(ER_C_ID);
        }
        skiptoeol();
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            printf("After skiptoeol: curchar='%c'(0x%x) nextchar='%c'(0x%x)\n",
                   curchar >= 32 ? curchar : '?', curchar,
                   nextchar >= 32 ? nextchar : '?', nextchar);
            printf("About to insertfile: '%s' sys=%d\n", strbuf, k == '>');
        }
#endif
        insertfile(strbuf, k == '>');
        return;
    }
}

/*
 * check if we have a literal string - hair here, since embedded nulls
 * return the value into strbuf, and maintain the count
 */
char 
isstring()
{
	char *s = strbuf;

    if (!charmatch('\"')) {
        return 0;
    }
	*s++ = 0;
    while (!charmatch('\"')) {
    	strbuf[0]++;
        *s++ = getlit();
    }
    *s = 0;
#ifdef DEBUG
    if (VERBOSE(V_STR)) {
        printf("isstring: %s(%d)\n", &strbuf[1], strbuf[0]);
    }
#endif
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
    LT, GT, BANG, TWIDDLE, QUES, COLON, STAR, 0
};

/*
 * list of tokens that can be doubled, and the resulting token
 */
char dbl_able[] = {
    PLUS, MINUS, OR, AND, ASSIGN, GT, LT, 0
};
char dbltok[] = {
    INCR, DECR, LOR, LAND, EQ, RSHIFT, LSHIFT, 0
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
 * when we bump over our current token, we may need to free any
 * memory we allocated
 */
void
freetoken()
{
    if (cur.type == SYM && cur.v.name) {
    	free(cur.v.name);
    	cur.v.name = NULL;  /* Prevent double-free */
    }
    /* Do not free STRING tokens - they are referenced in expressions and
     * global variable initializers throughout compilation */
}

char nbuf[1024];  // Large enough for escaped string: 1 + 255*4 + 1 = 1022 bytes

/*
 * we want a stream of lexemes to be placed into 
 * cur and next respectively.
 * we need 1 token of lookahead to do a recursive descent parse of C
 *
 * all the comment and preprocessor stuff is invisible above here
 * as is string, character escaping, and number bases
 *
 */
void
gettoken()
{
    token_t t;
    unsigned char incomment = 0;
    unsigned char lineend;
    unsigned char c;
    int i;
    char *s;

    freetoken();

    memcpy(&cur, &next, sizeof(cur));

    next.v.str = 0;
    next.type = NONE;

    lineend = 0;

    while (1) {
        if (curchar == 0) {
            next.type = E_O_F;
            break;
        }
        if (charmatch('#')) {
#ifdef DEBUG
            if (VERBOSE(V_CPP)) {
                printf("Found # at column=%d (will%s process)\n", column, (column == 1) ? "" : " NOT");
            }
#endif
            if (column != 1) {
                /* Not a CPP directive, treat as token */
                next.type = '#';
                break;
            }
            /* CPP directive at column 0 */
            skipwhite1();
            if (issym()) {
                t = kwlook(strbuf, cppkw);
#ifdef DEBUG
                if (VERBOSE(V_CPP)) {
                    printf("CPP keyword: '%s' -> %d\n", strbuf, t);
                }
#endif
                if (t) {
                    advance();
                    do_cpp(t);
                    continue;
                }
                lose(ER_C_BD);
            }
            if (isnumber()) {
                lineno = next.v.numeric;
                skiptoeol();
                continue;
            }
        }
        if (curchar == '\n') {
            lineend = 1;
        }
        if ((tflags & ONELINE) && (curchar == '\n')) {
            next.type = ';';
            break;
        }
        if (cond && !(cond->flags & C_TRUE) && curchar != '#') {
            skiptoeol();
            if (curchar == '\n') {
                advance();  // consume the newline
            }
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
        if ((curchar == ' ') || (curchar == '\t') || (curchar == '\n')) {
            advance();
            // After advancing past whitespace, check if we should skip rest of line
            if (cond && curchar != '#' && curchar != 0) {
                if (!(cond->flags & C_TRUE)) {
                    skiptoeol();
                    if (curchar == '\n') {
                        advance();  // consume the newline
                    }
                }
            }
            continue;
        }
        if (issym()) {
            /* Check for defined() pseudofunction in #if expressions */
            if (cpppseudofunc()) {
                continue;  /* cpppseudofunc() replaced the function with '0' or '1' */
            }
            if (macexpand(strbuf)) {
                continue;
            }
            advance();
            t = kwlook(strbuf, ckw);
            if (t) {
                next.type = t;
                next.v.name = 0;  // keywords don't have names
                break;
            }
            next.type = SYM;
            next.v.name = strdup(strbuf);
            break;
        }
        if (isnumber()) {
            next.type = NUMBER;
            break;
        }
        if (isstring()) {
            next.type = STRING;
            next.v.str = malloc(strbuf[0] + 1);  // +1 for length byte
            memcpy(next.v.str, strbuf, strbuf[0] + 1);
            break;
        }

        /* from here, it had better be an operator */
        t = lookupc(simple, curchar);
        if (t == 0xff) {
            lose(ER_C_UT);
            curchar= ';';
        }

        next.type = simple[t];
        c = curchar;	// save what we saw
        advance();

        /* see if the character is doubled.  this can be an operator */
        if (curchar == c) {
            t = lookupc(dbl_able, c);
            if (t != 0xff) {
                c = next.type = dbltok[t];
                advance();
            }
        }

        /* see if the character has an '=' appended.  this can be an operator */
        if (curchar == '=') {
            t = lookupc(eq_able, c);
            if (t != 0xff) {
                next.type = eqtok[t];
                advance();
            }
        }
        if ((c == '-') && (curchar == '>')) {
            next.type = ARROW;
            advance();
        }
        break;
    }

    /*
     * detokenize for cpp output
     */
    if (write_cpp_file) {
        switch (cur.type) {
        case SYM:
            cpp_out(cur.v.name, strlen(cur.v.name));
            cpp_out(" ", 1);
            break;
        case STRING:
            i = quoted_string(nbuf, cur.v.str);
            cpp_out(nbuf, i);
            cpp_out(" ", 1);
            break;
        case NUMBER:
            i = longout(nbuf, cur.v.numeric);
            cpp_out(nbuf, i);
            cpp_out(" ", 1);
            break;
        case NONE:
            break;
        default:
            if (detoken[cur.type]) {
                s = detoken[cur.type];
            } else {
                s = tokenname[cur.type];
            }
            cpp_out(s, strlen(s));
            cpp_out(" ", 1);
            break;
        }
        if (lineend) {
            cpp_out("\n", 1);
        }
    }
#ifdef DEBUG
    if (VERBOSE(V_TOKEN)) {
        printf("cur.type = 0x%02x \'%c\'\n", cur.type, cur.type > ' ' ? cur.type : ' ');
    }
#endif
    return;
}

/*
 * it's not k&r, but #if defined(foo) should return 1 or 0.
 */
char
cpppseudofunc()
{
    int r = 0;

    if ((strcmp("defined", strbuf) == 0) && (tflags & CPPFUNCS)) {
        /* Skip past the "defined" identifier */
        advance();
        while ((curchar == '\t') || (curchar == ' ')) advance();
        if (curchar != '(') {
            lose(ER_C_DP);
            curchar = '0';
            return 1;
        }
        advance();
        while ((curchar == '\t') || (curchar == ' ')) advance();
        if (issym()) {
            /* Check if the macro name in strbuf is defined */
            r = (maclookup(strbuf) != 0);
            advance();  /* Move past the macro name */
        }
        while ((curchar == '\t') || (curchar == ' ')) advance();
        if (curchar != ')') {
            lose(ER_C_DP);
            r = 0;
        } else {
            advance();  /* Move past the ')' */
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
    char savedtflags = tflags;
    int saved_write_cpp = write_cpp_file;
    struct token savecur, savenext;

    /* Save both cur and next tokens */
    memcpy(&savecur, &cur, sizeof(cur));
    memcpy(&savenext, &cur, sizeof(next));

    /* Reset next so gettoken() doesn't copy stale NONE value to cur */
    next.type = E_O_F;
    next.v.str = 0;

    /*
     * hack to make lexer translate newlines to ';', so that expressions
     * terminate at eol;  also enable defined pseudofunction
     */
    tflags = ONELINE | CPPFUNCS;

    /* Disable cpp output while evaluating the #if expression */
    write_cpp_file = 0;

    /* Skip whitespace before reading the first token */
    skipwhite1();

    /* Get the first token of the expression
     * Call gettoken() twice: first call reads into next, second moves to cur */
    gettoken();
    gettoken();

    val = parse_const(SEMI);
    tflags = savedtflags;
    write_cpp_file = saved_write_cpp;

    /* Restore both tokens */
    memcpy(&cur, &savecur, sizeof(cur));
    memcpy(&next, &savenext, sizeof(next));
    return val;
}

void
tdump(unsigned char c)
{
	printf("%s ", tokenname[c]);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
