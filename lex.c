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
static unsigned char incomment = 0;
/* Track comment state across gettoken() calls */

struct token cur, next;

/* Token history for debugging */
#define TOKEN_HISTORY_SIZE 10
struct token token_history[TOKEN_HISTORY_SIZE];
int token_history_index = 0;

/*
 * this is the place we build filenames, symbols and literal strings
 * no overflow checking or anything, we ain't got time for that shit
 */
char strbuf[STRBUFSIZE];

unsigned long readcppconst();
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

/* ASM block capture */
char *asm_capture_buf = NULL;
int asm_capture_size = 0;
int asm_capture_len = 0;

void
asm_out(char *s, int len)
{
    if (!s || !asm_capture_buf)
        return;

    /* Skip braces - they're for parsing only, not part of asm text */
    if (len == 2 && s[0] == '{' && s[1] == ' ')
        return;
    if (len == 2 && s[0] == '}' && s[1] == ' ')
        return;

    /* Grow buffer if needed */
    while (asm_capture_len + len >= asm_capture_size) {
        asm_capture_size *= 2;
        asm_capture_buf = realloc(asm_capture_buf, asm_capture_size);
    }

    memcpy(&asm_capture_buf[asm_capture_len], s, len);
    asm_capture_len += len;
    asm_capture_buf[asm_capture_len] = 0;  /* Null terminate */
}

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
        gripe(ER_C_NX);
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
            gripe(ER_C_BC);
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
            gripe(ER_C_CD);
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

    /* Skip optional 'L' or 'l' suffix for long constants */
    if ((curchar == 'L') || (curchar == 'l')) {
        advance();
    }

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
        fdprintf(2,"issym = %s curchar = %c nextchar = %c\n",
            strbuf, curchar, nextchar);
    }
#endif
    return 1;
}

/*
 * Output to both cpp file and asm buffer
 */
void
cpp_asm_out(char *s, int len)
{
    if (write_cpp_file) {
        cpp_out(s, len);
    }
    if (asm_capture_buf) {
        asm_out(s, len);
    }
}

/*
 * Helper: output string followed by space
 * Reduces code duplication in token output
 */
void
cpp_asm_out_with_space(char *s, int len)
{
    cpp_asm_out(s, len);
    cpp_asm_out(" ", 1);
}

/*
 * Output a token to cpp file and/or asm buffer
 */
void
output_token(struct token *tok)
{
    /*
     * Small buffer for chunked string output
     * (16 chars * 5 bytes max expansion)
     */
    char nbuf[128];
    int i;
    char *s;

    if (!write_cpp_file && !asm_capture_buf) {
        return;
    }

    switch (tok->type) {
    case SYM:
        cpp_asm_out_with_space(tok->v.name, strlen(tok->v.name));
        break;
    case STRING:
        if (!tok->v.str) {
            break;
        }
        /* Output string in chunks to avoid large stack buffer */
        {
            char *src = tok->v.str;
            int len = *src++;  /* First byte is length */
            int chunk_size = 16;  /* Process this many source chars at a time */
            int pos = 0;

            /* Opening quote */
            cpp_asm_out("\"", 1);

            /* Output string content in chunks */
            while (len > 0) {
                int todo = (len > chunk_size) ? chunk_size : len;
                for (i = 0; i < todo; i++) {
                    int n = controlify(nbuf, *src++);
                    cpp_asm_out(nbuf, n);
                }
                len -= todo;
            }

            /* Closing quote and space */
            cpp_asm_out("\" ", 2);
        }
        break;
    case NUMBER:
        i = longout(nbuf, tok->v.numeric);
        nbuf[i++] = ' ';
        cpp_asm_out(nbuf, i);
        break;
    case NONE:
        break;
    default:
        if (detoken[tok->type]) {
            s = detoken[tok->type];
        } else {
            s = tokenname[tok->type];
        }
#ifdef DEBUG
        if (VERBOSE(V_CPP) && tok->type == INT) {
            fdprintf(2,"output_token(INT): s='%s' write_cpp_file=%d\n",
                s, write_cpp_file);
        }
#endif
        cpp_asm_out_with_space(s, strlen(s));
        break;
    }
}

void
do_cpp(unsigned char t)
{
    char *s;
    unsigned char k;
    struct cond *c;
    unsigned long v;


    switch (t) {
    case IF:
        v = readcppconst();
        c = malloc(sizeof(*c));
        c->next = cond;
        cond = c;
        cond->flags = (v ? (C_TRUE|C_TRUESEEN) : 0);
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"#if %d: cond->flags = 0x%02x (C_TRUE=%d)\n",
                v, cond->flags, !!(cond->flags & C_TRUE));
        }
#endif
        /*
         * Don't call skiptoeol() - readcppconst() in ONELINE mode
         * already advanced past the line
         */
        return;
    case IFDEF:
        skipwhite1();
        if (!issym()) {
            gripe(ER_C_MN);
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
            gripe(ER_C_MN);
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
        if (!(tflags & ONELINE)) {
            skiptoeol();
        }
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"#endif: cond=%p", cond);
            if (cond) {
                fdprintf(2," flags=0x%02x (C_TRUE=%d)",
                    cond->flags, !!(cond->flags & C_TRUE));
            }
            fdprintf(2,"\n");
        }
#endif
        if (!cond) {
            gripe(ER_C_CU);
            return;
        }
        c = cond;
        cond = c->next;
        free(c);
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"After pop: cond=%p", cond);
            if (cond) {
                fdprintf(2," flags=0x%02x (C_TRUE=%d)",
                    cond->flags, !!(cond->flags & C_TRUE));
            }
            fdprintf(2,"\n");
        }
#endif
        return;
    case ELSE:
        if (!(tflags & ONELINE)) {
            skiptoeol();
        }
        if (!cond) {
            gripe(ER_C_CU);
            return;
        }
        if (cond->flags & C_ELSESEEN) {
            gripe(ER_C_ME);
            return;
        }
        cond->flags |= C_ELSESEEN;  // Mark that we've seen #else
        if (cond->flags & C_TRUESEEN) {
            // Already had a true condition, #else block should be false
            cond->flags &= ~C_TRUE;
        } else {
            // Haven't had a true condition yet, #else block should be true
            cond->flags |= (C_TRUE | C_TRUESEEN);
        }
        return;
    case ELIF:
        if (!cond) {
            skiptoeol();
            gripe(ER_C_CU);
            return;
        }
        v = readcppconst();
        if (cond->flags & C_ELSESEEN) {
            gripe(ER_C_ME);
            return;
        }
        if (cond->flags & C_TRUESEEN) {
            cond->flags ^= C_TRUE;
        } else {
            cond->flags |= (v ? (C_TRUE | C_TRUESEEN) : 0);
        }
        /*
         * Don't call skiptoeol() - readcppconst() in ONELINE mode
         * already advanced past the line
         */
        return;
    case DEFINE:
        skipwhite1();
        if (!issym()) {
            gripe(ER_C_MN);
            return;
        }
        advance();
        macdefine(strbuf);
        return;
    case UNDEF:
        skipwhite1();
        if (!issym()) {
            gripe(ER_C_MN);
            return;
        }
        advance();
        macundefine(strbuf);
        return;
    case INCLUDE:
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"Processing INCLUDE directive\n");
        }
#endif
        skipwhite1();
        if (curchar == '<') {
            k = '>';
        } else if (curchar == '\"') {
            k = '\"';
        } else {
            gripe(ER_C_ID);
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
            gripe(ER_C_ID);
        }
        skiptoeol();
#ifdef DEBUG
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"After skiptoeol: curchar='%c'(0x%x) "
                "nextchar='%c'(0x%x)\n",
                curchar >= 32 ? curchar : '?', curchar,
                nextchar >= 32 ? nextchar : '?', nextchar);
            fdprintf(2,"About to insertfile: '%s' sys=%d\n", strbuf, k == '>');
        }
#endif
        insertfile(strbuf, k == '>');
        /* Initialize curchar/nextchar from the new file */
        advance();
        advance();
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
        fdprintf(2,"isstring: %s(%d)\n", &strbuf[1], strbuf[0]);
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
/* Track when newline encountered for asm capture semicolon insertion */
unsigned char lineend = 0;

void
gettoken()
{
    token_t t;
    unsigned char c;
    int i;
    char *s;

    freetoken();

    memcpy(&cur, &next, sizeof(cur));

    /* Save current token to history for debugging */
    memcpy(&token_history[token_history_index], &cur, sizeof(cur));
    token_history_index = (token_history_index + 1) % TOKEN_HISTORY_SIZE;

    next.v.str = 0;
    next.type = NONE;

    while (1) {
        if (curchar == 0) {
            next.type = E_O_F;
#ifdef DEBUG
            if (VERBOSE(V_CPP)) {
                fdprintf(2,"Reached EOF: cond=%p\n", cond);
                if (cond) {
                    fdprintf(2,"  cond->flags=0x%02x (C_TRUE=%d)\n",
                        cond->flags, !!(cond->flags & C_TRUE));
                }
            }
#endif
            break;
        }
        /* Handle comments before checking for preprocessor directives */
        if (curchar == '/') {
            if (nextchar == '*') {
                /*
                 * Always enter comment mode, even if already in one
                 * (nested comments become single comment)
                 */
#ifdef DEBUG
                if (VERBOSE(V_TOKEN)) {
                    fdprintf(2,"Found comment start at line %d\n", lineno);
                }
#endif
                incomment = 1;
                advance();
                advance();
                continue;
            }
#ifdef DEBUG
            if (VERBOSE(V_TOKEN) && write_cpp_file) {
                fdprintf(2,"Slash but nextchar='%c'(0x%x) "
                    "not asterisk at line %d, incomment=%d\n",
                    nextchar >= 32 ? nextchar : '?', nextchar,
                    lineno, incomment);
            }
#endif
        }
        if ((curchar == '/') && (nextchar == '/')) {
            skiptoeol();
            continue;
        }
        if ((incomment) && (curchar == '*') && (nextchar == '/')) {
#ifdef DEBUG
            if (VERBOSE(V_TOKEN)) {
                fdprintf(2,
                    "Found comment end at line %d, advancing past */ \n",
                    lineno);
            }
#endif
            incomment = 0;
            advance();
            advance();
            continue;
        }
        if (incomment) {
            advance();
            continue;
        }
        /* Now safe to check for # - we know we're not in a comment */
        if (charmatch('#')) {
#ifdef DEBUG
            if (VERBOSE(V_CPP)) {
                fdprintf(2,"Found # at column=%d (will%s process) cond=%p",
                    column, (column == 1) ? "" : " NOT", cond);
                if (cond) {
                    fdprintf(2," C_TRUE=%d", !!(cond->flags & C_TRUE));
                }
                fdprintf(2,"\n");
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
                    fdprintf(2,"CPP keyword: '%s' -> %d\n", strbuf, t);
                }
#endif
                if (t) {
                    advance();
#ifdef DEBUG
                    if (VERBOSE(V_CPP) && (t == IF || t == ELIF)) {
                        fdprintf(2,
                            "Before do_cpp(%s): cur.type=0x%02x "
                            "next.type=0x%02x\n",
                            t == IF ? "IF" : "ELIF", cur.type, next.type);
                    }
#endif
                    do_cpp(t);
#ifdef DEBUG
                    if (VERBOSE(V_CPP) && (t == IF || t == ELIF)) {
                        fdprintf(2,
                            "After do_cpp(%s): cur.type=0x%02x "
                            "next.type=0x%02x cond=%p\n",
                            t == IF ? "IF" : "ELIF", cur.type, next.type,
                            cond);
                        if (cond) {
                            fdprintf(2,"  cond->flags=0x%02x (C_TRUE=%d)\n",
                                cond->flags, !!(cond->flags & C_TRUE));
                        }
                    }
#endif
                    /*
                     * After processing #if/#elif with a TRUE condition,
                     * break to return the token in next
                     */
                    /*
                     * For FALSE conditions, the token in next should be
                     * discarded by continuing to loop
                     */
                    if ((t == IF || t == ELIF) && cond &&
                        (cond->flags & C_TRUE)) {
                        /*
                         * True block - next contains the first token,
                         * return it
                         */
                        break;
                    }
                    /*
                     * False block - continue looping, token skipping
                     * will handle it
                     */
                    continue;
                }
                gripe(ER_C_BD);
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
            /*
             * Don't advance - leave curchar at newline so next
             * gettoken() stops
             */
            break;
        }
        if (!(tflags & ONELINE) && cond && !(cond->flags & C_TRUE) &&
            curchar != '#') {
#ifdef DEBUG
            if (VERBOSE(V_CPP)) {
                fdprintf(2,
                    "Skipping line in false block: curchar=0x%02x ('%c')\n",
                    curchar,
                    (curchar >= ' ' && curchar < 127) ? curchar : '?');
            }
#endif
            skiptoeol();
            if (curchar == '\n') {
                advance();  // consume the newline
            }
            continue;
        }
        if ((curchar == ' ') || (curchar == '\t') || (curchar == '\n')) {
            advance();
#ifdef DEBUG
            if (VERBOSE(V_CPP) && cond) {
                fdprintf(2,
                    "After advance past whitespace: curchar=0x%02x "
                    "('%c') column=%d\n", curchar,
                    (curchar >= ' ' && curchar < 127) ? curchar : '?',
                    column);
            }
#endif
            /*
             * After advancing past whitespace, check if we should
             * skip rest of line
             */
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
                /*
                 * cpppseudofunc() replaced the function with
                 * '0' or '1'
                 */
                continue;
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
            next.v.str = malloc(strbuf[0] + 1);
            memcpy(next.v.str, strbuf, strbuf[0] + 1);
            break;
        }

        /* from here, it had better be an operator */
        t = lookupc(simple, curchar);
        if (t == 0xff) {
            gripe(ER_C_UT);
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
     * detokenize for cpp output or asm capture
     */
#ifdef DEBUG
    if (VERBOSE(V_CPP) && (cur.type == INT || cur.type == SYM)) {
        fdprintf(2,"About to output cur.type=0x%02x", cur.type);
        if (cur.type == SYM && cur.v.name) {
            fdprintf(2," (SYM: %s)", cur.v.name);
        } else if (cur.type == INT) {
            fdprintf(2," (INT)");
        }
        fdprintf(2," write_cpp_file=%d\n", write_cpp_file);
    }
#endif
    output_token(&cur);

    if ((write_cpp_file || asm_capture_buf) && lineend) {
        if (write_cpp_file) {
            cpp_out("\n", 1);
        }
        /* For asm blocks, convert newlines to semicolons */
        /* (macexpand() handles its own newlines directly) */
        /*
         * Don't output semicolon for braces - they mark boundaries
         * of asm block
         */
        if (asm_capture_buf && cur.type != BEGIN && cur.type != END) {
            asm_out(";", 1);  /* Convert newline to semicolon for asm */
            asm_out(" ", 1);
        }
        lineend = 0;  /* Clear after using it */
    }
#ifdef DEBUG
    if (VERBOSE(V_TOKEN)) {
        fdprintf(2,"cur.type = 0x%02x \'%c\'\n", cur.type,
            cur.type > ' ' ? cur.type : ' ');
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
            gripe(ER_C_DP);
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
            gripe(ER_C_DP);
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
unsigned long
readcppconst()
{
    unsigned long val;
    char savedtflags = tflags;
    int saved_write_cpp = write_cpp_file;
    struct token saved_cur;

    /* Save cur because recursive gettoken() calls will modify it */
    memcpy(&saved_cur, &cur, sizeof(cur));

    /*
     * hack to make lexer translate newlines to ';', so that expressions
     * terminate at eol;  also enable defined pseudofunction
     */
    tflags = ONELINE | CPPFUNCS;

    /* Disable cpp output while evaluating the #if expression */
    write_cpp_file = 0;

    /* Skip whitespace before reading the first token */
    skipwhite1();

#ifdef DEBUG
    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "readcppconst: before gettoken: curchar=0x%02x ('%c')\n",
            curchar, (curchar >= ' ' && curchar < 127) ? curchar : '?');
    }
#endif

    /* Get the first token of the expression */
    gettoken();
#ifdef DEBUG
    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "After 1st gettoken: cur.type=0x%02x next.type=0x%02x "
            "curchar=0x%02x\n",
            cur.type, next.type, curchar);
    }
#endif
    gettoken();
#ifdef DEBUG
    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "After 2nd gettoken: cur.type=0x%02x next.type=0x%02x "
            "curchar=0x%02x\n",
            cur.type, next.type, curchar);
    }
#endif

    val = parse_const(SEMI);

#ifdef DEBUG
    if (VERBOSE(V_CPP)) {
        fdprintf(2,"After parse_const: curchar=0x%02x\n", curchar);
    }
#endif

    /* Restore cur so outer gettoken() sees original cur */
    memcpy(&cur, &saved_cur, sizeof(cur));

    /* Restore flags */
    tflags = savedtflags;
    write_cpp_file = saved_write_cpp;

    /* Leave next with whatever was read - it contains the INT token */
    /* The caller will handle discarding it if in a false block */

    /* Clear lineend flag */
    lineend = 0;

#ifdef DEBUG
    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "readcppconst returning: val=%lu curchar=0x%02x "
            "next.type=0x%02x\n",
            val, curchar, next.type);
    }
#endif

    return val;
}

void
tdump(unsigned char c)
{
	fdprintf(2,"%s ", tokenname[c]);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
