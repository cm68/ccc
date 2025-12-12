/*
 * this is a brute force lexer that uses a tight keyword lookup in kw.c
 * we do cpp conditionals in here
 * it handles different keyword tables for cpp, c, and asm
 *
 * XXX - maybe use character type bitmask
 */
#include "cc1.h"

static unsigned char incomment = 0;
/* Track comment state across gettoken() calls */

struct token cur, next;

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

/*
 * Check if current token matches and consume it if so
 *
 * Common pattern in parsing: test for token type and consume if matched.
 * This helper combines both operations to reduce code size throughout
 * the parser.
 *
 * Typical usage:
 *   if (match(SEMI)) { ... }  // consumes ';' if present
 *   while (match(COMMA)) { parseNext(); }  // loop over comma-separated list
 *
 * Parameters:
 *   t - Token type to check for
 *
 * Returns:
 *   1 if current token matched (and consumed), 0 otherwise
 *
 * Side effects:
 *   - Advances token stream if match succeeds (calls gettoken())
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
 * Check if current character matches and consume it if so
 *
 * Similar to match() but operates at character level rather than token level.
 * Used during low-level tokenization to test and consume single characters
 * from the input stream.
 *
 * Common usages:
 *   - Testing for specific delimiters: charmatch('\'')
 *   - Consuming expected characters: charmatch('0')
 *   - Operator parsing: charmatch('=')
 *
 * Parameters:
 *   c - Character to check for
 *
 * Returns:
 *   1 if curchar matched (and consumed), 0 otherwise
 *
 * Side effects:
 *   - Advances character stream if match succeeds (calls advance())
 */
int
charmatch(unsigned char c)
{
    if (curchar == c) {
        advance();
        return 1;
    } else {
        return 0;
    }
}

/*
 * Skip whitespace including newlines
 *
 * Advances past all space and newline characters. Used when whitespace
 * (including line breaks) is insignificant in the current context.
 *
 * Characters skipped:
 *   - Space (' ')
 *   - Newline ('\n')
 *
 * Characters NOT skipped:
 *   - Tab ('\t') - not considered whitespace by this function
 *
 * Side effects:
 *   - Advances character stream until non-whitespace found
 */
void
skipws()
{
    while ((curchar == ' ') || (curchar == '\n')) {
        advance();
    }
}

/*
 * Skip whitespace excluding newlines
 *
 * Advances past space characters but stops at newlines. Used in contexts
 * where newline is significant (preprocessor directives, ONELINE mode).
 *
 * Characters skipped:
 *   - Space (' ')
 *
 * Characters NOT skipped:
 *   - Newline ('\n') - significant in CPP directives
 *   - Tab ('\t') - not considered whitespace by this function
 *
 * Side effects:
 *   - Advances character stream until newline or non-space found
 */
void
skipws1()
{
    while (curchar == ' ' || curchar == '\t') {
        advance();
    }
}

/*
 * Skip to end of current line
 *
 * Advances past all characters until newline or EOF. Used to discard
 * rest of preprocessor directives, comments, or error recovery.
 *
 * Stops at:
 *   - Newline ('\n') - NOT consumed, left as curchar
 *   - EOF (curchar == 0)
 *
 * Side effects:
 *   - Advances character stream
 *   - Leaves curchar at newline or 0
 */
void
skiptoeol()
{
    while (curchar && (curchar != '\n')) {
        advance();
    }
}

/*
 * Read integer constant in specified base from character stream
 *
 * Converts character sequence to integer value. Handles multiple bases
 * (binary, octal, decimal, hexadecimal) with full digit validation.
 *
 * Supported bases:
 *   - Binary (2):  0b1010
 *   - Octal (8):   0755, 010
 *   - Decimal (10): 123, 0d99
 *   - Hexadecimal (16): 0xABCD, 0xff
 *
 * Digit validation:
 *   - Rejects digits >= base (e.g., '8' in octal)
 *   - Accepts a-f/A-F for hex (case insensitive)
 *   - Stops at first invalid character
 *
 * Error handling:
 *   - If no digits consumed for base 2 or 16, reports ER_C_NX error
 *   - Base 8/10 allow zero-length (return 0)
 *
 * Parameters:
 *   base - Number base (2, 8, 10, or 16)
 *
 * Returns:
 *   Integer value parsed from input stream
 *
 * Side effects:
 *   - Consumes digit characters from input
 *   - Leaves curchar at first non-digit
 *   - May report error for empty hex/binary literals
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
 * Parse character literal with escape sequence handling
 *
 * Processes a single character from a character or string literal, handling
 * all standard C escape sequences plus custom extensions. This is the core
 * function for interpreting escape codes in both 'x' and "string" literals.
 *
 * Standard C escape sequences:
 *   \\b - backspace       \\n - newline        \\r - carriage return
 *   \\t - tab             \\f - form feed      \\v - vertical tab
 *   \\NNN - octal (up to 3 digits, 0-7)
 *   \\xNN - hexadecimal (any length, 0-9 a-f A-F)
 *   \\\\ - backslash      \\' - single quote   \\" - double quote
 *
 * Custom extensions:
 *   \\e - escape (0x1b)
 *   \\BNN - binary (0b prefix, then digits)
 *   \\DNN - decimal (0d prefix, then digits)
 *
 * Line continuation:
 *   - Backslash-newline: Increment line number and retry from next line
 *   - Allows multi-line literals in source code
 *
 * Invalid characters:
 *   - Non-printable ASCII (< 0x20 or > 0x7e) replaced with space
 *   - Error reported via ER_C_BC
 *
 * Parameters:
 *   None (reads from curchar/nextchar)
 *
 * Returns:
 *   Unsigned char value (0-255) after escape processing
 *
 * Side effects:
 *   - Consumes character(s) from input stream
 *   - May increment lineno for backslash-newline
 *   - Reports errors for invalid characters
 */
/* escape char -> value lookup: index = char - 'a' for a-z */
static char escval[] = {
    0, '\b', 0, 0, '\x1b', '\f', 0, 0, 0, 0, 0, 0, 0,
    '\n', 0, 0, 0, '\r', 0, '\t', 0, '\v', 0, 0, 0, 0
};

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
        advance();
        if (curchar == '\n') { lineno++; goto top; }
        if (curchar >= '0' && curchar <= '7') return getint(8);
        if ((curchar | 0x20) == 'x') { advance(); return getint(16); }
        if (curchar == 'B') { advance(); return getint(2); }
        if (curchar == 'D') { advance(); return getint(10); }
        if (curchar >= 'a' && curchar <= 'z' && escval[curchar - 'a'])
            c = escval[curchar - 'a'];
        else
            c = curchar;
    }
    advance();
    return c;
}

/*
 * Parse numeric constant (character or integer literal)
 *
 * Detects and parses character literals ('x') and integer constants
 * (decimal, octal, hex, binary) from the input stream. Stores result
 * in next.v.numeric for token processing.
 *
 * Character literals:
 *   - Format: 'x' or '\\n' (with escape sequences)
 *   - Processes escape codes via getlit()
 *   - Missing closing quote generates ER_C_CD error
 *
 * Integer literals:
 *   - Decimal: 123, 456
 *   - Hexadecimal: 0x10, 0xFF
 *   - Octal: 0755, 010
 *   - Binary: 0b1010
 *   - Long suffix: 123L, 0xFFL (suffix consumed but ignored)
 *
 * Base detection:
 *   - 0x/0X prefix -> hexadecimal (base 16)
 *   - 0b/0B prefix -> binary (base 2)
 *   - 0d/0D prefix -> decimal (base 10)
 *   - 0 prefix (no suffix) -> octal (base 8)
 *   - Otherwise -> decimal (base 10)
 *
 * Parameters:
 *   None (reads from character stream)
 *
 * Returns:
 *   1 if number parsed (value stored in next.v.numeric), 0 if not a number
 *
 * Side effects:
 *   - Consumes numeric characters from input
 *   - Sets next.v.numeric to parsed value
 *   - May report error for malformed character literal
 */
char
isnumber()
{
    unsigned char base;
    char *p;

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

    /* Check for float literal (requires decimal point) */
    if (base == 10 && curchar == '.') {
        /* Build float string: integer part + . + fractional + optional exp */
        p = strbuf;
        sprintf(p, "%ld", next.v.numeric);
        p += strlen(p);
        *p++ = '.';
        advance();
        while (curchar >= '0' && curchar <= '9') {
            *p++ = curchar;
            advance();
        }
        if ((curchar | 0x20) == 'e') {
            *p++ = curchar;
            advance();
            while (curchar >= '0' && curchar <= '9') {
                *p++ = curchar;
                advance();
            }
        }
        *p = '\0';
        next.v.fval = (float)atof(strbuf);
        /* Skip optional f/F/l/L suffix */
        if ((curchar | 0x20) == 'f' || (curchar | 0x20) == 'l') {
            advance();
        }
        return 2;  /* Return 2 for float */
    }

    /* Skip optional 'L' or 'l' suffix for long constants */
    if ((curchar == 'L') || (curchar == 'l')) {
        advance();
    }

    return 1;
}

/*
 * Parse C identifier into string buffer
 *
 * Detects and extracts C identifiers (keywords and symbols) following the
 * standard identifier rules: [A-Za-z_][A-Za-z0-9_]*
 *
 * Identifier rules:
 *   - First character: letter (a-z, A-Z) or underscore (_)
 *   - Subsequent characters: letter, digit (0-9), or underscore
 *   - No length limit (limited by STRBUFSIZE)
 *
 * Lookahead behavior:
 *   - IMPORTANT: Does NOT advance past last character
 *   - Leaves curchar at last identifier character
 *   - Leaves nextchar at first non-identifier character
 *   - Caller must call advance() to move past identifier
 *
 * This unusual convention allows the caller to inspect the character
 * immediately following the identifier (for macro detection, etc.) before
 * consuming it.
 *
 * Buffer usage:
 *   - Writes to global strbuf
 *   - Null-terminates result
 *   - No overflow checking (assumes STRBUFSIZE sufficient)
 *
 * Parameters:
 *   None (reads from curchar/nextchar, writes to strbuf)
 *
 * Returns:
 *   1 if identifier found (result in strbuf), 0 if curchar not identifier start
 *
 * Side effects:
 *   - Advances curchar to last identifier character (NOT past it)
 *   - Writes identifier to strbuf with null terminator
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

    /* Check identifier length limit (13 chars + 1 for asm underscore = 14 total) */
    if ((s - strbuf) > 13) {
        gripe(ER_C_TL);
        fdprintf(2, "  Identifier '%s' exceeds 13 character limit\n", strbuf);
    }

    if (VERBOSE(V_SYM)) {
        fdprintf(2,"issym = %s curchar = %c nextchar = %c\n",
            strbuf, curchar, nextchar);
    }
    return 1;
}

/*
 * Process preprocessor directive
 *
 * Handles all C preprocessor directives (#define, #if, #include, etc.)
 * including conditional compilation with nesting support. This is the
 * core preprocessor implementation.
 *
 * Directives supported:
 *   #define NAME [value]     - Define macro (object-like or function-like)
 *   #undef NAME              - Remove macro definition
 *   #include "file" or <file> - Insert file contents
 *   #if expr                 - Start conditional block (nests)
 *   #ifdef NAME              - True if macro defined
 *   #ifndef NAME             - True if macro NOT defined
 *   #elif expr               - Else-if in conditional
 *   #else                    - Else in conditional
 *   #endif                   - End conditional block
 *
 * Conditional compilation stack:
 *   - Maintains linked list of struct cond
 *   - Each #if/#ifdef/#ifndef pushes new level
 *   - #endif pops level
 *   - Flags track state: C_TRUE (active), C_ELSESEEN, C_TRUESEEN
 *
 * State tracking:
 *   - C_TRUE: Current block is active (output tokens)
 *   - C_TRUESEEN: At least one branch was true (skips remaining branches)
 *   - C_ELSESEEN: #else seen (error if another #else encountered)
 *
 * Token skipping:
 *   - False blocks skip tokens via gettoken() loop
 *   - Nested conditionals handled correctly
 *   - #if/#elif in true blocks break to return next token
 *
 * #include handling:
 *   - Angle brackets <file> search system include path
 *   - Quotes "file" search user include paths
 *   - Calls insertfile() to push file on textbuf stack
 *   - Primes character stream after insertion
 *
 * Expression evaluation:
 *   - #if and #elif call readcppconst() to eval expression
 *   - ONELINE mode limits expression to single line
 *   - defined() pseudofunction supported
 *
 * Parameters:
 *   t - Preprocessor directive token type (IF, IFDEF, DEFINE, etc.)
 *
 * Side effects:
 *   - Modifies cond stack (pushes/pops levels)
 *   - Consumes tokens from input stream
 *   - May insert files or macro definitions
 *   - Updates C_TRUE/C_ELSESEEN/C_TRUESEEN flags
 */
void
doCpp(unsigned char t)
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
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"#if %d: cond->flags = 0x%02x (C_TRUE=%d)\n",
                v, cond->flags, !!(cond->flags & C_TRUE));
        }
        /*
         * Don't call skiptoeol() - readcppconst() in ONELINE mode
         * already advanced past the line
         */
        return;
    case IFNDEF:
    case IFDEF:
        skipws1();
        if (!issym()) {
            gripe(ER_C_MN);
            skiptoeol();
            return;
        }
        advance();
        v = (maclookup(strbuf) != 0);  // true if macro is defined
        if (t == IFNDEF) v = !v;       // invert for ifndef
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
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"#endif: cond=%p", cond);
            if (cond) {
                fdprintf(2," flags=0x%02x (C_TRUE=%d)",
                    cond->flags, !!(cond->flags & C_TRUE));
            }
            fdprintf(2,"\n");
        }
        if (!cond) {
            gripe(ER_C_CU);
            return;
        }
        c = cond;
        cond = c->next;
        free(c);
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"After pop: cond=%p", cond);
            if (cond) {
                fdprintf(2," flags=0x%02x (C_TRUE=%d)",
                    cond->flags, !!(cond->flags & C_TRUE));
            }
            fdprintf(2,"\n");
        }
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
        skipws1();
        if (!issym()) {
            gripe(ER_C_MN);
            return;
        }
        advance();
        macdefine(strbuf);
        return;
    case UNDEF:
        skipws1();
        if (!issym()) {
            gripe(ER_C_MN);
            return;
        }
        advance();
        macundefine(strbuf);
        return;
    case INCLUDE:
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"Processing INCLUDE directive\n");
        }
        skipws1();
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
        if (VERBOSE(V_CPP)) {
            fdprintf(2,"After skiptoeol: curchar='%c'(0x%x) "
                "nextchar='%c'(0x%x)\n",
                curchar >= 32 ? curchar : '?', curchar,
                nextchar >= 32 ? nextchar : '?', nextchar);
            fdprintf(2,"About to insertfile: '%s' sys=%d\n", strbuf, k == '>');
        }
        insertfile(strbuf, k == '>');
        /* Initialize curchar/nextchar from the new file */
        advance();
        advance();
        return;
    }
}

/*
 * Parse string literal into counted string buffer
 *
 * Processes double-quoted string literals with escape sequence handling.
 * Stores result as counted string (first byte is length, followed by data)
 * in strbuf, supporting embedded null characters.
 *
 * Counted string format:
 *   - strbuf[0]: Length byte (0-255)
 *   - strbuf[1..n]: String characters
 *   - strbuf[n+1]: Null terminator (for convenience, not counted)
 *
 * Escape processing:
 *   - Calls getlit() for each character
 *   - Handles all C escape sequences: \\n, \\t, \\", \\\, \\xNN, etc.
 *   - Supports multi-line strings with backslash-newline
 *
 * Embedded nulls:
 *   - Counted format allows embedded \\0 characters
 *   - Length tracking independent of null terminator
 *   - Example: "ab\\0cd" has length 5
 *
 * Parameters:
 *   None (reads from character stream, writes to strbuf)
 *
 * Returns:
 *   1 if string parsed (result in strbuf), 0 if curchar not '\"'
 *
 * Side effects:
 *   - Consumes characters from '\"' to closing '\"'
 *   - Writes counted string to strbuf
 *   - strbuf[0] contains final length
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
    if (VERBOSE(V_STR)) {
        fdprintf(2,"isstring: %s(%d)\n", &strbuf[1], strbuf[0]);
    }
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
char dblAble[] = {
    PLUS, MINUS, OR, AND, ASSIGN, GT, LT, 0
};
char dbltok[] = {
    INCR, DECR, LOR, LAND, EQ, RSHIFT, LSHIFT, 0
};

/*
 * list of tokens that can have '=' appended
 * and then, what token that turns them into
 */
char eqAble[] = {
    PLUS, MINUS, STAR, DIV, MOD, AND, OR, XOR,
    GT, LT, BANG, LOR, LAND, RSHIFT, LSHIFT, 0
};
char eqtok[] = {
    PLUSEQ, SUBEQ, MULTEQ, DIVEQ, MODEQ, ANDEQ, OREQ, XOREQ,
    GE, LE, NEQ, LOREQ, LANDEQ, RSHIFTEQ, LSHIFTEQ, 0
};

/*
 * Free memory allocated for current token
 *
 * Releases heap-allocated memory associated with the current token before
 * advancing to the next token. Prevents memory leaks during tokenization.
 *
 * Token memory management:
 *   - SYM tokens: Allocate name string (freed here)
 *   - STRING tokens: NOT freed (persist throughout compilation)
 *   - Other tokens: No heap allocation (nothing to free)
 *
 * STRING token persistence:
 *   - String literals referenced in expressions and initializers
 *   - Must survive entire compilation
 *   - Stored as synthetic global variables (_str0, _str1, etc.)
 *   - Freed at end of compilation (not per-token)
 *
 * Double-free prevention:
 *   - Sets cur.v.name to NULL after freeing
 *   - Prevents crashes if freetoken() called twice
 *
 * Side effects:
 *   - Frees cur.v.name if cur.type == SYM
 *   - Sets cur.v.name to NULL
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

/*
 * Get next token from input stream
 *
 * This is the main lexer/tokenizer function that implements the complete
 * C tokenization process including preprocessor integration, comment
 * handling, and operator recognition. It maintains a two-token lookahead
 * (cur and next) required for recursive descent parsing.
 *
 * Token stream management:
 *   - Shifts next -> cur
 *   - Parses new token into next
 *   - Maintains token history for debugging (tokHist circular buffer)
 *
 * Preprocessing integration:
 *   - Comment stripping (C and C++ style)
 *   - CPP directive processing (define, if, include, etc.)
 *   - Macro expansion via macexpand()
 *   - Conditional compilation (skips false blocks)
 *
 * Token types recognized:
 *   - Identifiers/keywords (via issym() + kwlook())
 *   - Numbers (via isnumber() - decimal, hex, octal, binary, char)
 *   - String literals (via isstring())
 *   - Operators (single-char, doubled, with '=' suffix)
 *   - Special operators: ->, punctuation
 *
 * Operator tokenization:
 *   - Single-char operators: + - * / % & | ^ < > ! ~ ? : = . , ; braces brackets parens
 *   - Doubled operators: ++ -- || && == >> << (same char twice)
 *   - Equals-suffix: += -= *= /= %= &= |= ^= >= <= != >>= <<= ||= &&=
 *   - Arrow operator: ->
 *
 * Preprocessor directive handling:
 *   - Hash at column 0 triggers CPP processing
 *   - Non-column-0 hash treated as token (for stringify in macros)
 *   - Directives processed by doCpp()
 *   - False blocks skip all tokens except nested directives
 *
 * Comment handling:
 *   - C-style: block comments (may nest, handled as single comment)
 *   - C++ style: line comments to end of line
 *   - Tracked via incomment flag across gettoken() calls
 *   - Comments invisible to parser
 *
 * Macro expansion:
 *   - Checks identifiers via macexpand()
 *   - Expands and re-tokenizes replacement text
 *   - Handles function-like macros with arguments
 *   - defined() pseudofunction in conditional expressions
 *
 * ONELINE mode:
 *   - Used for conditional expression evaluation
 *   - Translates newline to ';' to terminate expression
 *   - Enabled via tflags & ONELINE
 *
 * Output for preprocessor (-E flag):
 *   - Calls outputToken() to emit token text
 *   - Writes to .i file and/or asm capture buffer
 *   - Preserves spacing and formatting
 *
 * Asm block support:
 *   - Captures tokens to asmCbuf when active
 *   - Converts newlines to semicolons for asm statements
 *   - Skips braces (parsing markers, not asm text)
 *
 * EOF handling:
 *   - Returns E_O_F token when curchar == 0
 *   - Checks for unclosed conditional directives
 *
 * Error recovery:
 *   - Invalid tokens converted to ';' with ER_C_UT error
 *   - Continues parsing after errors
 *
 * Side effects:
 *   - Advances cur and next tokens
 *   - Consumes characters from input stream
 *   - May expand macros, process directives, include files
 *   - Updates incomment, lineend, tflags
 *   - Writes to preprocessor output and/or asm buffer
 */
/* Track when newline encountered for asm capture semicolon insertion */
unsigned char lineend = 0;

void
gettoken()
{
    token_t t;
    unsigned char c;

    freetoken();

    memcpy(&cur, &next, sizeof(cur));
    next.v.str = 0;
    next.type = NONE;

    while (1) {
        if (curchar == 0) {
            next.type = E_O_F;
            if (VERBOSE(V_CPP)) {
                fdprintf(2,"Reached EOF: cond=%p\n", cond);
                if (cond) {
                    fdprintf(2,"  cond->flags=0x%02x (C_TRUE=%d)\n",
                        cond->flags, !!(cond->flags & C_TRUE));
                }
            }
            break;
        }
        /* Handle comments before checking for preprocessor directives */
        if (curchar == '/') {
            if (nextchar == '*') {
                /*
                 * Always enter comment mode, even if already in one
                 * (nested comments become single comment)
                 */
                if (VERBOSE(V_TOKEN)) {
                    fdprintf(2,"Found comment start at line %d\n", lineno);
                }
                incomment = 1;
                advance();
                advance();
                continue;
            }
        }
        if ((curchar == '/') && (nextchar == '/')) {
            skiptoeol();
            continue;
        }
        if ((incomment) && (curchar == '*') && (nextchar == '/')) {
            if (VERBOSE(V_TOKEN)) {
                fdprintf(2,
                    "Found comment end at line %d, advancing past */ \n",
                    lineno);
            }
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
            if (VERBOSE(V_CPP)) {
                fdprintf(2,"Found # at column=%d (will%s process) cond=%p",
                    column, (column == 1) ? "" : " NOT", cond);
                if (cond) {
                    fdprintf(2," C_TRUE=%d", !!(cond->flags & C_TRUE));
                }
                fdprintf(2,"\n");
            }
            if (column != 1) {
                /* Not a CPP directive, treat as token */
                next.type = '#';
                break;
            }
            /* CPP directive at column 0 */
            skipws1();
            if (issym()) {
                t = kwlook((unsigned char *)strbuf, cppkw);
                if (VERBOSE(V_CPP)) {
                    fdprintf(2,"CPP keyword: '%s' -> %d\n", strbuf, t);
                }
                if (t) {
                    advance();
                    /*
                     * In false block, only process conditional directives.
                     * Skip #include, #define, #undef, etc.
                     */
                    if (cond && !(cond->flags & C_TRUE)) {
                        if (t != IF && t != IFDEF && t != IFNDEF &&
                            t != ELIF && t != ELSE && t != ENDIF) {
                            skiptoeol();
                            continue;
                        }
                    }
                    if (VERBOSE(V_CPP) && (t == IF || t == ELIF)) {
                        fdprintf(2,
                            "Before doCpp(%s): cur.type=0x%02x "
                            "next.type=0x%02x\n",
                            t == IF ? "IF" : "ELIF", cur.type, next.type);
                    }
                    doCpp(t);
                    if (VERBOSE(V_CPP) && (t == IF || t == ELIF)) {
                        fdprintf(2,
                            "After doCpp(%s): cur.type=0x%02x "
                            "next.type=0x%02x cond=%p\n",
                            t == IF ? "IF" : "ELIF", cur.type, next.type,
                            cond);
                        if (cond) {
                            fdprintf(2,"  cond->flags=0x%02x (C_TRUE=%d)\n",
                                cond->flags, !!(cond->flags & C_TRUE));
                        }
                    }
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
            if (VERBOSE(V_CPP)) {
                fdprintf(2,
                    "Skipping line in false block: curchar=0x%02x ('%c')\n",
                    curchar,
                    (curchar >= ' ' && curchar < 127) ? curchar : '?');
            }
            skiptoeol();
            if (curchar == '\n') {
                advance();  // consume the newline
            }
            continue;
        }
        if ((curchar == ' ') || (curchar == '\t') || (curchar == '\n')) {
            advance();
            if (VERBOSE(V_CPP) && cond) {
                fdprintf(2,
                    "After advance past whitespace: curchar=0x%02x "
                    "('%c') column=%d\n", curchar,
                    (curchar >= ' ' && curchar < 127) ? curchar : '?',
                    column);
            }
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
            t = kwlook((unsigned char *)strbuf, ckw);
            if (t) {
                next.type = t;
                next.v.name = 0;  /* keywords don't have names */
                /* Special handling for asm { } - capture raw text */
                if (t == ASM) {
                    /* Skip whitespace to find { */
                    while (curchar == ' ' || curchar == '\t' || curchar == '\n')
                        advance();
                    if (curchar == '{') {
                        int depth = 1;
                        int bufsiz = 256;
                        int buflen = 0;
                        char *buf = malloc(bufsiz);
                        advance();  /* skip { */
                        /* Capture until matching } */
                        while (depth > 0 && curchar) {
                            if (curchar == '{') depth++;
                            else if (curchar == '}') {
                                depth--;
                                if (depth == 0) break;
                            }
                            if (buflen + 1 >= bufsiz) {
                                bufsiz *= 2;
                                buf = realloc(buf, bufsiz);
                            }
                            buf[buflen++] = curchar;
                            advance();
                        }
                        buf[buflen] = 0;
                        if (curchar == '}') advance();
                        /* Trim whitespace */
                        while (buflen > 0 && (buf[buflen-1] == ' ' ||
                               buf[buflen-1] == '\t' || buf[buflen-1] == '\n'))
                            buf[--buflen] = 0;
                        {
                            char *p = buf;
                            while (*p == ' ' || *p == '\t' || *p == '\n') p++;
                            if (p != buf) memmove(buf, p, strlen(p) + 1);
                        }
                        next.v.str = buf;  /* Store raw text */
                    }
                }
                break;
            }
            next.type = SYM;
            /* Enforce symbol length limit with warning */
            if (strlen(strbuf) > MAXSYMLEN) {
                gripe(ER_W_SYMTRUNC);
                strbuf[MAXSYMLEN] = '\0';
            }
            next.v.name = strdup(strbuf);
            break;
        }
        {
            char numtype = isnumber();
            if (numtype) {
                next.type = (numtype == 2) ? FNUMBER : NUMBER;
                break;
            }
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
            t = lookupc(dblAble, c);
            if (t != 0xff) {
                c = next.type = dbltok[t];
                advance();
            }
        }

        /* see if the character has an '=' appended.  this can be an operator */
        if (curchar == '=') {
            t = lookupc(eqAble, c);
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

    lineend = 0;
    if (VERBOSE(V_TOKEN)) {
        fdprintf(2,"cur.type = 0x%02x \'%c\'\n", cur.type,
            cur.type > ' ' ? cur.type : ' ');
    }
    return;
}

/*
 * Handle defined() pseudofunction in preprocessor expressions
 *
 * Implements the defined() operator for #if and #elif directives. This
 * is a standard C preprocessor feature (though not in original K&R) that
 * tests if a macro name is currently defined.
 *
 * Syntax supported:
 *   defined(NAME)  - Returns 1 if NAME is defined, 0 otherwise
 *
 * Usage contexts:
 *   - #if defined(DEBUG) || defined(TRACE)
 *   - #elif defined(__GNUC__)
 *   - #if !defined(NDEBUG) && defined(CHECKS)
 *
 * Processing:
 *   1. Checks if identifier in strbuf is "defined"
 *   2. Verifies CPPFUNCS flag enabled (only in #if expressions)
 *   3. Expects '(' after "defined"
 *   4. Reads macro name identifier
 *   5. Expects ')' to close
 *   6. Looks up macro with maclookup()
 *   7. Replaces entire "defined(NAME)" with '0' or '1' in curchar
 *
 * Replacement mechanism:
 *   - Sets curchar to '0' or '1' character
 *   - Lexer continues and parses as number token
 *   - Expression evaluator sees 0 or 1 constant
 *
 * Error handling:
 *   - Missing '(': Reports ER_C_DP, returns '0'
 *   - Missing ')': Reports ER_C_DP, returns '0'
 *   - Not in CPPFUNCS mode: Returns 0 (not recognized)
 *
 * Parameters:
 *   None (reads from strbuf and character stream)
 *
 * Returns:
 *   1 if defined() pseudofunction processed (curchar set to '0' or '1')
 *   0 if not a defined() call
 *
 * Side effects:
 *   - Consumes "defined(NAME)" from input stream
 *   - Sets curchar to '0' or '1'
 */
char
cpppseudofunc()
{
    int r = 0;

    if ((strcmp("defined", strbuf) == 0) && (tflags & CPPFUNCS)) {
        advance();
        skipws1();
        if (curchar != '(') {
            gripe(ER_C_DP);
            curchar = '0';
            return 1;
        }
        advance();
        skipws1();
        if (issym()) {
            r = (maclookup(strbuf) != 0);
            advance();
        }
        skipws1();
        if (curchar != ')') {
            gripe(ER_C_DP);
            r = 0;
        } else {
            advance();
        }
        curchar = r ? '1' : '0';
        return 1;
    }
    return 0;
}

/*
 * Evaluate constant expression for #if/#elif directive
 *
 * Parses and evaluates compile-time constant expressions in preprocessor
 * conditionals. Handles recursive #if nesting during expression evaluation
 * by carefully saving and restoring global state.
 *
 * Recursion challenge:
 *   - Called from doCpp() which is called from gettoken()
 *   - May call gettoken() internally to parse expression
 *   - Expression parsing may encounter #if and call doCpp() recursively
 *   - Must preserve cur/next/tflags across recursion
 *
 * State management:
 *   - Saves cur token (expression parser will modify it)
 *   - Sets ONELINE mode (newline terminates expression)
 *   - Enables CPPFUNCS (allows defined() pseudofunction)
 *   - Disables writeCppfile (don't output #if expression text)
 *   - Restores all state after evaluation
 *
 * ONELINE mode:
 *   - Translates newline to ';' token
 *   - Expression parser stops at ';'
 *   - Prevents #if expression from spanning multiple lines
 *
 * Token priming:
 *   - Calls gettoken() twice to fill cur and next
 *   - First call loads next
 *   - Second call shifts next->cur and loads new next
 *   - parseConst() then has proper two-token lookahead
 *
 * Expression evaluation:
 *   - Calls parseConst(SEMI) to parse until semicolon
 *   - Supports all C operators: arithmetic, logical, bitwise, relational
 *   - Constant folding performed during parse
 *   - Result is compile-time constant (unsigned long)
 *
 * defined() support:
 *   - CPPFUNCS flag enables cpppseudofunc()
 *   - Allows: #if defined(DEBUG) && !defined(NDEBUG)
 *
 * Parameters:
 *   None (reads from character stream)
 *
 * Returns:
 *   Unsigned long value of evaluated expression
 *
 * Side effects:
 *   - Consumes expression tokens from input
 *   - Temporarily modifies tflags, writeCppfile
 *   - Saves and restores cur token
 *   - Leaves curchar after expression
 *   - Clears lineend flag
 */
unsigned long
readcppconst()
{
    unsigned long val;
    char savedtflags = tflags;
    struct token saved_cur;

    memcpy(&saved_cur, &cur, sizeof(cur));

    tflags = ONELINE | CPPFUNCS;

    /* Skip whitespace before reading the first token */
    skipws1();

    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "readcppconst: before gettoken: curchar=0x%02x ('%c')\n",
            curchar, (curchar >= ' ' && curchar < 127) ? curchar : '?');
    }

    /* Get the first token of the expression */
    gettoken();
    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "After 1st gettoken: cur.type=0x%02x next.type=0x%02x "
            "curchar=0x%02x\n",
            cur.type, next.type, curchar);
    }
    gettoken();
    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "After 2nd gettoken: cur.type=0x%02x next.type=0x%02x "
            "curchar=0x%02x\n",
            cur.type, next.type, curchar);
    }

    val = parseConst(SEMI);

    if (VERBOSE(V_CPP)) {
        fdprintf(2,"After parseConst: curchar=0x%02x\n", curchar);
    }

    memcpy(&cur, &saved_cur, sizeof(cur));
    tflags = savedtflags;
    lineend = 0;

    if (VERBOSE(V_CPP)) {
        fdprintf(2,
            "readcppconst returning: val=%lu curchar=0x%02x "
            "next.type=0x%02x\n",
            val, curchar, next.type);
    }

    return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
