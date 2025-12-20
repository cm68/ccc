/*
 * cpp.h - C Preprocessor header
 *
 * Common definitions for the C preprocessor that produces
 * lexeme streams (.x) and preprocessed output (.i)
 */

#ifndef CPP_H
#define CPP_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
 * Token types - must match pass1/token.h
 */
typedef unsigned char token_t;

enum {
    E_O_F = 0,
    NONE = ' ',

    /* C keywords */
    ASM = 'A', AUTO = 'o',
    BREAK = 'B',
    CASE = 'C', CHAR = 'c', CONST = 'k', CONTINUE = 'N',
    DEFAULT = 'O', DO = 'D', DOUBLE = 'd',
    ELSE = 'E', ENUM = 'e', EXTERN = 'x',
    FLOAT = 'f', FOR = 'F',
    GOTO = 'G',
    IF = 'I', INT = 'i',
    LONG = 'l',
    REGISTER = 'r', RETURN = 'R',
    SIZEOF = 'z', SHORT = 's', STATIC = 'p', STRUCT = 'a', SWITCH = 'S',
    TYPEDEF = 't',
    UNION = 'm', UNSIGNED = 'u',
    VOID = 'v', VOLATILE = '4',
    WHILE = 'W',

    /* syntactic cogs */
    BEGIN = '{', END = '}',
    LBRACK = '[', RBRACK = ']',
    LPAR = '(', RPAR = ')',
    SEMI = ';', COMMA = ',',
    LABEL = '3',

    /* terminals */
    SYM = '5', NUMBER = '9', FNUMBER = 'b', STRING = '"',

    /* operators */
    ASSIGN = '=', DOT = '.', ARROW = 'q', DEREF = 'M',
    PLUS = '+', MINUS = '-', STAR = '*', DIV = '/', MOD = '%',
    AND = '&', OR = '|', XOR = '^',
    LT = '<', GT = '>', BANG = '!', TWIDDLE = '~',
    QUES = '?', COLON = ':',
    INCR = 'U', DECR = 'V',
    LSHIFT = 'y', RSHIFT = 'w',
    LOR = 'h', LAND = 'j',
    EQ = 'Q', NEQ = 'n', LE = 'L', GE = 'g',
    PLUSEQ = 'P', SUBEQ = 0xdf, MULTEQ = 'T', DIVEQ = '2', MODEQ = 0xfe,
    ANDEQ = 0xc6, OREQ = '1', XOREQ = 'X',
    LANDEQ = 'J', LOREQ = 'H',
    RSHIFTEQ = '6', LSHIFTEQ = '0',

    /* CPP directives */
    INCLUDE = '#',
    DEFINE = '$', UNDEF = 'K',
    IFDEF = 'Y', IFNDEF = '7', ENDIF = 'Z', ELIF = '8'
};

/*
 * Basic types
 */
typedef char *cstring;      /* counted string - first char is length */
typedef unsigned char byte;
typedef unsigned short word;
typedef unsigned long dword;

/*
 * Limits
 */
#define MAXPARMS 10         /* macro parameters */
#define TBSIZE 1024         /* text buffer size for includes/macros */
#define STRBUFSIZE 128      /* string/symbol/identifier buffer */
#define MAXSYMLEN 32        /* maximum symbol/identifier length */

/*
 * Token flags (tflags)
 */
#define ONELINE   0x01      /* CPP: single line mode */
#define CPPFUNCS  0x02      /* CPP: allow defined() pseudo-function */

/*
 * Error codes - simplified for cpp
 */
typedef int error_t;
#define ER_C_NX     1       /* invalid escape sequence */
#define ER_C_BC     2       /* bad character constant */
#define ER_C_CD     3       /* bad numeric constant */
#define ER_C_TL     4       /* token too long */
#define ER_C_MN     5       /* macro name expected */
#define ER_C_CU     6       /* #elif without #if */
#define ER_C_ME     7       /* missing #endif */
#define ER_C_ID     8       /* invalid directive */
#define ER_C_BD     9       /* bad digit */
#define ER_C_UT     10      /* unknown token */
#define ER_C_DP     11      /* defined requires identifier */
#define ER_W_SYMTRUNC 12    /* symbol truncated */

/*
 * Token structure - lexeme with value
 */
struct token {
    token_t type;
    union {
        long numeric;       /* char, short, int, long */
        float fval;         /* float, double */
        char *name;         /* if we have a symbol */
        cstring str;        /* counted literal string */
    } v;
};

/*
 * Text buffer - for file/macro buffer management
 */
struct textbuf {
    int fd;                 /* if == -1, macro buffer */
    char *name;             /* filename or macro name */
    char *storage;          /* data - free when done */
    short offset;           /* always points at nextchar */
    short valid;            /* total valid in buffer */
    short lineno;           /* current line # in file */
    short saved_column;     /* saved column position for parent file */
    struct textbuf *prev;   /* a stack */
};

/*
 * Macro definition
 */
struct macro {
    unsigned char parmcount;
    char *name;
    char **parms;
    char *mactext;
    struct macro *next;
};

/*
 * CPP conditional state
 */
struct cond {
    int flags;
#define C_TRUE      0x01
#define C_ELSESEEN  0x02
#define C_TRUESEEN  0x04
    struct cond *next;
};

/* Global state */
extern int lexFd;           /* .x output file descriptor */
extern int ppFd;            /* .i output file descriptor */
extern char *curFile;       /* current source file */
extern int lineNo;          /* current line number (for errors) */

extern unsigned char curchar;
extern unsigned char nextchar;
extern int lineno;
extern char *filename;
extern int column;
extern char linebuf[];
extern char prevline[];
extern int linepos;
extern char *sysIncPath;
extern struct textbuf *tbtop;

extern struct token cur, next;
extern char strbuf[];
extern struct macro *macros;
extern char *macbuffer;
extern struct cond *cond;

/* io.c */
extern void pushfile(char *name);
extern void insertmacro(char *name, char *macbuf);
extern void insertfile(char *name, int sysdirs);
extern void advance();
extern void ioinit();
extern void addInclude(char *name);

/* lex.c */
extern void gettoken();
extern void skipws();
extern void skipws1();
extern char match(token_t t);
extern char issym();

/* kw.c */
extern unsigned char cppkw[];
extern unsigned char ckw[];
extern char kwlook(unsigned char *str, unsigned char *table);

/* macro.c */
extern void macdefine(char *s);
extern void macundefine(char *s);
extern void addDefine(char *s);

/* emit.c - output functions */
extern void emitToken(unsigned char tok);
extern void emitSym(char *name);
extern void emitNumber(long val);
extern void emitFNumber(float val);
extern void emitString(char *str, int len);
extern void emitLabel(char *name);
extern void emitPP(char *text, int len);
extern void emitPPStr(char *text);
extern void emitCurToken(void);

/* error handling */
extern void error(char *msg);
extern void fatal(char *msg);
extern void gripe(error_t err);

/* lex.c additional exports */
extern unsigned char tflags;
extern cstring nextstr;
extern unsigned long readcppconst(void);
extern char cpppseudofunc(void);

/* macro.c additional exports */
extern struct macro *maclookup(char *name);
extern char macexpand(char *name);

/* util.c exports */
extern unsigned char lookupc(char *table, unsigned char c);

/* Utility functions */
extern int fdprintf(int fd, char *fmt, ...);
extern long parseConst(token_t stop);

/* Character classification */
#define iswhite(c) ((c) == ' ' || (c) == '\t' || (c) == '\r')

#endif /* CPP_H */
