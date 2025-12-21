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
 * Token types - match pass1/c0.h values for binary .x output
 */
typedef unsigned char token_t;

enum {
    /* Delimiters - match c0.h exactly */
    E_O_F = 0,
    SEMI = 1, BEGIN = 2, END = 3, LBRACK = 4, RBRACK = 5,
    LPAR = 6, RPAR = 7, COLON = 8, COMMA = 9,

    /* Terminals */
    KEYW = 19,
    SYM = 20, NUMBER = 21, STRING = 22, FNUMBER = 23, LNUMBER = 25,
    LABEL = 112,    /* c0.h LABEL */
    LINENO = 116,   /* line number marker: LINENO + 2-byte line + len + filename */
    NEWLINE = 117,  /* increment line by 1 (single byte, no payload) */
    ASMSTR = 118,   /* asm string: ASMSTR + 2-byte len + text (up to 65535 bytes) */

    /* Unary/Binary operators */
    INCR = 30, DECR = 31,           /* INCBEF, DECBEF */
    BANG = 34, AMPER = 35, STAR = 36, TWIDDLE = 38,  /* EXCLA, AMPER, STAR, COMPL */
    DOT = 39, PLUS = 40, MINUS = 41, TIMES = 42, DIV = 43, MOD = 44,
    RSHIFT = 45, LSHIFT = 46, AND = 47, OR = 48, XOR = 49, ARROW = 50,
    LAND = 53, LOR = 54,            /* LOGAND, LOGOR */

    /* Relational */
    EQ = 60, NEQ = 61, LE = 62, LT = 63, GE = 64, GT = 65,

    /* Assignment operators */
    PLUSEQ = 70, SUBEQ = 71, MULTEQ = 72, DIVEQ = 73, MODEQ = 74,
    RSHIFTEQ = 75, LSHIFTEQ = 76, ANDEQ = 77, OREQ = 78, XOREQ = 79,
    ASSIGN = 80, QUES = 90, SIZEOF = 91, ELLIPSIS = 92,

    /* Keyword cval values (emitted after KEYW token) */
    /* Type keywords */
    KW_INT = 0, KW_CHAR = 1, KW_FLOAT = 2, KW_DOUBLE = 3, KW_STRUCT = 4,
    KW_SIGNED = 5, KW_LONG = 6, KW_UNSIGNED = 7, KW_VOID = 10, KW_UNION = 8, KW_SHORT = 0,
    /* Storage class keywords */
    KW_TYPEDEF = 9, KW_AUTO = 11, KW_EXTERN = 12, KW_STATIC = 13, KW_REGISTER = 14,
    /* Statement keywords */
    KW_GOTO = 20, KW_RETURN = 21, KW_IF = 22, KW_WHILE = 23, KW_ELSE = 24,
    KW_SWITCH = 25, KW_CASE = 26, KW_BREAK = 27, KW_CONTINUE = 28, KW_DO = 29,
    KW_DEFAULT = 30, KW_FOR = 31, KW_ENUM = 32, KW_ASM = 33, KW_CONST = 34,
    /* Special - sizeof is an operator, not a keyword */
    KW_SIZEOF = 127,    /* marker: emit SIZEOF(91) token instead of KEYW */

    /* CPP-only directives (not emitted to .x) */
    INCLUDE = 128, DEFINE = 129, UNDEF = 130,
    IF = 131, IFDEF = 132, IFNDEF = 133, ENDIF = 134, ELIF = 135, ELSE = 136,
    NONE = 255
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
extern unsigned char kwlook(unsigned char *str, unsigned char *table);

/* macro.c */
extern void macdefine(char *s);
extern void macundefine(char *s);
extern void addDefine(char *s);

/* emit.c - output functions */
extern void emitToken(unsigned char tok);
extern void emitKeyword(unsigned char kwval);
extern void emitSym(char *name);
extern void emitNumber(long val);
extern void emitFNumber(float val);
extern void emitString(char *str, int len);
extern void emitLabel(char *name);
extern void emitLine(int line, char *file);
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
