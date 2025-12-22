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

/* Token types - using #defines for Ritchie C compatibility */
/* Delimiters - match c0.h exactly */
#define E_O_F 0
#define SEMI 1
#define BEGIN 2
#define END 3
#define LBRACK 4
#define RBRACK 5
#define LPAR 6
#define RPAR 7
#define COLON 8
#define COMMA 9

/* Terminals */
#define KEYW 19
#define SYM 20
#define NUMBER 21
#define STRING 22
#define FNUMBER 23
#define LNUMBER 25
#define LABEL 112
#define LINENO 116
#define NEWLINE 117
#define ASMSTR 118

/* Unary/Binary operators */
#define INCR 30
#define DECR 31
#define BANG 34
#define AMPER 35
#define STAR 36
#define TWIDDLE 38
#define DOT 39
#define PLUS 40
#define MINUS 41
#define TIMES 42
#define DIV 43
#define MOD 44
#define RSHIFT 45
#define LSHIFT 46
#define AND 47
#define OR 48
#define XOR 49
#define ARROW 50
#define LAND 53
#define LOR 54

/* Relational */
#define EQ 60
#define NEQ 61
#define LE 62
#define LT 63
#define GE 64
#define GT 65

/* Assignment operators */
#define PLUSEQ 70
#define SUBEQ 71
#define MULTEQ 72
#define DIVEQ 73
#define MODEQ 74
#define RSHIFTEQ 75
#define LSHIFTEQ 76
#define ANDEQ 77
#define OREQ 78
#define XOREQ 79
#define ASSIGN 80
#define QUES 90
#define SIZEOF 91
#define ELLIPSIS 92

/* Keyword cval values (emitted after KEYW token) */
/* Type keywords */
#define KW_INT 0
#define KW_CHAR 1
#define KW_FLOAT 2
#define KW_DOUBLE 3
#define KW_STRUCT 4
#define KW_SIGNED 5
#define KW_LONG 6
#define KW_UNSIGNED 7
#define KW_UNION 8
#define KW_TYPEDEF 9
#define KW_VOID 10
#define KW_SHORT 0

/* Storage class keywords */
#define KW_AUTO 11
#define KW_EXTERN 12
#define KW_STATIC 13
#define KW_REGISTER 14

/* Statement keywords */
#define KW_GOTO 20
#define KW_RETURN 21
#define KW_IF 22
#define KW_WHILE 23
#define KW_ELSE 24
#define KW_SWITCH 25
#define KW_CASE 26
#define KW_BREAK 27
#define KW_CONTINUE 28
#define KW_DO 29
#define KW_DEFAULT 30
#define KW_FOR 31
#define KW_ENUM 32
#define KW_ASM 33
#define KW_CONST 34
#define KW_SIZEOF 127

/* CPP-only directives (not emitted to .x) */
#define INCLUDE 128
#define DEFINE 129
#define UNDEF 130
#define IF 131
#define IFDEF 132
#define IFNDEF 133
#define ENDIF 134
#define ELIF 135
#define ELSE 136
#define NONE 255

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
#define STRBUFSIZE 256      /* string/symbol/identifier buffer */
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
    int lineno;             /* line number where token was scanned */
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
extern int noLineMarkers;   /* -N flag: suppress LINENO/NEWLINE */

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
