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
#include "lexeme.h"

/*
 * generated files
 */
#ifdef DEBUG
#include "debug.h"
#endif

typedef unsigned char token_t;

/* CPP-only directives (not emitted to .x) */
#define PP_INCLUDE 240
#define PP_DEFINE 241
#define PP_UNDEF 242
#define PP_IF 243
#define PP_IFDEF 244
#define PP_IFNDEF 245
#define PP_ENDIF 246
#define PP_ELIF 247
#define PP_ELSE 248
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
#define TBSIZE 512          /* text buffer size - matches Micronix disk block */
#define STRBUFSIZE 256      /* string/symbol/identifier buffer */
#define MAXSYMLEN 16        /* symbol buffer size (15 chars + null) */

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
#define ER_C_MA     12      /* macro argument count mismatch */
#define ER_W_SYMTRUNC 13    /* symbol truncated */
#define ER_LAST     ER_W_SYMTRUNC

/*
 * Token structure - lexeme with value
 */
struct token {
    token_t type;
    int lineno;             /* line number where token was scanned */
    char *filename;         /* file where token was scanned */
    union {
        long numeric;       /* char, short, int, long */
        float fval;         /* float, double */
        char *name;         /* if we have a symbol */
        cstring str;        /* counted literal string */
    } v;
};

/* Copy token: d = destination ptr, s = source ptr */
extern void tokcpy(struct token *d, struct token *s);
extern void toksynth(struct token *out, unsigned char type);
extern void toksynthnam(struct token *out, unsigned char type, char *name);

/* Shared filter utilities */
extern int is_type_kw(unsigned char type);
extern int is_type_tok(struct token *t);

/* Pending buffer for filters */
struct pendbuf {
	struct token *buf;
	int max, rd, wr;
};
extern void pend_init(struct pendbuf *p, struct token *buf, int max);
extern void pend_push(struct pendbuf *p, struct token *t);
extern int pend_has(struct pendbuf *p);
extern void pend_pop(struct pendbuf *p, struct token *out);
extern void pend_tok(struct pendbuf *p, unsigned char type);
extern void pend_tok_at(struct pendbuf *p, unsigned char type, struct token *ref);
extern void pend_buf(struct pendbuf *p, struct token *buf, int len);
extern int filt_entry(struct pendbuf *pb, struct token *out,
                      void (*up)(struct token *), struct token *t);
extern void emit_label(struct pendbuf *p, char pfx, int num, char sfx);
extern void emit_goto(struct pendbuf *p, char pfx, int num, char sfx);

/*
 * Text buffer - for file/macro buffer management and output diversions
 * Used for both input (includes/macros) and output (loop body buffering)
 */
struct textbuf {
    char fd;                /* if == -1, memory buffer; else file descriptor */
    char *name;             /* filename or macro/buffer name */
    char *storage;          /* buffer data - malloc'd */
    short offset;           /* read/write position in storage */
    short valid;            /* valid bytes (input) */
    short lineno;           /* current line # (input only) */
    long file_size;         /* bytes written to file (output spill) */
    char saved_column;      /* saved column position for parent file */
    char direction;         /* 'r' = reading, 'w' = writing */
    struct textbuf *prev;   /* stack link */
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
    unsigned char flags;
#define C_TRUE      0x01
#define C_ELSESEEN  0x02
#define C_TRUESEEN  0x04
    struct cond *next;
};

/* Global state */
extern char lexFd;          /* .x output file descriptor */
extern char *curFile;       /* current source file */
extern int lineNo;          /* current line number (for errors) */
extern char noLineMarkers;  /* -N flag: suppress LINENO/NEWLINE */

extern unsigned char curchar;
extern unsigned char nextchar;
extern int lineno;
extern char *filename;
extern char column;
extern char *sysIncPath;
extern struct textbuf *tbtop;

extern struct token cur, next;
extern char strbuf[];
extern struct macro *macros;
extern char *macbuffer;
extern struct cond *cond;

/* io.c - input buffer stack */
extern void pushfile(char *name);
extern void insertmacro(char *name, char *macbuf);
extern void insertfile(char *name, int sysdirs);
extern void advance();
extern void ioinit();
extern void addInclude(char *name);

/* io.c - output buffer stack */
extern struct textbuf *obtop;   /* output buffer stack top */
extern void outbufPush(void);
extern void outbufPop(void);
extern void outbufReplay(void);
extern void outbufWrite(void *data, int len);

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
extern void emitFileStart(char *file);
extern void emitToken(unsigned char tok);
extern void emitKeyword(unsigned char kwval);
extern void emitSym(char *name);
extern void emitNumber(long val);
extern void emitFNumber(float val);
extern void emitString(char *str, int len);
extern void emitLabel(char *name);
extern void emitLine(int line, char *file);
extern void emitCurToken(void);
extern void emitStructTok(struct token *t);

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
extern char *fmtstr(char *buf, char *fmt, ...);
#ifdef DEBUG
extern int fdprintf(int fd, char *fmt, ...);
extern void hexdump(char *tag, char *h, int l);
#endif
extern long parseConst(token_t stop);

/* debug options */
#ifdef DEBUG
#define VERBOSE(x) (verbose & (x))
extern short verbose;
#else
#define VERBOSE(x) (0)
#endif
#ifndef _XOPEN_SOURCE
extern char *strdup(char *s);
#endif

/* filter.c - token filter for normalization */
extern void filterInit(void);
extern void filtAddTdef(char *name);
extern void filter(unsigned char type, long num, float fnum,
                   char *str, int slen);
extern void filtToken(unsigned char type);
extern void filtKw(unsigned char kw);
extern void filtSym(char *name);
extern void filtNum(long val);
extern void filtFNum(float val);
extern void filtStr(char *str, int len);

/* Character classification */
#define iswhite(c) ((c) == ' ' || (c) == '\t' || (c) == '\r')

#endif /* CPP_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
