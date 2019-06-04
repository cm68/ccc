/*
 * global defs for curt's c compiler
 */
#include "type.h"
#include "expr.h"
#include "stmt.h"

#include "token.h"
#include "debug.h"

/* kw.c */
extern unsigned char cppkw[];
extern unsigned char ckw[];
extern unsigned char asmkw[];
extern char kwlook(unsigned char *str, unsigned char *table);

/* lex.c */
extern int write_cpp_file;
extern int cpp_file;
extern char *cpp_file_name;
extern token_t curtok;
extern token_t nexttok;
extern long curval;
extern long nextval;
extern char *curstr;
extern char *nextstr;
extern char strbuf[];
extern char match(token_t t);
extern void gettoken();
extern void skipwhite1();
extern void skipwhite();
extern char issym();

/* io.c */
extern void pushfile(char *name);
extern void insertmacro(char *name, char *macbuf);
extern void insertfile(char *name, int sysdirs);
extern void advance();
void iodump();
void ioinit();
void add_include(char *name);

extern char curchar;
extern char nextchar;
extern int lineno;
extern char *filename;
extern int column;

/*
 * error numbers for formatting
 * these should match the indexes for the errmsg array
 */
#undef DEF_ERRMSG
#include "error.h"

/* expr.c */
struct expr *expr(char priority, struct stmt *st);
void freeexpr(struct expr *e);

/* main.c */
extern void err(error_t errcode);
extern void fatal(error_t errcode);
extern void recover(error_t errcode, token_t skipto);
extern void need(token_t check, token_t skipto, error_t errcode);
extern void err(error_t errcode);
int main(int argc, char **argv);
void process(char *f);
void usage(char *complaint, char *p);

/* macro.c */
struct macro {
	char parmcount;
	char *name;
	char **parms;
	char *mactext;
	struct macro *next;
};
extern struct macro *macros;
extern char *macbuffer;
void macdefine(char *s);
void macundefine(char *s);
int macexpand(char *s);
struct macro *maclookup(char *s);
void add_define(char *s);

/* util.c */
extern int lookupc(char *s, char c);
extern void hexdump(char *tag, char *s, int len, int (*high)(int i));
void cpp_out(char *s);
int iswhite(char c);

/* type.c */
extern struct scope *cur_block;
void push_scope(char *name);
void pop_scope();
struct type *findtype(char *name, kind_t kind);

/* tokenlist.c */
extern char *tokenname[];
extern char *detoken[];

/* debug options */
#define VERBOSE(x) (verbose & (x))
extern int verbose;

/* libc functions */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef __SDCC
#include "unixlib.h"
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
