/*
 * global defs for curt's c compiler
 */

/* libc functions */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
 * generated files
 */
#include "debug.h"
#include "token.h"

/*
 * error numbers for formatting
 * these should match the indexes for the errmsg array
 */
#undef DEF_ERRMSG
#include "error.h"

/*
 * expressions hold computations
 */
struct expr {
	int flags;
#define	E_CONST     0x01
#define E_RESOLVED 0x02
#define	E_FUNARG    0x04
	char op;
	struct expr *left;
	struct expr *right;
	struct expr *up;
	struct expr *prev;
	struct expr *next;

	struct type *type;
	struct var *var;

	unsigned long v;
	char location;
	char cost;
	char regs;
	struct stmt *stmt;
	struct inst *inst;
};
#define	PRI_ALL	0

/*
 * a statement is the basic execution unit that is managed by the compiler
 * it has a parent statement, and is linked to all the other statements that
 * belong to the same parent
 */
struct stmt {
	struct stmt *parent;
	struct stmt *next;
	struct expr *left;
	struct expr *right;
	char op;
	int flags;
};

/*
 * synthetic type information like enum, struct, union, etc goes away as soon
 * as the scope does.  that means that by the time we get to expression trees
 * we only have references to primitive types, sizes and offsets.
 *
 * there are dialects of C that have polluted the lexical scope, for example
 * where struct elements or tags leak upwards.  this isn't one of them.
 *
 * if you have a c source that depends on this kind of thing, fix the source.
 */
extern struct scope *scope;

struct scope {
	char *scopename;
	struct name *names;
	struct scope *prev;			// all the way up to and including global
};

extern struct scope *global;
extern struct scope	*local;

/*
 * namespaces
 */
typedef enum namespace {
	SYMBOL,
	TYPE_DEF,
	ENUMTAG,
	ENUMELEMENT, // only found in sub-types of ENUMTAB
	AGGTAG,
	AGGELEMENT // only found in sub-types of AGGTAG
} namespace_t;

/*
 * how big a pointer is 
 */
#define TS_PTR  2

/*
 * this is a handle for types. types are scoped just like names
 */
struct type {
    struct name *name;  	// if a named struct/union/enum - not always present
	namespace_t space;
	int size;		    	// how big is one of me
	int count;		    	// if we are an array, how many
	struct name *elem;		// element list
    struct type *sub;		// pointer to what, array of what, etc.
    unsigned char flags;
};
#define TF_AGGREGATE	0x01
#define TF_INCOMPLETE	0x02
#define TF_UNSIGNED		0x04
#define TF_NORMALIZED	0x08
#define	TF_POINTER		0x10
#define	TF_ARRAY		0x20
#define	TF_FLOAT		0x40

/*
 * this is an instance of a type with a name.
 * a variable.  note that at the same scope, you can have
 * multiple instances of the same name with different namespaces.
 * this is a container for functions, variables, constants, and fields
 */
struct name {
	namespace_t space;
	char *name;
	struct name *next;		// all names in same container
	struct type *type;
	unsigned char sclass;
	int offset;				// if inside a struct
    int bitoff;
    int width;
    struct expr *init;      // value of constant or initializer
    struct stmt *body;      // function body
};

/*
 * storage classes - many combinations are illogical
 */
#define	SC_LOCAL	0x04
#define	SC_GLOBAL	0x01
#define SC_FUNARG   0x80
#define	SC_CONST	0x20
#define	SC_STATIC	0x02
#define	SC_REGISTER	0x08
#define	SC_VOLATILE	0x10
#define	SC_EXTERN	0x40
#define SC_BITFIELD 0x99

/*
 * we use counted strings in places to handle the somewhat gnarly case of
 * literal strings with embedded nulls - it also means we don't need to do strlen
 */
typedef char *cstring;

/* kw.c */
extern unsigned char cppkw[];
extern unsigned char ckw[];
extern unsigned char asmkw[];
extern char kwlook(unsigned char *str, unsigned char *table);

/* lex.c */
extern cstring nextstr;

struct token {				// lexeme
	token_t type;
	union {
		long numeric;		// char, short, int, long
		char *name;			// if we have a symbol
		cstring str;		// counted literal string
	} v;
} cur, next;

extern void lexinit();
extern int write_cpp_file;
extern int cpp_file;
extern char *cpp_file_name;
extern char strbuf[];
extern char match(token_t t);
extern void gettoken();
extern void skipwhite1();
extern void skipwhite();
extern char issym();

/* parse.c */
extern struct stmt *makestmt(char op, struct expr *left);

/* io.c */
extern void pushfile(char *name);
extern void insertmacro(char *name, char *macbuf);
extern void insertfile(char *name, int sysdirs);
extern void advance();
void iodump();
void ioinit();
void add_include(char *name);
void cpp_flush();
void cpp_out(char *s, int len);

extern char curchar;
extern char nextchar;
extern int lineno;
extern char *filename;
extern int column;

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
extern void hexdump(char *tag, char *s, int len);
int iswhite(char c);
char *bitdef(unsigned char v, char **defs);

/* type.c */
extern char *blockname();
extern struct scope *cur_block;
void push_scope(char *name);
void pop_scope();
// struct type *findtype(char *name);

/* tokenlist.c */
extern char *tokenname[];
extern char *detoken[];

/* debug options */
#define VERBOSE(x) (verbose & (x))
extern int verbose;

#ifdef __SDCC
/*
 * this is a minimal unix library header file for use on compilers
 * that don't have unixlike libraries and includes
 */
char *strdup(char *s);
int open(char *name, int mode);
int close(int fd);
int creat(char *name, int mode);
void perror(char *msg);
void exit(int exitcode);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
