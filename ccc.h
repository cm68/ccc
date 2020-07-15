/*
 * data structures for the compiler.  we don't have any other non-generated includes.
 * so, everything is right here
 */

/* nested includes are a bit ugly, but it means that i can just include ccc.h */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/*
 * generated files
 */
#include "debug.h"
#include "token.h"

/*
 * basic types - everything externally visible has one of these
 */
typedef char *cstring;		// counted string - first char is length
typedef unsigned char byte;
typedef unsigned short word;
typedef unsigned long dword;

/*
 * we just want the error symbols
 * error.h is generated, and contains actual error strings if DEF_ERRMSG.
 */
#undef DEF_ERRMSG
#include "error.h"

/*
 * expressions hold computations
 */
struct expr {
	int flags;
#define	E_CONST     0x01
#define E_RESOLVED	0x02
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

// expression priority - used to control the precedence
#define	PRI_ALL	0

extern struct expr *parse_expr(char priority, struct stmt *);
extern struct expr *new_expr(char op);
extern void destroy_expr(struct expr *e);
extern int parse_const();
#ifdef DEBUG
extern void dump_expr(struct expr *e);
#endif

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
	char *label;
};

extern struct stmt *new_stmt(char op, struct expr *left);
extern void destroy_stmt(struct stmt *s);
#ifdef DEBUG
extern void dump_stmt(struct stmt *s);
#endif

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
struct scope {
	char *name;
	struct name *names;
	struct scope *prev;			// all the way up to and including global
};

extern struct scope *global;	// file level
extern struct scope	*local;		// our current block

extern void push_scope(char *name);
extern void pop_scope();

/*
 * namespaces - symbols of the same name can exist at the same scope if they
 * are in different namespaces
 */
typedef enum namespace {
	SYMBOL,
	TYPE_DEF,
	ENUMTAG,
	ENUMELEMENT,	// only found in sub-types of ENUMTAB
	AGGTAG,
	AGGELEMENT		// only found in sub-types of AGGTAG
} namespace_t;

extern char *namespace_name[];

/*
 * how big a pointer is 
 */
#define TS_PTR  2

/*
 * this is a handle for types. types are scoped just like names, so their handles
 * are linked to a name
 */
struct type {
	struct name *name;		// the type name
    char *typename;         // just a copy - XXX
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

extern struct type *getbasetype();
extern struct type *new_type(char *name, namespace_t space, struct type *sub);
extern void destroy_type(struct type *t);
extern struct type *lookup_type(char *name, namespace_t space);
#ifdef DEBUG
extern void dump_type(struct type *t);
#endif

/*
 * note that at the same scope, you can have
 * multiple instances of the same name with different namespaces.
 * this is a container for types, functions, variables, constants, and fields
 */
struct name {
    char *name;
    namespace_t space;
    struct name *next;		// all names in same container
    struct type *type;
    char visibility;
    char sclass;
    int offset;				// if inside a struct
    int bitoff;
    int width;
    struct expr *init;      // value of constant or initializer
    struct stmt *body;      // function body
    int flags;
#define V_BITFIELD  0x01
};

#define MAXBITS 32          // maximum size of bitfield

extern struct name *new_name(char *name, struct type *t, namespace_t space);
extern void destroy_name(struct name *s);
extern struct name *lookup_name(char *name, namespace_t space);
extern struct name *lookup_element(char *name, struct type *t);
#ifdef DEBUG
extern void dump_name(struct name *s);
#endif

void parse();

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

/* cc1.c */
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
extern unsigned char lookupc(char *s, char c);
extern void hexdump(char *tag, char *s, int len);
int iswhite(char c);
char *bitdef(unsigned char v, char **defs);
int quoted_string(char *d, char *s);
int longout(char *d, long v);

/* declare.c */
extern struct name *declare(struct type **btp);

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
int open(char *filename, int mode);
int close(int fd);
int creat(char *filename, int mode);
void perror(char *msg);
void exit(int exitcode);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);
long strtol(char *str, char **endptr, int base);
void bcopy(void *src, void *dst, int len);
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
