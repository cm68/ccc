/*
 * data structures for the compiler (pass 1).
 *
 * we don't have any other non-generated includes.
 * so, everything is right here
 *
 * nested includes are a bit ugly, but it means that i can just include cc1.h
 */

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
typedef unsigned char boolean;

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
	unsigned char op;
	struct expr *left;
	struct expr *right;
	struct expr *up;
	struct expr *prev;
	struct expr *next;

	struct type *type;
	struct var *var;      /* for STRING expressions, cast to (struct name *) to get synthetic name */

	unsigned long v;
	char location;
	char cost;
	char regs;
	struct stmt *stmt;
	struct inst *inst;
};

/*
 * Operator precedence levels for expression parsing
 * Lower numbers bind tighter (higher precedence)
 * When parse_expr(N) encounters operator with priority >= N, it stops
 */
#define	PRI_ALL        0   /* parse all operators regardless of precedence */

#define OP_PRI_NONE    0   /* not an operator */
#define OP_PRI_PRIMARY 1   /* postfix/member access: . -> [] () */
#define OP_PRI_MULT    3   /* multiplicative: * / % */
#define OP_PRI_ADD     4   /* additive: + - */
#define OP_PRI_SHIFT   5   /* bitwise shift: << >> */
#define OP_PRI_REL     6   /* relational: < <= > >= */
#define OP_PRI_EQUAL   7   /* equality: == != */
#define OP_PRI_BITAND  8   /* bitwise AND: & */
#define OP_PRI_BITXOR  9   /* bitwise XOR: ^ */
#define OP_PRI_BITOR   10  /* bitwise OR: | */
#define OP_PRI_LOGAND  11  /* logical AND: && */
#define OP_PRI_LOGOR   12  /* logical OR: || */
#define OP_PRI_COND    13  /* conditional: ?: */
#define OP_PRI_ASSIGN  14  /* assignment: = += -= *= /= %= &= |= ^= <<= >>= */
#define OP_PRI_COMMA   15  /* comma: , */

extern struct expr *cfold(struct expr *e);
extern struct expr *parse_expr(char priority, struct stmt *);
int parse_const(char priority);
extern struct expr *new_expr(char op);
extern void destroy_expr(struct expr *e);

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
	/* extended fields used by parser */
	struct stmt *chain;     /* child / body statement */
	struct stmt *otherwise; /* else branch */
	struct expr *middle;    /* for for-loop middle expression */
	struct name *function;  /* owning function */
	struct name *locals;    /* linked list of local variables declared in this scope */
};

extern struct stmt *new_stmt(char op, struct expr *left);
extern void destroy_stmt(struct stmt *s);
extern void free_stmt(struct stmt *s);
extern void emit_function(struct name *func);
extern void emit_literals(void);
extern void emit_global_vars(void);

/* statement flags used in parse.c */
#define S_PARENT 0x01
#define S_LABEL  0x02
#define S_FUNC   0x04

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

/*
 * how big a pointer is 
 */
#define TS_PTR  2

/*
 * a function is an odd type, since it has a return type and argument types,
 * so there needs to be a collection of the latter.  also, a declaration of
 * an instance of a function type can and will have the arguments with different
 * names than a prototype or forward reference.  old style forward function 
 * declarations don't have argument types.
 */

/*
 * this is a handle for types.
 */
struct type {
	char *name;     		// the type name
	int size;		    	// how big is one of me
	int count;		    	// if we are an array, how many
	struct name *elem;		// element list (struct members, function parameters)
    struct type *sub;		// pointer to what, array of what, function return type
    unsigned char flags;
    struct type *next;
};
#define TF_AGGREGATE	0x01
#define TF_INCOMPLETE	0x02
#define TF_UNSIGNED		0x04
#define TF_FUNC         0x08
#define	TF_POINTER		0x10
#define	TF_ARRAY		0x20
#define	TF_FLOAT		0x40
#define TF_OLD          0x80    // no argument list - K&R

/* legacy aliases used by older parser code */
#define T_FUNC TF_FUNC

extern struct type *getbasetype();
extern void dump_type(struct type *t, int lv);
struct type *get_type(int flags, struct type *sub, int count);
extern int compatible_function_types(struct type *t1, struct type *t2);

typedef enum { prim, etag, stag, utag, var, elem, tdef, fdef, bitfield, funarg, local } kind;
/*
 * note that at the same scope, you can have
 * multiple instances of the same name with different namespaces.
 * this is a container for types, functions, variables, constants, and fields
 */
struct name {
	char *name;
    boolean is_tag;         // true if (enum, struct, union),
                            // else false for var,enum elem,typedef
    int level;              // lexical level
	struct name *next;		// all names in same container
	struct type *type;
	char sclass;            // storage class (SC_STATIC, SC_EXTERN, etc.)
	int offset;				// if inside a struct
    int bitoff;
    int width;
    char *mangled_name;     // mangled name for static variables (NULL for others)
    union {
        struct expr *init;  // value of constant or initializer (for var)
        struct stmt *body;  // function body (for fdef)
    };
    kind kind;
};

#define MAXBITS 32          // maximum size of bitfield

/* Storage class specifiers (used in struct name sclass field) */
#define	SC_EXTERN	0x01
#define	SC_REGISTER	0x02
#define	SC_STATIC	0x04
#define	SC_CONST	0x88
#define	SC_VOLATILE	0x10
#define	SC_AUTO		0x20
#define	SC_TYPEDEF	0x40

extern struct name *new_name(char *name, kind k, struct type *t, boolean is_tag);
extern void add_name(struct name *n);
extern struct name *lookup_name(char *name, boolean is_tag);
extern struct name *lookup_element(char *name, struct type *t);
extern void dump_name(struct name *s);
extern void push_scope(char *name);
extern void pop_scope(void);

/* declare.c */
extern int lexlevel;
extern struct name *declare_internal(struct type **btp, boolean struct_elem);
extern struct name *declare(struct type **btp);
extern int is_cast_start(void);
extern struct type *parse_type_name(void);

extern struct type *inttype;
extern struct type *chartype;
extern struct type *uchartype;

void parse();
void cleanup_parser();

/* Global context for static variable name mangling */
extern char *source_file_root;      // basename of current source file (without .c)
extern struct name *current_function; // current function being parsed
extern int static_counter;          // counter for statics in current function

/* AST output control */
extern int ast_fd;                  // where to write AST output (1=stdout or -o file)

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
};

extern struct token cur, next;

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
int fdprintf(int fd, const char *fmt, ...);
int quoted_string(char *d, char *s);
int longout(char *d, long v);

/* declare.c / type.c */
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
