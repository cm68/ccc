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

/*
 * Compiler limits - adjust these for larger files
 */
#define MAXNAMES 10000         // symbol table entries
#define MAXPARMS 10            // macro parameters
#define TBSIZE 1024            // text buffer size for includes/macros
#define STRBUFSIZE 128         // string/symbol/identifier buffer
#define MAXSYMLEN 32           // maximum symbol/identifier length
#define PSIZE 80               // max string containing bitdefs
#define MAX_DECL_INI 32      // local variable initializers
#define MAXBITS 32             // maximum size of bitfield

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
	unsigned char flags;
#define	E_CONST     0x01
#define E_RESOLVED	0x02
#define	E_FUNARG    0x04
#define E_POSTFIX   0x08
	unsigned char op;
	struct expr *left;
	struct expr *right;
	struct expr *up;
	struct expr *prev;
	struct expr *next;

	struct type *type;
    /* for STRING expressions, cast to (struct name *) to get synthetic name */
	struct var *var;      

	unsigned long v;
	unsigned char location;
	unsigned char regs;
	struct stmt *stmt;
	struct inst *inst;
};

/*
 * Operator precedence levels for expression parsing
 * Lower numbers bind tighter (higher precedence)
 * When parseExpr(N) encounters operator with priority >= N, it stops
 */
#define	PRI_ALL        0   /* parse all operators regardless of precedence */

#define OP_PRI_NONE    0   /* not an operator */
#define OP_PRI_PRIM 1   /* postfix/member access: . -> [] () */
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

extern struct expr *mkexpr(unsigned char op, struct expr *left);
extern struct expr *mkexprI(unsigned char op, struct expr *left,
    struct type *type, unsigned long v, int flags);
extern struct expr *cfold(struct expr *e);
extern struct expr *parseExpr(unsigned char priority, struct stmt *);
unsigned long parseConst(unsigned char priority);
extern struct expr *newExpr(unsigned char op);
extern void frExp(struct expr *e);

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
	unsigned char op;
	int flags;
	char *label;
	/* extended fields used by parser */
	struct stmt *chain;     /* child / body statement */
	struct stmt *otherwise; /* else branch */
	struct expr *middle;    /* for for-loop middle expression */
	struct name *function;  /* owning function */
	struct name *locals;    /* linked list of local variables in this scope */
};

extern struct stmt *newStmt(unsigned char op, struct expr *left);
extern void frStm(struct stmt *s);
extern void frStmt(struct stmt *s);
extern void emitFunction(struct name *func);
extern void emitGlobalAsm(struct stmt *st);
extern void emitGv(struct name *var);
extern void emitStrLit(struct name *strname);

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
	unsigned char size;		// how big is one of me (0-255)
	int count;		    	// if we are an array, how many
	struct name *elem;		// element list (struct members, function parameters)
    struct type *sub;		// pointer to, array of, function return type
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
#define TF_VARIADIC     0x80    // for functions: has ... parameter

/* legacy aliases used by older parser code */
#define T_FUNC TF_FUNC

extern struct type *getbasetype();
extern void initbasictype(void);
struct type *getType(char flags, struct type *sub, int count);
extern char compatFnTyp(struct type *t1, struct type *t2);

typedef enum { 
    prim, etag, stag, utag, var, elem, tdef, fdef, bitfield, funarg, local 
} kind;

/*
 * note that at the same scope, you can have
 * multiple instances of the same name with different namespaces.
 * this is a container for types, functions, variables, constants, and fields
 */
struct name {
	char *name;
    unsigned char is_tag;   // true if (enum, struct, union),
                            // else false for var,enum elem,typedef
    unsigned char emitted;  // true if string literal already emitted
    unsigned char level;    // lexical level (0+)
	struct name *next;		// all names in same container
	struct type *type;
	unsigned char sclass;   // storage class (SC_STATIC, SC_EXTERN, etc.)
	unsigned char offset;	// if inside a struct (0-255)
    unsigned char bitoff;   // bit offset (0-7)
    unsigned char width;    // bitfield width (1-32)
    char *mangled_name;     // mangled name for statics (NULL for others)
    union {
        struct expr *init;  // value of constant or initializer (for var)
        struct stmt *body;  // function body (for fdef)
    } u;
    kind kind;
};

/* Storage class specifiers (used in struct name sclass field) */
#define	SC_EXTERN	0x01
#define	SC_REGISTER	0x02
#define	SC_STATIC	0x04
#define	SC_CONST	0x08
#define	SC_VOLATILE	0x10
#define	SC_AUTO		0x20
#define	SC_TYPEDEF	0x40

extern struct name *newName(char *name, kind k, struct type *t,
    unsigned char is_tag);
extern void addName(struct name *n);
extern struct name *findName(char *name, unsigned char is_tag);
extern struct name *findElement(char *name, struct type *t);
extern void pushScope(char *name);
extern void popScope(void);

/* declare.c */
extern unsigned char lexlevel;
extern int lastname;
extern struct name **names;
extern struct type *types;
extern char *kindname[];
extern struct name *declInternal(struct type **btp, unsigned char struct_elem);
extern struct name *declare(struct type **btp);
extern char isCastStart(void);
extern struct type *parseTypeName(void);

extern struct type *chartype;
extern struct type *inttype;
extern struct type *longtype;
extern struct type *uchartype;
extern struct type *ushorttype;
extern struct type *ulongtype;
extern struct type *voidtype;

void parse();
void cleanupParse();

/* Global context for static variable name mangling */
extern char *srcFileRoot;
extern struct name *curFunc;
extern unsigned char staticCtr;  // file-global counter for static variable names
extern unsigned char shadowCtr;  // counter for shadowed locals

/* AST output control */
extern unsigned char astFd;         // where to write AST output

/* kw.c */
extern unsigned char cppkw[];
extern unsigned char ckw[];
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
extern char strbuf[];
extern char match(token_t t);
extern void gettoken();
extern void skipws1();
extern void skipws();
extern char issym();

/* io.c */
struct textbuf {
	int fd;                 // if == -1, macro buffer
	char *name;             // filename or macro name
	char *storage;          // data - free when done
	short offset;           // always points at nextchar.
	short valid;            // total valid in buffer
	short lineno;           // current line # in file
	short saved_column;     // saved column position for parent file
	struct textbuf *prev;	// a stack
};

extern void pushfile(char *name);
extern void insertmacro(char *name, char *macbuf);
extern void insertfile(char *name, int sysdirs);
extern void advance();
void iodump();
void ioinit();
void addInclude(char *name);
void cppFlush();
void cppOut(char *s, int len);

extern unsigned char curchar;
extern unsigned char nextchar;
extern int lineno;
extern char *filename;
extern int column;
extern char *sysIncPath;
extern struct textbuf *tbtop;
extern int exitCode;  /* Global exit code: 0=success, 1=errors occurred */

/* cc1.c */
extern void gripe(error_t errcode);
extern void fatal(error_t errcode);
extern void recover(error_t errcode, token_t skipto);
extern void need(token_t check, token_t skipto, error_t errcode);
extern void expect(token_t check, error_t errcode);
int main(int argc, char **argv);
void process(char *f);
void usage(char *complaint, char *p);

/* macro.c */
struct macro {
	unsigned char parmcount;
	char *name;
	char **parms;
	char *mactext;
	struct macro *next;
};
extern struct macro *macros;
extern char *macbuffer;
void macdefine(char *s);
void macundefine(char *s);
char macexpand(char *s);
struct macro *maclookup(char *s);
void addDefine(char *s);

/* util.c */
extern unsigned char lookupc(char *s, char c);
extern void hexdump(char *tag, char *s, int len);
char iswhite(unsigned char c);
char *bitdef(unsigned char v, char **defs);
int fdprintf(unsigned char fd, const char *fmt, ...);
int quotedString(char *d, char *s);
int longout(char *d, long v);
int controlify(char *d, unsigned char c);

/* declare.c / type.c */
extern struct name *declare(struct type **btp);

/* tokenlist.c */
/* debug options */
#ifdef DEBUG
#define VERBOSE(x) (verbose & (x))
extern char verbose;
#else
#define VERBOSE(x) (0)
#endif

/* lexer flags */
extern unsigned char tflags;
#define ONELINE     0x01
#define CPPFUNCS    0x02
#define ASM_BLOCK   0x04  /* Special mode for asm blocks */

/* Track newlines for statement separation */
extern unsigned char lineend;

#ifdef ASMKWLOOK
/* Test inline assembly with asm { } syntax */
#define ASMFUNC
#define ASMSTART asm {
#define ASMEND }
#endif

#if defined(CCC)
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
