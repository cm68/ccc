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
#ifdef CCC
#include <unixio.h>
#endif

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
#define MAXPARMS 10            // macro parameters
#define TBSIZE 1024            // text buffer size for includes/macros
#define STRBUFSIZE 128         // string/symbol/identifier buffer
#define MAXSYMLEN 32           // maximum symbol/identifier length
#define PSIZE 80               // max string containing bitdefs
#define MAXBITS 32             // maximum size of bitfield
#define MAX_COUNTS 256         // pre-computed counts for streaming AST

/*
 * we just want the error symbols
 * error.h is generated, and contains actual error strings if DEF_ERRMSG.
 */
#undef DEF_ERRMSG
#include "error.h"

/*
 * Allocation counters for tracking memory usage
 */
#ifdef DEBUG
extern int nameAllocCnt;
extern int nameCurCnt;
extern int nameHighWater;
extern int exprAllocCnt;
extern int exprCurCnt;
extern int exprHighWater;
#endif

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
	struct expr *next;

	struct type *type;
    /* for STRING expressions, cast to (struct name *) to get synthetic name */
	struct var *var;      

	unsigned long v;
	unsigned char location;
	unsigned char regs;
};

/*
 * Operator precedence levels for expression parsing
 * Lower numbers bind tighter (higher precedence)
 * When parseExpr(N) encounters operator with priority >= N, it stops
 */
#define	PRI_ALL        0   /* parse all operators regardless of precedence */

#define OP_PRI_NONE     0   /* not an operator */
#define OP_PRI_PRIM     1   /* postfix/member access: . -> [] () */
#define OP_PRI_MULT     3   /* multiplicative: * / % */
#define OP_PRI_ADD      4   /* additive: + - */
#define OP_PRI_SHIFT    5   /* bitwise shift: << >> */
#define OP_PRI_REL      6   /* relational: < <= > >= */
#define OP_PRI_EQUAL    7   /* equality: == != */
#define OP_PRI_BITAND   8   /* bitwise AND: & */
#define OP_PRI_BITXOR   9   /* bitwise XOR: ^ */
#define OP_PRI_BITOR   10   /* bitwise OR: | */
#define OP_PRI_LOGAND  11   /* logical AND: && */
#define OP_PRI_LOGOR   12   /* logical OR: || */
#define OP_PRI_COND    13   /* conditional: ?: */
#define OP_PRI_ASSIGN  14   /* assignment: = += -= *= /= %= &= |= ^= <<= >>= */
#define OP_PRI_COMMA   15   /* comma: , */

extern struct expr *mkexpr(unsigned char op, struct expr *left);
extern struct expr *mkexprI(unsigned char op, struct expr *left,
    struct type *type, unsigned long v, int flags);
extern struct expr *parseExpr(unsigned char priority);
extern int isTypeToken(unsigned char t);
unsigned long parseConst(unsigned char priority);
extern void FreeExpr(struct expr *e);
extern struct expr *foldTree(struct expr *e);
extern unsigned short globalStrCtr;  /* string literal counter ("str") */

extern void statement(void);
extern void declaration(void);
extern struct name *capLocals(void);
extern void emitFuncPre(struct name *func);
extern void emitGlobalAsm(char *text);
extern void emitAsmStmt(char *text);
extern char *getAsmText(void);
extern void emitGv(struct name *var);
extern void emitExpr(struct expr *e);
extern int cntCondLbls(struct expr *e);
extern void emitLabel(char *base, char *suffix);
extern void emitGoto(char *base, char *suffix);

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
	unsigned char size;		// how big is one of me (0-255)
	int count;		    	// array: how many; function: 1 if elem is
							// a slim farg list (see below), else 0
	struct name *elem;		// element list (struct members, function parameters)
    struct type *sub;		// pointer to, array of, function return type
    unsigned char flags;
    struct type *next;
};

/*
 * parameter names in a plain declaration are meaningless; only the
 * types matter.  once a declarator turns out not to be a function
 * definition, slimFnArgs() replaces the funarg name entries in a
 * function type's elem list with these compact nodes and sets
 * count = 1 to mark the shape.  a definition's own type keeps full
 * struct name entries: parsefunc/emitPrmDecls/regalloc bind the
 * body's parameters by name.
 */
struct farg {
	struct type *type;
	struct farg *next;
};
extern void slimFnArgs(struct type *t);
#define TF_AGGREGATE	0x01
#define TF_INCOMPLETE	0x02
#define TF_UNSIGNED		0x04
#define TF_FUNC         0x08
#define	TF_POINTER		0x10
#define	TF_ARRAY		0x20
#define	TF_FLOAT		0x40
#define TF_VARIADIC     0x80    // for functions: has ... parameter

extern struct type *getbasetype();
extern unsigned char parseSclass();
extern int isBasicType(struct type *t);
struct type *getType(char flags, struct type *sub, int count);
extern char compatFnTyp(struct type *t1, struct type *t2);

/*
 * enum constants are lowered to #defines by cpp, so their names are
 * global: prefix with k to keep them clear of member/variable names.
 */
typedef enum {
    kprim, ketag, kstag, kutag, kvar, kelem, ktdef, kfdef, kbitfield,
    kfunarg, klocal
} kind;

/*
 * note that at the same scope, you can have
 * multiple instances of the same name with different namespaces.
 * this is a container for types, functions, variables, constants, and fields
 *
 * Fields ordered so basicnames[] static initializer needs no trailing zeros:
 * name, type, chain are always set; remaining fields zero from calloc/elision
 */
struct name {
	/* Non-zero in basicnames[] - put first for trailing zero elision */
	char name[16];          // symbol name (max 15 chars + null)
	struct type *type;
	struct name *chain;     // symbol table chain (most recent first)

	/* Zero in basicnames[], set dynamically elsewhere */
	kind kind;
	unsigned char level;    // lexical level (0+)
	unsigned char is_tag;   // true if (enum, struct, union)
	unsigned char sclass;   // storage class (SC_STATIC, SC_EXTERN, etc.)
	unsigned char emitted;  // true if string literal already emitted
	unsigned char static_id; // 0=normal, >0=static (emit as S<id-1>)
	struct name *next;		// all names in same container
	union {
		struct expr *init;  // value of constant or initializer (for var)
		struct name *locals; // local variables (for fdef)
	} u;
	/*
	 * struct members and bitfields (kind elem/bitfield) never reach
	 * register allocation, and vars/locals/funargs are never struct
	 * members, so the two field groups can share storage.
	 */
	union {
		struct {
			unsigned char offset;   // offset inside the struct (0-255)
			unsigned char bitoff;   // bit offset (0-7)
			unsigned char width;    // bitfield width (1-32)
		} m;
		struct {
			unsigned char ref_count;  // reference count (capped at 255)
			unsigned char agg_refs;   // struct member access count (for IX allocation)
			unsigned char reg;        // allocated register: 0=none, 1=B, 2=C, 3=BC, 4=IX
			unsigned char addr_taken; // true if address taken (can't use register)
			short frm_off;            // frame offset: params positive, locals
			                          // negative; arrays sit below the save
			                          // slots and may pass -128
		} r;
	} w;
};

/*
 * Storage class specifiers (used in struct name sclass field)
 * these are in the same order as the storage class lexemes
 */
#define	SC_TYPEDEF	0x01
#define	SC_AUTO		0x02
#define	SC_EXTERN	0x04
#define	SC_STATIC	0x08
#define	SC_REGISTER	0x10

/* Register allocation values (used in struct name reg field) */
#define REG_NONE    0   /* Not allocated to a register (on stack) */
#define REG_B       1   /* B register (byte) */
#define REG_C       2   /* C register (byte) */
#define REG_BC      3   /* BC register pair (word) */
#define REG_IX      4   /* IX index register (struct pointer) */

extern struct name *newName(char *name, kind k, struct type *t,
    unsigned char is_tag);
extern struct name *addName(struct name *n);
extern struct name *findName(char *name, unsigned char is_tag);
extern struct name *findElement(char *name, struct type *t);
extern void pushScope(char *name);
extern void popScope(void);

/* declare.c */
extern unsigned char lexlevel;
extern struct name *names;
extern struct name *deadNames;
extern struct type *types;
extern char *kindname[];
extern struct name *declare(struct type **btp, unsigned char struct_elem);
extern char isCastStart(void);
extern struct type *parseTypeName(void);

extern struct type *chartype;
extern struct type *inttype;
extern struct type *longtype;
extern struct type *uchartype;
extern struct type *ushorttype;
extern struct type *ulongtype;
extern struct type *voidtype;
extern struct type *floattype;

void parse();
void cleanupParse();

/*
 * Switch statement table tracking (phase 1)
 * Accumulates case values and labels for each switch statement.
 * Nested switches use swStack to track which switch is current.
 * Dynamically allocated - no fixed limits on switch count.
 */
#define MAX_SWDEPTH 8       /* max switch nesting */
#define SW_INIT_CASES 8     /* initial cases per switch */

struct swcase {
    long value;             /* case constant value */
    unsigned char is_default; /* 1 if default, 0 if case */
    unsigned char stmts;    /* statement count for this case section */
};

struct swtab {
    struct swcase *cases;   /* allocated case array for this switch */
    unsigned char count;    /* number of cases */
    unsigned char capacity; /* allocated size of cases array */
    unsigned char num;      /* switch number (for labels) */
    unsigned char base_stmts; /* stmt_count at start of current case */
    unsigned char final_cnt;  /* stmt_count when switch body ends */
    unsigned char emitIdx;  /* phase 2: current case being emitted */
};

/* Dynamic switch list */
extern struct swtab *swList;
extern unsigned char swCount;       /* number of switches in function */
extern unsigned char swCapacity;    /* allocated size of swList */
extern unsigned char swStack[];     /* nesting stack (indices into swList) */
extern unsigned char swDepth;       /* nesting depth */
extern unsigned char swEmitIdx;     /* phase 2: next switch to emit */
extern unsigned char swEmitStack[]; /* phase 2: stack of switch indices */
extern unsigned char swEmitDepth;   /* phase 2: emit stack depth */

void resetSwitch(void);             /* reset for new function */
void pushSwitch(void);              /* enter switch statement */
void popSwitch(void);               /* exit switch statement */
void addCase(long value, unsigned char stmt_cnt);  /* add case to current switch */
void addDefault(unsigned char stmt_cnt);           /* add default to current switch */
void finishCase(unsigned char stmt_cnt);           /* finalize current case stmt count */

/* Global context for static variable name mangling */
extern struct name *curFunc;
extern unsigned char staticCtr;  // file-global counter for static variable names
extern unsigned char shadowCtr;  // counter for shadowed locals

/* AST output control */
extern unsigned char astFd;         // where to write AST output
extern unsigned char asmFd;         // where to write global data assembly

/* Two-phase parsing control */
extern unsigned char phase;         // 1 = build symbol table, 2 = emit AST

/*
 * Count storage for streaming AST emission
 * Phase 1 computes counts (args, cases, stmts), phase 2 retrieves them.
 * Reset between functions, flip after phase 1 for LIFO retrieval.
 */
void pushCount(char c);
char popCount(void);
void resetCounts(void);
void resetCountIdx(void);  /* Reset read pointer for phase 2 */

/* Block statement counts (phase 1 -> phase 2) */
void enterBlkCnt(void);  /* call when entering block in phase 1 */
void pushBlkCnt(unsigned char n);
unsigned char popBlkCnt(void);
void flipBlkCnts(void);  /* prepare for phase 2 */
void resetBlkCnts(void);
void pushFuncCnt(unsigned char n);
unsigned char popFuncCnt(void);
void resetFuncIdx(void);

/* lexread.c - lexeme stream reader */
struct token {
	token_t type;
	union {
		long numeric;
		float fval;
		char *name;
		cstring str;
	} v;
};

extern struct token cur, next;
extern char strbuf[];
extern char match(token_t t);
extern void gettoken(void);
extern void lexOpen(char *filename);
extern void lexClose(void);
extern void lexRewind(void);

/* Error reporting */
extern int lineno;
extern char *filename;
extern int exitCode;

/* cc1.c */
extern void gripe(error_t errcode);
extern void fatal(error_t errcode);
extern char *galloc(unsigned int size);
extern char *permalloc(unsigned int n);
extern void recover(error_t errcode, token_t skipto);
extern void need(token_t check, token_t skipto, error_t errcode);
extern void expect(token_t check, error_t errcode);
int main(int argc, char **argv);
void process(char *f, char *o1, char *o2);
void usage(char *complaint);

/* util.c */
#ifndef CCC
char *bitdef(unsigned char v, char **defs);
#endif
char *fmtstr(char *buf, char *fmt, ...);
#ifdef DEBUG
int fdprintf(unsigned char fd, char *fmt, ...);
#endif
void emit1(unsigned char b);
void emit2(unsigned short w);
void emit4(unsigned long l);
void emitN(char *s, unsigned char len);
void emitS(char *s);
void emitRaw(char *s, unsigned short len);

/* Assembly output helpers (write to asmFd) */
void asmStr(char *s);
void asmLine(char *s);
void asmLabel(char *name);
void asmDb(int val);
void asmDbStr(unsigned char *data, int len);
void asmDw(int val);
void asmDwSym(char *name);
void asmDs(int size);

/* Segment tracking */
#define SEG_TEXT 0
#define SEG_DATA 1
#define SEG_BSS  2
void setSeg(unsigned char seg);

/* debug options */
#ifdef DEBUG
#define VERBOSE(x) (verbose & (x))
extern short verbose;
#else
#define VERBOSE(x) (0)
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
