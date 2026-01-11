/*
 * expr.h - expression tree definitions
 */
#ifndef EXPR_H
#define EXPR_H

#include "../cpp/lexeme.h"

/* destination values */
#define DEST_NONE	0	/* statement expression, discard result */
#define DEST_FLAGS	1	/* result needed in flags (conditionals) */
#define DEST_VALUE	2	/* result needed as value */

/* expression node */
typedef struct Expr {
	unsigned char	op;	/* operator (lexeme token) */
	unsigned char	width;	/* type: b/B/s/S/l/L/p/v */
	unsigned char	dest;	/* destination */
	struct Expr	*left;	/* left child (unary: only child) */
	struct Expr	*right;	/* right child (binary ops only) */
	union {
		long		val;	/* constant value */
		char		*name;	/* symbol name */
		struct {
			unsigned char	argc;	/* call: arg count */
		} call;
		struct {
			unsigned char	reg;	/* register number */
			char	off;	/* frame offset */
		} var;
		struct {
			unsigned short	amt;	/* inc/dec amount */
		} incdec;
		struct {
			unsigned char	off;	/* bitfield offset */
			unsigned char	wid;	/* bitfield width */
		} bf;
	} u;
} Expr;

/* tree builder functions */
Expr	*mkconst(char width, long val);
Expr	*mksym(char *name);
Expr	*mklocalvar(char width, char reg, char off);
Expr	*mkregvar(char width, char reg);
Expr	*mkindex(char width, char reg, char off);
Expr	*mkunary(int op, char width, Expr *child);
Expr	*mkbinary(int op, char width, Expr *left, Expr *right);
Expr	*mkcall(char width, int argc, Expr *func, Expr *args);
Expr	*mkincdec(int op, char width, Expr *e, int amt);
Expr	*mkbfext(char off, char wid, Expr *addr);
Expr	*mkbfass(char off, char wid, Expr *addr, Expr *val);

/* tree operations */
void	setdest(Expr *e, char dest);
void	freeexpr(Expr *e);
Expr	*rewrite(Expr *e);

/* parser */
Expr	*readexpr(void);

#ifdef DEBUG
void	dumpexpr(Expr *e);
char	*opname(int op);
#endif

#endif
