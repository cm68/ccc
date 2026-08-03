/*
 * filtenum.c - Enum lowering filter
 *
 * Enum constants are glorified #defines: each constant is entered into
 * the macro table as its numeric value, so every later use expands to a
 * NUMBER before the stream leaves the lexer.  The enum type itself is
 * rewritten to unsigned char (pass1's model of an enum), so pass1 never
 * sees the ENUM keyword at all:
 *
 *   typedef enum { A, B = 5 } kind;  ->  typedef unsigned char kind;
 *   enum tag { X, Y } v;             ->  unsigned char v;
 *   enum tag *p;                     ->  unsigned char *p;
 *   enum { P, Q };                   ->  (nothing)
 *
 * Because the constants become ordinary macros, their names are global
 * for the rest of the file and must not be reused as identifiers
 * (members, locals, tags) - the expansion is textual, exactly like
 * #define.  Fix the source if it depends on scoped enum constants.
 *
 * Values accept NUMBER, previously defined enum constants (already
 * expanded to NUMBER by the macro engine), unary - and ~, parens, and
 * left-associative + - * with * binding tighter.  Anything richer
 * gripes ER_C_EV.
 *
 * Pipeline: lex -> filtenum -> filtknr -> filtdecl -> filtbrace ->
 * filtctrl -> emit
 */

#include <unistd.h>
#include <stdlib.h>
#include "cpp.h"
#include "lexeme.h"

extern void addDefine(char *s);

static void (*up)(struct token *);
static struct pendbuf epend;

void
filtenum_init(void (*upstream)(struct token *))
{
	up = upstream;
	pend_setup(&epend, 8);
}

long enum_expr(struct token *t);

/* t holds the current lookahead on entry and exit */
long
enum_prim(struct token *t)
{
	long v;

	if (t->type == MINUS) {
		up(t);
		return -enum_prim(t);
	}
	if (t->type == TWIDDLE) {
		up(t);
		return ~enum_prim(t);
	}
	if (t->type == LPAR) {
		up(t);
		v = enum_expr(t);
		if (t->type == RPAR)
			up(t);
		else
			gripe(ER_C_EV);
		return v;
	}
	/* an enum constant is an int however the value was spelled */
	if (t->type == NUMBER || t->type == LNUMBER) {
		v = t->v.numeric;
		up(t);
		return v;
	}
	gripe(ER_C_EV);
	if (t->type != END && t->type != E_O_F)
		up(t);
	return 0;
}

long
enum_term(struct token *t)
{
	long v = enum_prim(t);

	while (t->type == STAR || t->type == TIMES) {
		up(t);
		v *= enum_prim(t);
	}
	return v;
}

long
enum_expr(struct token *t)
{
	long v = enum_term(t);
	unsigned char op;

	while (t->type == PLUS || t->type == MINUS) {
		op = t->type;
		up(t);
		if (op == PLUS)
			v += enum_term(t);
		else
			v -= enum_term(t);
	}
	return v;
}

void
filtenum(struct token *out)
{
	struct token t;
	struct token syn;
	char def[48];
	char *p;
	long val;

	if (pend_has(&epend)) {
		pend_pop(&epend, out);
		return;
	}

	up(&t);
	while (t.type == ENUM) {
		up(&t);				/* consume 'enum' */
		if (t.type == SYM)
			up(&t);			/* tag name: documentation only */

		if (t.type == BEGIN) {
			val = 0;
			up(&t);
			while (t.type != END && t.type != E_O_F) {
				if (t.type != SYM) {
					gripe(ER_C_EV);
					up(&t);
					continue;
				}
				/* build "NAME=" while the name is live */
				p = def;
				{
					char *n = t.v.name;
					while (*n && p < def + 32)
						*p++ = *n++;
				}
				*p++ = '=';
				up(&t);
				if (t.type == ASSIGN) {
					up(&t);
					val = enum_expr(&t);
				}
				fmtstr(p, "%ld", val);
				addDefine(def);
				val++;
				if (t.type == COMMA)
					up(&t);
			}
			if (t.type == END)
				up(&t);		/* consume '}' */
			else
				gripe(ER_C_EV);
		}

		/* t is now the token after the enum construct */
		if (t.type == SEMI) {
			/* bare "enum [tag] { ... };" - swallow entirely */
			up(&t);
			continue;
		}
		/* type reference: replace with 'unsigned char' */
		toksynth(out, UNSIGNED);
		toksynth(&syn, CHAR);
		pend_push(&epend, &syn);
		pend_push(&epend, &t);
		return;
	}
	tokcpy(out, &t);
}
