/*
 * AST serialization for second pass - binary format
 */
#include "cc1.h"
#include <unistd.h>

/* Forward declarations */
extern int analyzeFunc(struct name *func);  /* regalloc.c */

/* Current function's locals from phase 1 - for frm_off/reg lookup */
static struct name *curFuncLocals = NULL;

/*
 * Look up a local variable by name in phase 1 captured locals.
 * Returns the captured name struct with correct frm_off/reg, or NULL.
 */
static struct name *
findInLocals(char *name)
{
	struct name *n;
	for (n = curFuncLocals; n; n = n->next) {
		if (strcmp(n->name, name) == 0)
			return n;
	}
	return NULL;
}

/*
 * Get size suffix for memory operations based on type
 * Returns: 'b' (byte), 's' (short/int), 'l' (long),
 * 'f' (float), 'd' (double), 'v' (void)
 * Uppercase B/S/L for unsigned types
 * Pointers use 's' since they're 16-bit on Z80
 */
static char
typeSfx(struct type *t)
{
	char c;
	if (!t)
		return 's';  /* default to short */

	if (t->flags & TF_POINTER)
		return 's';  /* pointers are 16-bit like short */

	/* Check primitive types by size */
	if (t->size == 0)
		return 'v';  /* void */
	else if (t->size == 1)
		c = 'b';  /* char/byte */
	else if (t->size == 2)
		c = 's';  /* short/int */
	else if (t->size == 4) {
		if (t->flags & TF_FLOAT)
			return 'f';  /* float/double */
		c = 'l';  /* long */
	} else
		c = 's';  /* default to short */

	/* Uppercase for unsigned */
	if (t->flags & TF_UNSIGNED)
		c = c - 'a' + 'A';
	return c;
}

/* Helper: build label name from base+suffix */
static char lblBuf[16];

static char *
mkLbl(char *base, char *suffix)
{
	fmtstr(lblBuf, "%s%s", base, suffix);
	return lblBuf;
}

void
emitLabel(char *base, char *suffix)
{
	emit1('L');
	emitS(mkLbl(base, suffix));
}

void
emitGoto(char *base, char *suffix)
{
	emit1('G');
	emitS(mkLbl(base, suffix));
}

void emitExpr(struct expr *e);  /* forward decl */

/*
 * Count intermediate labels needed for short-circuit && and || in condition.
 * Each && or || that's not at the top of its kind needs a label.
 */
int
cntCondLbls(struct expr *e)
{
	int cnt = 0;
	if (!e) return 0;
	/* Count in children first */
	cnt += cntCondLbls(e->left);
	cnt += cntCondLbls(e->right);
	/* && and || each need one label for short-circuit */
	if (e->op == LAND || e->op == LOR)
		cnt++;
	return cnt;
}

/*
 * Helper: emit child expression (if non-null)
 */
static void
emitChild(struct expr *e)
{
	if (e)
		emitExpr(e);
}

/*
 * Output an expression in paren-free format
 * Constants: just the value (hex with dot)
 * Symbols: $name
 * Binary ops: op width left right
 * Unary ops: op width operand
 * Memory ops annotated with size: Mb expr, =l lvalue rvalue
 * Empty/null expression: _
 */
void
emitExpr(struct expr *e)
{
	/* Hoisted locals for stack reuse */
	struct name *np;
	struct expr *left, *right, *ep;
	struct type *type;
	unsigned char op, uc;
	char fullname[32], c;
	int n;

	if (!e) {
		emit1('_');
		return;
	}

	op = e->op;
	left = e->left;
	right = e->right;
	type = e->type;

	switch (op) {
	case CONST:
		emit1(AST_CONST);
		emit1(typeSfx(type));
		emit4(e->v);
		break;

	case SYM:
		np = (struct name *)e->var;
		/* Local variables: emit LOCALVAR/REGVAR directly */
		if (np->level > 1 && !(np->sclass & SC_EXTERN)) {
			/* Look up frm_off/reg from phase 1 captured locals */
			struct name *local = findInLocals(np->name);
			char reg = local ? local->reg : np->reg;
			char off = local ? local->frm_off : np->frm_off;
			if (reg) {
				emit1(REGVAR);
				emit1(typeSfx(e->type));
				emit1(reg);
			} else {
				emit1(LOCALVAR);
				emit1(typeSfx(e->type));
				emit1(off);
			}
			break;
		}
		/* extern/global get underscore prefix */
		if ((np->sclass & SC_EXTERN) ||
		    (np->level == 1 && !(np->sclass & SC_STATIC)))
			fmtstr(fullname, "_%s", np->name);
		else if (np->static_id)
			fmtstr(fullname,
			    "%c%d", np->sclass & SC_STATIC ? 'S' : 'L', np->static_id - 1);
		else
			fmtstr(fullname, "%s", np->name);
		emit1(SYM);
		emitS(fullname);
		break;

	case STRING:
		/* String literals - reference by name (already emitted in phase 1) */
		np = (struct name *)e->var;
		emit1(SYM);
		emitS(np->name);
		break;

	case CALL:
		/* Function call: CALL type count func arg1 arg2 ... */
		n = 0;
		c = typeSfx(type);
		/* Count arguments from the expression tree */
		for (ep = right; ep; ep = ep->next)
			n++;
		emit1(CALL);
		emit1(c);
		emit1(n);
		emitChild(left);
		for (ep = right; ep; ep = ep->next)
			emitChild(ep);
		break;


	case INCR:
	case DECR:
		/* Increment/decrement operators: emit with increment amount */
		n = 1;
		c = typeSfx(type);
		if (op == INCR)
			uc = (e->flags & E_POSTFIX) ? POSTINC : PREINC;
		else
			uc = (e->flags & E_POSTFIX) ? POSTDEC : PREDEC;
		if (type && (type->flags & TF_POINTER) && type->sub)
			n = type->sub->size;
		emit1(uc);
		emit1(c);
		emitChild(left);
		emit2(n);
		break;

	case BFEXTRACT:
		/* Bitfield extract: offset width addr */
		np = (struct name *)e->var;
		emit1(BFEXTRACT);
		emit1(np ? np->bitoff : 0);
		emit1(np ? np->width : 0);
		emitChild(left);
		break;

	case BFASSIGN:
		/* Bitfield assign: offset width addr value */
		np = (struct name *)e->var;
		emit1(BFASSIGN);
		emit1(np ? np->bitoff : 0);
		emit1(np ? np->width : 0);
		emitChild(left);
		emitChild(right);
		break;

	case QUES:
		/* Ternary: QUES width cond then else - flatten the COLON node */
		emit1(QUES);
		emit1(typeSfx(type));
		emitChild(left);
		emitChild(right->left);
		emitChild(right->right);
		break;

	case INITLIST:
		/* Nested initializer list - emit contents */
		n = 0;
		for (ep = left; ep; ep = ep->next)
			n++;
		emit1(BEGIN);
		emit1(n);
		for (ep = left; ep; ep = ep->next)
			emitExpr(ep);
		emit1(END);
		break;

	case DEREF:
		/* Optimize: *++p -> (++p, *p) using comma operator */
		if ((left->op == INCR || left->op == DECR) &&
		    !(left->flags & E_POSTFIX)) {
			emit1(COMMA);
			emit1(typeSfx(type));
			emitExpr(left);
			emit1(DEREF);
			emit1(typeSfx(type));
			emitExpr(left->left);
			break;
		}
		/* fall through - standard unary */

	case BANG:
	case NEG:
	case TWIDDLE:
		/* Unary operators */
		emit1(op);
		emit1(typeSfx(type));
		emitChild(left);
		break;

	case GT:
	case GE:
		/* Normalize GT/GE to LT/LE by swapping operands */
		emit1(op == GT ? LT : LE);
		emit1(typeSfx(type));
		emitChild(right);
		emitChild(left);
		break;

	default:
		/* All operators get width suffix */
		emit1(op);
		emit1(typeSfx(type));
		emitChild(left);
		emitChild(right);
		break;
	}
}

/* Emit function parameter declarations */
static void
emitPrmDecls(struct type *functype, struct name *locals)
{
	struct name *param, *local, *found;

	if (!functype || !(functype->flags & TF_FUNC))
		return;
	for (param = functype->elem; param; param = param->next) {
		if (param->type->size == 0)
			continue;
		found = NULL;
		if (param->name[0])
			for (local = locals; local; local = local->next)
				if (strcmp(local->name, param->name) == 0) {
					found = local;
					break;
				}
		emit1('d');
		emit1(typeSfx(param->type));
		emitS(param->name[0] ? param->name : "_");
		emit1(found ? found->reg : 0);
		emit1(found ? (unsigned char)found->frm_off : 0);
	}
}

/* Emit local variable declarations */
static void
emitLocals(struct name *locals)
{
	struct name *local;
	char lbuf[32];

	for (local = locals; local; local = local->next) {
		if (local->kind == funarg)
			continue;
		if (local->sclass & SC_STATIC)
			fmtstr(lbuf, "S%d", local->static_id - 1);
		else if (local->static_id)
			fmtstr(lbuf, "L%d", local->static_id - 1);
		else
			fmtstr(lbuf, "%s", local->name);
		emit1('d');
		emit1(typeSfx(local->type));
		emitS(lbuf);
		emit1(local->reg);
		emit1((unsigned char)local->frm_off);
	}
}

/*
 * Output a global asm block - write directly to assembly file
 */
void
emitGlobalAsm(char *text)
{
	/* Phase 1: don't emit */
	if (phase == 1)
		return;

	if (!text)
		return;
	asmLine(text);
}

/*
 * Output function header in AST format
 * Format: F rettype name param_count local_count frm_size params... locals...
 */
void
emitFuncPre(struct name *func)
{
	char func_name[32];
	char frm_size, param_count, local_count;
	struct name *n;

	if (!func)
		return;

	frm_size = analyzeFunc(func);
	curFuncLocals = func->u.locals;

	/* Count params and locals first */
	param_count = local_count = 0;
	if (func->type->flags & TF_FUNC)
		for (n = func->type->elem; n; n = n->next)
			if (n->type->size > 0)
				param_count++;
	for (n = func->u.locals; n; n = n->next)
		if (n->kind != funarg)
			local_count++;

	/* Emit function header */
	if (func->sclass & SC_STATIC)
		fmtstr(func_name, "S%d", func->static_id - 1);
	else
		fmtstr(func_name, "_%s", func->name);
	emit1('F');
	emit1(func->type->sub ? typeSfx(func->type->sub) : 'v');
	emitS(func_name);
	emit1(param_count);
	emit1(local_count);
	emit1(frm_size);

	/* Emit declarations */
	emitPrmDecls(func->type, func->u.locals);
	emitLocals(func->u.locals);

	/* Block header */
	emit1('B');
	emit1(0);
	emit1(popFuncCnt());
}

/*
 * Emit an initializer list (linked via next pointers)
 * Used for array/struct initializers like {1, 2, 3}
 * elem_type: type of array elements for width annotation
 * Format: [ width count. items...
 */
/*
 * Emit struct initializer with field types from struct definition
 */
static void emitInit(struct expr *init, struct type *type);
static void emitInitList(struct expr *init, struct type *elem_type);

static void
emitStInit(struct expr *init, struct type *stype)
{
	struct expr *val;
	struct name *field, *fields[32];
	int count = 0, nfields = 0, i;

	/* Count initializer items */
	for (val = init; val; val = val->next)
		count++;

	/* Build forward-order field array (struct elem list is reversed) */
	if (stype && (stype->flags & TF_AGGREGATE)) {
		for (field = stype->elem; field && nfields < 32; field = field->next)
			fields[nfields++] = field;
	}

	emit1(BEGIN);
	emit1(count);

	/* Emit each initializer with corresponding field's type */
	i = nfields - 1;  /* Start from last field (first in source order) */
	for (val = init; val; val = val->next) {
		field = (i >= 0) ? fields[i--] : NULL;
		emitInit(val, field ? field->type : NULL);
	}
	emit1(END);
}

/*
 * Recursively emit an initializer with expected type
 * type: expected type from declaration (array element type or struct field type)
 */
static void
emitInit(struct expr *init, struct type *type)
{
	if (init->op == INITLIST) {
		/* Nested aggregate - type tells us struct vs array */
		if (type->flags & TF_AGGREGATE) {
			emitStInit(init->left, type);
		} else if (type->flags & TF_ARRAY) {
			emitInitList(init->left, type->sub);
		}
	} else if (init->op == STRING && type &&
		   (type->flags & TF_ARRAY) && type->sub &&
		   type->sub->size == 1) {
		/* String literal initializing char array - emit bytes inline */
		unsigned char *str = (unsigned char *)init->v;
		int slen = str ? str[0] : 0;
		int arrlen = type->count;
		int i;
		/* Mark string as emitted so it won't be emitted separately */
		((struct name *)init->var)->emitted = 1;
		emit1(LBRACK);
		emit1('b');
		emit1(arrlen);
		for (i = 0; i < arrlen; i++) {
			int b = (i < slen) ? str[i + 1] : 0;
			emit1('#');
			emit1('b');
			emit4(b);
		}
		emit1(RBRACK);
	} else if (init->op == CONST && type) {
		/* Scalar constant - use declared type */
		struct type *saved = init->type;
		init->type = type;
		emitExpr(init);
		init->type = saved;
	} else {
		emitExpr(init);
	}
}

static void
emitInitList(struct expr *init, struct type *elem_type)
{
	struct expr *item;
	char width, count = 0;

	/* Count items and get element width */
	for (item = init; item; item = item->next)
		count++;
	width = typeSfx(elem_type);

	emit1(LBRACK);
	emit1(width);
	emit1(count);
	for (item = init; item; item = item->next) {
		emitInit(item, elem_type);
	}
	emit1(RBRACK);
}

/*
 * Emit a single string literal as assembly
 * Output: label: .db 'c','c',...,0
 * Only emits during phase 2; global strings are handled by streaming.
 */
void
emitStrLit(struct name *strname)
{
	cstring str;
	unsigned char len;
	unsigned char *data;
	int i;

	/* Only emit in phase 2 */
	if (phase != 2)
		return;

	if (!strname || !strname->u.init || strname->u.init->op != STRING)
		return;

	/* Only emit once */
	if (strname->emitted)
		return;
	strname->emitted = 1;

	str = (cstring)strname->u.init->v;
	if (!str)
		return;

	len = (unsigned char)str[0];
	data = (unsigned char *)str + 1;

	/* Output label and string bytes */
	asmLabel(strname->name);
	for (i = 0; i < len; i++)
		asmDb(data[i]);
	asmDb(0);  /* null terminator */
}

/*
 * Output an uninitialized global variable declaration
 * Initialized globals are handled by streaming in doInitlzr()
 */
void
emitGv(struct name *var)
{
	char fullname[32];
	int size;

	/* Phase 1: don't emit, just build symbol table */
	if (phase == 1)
		return;

	if (!var)
		return;

	/* Build name: static uses S<id>, public gets underscore prefix */
	if (var->sclass & SC_STATIC)
		fmtstr(fullname, "S%d", var->static_id - 1);
	else
		fmtstr(fullname, "_%s", var->name);

	/* Calculate total size */
	if (var->type->flags & TF_ARRAY)
		size = var->type->count * var->type->sub->size;
	else
		size = var->type->size;

	/* Emit .globl for non-static */
	if (!(var->sclass & SC_STATIC))
		asmGlobl(fullname);

	/* Uninitialized variable - use .bss */
	asmLine("\t.bss");
	asmLabel(fullname);
	asmDs(size);
	asmLine("\t.text");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
