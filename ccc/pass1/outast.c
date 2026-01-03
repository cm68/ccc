/*
 * AST serialization for second pass - binary format
 */
#include "cc1.h"
#include <unistd.h>

/* Forward declarations */
static void emitTypeInfo(struct type *type);
extern int analyzeFunc(struct name *func);  /* regalloc.c */

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
	sprintf(lblBuf, "%s%s", base, suffix);
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
		if (e->var) {
			np = (struct name *)e->var;
			/* extern/global get underscore prefix */
			if ((np->sclass & SC_EXTERN) ||
			    (np->level == 1 && !(np->sclass & SC_STATIC)))
				sprintf(fullname, "_%s", np->name);
			else if (np->static_id)
				sprintf(fullname,
                    "%c%d", np->sclass & SC_STATIC ? 'S' : 'L', np->static_id - 1);
			else
				sprintf(fullname, "%s", np->name);
			emit1(SYM);
			emitS(fullname);
		} else {
			emit1(SYM);
			emitS("?");
		}
		break;

	case STRING:
		/* String literals - reference by name (already emitted in phase 1) */
		if (e->var) {
			np = (struct name *)e->var;
			/* Synthetic string names are local - no _ prefix */
			emit1(SYM);
			emitS(np->name);
		} else {
			/* Fallback to address if name not available */
			emit1('S');
			emit2(e->v);
		}
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

	case NARROW:
	case WIDEN:
		/* Cast operators with destination width annotation */
		c = typeSfx(type);
		emit1(op);  /* NARROW=206, WIDEN=207 */
		emit1(c);
		emitChild(left);
		break;
	case SEXT:
		c = typeSfx(type);
		emit1(SEXT);
		emit1(c);
		emitChild(left);
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
		if (right && right->op == COLON) {
			emitChild(right->left);
			emitChild(right->right);
		}
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
		if (left && (left->op == INCR || left->op == DECR) &&
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
		/* Greater than - normalize to < by swapping operands */
		emit1(LT);
		emit1(typeSfx(type));
		emitChild(right);
		emitChild(left);
		break;

	case GE:
		/* Greater or equal - normalize to <= by swapping */
		emit1(LE);
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

/*
 * Output type information for AST
 */
static void
emitTypeInfo(struct type *type)
{
	/* For arrays: a count elemsize */
	if (type->flags & TF_ARRAY) {
		int elemsize = type->sub ? type->sub->size : 0;
		emit1('a');
		emit2(type->count);
		emit2(elemsize);
		return;
	}

	/* For pointers: p */
	if (type->flags & TF_POINTER) {
		emit1('p');
		return;
	}

	/* For aggregates: r size */
	if (type->flags & TF_AGGREGATE) {
		emit1('r');
		emit2(type->size);
		return;
	}

	/* For primitives: size char */
	emit1(typeSfx(type));
}

/*
 * Count statements in a chain
 */
static char
countStmts(struct stmt *st)
{
	char count = 0;
	while (st) {
		count++;
		st = st->next;
	}
	return count;
}

/*
 * Output a statement in paren-free format
 * Each statement type has its own format with counted children
 */
static void
emitStmt(struct stmt *st)
{
	/* Hoisted locals for stack reuse */
	struct stmt *sp, *sp2, *sp3;
	int n, n2;

	if (!st)
		return;

	/* Output this statement */
	switch (st->op) {
	case BEGIN:
		n = countStmts(st->chain);
		/* Emit: B 00 stmt_count stmts...
		 * All locals hoisted to function prolog, so decl_count=0 */
		emit1('B');
		emit1(0);
		emit1(n);
		/* Emit statements */
		for (sp = st->chain; sp; sp = sp->next)
			emitStmt(sp);
		break;

	/* IF, WHILE, DO, FOR are emitted directly in phase 2 - no stmt nodes */

	case SWITCH:
		/* Switch: S has_label [label] case_count expr cases... */
		/* Case count was pre-computed in phase 1 and stored via pushCount */
		n = popCount();
#ifdef DEBUG
		fdprintf(2, "SWITCH: popCount=%d, emitting case_count=%d\n", n, n);
#endif
		emit1('S');
		emit1(st->label ? 1 : 0);
		if (st->label)
			emitS(st->label);
		emit1(n);
		emitExpr(st->left);
		for (sp = st->chain; sp; ) {
			if (sp->op == CASE || sp->op == DEFAULT) {
				sp2 = sp->next;
				n2 = 0;
				for (sp3 = sp2; sp3 && sp3->op != CASE && sp3->op != DEFAULT; sp3 = sp3->next)
					n2++;
				emit1(sp->op == CASE ? 'C' : 'O');
				emit1(n2);
				if (sp->op == CASE)
					emitExpr(sp->left);
				for (sp3 = sp2; sp3 && sp3->op != CASE && sp3->op != DEFAULT; sp3 = sp3->next)
					emitStmt(sp3);
				sp = sp2;
				while (sp && sp->op != CASE && sp->op != DEFAULT)
					sp = sp->next;
			} else {
				emitStmt(sp);
				sp = sp->next;
			}
		}
		break;

	case CASE:
		/* Case labels are handled by SWITCH - this shouldn't be called directly */
		emit1('C');
		emit1(0);
		emitExpr(st->left);
		break;

	case DEFAULT:
		/* Default labels are handled by SWITCH - this shouldn't be called directly */
		emit1('O');
		emit1(0);
		break;

	/* RETURN, BREAK, CONTINUE, GOTO, LABEL emitted directly in phase 2 */

	case EXPR:
		/* Convert postinc/postdec to preinc/predec since result unused */
		if (st->left && (st->left->op == INCR || st->left->op == DECR) &&
		    (st->left->flags & E_POSTFIX)) {
			st->left->flags &= ~E_POSTFIX;  /* Make it prefix */
		}
		emit1('E');
		emitExpr(st->left);
		break;

	case ';':
		emit1(';');
		break;

	case ASM:
		n = st->label ? strlen(st->label) : 0;
		emit1('A');
		emit2(n);
		if (n > 0)
			write(astFd, st->label, n);
		break;

	default:
		emit1('X');
		emit1(st->op);
		break;
	}
	/* Note: st->next is handled by caller (block counts statements) */
}

/*
 * Count function parameters
 */
static char
countParams(struct type *functype)
{
	char count = 0;
	struct name *param;
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (param->type && param->type->size == 0)
				continue;  /* skip void */
			count++;
		}
	}
	return count;
}

/*
 * Output function parameter declarations
 * Format: d suffix name reg off d suffix name reg off ...
 * reg is 1 byte: 0=none, 1=B, 2=C, 3=BC, 4=IX
 * off is 1 byte: signed frame offset (params positive, locals negative)
 */
static void
emitPrmDecls(struct type *functype, struct stmt *body)
{
	struct name *param, *local, *found;

	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			/* Skip void parameters - (void) means no params */
			if (param->type && param->type->size == 0)
				continue;
			/* Look up register/offset from body locals */
			found = NULL;
			if (body && body->locals && param->name[0]) {
				for (local = body->locals; local; local = local->next) {
					if (strcmp(local->name, param->name) == 0) {
						found = local;
						break;
					}
				}
			}
			/* Emit as: d suffix name reg off */
			emit1('d');
			emit1(typeSfx(param->type));
			if (param->name[0])
				emitS(param->name);
			else
				emitS("_");  /* anonymous parameter */
			emit1(found ? found->reg : 0);
			emit1(found ? (unsigned char)found->frm_off : 0);
		}
	}
}

/* Count local variables (non-params) */
static char
countLocals(struct stmt *body)
{
	struct name *local;
	char count = 0;

	if (!body || !body->locals)
		return 0;

	for (local = body->locals; local; local = local->next) {
		if (local->kind != funarg)
			count++;
	}
	return count;
}

/*
 * Emit local variable declarations (non-params) at function prolog
 * All locals are hoisted to function level - no scope tracking needed
 */
static void
emitLocals(struct stmt *body)
{
	struct name *local;

	if (!body || !body->locals)
		return;

	for (local = body->locals; local; local = local->next) {
		char lbuf[32];
		if (local->kind == funarg)
			continue;  /* params already emitted */
		if (local->sclass & SC_STATIC)
			sprintf(lbuf, "S%d", local->static_id - 1);
		else if (local->static_id)
			sprintf(lbuf, "L%d", local->static_id - 1);
		else
			sprintf(lbuf, "%s", local->name);
		emit1('d');
		emit1(typeSfx(local->type));
		emitS(lbuf);
		emit1(local->reg);
		emit1((unsigned char)local->frm_off);
	}
}

/*
 * Output a global asm block in AST format
 * Format: A len data (same as inline asm but at top level)
 */
void
emitGlobalAsm(char *text)
{
	int len;

	/* Phase 1: don't emit */
	if (phase == 1)
		return;

	if (!text)
		return;
	len = strlen(text);
	emit1('A');
	emit2(len);
	if (len > 0)
		write(astFd, text, len);
}

/*
 * Output function header in AST format (everything before statements)
 * Format: F rettype name param_count local_count frm_size params... locals...
 * Called before streaming statements.
 */
void
emitFuncPre(struct name *func)
{
	char func_name[32];
	char ret_suffix;
	char frm_size, param_count, local_count;
	char stmt_count;

	if (!func || !func->u.body)
		return;

	/* Analyze variable usage and allocate registers BEFORE emission */
	frm_size = analyzeFunc(func);

	/* Static functions use S<id>, public get underscore prefix */
	if (func->sclass & SC_STATIC)
		sprintf(func_name, "S%d", func->static_id - 1);
	else
		sprintf(func_name, "_%s", func->name);

	/* Get return type suffix (void uses 'v') */
	if (func->type && func->type->sub)
		ret_suffix = typeSfx(func->type->sub);
	else
		ret_suffix = 'v';  /* void */

	emit1('F');
	emit1(ret_suffix);
	emitS(func_name);

	/* Output param count, local count, and frame size */
	param_count = func->type ? countParams(func->type) : 0;
	local_count = countLocals(func->u.body);
	emit1(param_count);
	emit1(local_count);
	emit1(frm_size);

	/* Emit parameter declarations */
	if (func->type)
		emitPrmDecls(func->type, func->u.body);

	/* Emit local variable declarations (hoisted from all blocks) */
	emitLocals(func->u.body);

	/* Output block header with statement count from phase 1 */
	stmt_count = popFuncCnt();
	emit1('B');
	emit1(0);
	emit1(stmt_count);
}

/*
 * Emit a single statement (called during streaming)
 * Wraps the internal emitStmt for external use.
 */
void
emitOneStmt(struct stmt *st)
{
	if (st)
		emitStmt(st);
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

/*
 * Pre-scan initializer to mark strings that will be inlined.
 * Must be called before emission so emitStrLit knows to skip them.
 */
static void markInlStr(struct expr *init, struct type *type);

static void
markInlStIn(struct expr *init, struct type *stype)
{
	struct expr *val;
	struct name *field, *fields[32];
	int nfields = 0, i;

	if (stype && (stype->flags & TF_AGGREGATE)) {
		for (field = stype->elem; field && nfields < 32; field = field->next)
			fields[nfields++] = field;
	}
	i = nfields - 1;
	for (val = init; val; val = val->next) {
		field = (i >= 0) ? fields[i--] : NULL;
		markInlStr(val, field ? field->type : NULL);
	}
}

static void
markInlStr(struct expr *init, struct type *type)
{
	if (init->op == INITLIST) {
		if (type && (type->flags & TF_AGGREGATE)) {
			markInlStIn(init->left, type);
		} else if (type && (type->flags & TF_ARRAY)) {
			struct expr *item;
			for (item = init->left; item; item = item->next)
				markInlStr(item, type->sub);
		}
	} else if (init->op == STRING && type &&
		   (type->flags & TF_ARRAY) && type->sub &&
		   type->sub->size == 1) {
		/* String initializing char array - mark as inlined */
		if (init->var) {
			struct name *strname = (struct name *)init->var;
			strname->emitted = 1;
		}
	}
}

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
		if (init->var) {
			struct name *strname = (struct name *)init->var;
			strname->emitted = 1;
		}
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
 * Emit a single string literal immediately
 * Called when string literal is created during parsing
 * Format: U name len data
 */
void
emitStrLit(struct name *strname)
{
	cstring str;
	unsigned char len;
	unsigned char *data;

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

	/* Output: U name len data */
	emit1('U');
	emitS(strname->name);
	emit1(len);
	if (len > 0)
		write(astFd, data, len);

	/* Don't free string data - needed for array size inference in char[] = "str" */
}

/*
 * Output a global variable declaration with optional initializer
 * Format: Z $ name type has_init [init-expr]
 */
void
emitGv(struct name *var)
{
	char fullname[32];

	/* Phase 1: don't emit, just build symbol table */
	if (phase == 1)
		return;

	if (!var || !var->type)
		return;

	/*
	 * Pre-scan struct array initializers to mark strings that will be
	 * inlined as bytes. Must happen before any emitStrLit calls.
	 */
	if ((var->type->flags & TF_ARRAY) && var->u.init) {
		struct expr *item;
		for (item = var->u.init; item; item = item->next) {
			if (item->op == INITLIST)
				markInlStr(item, var->type->sub);
		}
	}

	/*
	 * For char[] = "string", emit the string directly with var name
	 * and skip the Z record
	 */
	if ((var->type->flags & TF_ARRAY) && var->type->sub &&
	    var->type->sub->size == 1 &&
	    var->u.init && var->u.init->op == STRING && !var->u.init->next) {
		/* Emit string literal with the variable's name */
		struct name *strname = (struct name *)var->u.init->var;
		if (strname) {
			emitStrLit(strname);
		}
		return;
	}

	/*
	 * For pointers initialized to string literals, emit the string
	 * BEFORE the Z record so it doesn't interrupt the record
	 */
	if ((var->type->flags & TF_POINTER) &&
	    var->u.init && var->u.init->op == STRING && !var->u.init->next) {
		struct name *strname = (struct name *)var->u.init->var;
		if (strname) {
			emitStrLit(strname);
		}
	}

	/*
	 * For arrays with initializer lists containing strings, emit all
	 * strings BEFORE the Z record so they don't interrupt it.
	 * Skip strings inside INITLIST (struct initializers) - those will
	 * be emitted inline as bytes for embedded char arrays.
	 */
	if ((var->type->flags & TF_ARRAY) && var->u.init) {
		struct expr *init = var->u.init;
		struct expr *item;
		/* Unwrap INITLIST if present */
		if (init->op == INITLIST)
			init = init->left;
		if (init->next) {
			for (item = init; item; item = item->next) {
				if (item->op == STRING && item->var) {
					emitStrLit((struct name *)item->var);
				}
				/* Skip INITLIST - strings in structs are emitted inline */
			}
		}
	}

	emit1('Z');
	emit1('$');

	/* Static uses S<id>, public gets underscore prefix */
	if (var->sclass & SC_STATIC)
		sprintf(fullname, "S%d", var->static_id - 1);
	else
		sprintf(fullname, "_%s", var->name);
	emitS(fullname);

	emitTypeInfo(var->type);

	emit1(var->u.init ? 1 : 0);
	if (var->u.init) {
		struct expr *init = var->u.init;
		struct type *elem_type =
		    (var->type && (var->type->flags & TF_ARRAY)) ?
		    var->type->sub : var->type;
		/* INITLIST wrapper indicates brace-enclosed list */
		if (init->op == INITLIST)
			init = init->left;
		if (init->next) {
			emitInitList(init, elem_type);
		} else {
			emitExpr(init);
		}
	}
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
