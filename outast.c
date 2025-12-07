/*
 * AST serialization for second pass
 * Hex-based format: names as <2-hex-len><hex-bytes>, numbers as hex with '.'
 */
#include "cc1.h"

/* Forward declarations */
static void emitTypeInfo(struct type *type);

/*
 * Get size suffix for memory operations based on type
 * Returns: 'b' (byte), 's' (short/int), 'l' (long), 'p' (pointer),
 * 'f' (float), 'd' (double), 'v' (void)
 * Uppercase B/S/L for unsigned types
 */
static char
getSizeSuffix(struct type *t)
{
	char c;
	if (!t)
		return 's';  /* default to short */

	if (t->flags & TF_POINTER)
		return 'p';

	/* Check primitive types by size */
	if (t->size == 0)
		return 'v';  /* void */
	else if (t->size == 1)
		c = 'b';  /* char/byte */
	else if (t->size == 2)
		c = 's';  /* short/int */
	else if (t->size == 4) {
		if (t->flags & TF_FLOAT)
			return 'f';  /* float */
		c = 'l';  /* long */
	} else if (t->size == 8)
		return 'd';  /* double */
	else
		c = 's';  /* default to short */

	/* Uppercase for unsigned */
	if (t->flags & TF_UNSIGNED)
		c = c - 'a' + 'A';
	return c;
}

/* Emit number as 4 hex digits (unsigned 16-bit) */
static void
emitHexNum(long v)
{
	fdprintf(astFd, "%04lx", (unsigned long)(v & 0xffff));
}

/* Emit number as 8 hex digits (two's complement) - for constants */
static void
emitHexNum32(long v)
{
	fdprintf(astFd, "%08lx", (unsigned long)v);
}

/* Emit string as hex-length-prefixed ASCII */
static void
emitHexName(const char *s)
{
	int len = strlen(s);
	fdprintf(astFd, "%02x%s", len, s);
}

/* Emit a label or goto statement with hex-encoded name */
static void
emitLG(char op, const char *base, const char *suffix)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "%s%s", base, suffix);
	fdprintf(astFd, "%c", op);
	emitHexName(buf);
}
#define emitLabel(b,s) emitLG('L',b,s)
#define emitGoto(b,s)  emitLG('G',b,s)

/*
 * Helper: emit child expression (if non-null)
 */
static void emitExpr(struct expr *e);  /* forward declaration */

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
static void
emitExpr(struct expr *e)
{
	struct name *sym;
	char *name;

	if (!e) {
		fdprintf(astFd, "_");
		return;
	}

	switch (e->op) {
	case CONST:
		fdprintf(astFd, "#");
		emitHexNum32(e->v);
		break;

	case SYM:
		if (e->var) {
			char fullname[256];
			sym = (struct name *)e->var;
			/* extern/global get underscore prefix, others use mangled or plain */
			if ((sym->sclass & SC_EXTERN) || sym->level == 1)
				snprintf(fullname, sizeof(fullname), "_%s", sym->name);
			else {
				name = sym->mangled_name ? sym->mangled_name : sym->name;
				snprintf(fullname, sizeof(fullname), "%s", name);
			}
			fdprintf(astFd, "$");
			emitHexName(fullname);
		} else {
			fdprintf(astFd, "$01?");
		}
		break;

	case STRING:
		/* String literals - output as reference to local synthetic name */
		if (e->var) {
			struct name *strname = (struct name *)e->var;
			/* Synthetic string names are local - no _ prefix */
			fdprintf(astFd, "$");
			emitHexName(strname->name);
		} else {
			/* Fallback to address if name not available */
			fdprintf(astFd, "S");
			emitHexNum(e->v);
		}
		break;

	case CALL:
		/* Function call: @type count. func arg1 arg2 ... */
		{
			int argc = 0;
			struct expr *arg;
			char ret_type = getSizeSuffix(e->type);
			for (arg = e->right; arg; arg = arg->next) argc++;
			fdprintf(astFd, "@%c%02x", ret_type, argc);
			emitChild(e->left);
			for (arg = e->right; arg; arg = arg->next)
				emitChild(arg);
		}
		break;

	case NARROW:
	case WIDEN:
	case SEXT:
		/* Cast operators with destination width annotation */
		{
			char size_suffix = getSizeSuffix(e->type);
			unsigned char op_char = (e->op == NARROW) ? 'N' :
			    (e->op == WIDEN) ? 'W' : AST_SEXT;
			fdprintf(astFd, "%c%c", op_char, size_suffix);
			emitChild(e->left);
		}
		break;

	case COPY:
		/* Memory copy operator: Y length. dest src */
		fdprintf(astFd, "Y");
		emitHexNum(e->v);  /* v field contains byte count */
		emitChild(e->left);
		emitChild(e->right);
		break;

	case INCR:
	case DECR:
		/* Increment/decrement operators: emit with increment amount */
		/* For pointers, amount is size of pointed-to type */
		/* For scalars, amount is 1 */
		{
			unsigned char op_char;
			int amount = 1;
			char size_suffix = getSizeSuffix(e->type);

			if (e->op == INCR) {
				op_char = (e->flags & E_POSTFIX) ? AST_POSTINC : AST_PREINC;
			} else {
				op_char = (e->flags & E_POSTFIX) ? AST_POSTDEC : AST_PREDEC;
			}

			/* Calculate increment amount based on type */
			if (e->type && (e->type->flags & TF_POINTER) && e->type->sub) {
				amount = e->type->sub->size;
			}

			fdprintf(astFd, "%c%c", op_char, size_suffix);
			emitChild(e->left);
			emitHexNum(amount);
		}
		break;

	case BFEXTRACT:
		/* Bitfield extract: AST_BFEXTRACT offset width addr */
		{
			struct name *member = (struct name *)e->var;
			fdprintf(astFd, "%c", AST_BFEXTRACT);
			if (member) {
				fdprintf(astFd, "%02x%02x", member->bitoff, member->width);
			} else {
				fdprintf(astFd, "0000");  /* fallback */
			}
			emitChild(e->left);
		}
		break;

	case BFASSIGN:
		/* Bitfield assign: AST_BFASSIGN offset width addr value */
		{
			struct name *member = (struct name *)e->var;
			fdprintf(astFd, "%c", AST_BFASSIGN);
			if (member) {
				fdprintf(astFd, "%02x%02x", member->bitoff, member->width);
			} else {
				fdprintf(astFd, "0000");  /* fallback */
			}
			emitChild(e->left);
			emitChild(e->right);
		}
		break;

	case QUES:
		/* Ternary: ?w cond then else - flatten the COLON node */
		fdprintf(astFd, "?%c", getSizeSuffix(e->type));
		emitChild(e->left);
		if (e->right && e->right->op == COLON) {
			emitChild(e->right->left);
			emitChild(e->right->right);
		}
		break;

	case SUBEQ:
	case ANDEQ:
	case MODEQ:
		/* Compound assignment operators with high-bit tokens - map to ASCII */
		{
			unsigned char op_char = (e->op == SUBEQ) ? AST_SUBEQ :
			    (e->op == ANDEQ) ? AST_ANDEQ : AST_MODEQ;
			fdprintf(astFd, "%c%c", op_char, getSizeSuffix(e->type));
			emitChild(e->left);
			emitChild(e->right);
		}
		break;

	default:
		/* All operators get width suffix: op width operands... */
		fdprintf(astFd, "%c%c", e->op, getSizeSuffix(e->type));
		emitChild(e->left);
		emitChild(e->right);
		break;
	}
}

/*
 * Output type information for AST
 * Hex format: 'a' count. elemsize. for arrays, 'p' for ptr, size char otherwise
 */
static void
emitTypeInfo(struct type *type)
{
	if (!type)
		return;

	/* For arrays: a count. elemsize. */
	if (type->flags & TF_ARRAY) {
		int elemsize = type->sub ? type->sub->size : 0;
		fdprintf(astFd, "a");
		emitHexNum(type->count);
		emitHexNum(elemsize);
		return;
	}

	/* For pointers: p */
	if (type->flags & TF_POINTER) {
		fdprintf(astFd, "p");
		return;
	}

	/* For aggregates: r size. */
	if (type->flags & TF_AGGREGATE) {
		fdprintf(astFd, "r");
		emitHexNum(type->size);
		return;
	}

	/* For primitives: size char */
	fdprintf(astFd, "%c", getSizeSuffix(type));
}

/*
 * Count statements in a chain
 */
static int
countStmts(struct stmt *st)
{
	int count = 0;
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
	if (!st)
		return;

	/* Output this statement */
	switch (st->op) {
	case BEGIN:
		{
			int decl_count = 0, stmt_count = 0;
			struct name *local;
			struct stmt *s;
			const char *lname;

			/* Count declarations and statements */
			if (st->locals) {
				for (local = st->locals; local; local = local->next) {
					if (local->kind != funarg)
						decl_count++;
				}
			}
			stmt_count = countStmts(st->chain);

			/* Emit: B decl_count stmt_count decls... stmts... */
			fdprintf(astFd, "B%02x%02x", decl_count, stmt_count);

			/* Emit declarations */
			if (st->locals) {
				for (local = st->locals; local; local = local->next) {
					if (local->kind == funarg)
						continue;
					lname = local->mangled_name ?
						local->mangled_name : local->name;
					fdprintf(astFd, "d%c", getSizeSuffix(local->type));
					emitHexName(lname);
				}
			}

			/* Emit statements */
			for (s = st->chain; s; s = s->next)
				emitStmt(s);
		}
		break;

	case IF:
		/* Dead code elimination for constant conditions */
		if (st->left && (st->left->flags & E_CONST)) {
			if (st->left->v == 0) {
				/* if (0) - emit only else branch */
				if (st->otherwise)
					emitStmt(st->otherwise);
			} else {
				/* if (non-zero) - emit only then branch */
				if (st->chain)
					emitStmt(st->chain);
			}
		} else {
			/* If: I has_else cond then [else] */
			fdprintf(astFd, "I%02x", st->otherwise ? 1 : 0);
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");  /* empty statement */
			if (st->otherwise)
				emitStmt(st->otherwise);
		}
		break;

	case WHILE:
		/* Emit as labeled sequence wrapped in block */
		if (st->label) {
			fdprintf(astFd, "B0005");  /* 5 stmts: label, if, label, goto, label */
			emitLabel(st->label, "_top");
			fdprintf(astFd, "I01");  /* has else */
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
			/* else: block with goto break */
			fdprintf(astFd, "B0001");
			emitGoto(st->label, "_break");
			emitLabel(st->label, "_continue");
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "W");
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
		}
		break;

	case DO:
		/* Emit as labeled sequence wrapped in block */
		if (st->label) {
			fdprintf(astFd, "B0005");  /* 5 stmts: top, body, test, if, break */
			emitLabel(st->label, "_top");
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
			emitLabel(st->label, "_test");
			fdprintf(astFd, "I00");  /* no else */
			emitExpr(st->left);
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "D");
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
			emitExpr(st->left);
		}
		break;

	case FOR:
		/* Emit as labeled sequence wrapped in block */
		if (st->label) {
			/* Count statements: init? + top + (if or body) + continue + incr? + goto + break */
			int stmt_count = 5;  /* top, (if or body), continue, goto, break */
			if (st->left) stmt_count++;   /* init */
			if (st->right) stmt_count++;  /* incr */
			fdprintf(astFd, "B00%02x", stmt_count);
			if (st->left) {
				fdprintf(astFd, "E");
				emitExpr(st->left);
			}
			emitLabel(st->label, "_top");
			if (st->middle) {
				fdprintf(astFd, "I01");  /* has else */
				emitExpr(st->middle);
				if (st->chain)
					emitStmt(st->chain);
				else
					fdprintf(astFd, ";");
				fdprintf(astFd, "B0001");
				emitGoto(st->label, "_break");
			} else {
				if (st->chain)
					emitStmt(st->chain);
				else
					fdprintf(astFd, ";");
			}
			emitLabel(st->label, "_continue");
			if (st->right) {
				fdprintf(astFd, "E");
				emitExpr(st->right);
			}
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "F");
			emitExpr(st->left);
			emitExpr(st->middle);
			emitExpr(st->right);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, ";");
		}
		break;

	case SWITCH:
		/* Switch: S has_label. [hexlabel] case_count. expr cases... */
		{
			struct stmt *s, *body, *t;
			int case_count = 0, body_count;
			for (s = st->chain; s; s = s->next)
				if (s->op == CASE || s->op == DEFAULT)
					case_count++;
			fdprintf(astFd, "S%02x", st->label ? 1 : 0);
			if (st->label)
				emitHexName(st->label);
			fdprintf(astFd, "%02x", case_count);
			emitExpr(st->left);
			for (s = st->chain; s; ) {
				if (s->op == CASE || s->op == DEFAULT) {
					body = s->next;
					body_count = 0;
					for (t = body; t && t->op != CASE && t->op != DEFAULT; t = t->next)
						body_count++;
					fdprintf(astFd, "%c%02x", s->op == CASE ? 'C' : 'O', body_count);
					if (s->op == CASE)
						emitExpr(s->left);
					for (t = body; t && t->op != CASE && t->op != DEFAULT; t = t->next)
						emitStmt(t);
					s = body;
					while (s && s->op != CASE && s->op != DEFAULT)
						s = s->next;
				} else {
					emitStmt(s);
					s = s->next;
				}
			}
		}
		break;

	case CASE:
		/* Case labels are handled by SWITCH - this shouldn't be called directly */
		fdprintf(astFd, "C00");
		emitExpr(st->left);
		break;

	case DEFAULT:
		/* Default labels are handled by SWITCH - this shouldn't be called directly */
		fdprintf(astFd, "O00");
		break;

	case RETURN:
		/* Return: R has_value [expr] */
		fdprintf(astFd, "R%02x", st->left ? 1 : 0);
		if (st->left)
			emitExpr(st->left);
		break;

	case BREAK:
		fdprintf(astFd, "K");
		break;

	case CONTINUE:
		fdprintf(astFd, "N");
		break;

	case GOTO:
	case LABEL:
		fdprintf(astFd, "%c", st->op == GOTO ? 'G' : 'L');
		emitHexName(st->label ? st->label : "?");
		break;

	case EXPR:
		fdprintf(astFd, "E");
		emitExpr(st->left);
		break;

	case ';':
		fdprintf(astFd, ";");
		break;

	case ASM:
		{
			int len = st->label ? strlen(st->label) : 0;
			int i;
			fdprintf(astFd, "A%04x", len);
			for (i = 0; i < len; i++)
				fdprintf(astFd, "%02x", (unsigned char)st->label[i]);
		}
		break;

	default:
		fdprintf(astFd, "X%d.", st->op);  /* unknown */
		break;
	}
	/* Note: st->next is handled by caller (block counts statements) */
}

/*
 * Count function parameters
 */
static int
countParams(struct type *functype)
{
	int count = 0;
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
 * Output function parameter list
 * Format: param_count. d suffix name d suffix name ...
 */
static void
emitParams(struct type *functype)
{
	struct name *param;
	int count = countParams(functype);

	fdprintf(astFd, "%02x", count);
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			/* Skip void parameters - (void) means no params */
			if (param->type && param->type->size == 0)
				continue;
			/* Emit as: d suffix hexname */
			fdprintf(astFd, "d%c", getSizeSuffix(param->type));
			if (param->name && param->name[0] != '\0')
				emitHexName(param->name);
			else
				emitHexName("_");  /* anonymous parameter */
		}
	}
}

/*
 * Output a global asm block in AST format
 * Format: A len hexdata (same as inline asm but at top level)
 */
void
emitGlobalAsm(struct stmt *st)
{
	int len, i;
	if (!st || !st->label)
		return;
	len = strlen(st->label);
	fdprintf(astFd, "\nA%04x", len);
	for (i = 0; i < len; i++)
		fdprintf(astFd, "%02x", (unsigned char)st->label[i]);
	fdprintf(astFd, "\n");
}

/*
 * Output a function in AST format
 * Format: F rettype hexname param_count. params... body
 */
void
emitFunction(struct name *func)
{
	char func_name[256];
	char ret_suffix;

	if (!func || !func->u.body)
		return;

	/* Static functions use mangled name, public get underscore prefix */
	if (func->mangled_name) {
		snprintf(func_name, sizeof(func_name), "%s", func->mangled_name);
	} else {
		snprintf(func_name, sizeof(func_name), "_%s", func->name);
	}

	/* Get return type suffix (void uses 'v') */
	if (func->type && func->type->sub)
		ret_suffix = getSizeSuffix(func->type->sub);
	else
		ret_suffix = 'v';  /* void */

	fdprintf(astFd, "\nF%c", ret_suffix);
	emitHexName(func_name);

	/* Output parameter list with declarations */
	if (func->type)
		emitParams(func->type);
	else
		fdprintf(astFd, "0.");

	/* Output function body */
	fdprintf(astFd, "\n");
	emitStmt(func->u.body);
	fdprintf(astFd, "\n");
}

/*
 * Emit an initializer list (linked via next pointers)
 * Used for array/struct initializers like {1, 2, 3}
 * elem_type: type of array elements for width annotation
 * Format: [ width count. items...
 */
static void
emitInitList(struct expr *init, struct type *elem_type)
{
	struct expr *item;
	char width;
	int count = 0;

	/* Count items and get element width */
	for (item = init; item; item = item->next)
		count++;
	width = getSizeSuffix(elem_type);

	fdprintf(astFd, "[%c%02x", width, count);
	for (item = init; item; item = item->next)
		emitExpr(item);
}

/*
 * Emit a single string literal immediately
 * Called when string literal is created during parsing
 * Format: U hexname hexdata
 */
void
emitStrLit(struct name *strname)
{
	cstring str;
	unsigned char len;
	unsigned char *data;
	int j;

	if (!strname || !strname->u.init || strname->u.init->op != STRING)
		return;

	str = (cstring)strname->u.init->v;
	if (!str)
		return;

	len = (unsigned char)str[0];
	data = (unsigned char *)str + 1;

	/* Output: U hexname hexdata */
	fdprintf(astFd, "\nU");
	emitHexName(strname->name);
	fdprintf(astFd, "%02x", len);
	for (j = 0; j < len; j++)
		fdprintf(astFd, "%02x", data[j]);
	fdprintf(astFd, "\n");

	/* Free the string data after emitting */
	free((void*)str);
	strname->u.init->v = 0;
}

/*
 * Output a global variable declaration with optional initializer
 * Format: Z $hexname type has_init. [init-expr]
 */
void
emitGv(struct name *var)
{
	char fullname[256];
	char *name;

	if (!var || !var->type)
		return;

	fdprintf(astFd, "\nZ$");

	/* Static uses mangled name, public gets underscore prefix */
	if (var->sclass & SC_STATIC) {
		name = var->mangled_name ? var->mangled_name : var->name;
		snprintf(fullname, sizeof(fullname), "%s", name);
	} else {
		snprintf(fullname, sizeof(fullname), "_%s", var->name);
	}
	emitHexName(fullname);

	emitTypeInfo(var->type);

	fdprintf(astFd, "%02x", var->u.init ? 1 : 0);
	if (var->u.init) {
		if (var->u.init->next) {
			struct type *elem_type =
			    (var->type && (var->type->flags & TF_ARRAY)) ?
			    var->type->sub : var->type;
			emitInitList(var->u.init, elem_type);
		} else {
			emitExpr(var->u.init);
		}
	}
	fdprintf(astFd, "\n");
}

/*
 * Emit all global variables (DEPRECATED - kept for compatibility)
 * Global variables are now emitted incrementally during parsing
 * Called after parsing completes with all names still in scope
 */
void
emitGvs(void)
{
	/* No-op: globals are now emitted incrementally in declaration() */
	/* This function is kept for API compatibility but does nothing */
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
