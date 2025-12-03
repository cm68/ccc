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
 */
static char
getSizeSuffix(struct type *t)
{
	if (!t)
		return 's';  /* default to short */

	if (t->flags & TF_POINTER)
		return 'p';

	/* Check primitive types by size */
	if (t->size == 0)
		return 'v';  /* void */
	else if (t->size == 1)
		return 'b';  /* char/byte */
	else if (t->size == 2)
		return 's';  /* short/int */
	else if (t->size == 4) {
		if (t->flags & TF_FLOAT)
			return 'f';  /* float */
		return 'l';  /* long */
	} else if (t->size == 8)
		return 'd';  /* double */

	return 's';  /* default to short */
}

/* Emit number as hex digits followed by '.' */
static void
emitHexNum(long v)
{
	if (v < 0) {
		fdprintf(astFd, "-%lx.", -v);
	} else {
		fdprintf(astFd, "%lx.", v);
	}
}

/* Emit string as hex-length-prefixed bytes */
static void
emitHexName(const char *s)
{
	int len = strlen(s);
	int i;
	fdprintf(astFd, "%02x", len);
	for (i = 0; i < len; i++)
		fdprintf(astFd, "%02x", (unsigned char)s[i]);
}

/* Emit a label statement with hex-encoded name */
static void
emitLabel(const char *base, const char *suffix)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "%s%s", base, suffix);
	fdprintf(astFd, "(L");
	emitHexName(buf);
	fdprintf(astFd, ")");
}

/* Emit a goto statement with hex-encoded name */
static void
emitGoto(const char *base, const char *suffix)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "%s%s", base, suffix);
	fdprintf(astFd, "(G");
	emitHexName(buf);
	fdprintf(astFd, ")");
}

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
 * Output an expression in S-expression format
 * Constants: just the value (decimal)
 * Symbols: $name
 * Binary ops: (op left right)
 * Unary ops: (op operand)
 * Memory ops annotated with size: (M:b expr) (=:l lvalue rvalue)
 */
static void
emitExpr(struct expr *e)
{
	struct name *sym;
	char *name;

	if (!e) {
		fdprintf(astFd, "()");
		return;
	}

	switch (e->op) {
	case CONST:
		emitHexNum(e->v);
		break;

	case SYM:
		if (e->var) {
			char fullname[256];
			sym = (struct name *)e->var;
			/* Build full name based on scope/storage class:
			 * - extern/global (level 1): prefix with underscore
			 * - static: use mangled name
			 * - local variables and arguments: no prefix
			 */
			if (sym->sclass & SC_STATIC) {
				name = sym->mangled_name ? sym->mangled_name : sym->name;
				snprintf(fullname, sizeof(fullname), "%s", name);
			} else if ((sym->sclass & SC_EXTERN) || sym->level == 1) {
				snprintf(fullname, sizeof(fullname), "_%s", sym->name);
			} else {
				name = sym->mangled_name ? sym->mangled_name : sym->name;
				snprintf(fullname, sizeof(fullname), "%s", name);
			}
			fdprintf(astFd, "$");
			emitHexName(fullname);
		} else {
			fdprintf(astFd, "$01%02x", '?');
		}
		break;

	case STRING:
		/* String literals - output as reference to global synthetic name */
		if (e->var) {
			char fullname[256];
			struct name *strname = (struct name *)e->var;
			/* Synthetic string names are global variables, use _ prefix */
			snprintf(fullname, sizeof(fullname), "_%s", strname->name);
			fdprintf(astFd, "$");
			emitHexName(fullname);
		} else {
			/* Fallback to address if name not available */
			fdprintf(astFd, "S");
			emitHexNum(e->v);
		}
		break;

	case CALL:
		/* Function call: (@ func arg1 arg2 ...) */
		fdprintf(astFd, "(@");
		emitChild(e->left);
		/* Arguments are in e->right and linked via next */
		if (e->right) {
			struct expr *arg;
			for (arg = e->right; arg; arg = arg->next) {
				emitChild(arg);
			}
		}
		fdprintf(astFd, ")");
		break;

	case NARROW:
	case WIDEN:
	case SEXT:
		/* Cast operators with destination width annotation */
		{
			char size_suffix = getSizeSuffix(e->type);
			unsigned char op_char = (e->op == NARROW) ? 'N' :
			    (e->op == WIDEN) ? 'W' : 0xab;  /* SEXT = 0xab */
			fdprintf(astFd, "(%c:%c", op_char, size_suffix);
			emitChild(e->left);
			fdprintf(astFd, ")");
		}
		break;

	case COPY:
		/* Memory copy operator: (Y length. dest src) */
		fdprintf(astFd, "(Y");
		emitHexNum(e->v);  /* v field contains byte count */
		emitChild(e->left);
		emitChild(e->right);
		fdprintf(astFd, ")");
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
				op_char = (e->flags & E_POSTFIX) ? POSTINC : PREINC;
			} else {
				op_char = (e->flags & E_POSTFIX) ? POSTDEC : PREDEC;
			}

			/* Calculate increment amount based on type */
			if (e->type && (e->type->flags & TF_POINTER) && e->type->sub) {
				amount = e->type->sub->size;
			}

			fdprintf(astFd, "(%c%c", op_char, size_suffix);
			emitChild(e->left);
			fdprintf(astFd, " %x.)", amount);
		}
		break;

	case BFEXTRACT:
		/* Bitfield extract: (0xa7 offset. width. addr) */
		{
			struct name *member = (struct name *)e->var;
			fdprintf(astFd, "(%c", BFEXTRACT);
			if (member) {
				emitHexNum(member->bitoff);
				emitHexNum(member->width);
			} else {
				fdprintf(astFd, "0.0.");  /* fallback */
			}
			emitChild(e->left);
			fdprintf(astFd, ")");
		}
		break;

	case BFASSIGN:
		/* Bitfield assign: (0xdd offset. width. addr value) */
		{
			struct name *member = (struct name *)e->var;
			fdprintf(astFd, "(%c", BFASSIGN);
			if (member) {
				emitHexNum(member->bitoff);
				emitHexNum(member->width);
			} else {
				fdprintf(astFd, "0.0.");  /* fallback */
			}
			emitChild(e->left);
			emitChild(e->right);
			fdprintf(astFd, ")");
		}
		break;

	default:
		/* Operator - output in prefix notation */
		/* For DEREF (M), ASSIGN (=), add size annotation (no colon) */
		if (e->op == DEREF || e->op == ASSIGN ||
		    e->op == PLUSEQ || e->op == SUBEQ || e->op == MULTEQ ||
		    e->op == DIVEQ || e->op == MODEQ || e->op == ANDEQ ||
		    e->op == OREQ || e->op == XOREQ || e->op == LSHIFTEQ ||
		    e->op == RSHIFTEQ || e->op == LANDEQ || e->op == LOREQ) {
			char size_suffix = getSizeSuffix(e->type);
			fdprintf(astFd, "(%c%c", e->op, size_suffix);
		} else {
			fdprintf(astFd, "(%c", e->op);
		}
		emitChild(e->left);
		emitChild(e->right);
		fdprintf(astFd, ")");
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
 * Output a statement in S-expression format
 * Each statement type has its own format
 */
static void
emitStmt(struct stmt *st)
{
	if (!st)
		return;

	/* Output this statement */
	switch (st->op) {
	case BEGIN:
		fdprintf(astFd, "(B");  /* Block */

		/* Emit declarations for local variables in this scope (skip args) */
		if (st->locals) {
			struct name *local;
			const char *lname;
			for (local = st->locals; local; local = local->next) {
				if (local->kind == funarg)
					continue;  /* args are in function header */
				lname = local->mangled_name ?
					local->mangled_name : local->name;
				fdprintf(astFd, "(d%c", getSizeSuffix(local->type));
				emitHexName(lname);
				fdprintf(astFd, ")");
			}
		}

		if (st->chain) {
			emitStmt(st->chain);
		}
		fdprintf(astFd, ")");
		break;

	case IF:
		fdprintf(astFd, "(I");
		emitExpr(st->left);
		if (st->chain)
			emitStmt(st->chain);
		else
			fdprintf(astFd, "()");
		if (st->otherwise)
			emitStmt(st->otherwise);
		fdprintf(astFd, ")");
		break;

	case WHILE:
		/* Emit as labeled sequence */
		if (st->label) {
			emitLabel(st->label, "_top");
			fdprintf(astFd, "(I");
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			fdprintf(astFd, "(B");
			emitGoto(st->label, "_break");
			fdprintf(astFd, "))");
			emitLabel(st->label, "_continue");
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "(W");
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			fdprintf(astFd, ")");
		}
		break;

	case DO:
		/* Emit as labeled sequence */
		if (st->label) {
			emitLabel(st->label, "_top");
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			emitLabel(st->label, "_test");
			fdprintf(astFd, "(I");
			emitExpr(st->left);
			emitGoto(st->label, "_top");
			fdprintf(astFd, "())");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "(D");
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			emitExpr(st->left);
			fdprintf(astFd, ")");
		}
		break;

	case FOR:
		/* Emit as labeled sequence */
		if (st->label) {
			if (st->left) {
				fdprintf(astFd, "(E");
				emitExpr(st->left);
				fdprintf(astFd, ")");
			}
			emitLabel(st->label, "_top");
			if (st->middle) {
				fdprintf(astFd, "(I");
				emitExpr(st->middle);
				if (st->chain)
					emitStmt(st->chain);
				else
					fdprintf(astFd, "()");
				fdprintf(astFd, "(B");
				emitGoto(st->label, "_break");
				fdprintf(astFd, "))");
			} else {
				if (st->chain)
					emitStmt(st->chain);
				else
					fdprintf(astFd, "()");
			}
			emitLabel(st->label, "_continue");
			if (st->right) {
				fdprintf(astFd, "(E");
				emitExpr(st->right);
				fdprintf(astFd, ")");
			}
			emitGoto(st->label, "_top");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "(F");
			emitExpr(st->left);
			emitExpr(st->middle);
			emitExpr(st->right);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			fdprintf(astFd, ")");
		}
		break;

	case SWITCH:
		/* Emit as labeled sequence with break label */
		if (st->label) {
			emitLabel(st->label, "_top");
			fdprintf(astFd, "(S");
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			fdprintf(astFd, ")");
			emitLabel(st->label, "_break");
		} else {
			fdprintf(astFd, "(S");
			emitExpr(st->left);
			if (st->chain)
				emitStmt(st->chain);
			else
				fdprintf(astFd, "()");
			fdprintf(astFd, ")");
		}
		break;

	case CASE:
		fdprintf(astFd, "(C");
		emitExpr(st->left);
		if (st->chain)
			emitStmt(st->chain);
		else
			fdprintf(astFd, "()");
		fdprintf(astFd, ")");
		break;

	case DEFAULT:
		fdprintf(astFd, "(O");
		if (st->chain)
			emitStmt(st->chain);
		else
			fdprintf(astFd, "()");
		fdprintf(astFd, ")");
		break;

	case RETURN:
		fdprintf(astFd, "(R");
		if (st->left)
			emitExpr(st->left);
		fdprintf(astFd, ")");
		break;

	case BREAK:
		fdprintf(astFd, "(K)");
		break;

	case CONTINUE:
		fdprintf(astFd, "(N)");
		break;

	case GOTO:
		fdprintf(astFd, "(G");
		emitHexName(st->label ? st->label : "?");
		fdprintf(astFd, ")");
		break;

	case LABEL:
		fdprintf(astFd, "(L");
		emitHexName(st->label ? st->label : "?");
		fdprintf(astFd, ")");
		break;

	case EXPR:
		fdprintf(astFd, "(E");
		emitExpr(st->left);
		fdprintf(astFd, ")");
		break;

	case ';':
		fdprintf(astFd, "(;)");
		break;

	case ASM:
		fdprintf(astFd, "(A");
		if (st->label) {
			/* Emit assembly text as hex-encoded string */
			int len = strlen(st->label);
			int i;
			fdprintf(astFd, "%02x", len);
			for (i = 0; i < len; i++)
				fdprintf(astFd, "%02x", (unsigned char)st->label[i]);
		} else {
			fdprintf(astFd, "00");  /* empty string */
		}
		fdprintf(astFd, ")");
		break;

	default:
		fdprintf(astFd, "(?%d)", st->op);
		break;
	}

	/* Output sibling statements */
	if (st->next)
		emitStmt(st->next);
}

/*
 * Output function parameter list
 */
static void
emitParams(struct type *functype)
{
	struct name *param;

	fdprintf(astFd, "(");
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			/* Skip void parameters - (void) means no params */
			if (param->type && param->type->size == 0)
				continue;
			/* Emit as (d suffix hexname) */
			fdprintf(astFd, "(d%c", getSizeSuffix(param->type));
			if (param->name && param->name[0] != '\0')
				emitHexName(param->name);
			else
				emitHexName("_");  /* anonymous parameter */
			fdprintf(astFd, ")");
		}
	}
	fdprintf(astFd, ")");
}

/*
 * Output a function in AST format
 * Format: (f rettype hexname (params) body)
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

	fdprintf(astFd, "\n; Function: %s\n", func_name);
	fdprintf(astFd, "(f%c", ret_suffix);
	emitHexName(func_name);

	/* Output parameter list with declarations */
	if (func->type)
		emitParams(func->type);
	else
		fdprintf(astFd, "()");

	/* Output function body */
	fdprintf(astFd, "\n");
	emitStmt(func->u.body);
	fdprintf(astFd, ")\n");
}

/*
 * Emit an initializer list (linked via next pointers)
 * Used for array/struct initializers like {1, 2, 3}
 * elem_type: type of array elements for width annotation
 */
static void
emitInitList(struct expr *init, struct type *elem_type)
{
	struct expr *item;
	char width;

	/* Get element width for array initializers */
	width = getSizeSuffix(elem_type);

	fdprintf(astFd, "([%c", width);
	for (item = init; item; item = item->next)
		emitExpr(item);
	fdprintf(astFd, ")");
}

/*
 * Emit a single string literal immediately
 * Called when string literal is created during parsing
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

	/* Output: (s hexname hexdata) */
	fdprintf(astFd, "\n; String literal: %s\n", strname->name);
	fdprintf(astFd, "(s");
	emitHexName(strname->name);
	fdprintf(astFd, "%02x", len);
	for (j = 0; j < len; j++)
		fdprintf(astFd, "%02x", data[j]);
	fdprintf(astFd, ")\n");

	/* Free the string data after emitting */
	free((void*)str);
	strname->u.init->v = 0;
}

/*
 * Emit string literals section (DEPRECATED - kept for compatibility)
 * Now string literals are emitted incrementally
 */
void
emitLiterals(void)
{
	extern struct name **names;
	extern int lastname;
	struct name *n;
	int i;
	int found_any = 0;

	/* First pass: check if we have any string literals */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (!n)
			continue;

		/*
		 * Look for synthetic string literal names (str0, str1, etc.)
		 * at any level
		 */
		if (n->kind == var && n->u.init && n->u.init->op == STRING &&
		    n->name && strncmp(n->name, "str", 3) == 0 &&
		    n->name[3] >= '0' && n->name[3] <= '9') {
			found_any = 1;
			break;
		}
	}

	if (!found_any)
		return;

	/* Output literals section header */
	fdprintf(astFd, "\n; String literals\n");
	fdprintf(astFd, "(L\n");

	/* Second pass: output each string literal */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (!n)
			continue;

		/*
		 * Output string literal data (only synthetic str names at any
		 * level)
		 */
		if (n->kind == var && n->u.init && n->u.init->op == STRING &&
		    n->name && strncmp(n->name, "str", 3) == 0 &&
		    n->name[3] >= '0' && n->name[3] <= '9') {
			cstring str = (cstring)n->u.init->v;
			if (str) {
				unsigned char len = (unsigned char)str[0];
				unsigned char *data = (unsigned char *)str + 1;
				int j;

				/* Output: (s name "literal_data") */
				fdprintf(astFd, "  (s %s \"", n->name);
				for (j = 0; j < len; j++) {
					unsigned char c = data[j];
					if (c == '"') {
						fdprintf(astFd, "\\\"");
					} else if (c == '\\') {
						fdprintf(astFd, "\\\\");
					} else if (c == '\n') {
						fdprintf(astFd, "\\n");
					} else if (c == '\t') {
						fdprintf(astFd, "\\t");
					} else if (c == '\r') {
						fdprintf(astFd, "\\r");
					} else if (c >= ' ' && c < 0x7f) {
						fdprintf(astFd, "%c", c);
					} else {
						fdprintf(astFd, "\\x%02x", c);
					}
				}
				fdprintf(astFd, "\")\n");
			}
		}
	}

	fdprintf(astFd, ")\n");
}

/*
 * Output a global variable declaration with optional initializer
 * Format: (g hexname type [init-expr])
 */
void
emitGv(struct name *var)
{
	char fullname[256];

	if (!var || !var->type)
		return;

	fdprintf(astFd, "\n; Global variable: %s\n", var->name);
	fdprintf(astFd, "(g$");

	/* Build variable name with scope prefix */
	if (var->sclass & SC_STATIC) {
		if (var->mangled_name)
			snprintf(fullname, sizeof(fullname), "%s", var->mangled_name);
		else
			snprintf(fullname, sizeof(fullname), "%s", var->name);
	} else {
		snprintf(fullname, sizeof(fullname), "_%s", var->name);
	}
	emitHexName(fullname);

	/* Output type */
	emitTypeInfo(var->type);

	/* Output initializer if present */
	if (var->u.init) {
		/* Check if this is an initializer list (has next pointers) */
		if (var->u.init->next) {
			struct type *elem_type =
			    (var->type && (var->type->flags & TF_ARRAY)) ?
			    var->type->sub : var->type;
			emitInitList(var->u.init, elem_type);
		} else {
			emitExpr(var->u.init);
		}
	}

	fdprintf(astFd, ")\n");
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
