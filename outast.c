/*
 * AST serialization for second pass
 * Outputs statements and expressions in a parseable S-expression format
 */
#include "cc1.h"

/* Forward declarations */
static void emitTypeInfo(struct type *type);

/*
 * Get size suffix for memory operations based on type
 * Returns: 'b' (byte), 's' (short/int), 'l' (long), 'p' (pointer),
 * 'f' (float), 'd' (double)
 */
static char
getSizeSuffix(struct type *t)
{
	if (!t)
		return 's';  // default to short

	if (t->flags & TF_POINTER)
		return 'p';

	// Check primitive types by size
	if (t->size == 1)
		return 'b';  // char/byte
	else if (t->size == 2)
		return 's';  // short/int
	else if (t->size == 4) {
		if (t->flags & TF_FLOAT)
			return 'f';  // float
		return 'l';  // long
	} else if (t->size == 8)
		return 'd';  // double

	return 's';  // default to short
}

/*
 * Helper: emit space followed by child expression (if non-null)
 * Used for AST tree serialization to reduce code duplication
 */
static void emitExpr(struct expr *e);  /* forward declaration */

static void
emitChild(struct expr *e)
{
	if (e) {
		fdprintf(astFd, " ");
		emitExpr(e);
	}
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
	char *prefix;
	char *name;

	if (!e) {
		fdprintf(astFd, "()");
		return;
	}

	switch (e->op) {
	case CONST:
		fdprintf(astFd, "%ld", e->v);
		break;

	case SYM:
		if (e->var) {
			sym = (struct name *)e->var;
			/* Add prefix based on scope/storage class:
			 * - extern/global (level 1): prefix with underscore
			 * - static: use mangled name with S prefix
			 * - function arguments: prefix with A
			 * - local variables: no prefix
			 */
			if (sym->sclass & SC_STATIC) {
				/* Use mangled name for statics */
				prefix = "$S";
				name = sym->mangled_name ? sym->mangled_name : sym->name;
			} else if ((sym->sclass & SC_EXTERN) || sym->level == 1) {
				/* Global variable (extern or level 1) */
				prefix = "$_";
				name = sym->name;
			} else if (sym->kind == funarg) {
				/* Function argument */
				prefix = "$A";
				name = sym->name;
			} else {
				/* Local variable */
				prefix = "$";
				name = sym->name;
			}
			fdprintf(astFd, "%s%s", prefix, name);
		} else {
			fdprintf(astFd, "$?");
		}
		break;

	case STRING:
		/* String literals - output as reference to global synthetic name */
		if (e->var) {
			struct name *strname = (struct name *)e->var;
			/* Synthetic string names are global variables, use $_ prefix */
			fdprintf(astFd, "$_%s", strname->name);
		} else {
			/* Fallback to address if name not available */
			fdprintf(astFd, "S%ld", e->v);
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
		/* Memory copy operator: (Y:length dest src) */
		fdprintf(astFd, "(Y:%ld", e->v);  /* v field contains byte count */
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

			if (e->op == INCR) {
				op_char = (e->flags & E_POSTFIX) ? POSTINC : PREINC;
			} else {
				op_char = (e->flags & E_POSTFIX) ? POSTDEC : PREDEC;
			}

			/* Calculate increment amount based on type */
			if (e->type && (e->type->flags & TF_POINTER) && e->type->sub) {
				amount = e->type->sub->size;
			}

			fdprintf(astFd, "(%c", op_char);
			emitChild(e->left);
			fdprintf(astFd, " %d)", amount);
		}
		break;

	case BFEXTRACT:
		/* Bitfield extract: (0xa7:offset:width addr) */
		{
			struct name *member = (struct name *)e->var;
			if (member) {
				fdprintf(astFd, "(%c:%d:%d", BFEXTRACT, member->bitoff, 
                    member->width);
			} else {
				fdprintf(astFd, "(%c:0:0", BFEXTRACT);  /* fallback */
			}
			emitChild(e->left);
			fdprintf(astFd, ")");
		}
		break;

	case BFASSIGN:
		/* Bitfield assign: (0xdd:offset:width addr value) */
		{
			struct name *member = (struct name *)e->var;
			if (member) {
				fdprintf(astFd, "(%c:%d:%d", BFASSIGN, member->bitoff, 
                    member->width);
			} else {
				fdprintf(astFd, "(%c:0:0", BFASSIGN);  /* fallback */
			}
			emitChild(e->left);
			emitChild(e->right);
			fdprintf(astFd, ")");
		}
		break;

	default:
		/* Operator - output in prefix notation */
		/* For DEREF (M), ASSIGN (=), add size annotation */
		if (e->op == DEREF || e->op == ASSIGN ||
		    e->op == PLUSEQ || e->op == SUBEQ || e->op == MULTEQ ||
		    e->op == DIVEQ || e->op == MODEQ || e->op == ANDEQ ||
		    e->op == OREQ || e->op == XOREQ || e->op == LSHIFTEQ ||
		    e->op == RSHIFTEQ || e->op == LANDEQ || e->op == LOREQ) {
			char size_suffix = getSizeSuffix(e->type);
			fdprintf(astFd, "(%c:%c", e->op, size_suffix);
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
 * For primitive types, outputs the type name
 * For aggregates (struct/union), outputs size
 */
static void
emitTypeInfo(struct type *type)
{
	if (!type)
		return;

	/* For arrays, emit array info with element size */
	if (type->flags & TF_ARRAY) {
		int elemsize = type->sub ? type->sub->size : 0;
		fdprintf(astFd, " :array:%d:%d", type->count, elemsize);
		return;
	}

	/* For pointers, just emit :ptr - all pointers are 2 bytes */
	if (type->flags & TF_POINTER) {
		fdprintf(astFd, " :ptr");
		return;
	}

	if (type->name && type->name[0]) {
		/* Primitive or named type */
		fdprintf(astFd, " %s", type->name);
	} else if (type->flags & TF_AGGREGATE) {
		/* Struct/union without type name - output size */
		fdprintf(astFd, " :struct:%d", type->size);
	} else {
		/* Unknown type - output size */
		fdprintf(astFd, " :size:%d", type->size);
	}
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

		/* Emit declarations for local variables in this scope */
		if (st->locals) {
			struct name *local;
			for (local = st->locals; local; local = local->next) {
				fdprintf(astFd, " (d %s", local->name);
				emitTypeInfo(local->type);
				fdprintf(astFd, ")");
			}
		}

		if (st->chain) {
			fdprintf(astFd, " ");
			emitStmt(st->chain);
		}
		fdprintf(astFd, ")");
		break;

	case IF:
		fdprintf(astFd, "(I ");  /* If */
		emitExpr(st->left);
		fdprintf(astFd, " ");
		if (st->chain) {
			emitStmt(st->chain);
		} else {
			fdprintf(astFd, "()");
		}
		if (st->otherwise) {
			fdprintf(astFd, " ");
			emitStmt(st->otherwise);
		}
		fdprintf(astFd, ")");
		break;

	case WHILE:
		/* Emit as labeled sequence:
		 * Lxxx_top: (test condition) (body) Lxxx_continue: Lxxx_break: */
		if (st->label) {
			/* Top label */
			fdprintf(astFd, "(L %s_top) ", st->label);
			/* Test condition (if false, goto break) */
			fdprintf(astFd, "(I ");
			emitExpr(st->left);
			fdprintf(astFd, " ");
			/* Body */
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, " ()) ");  /* close IF */
			/* Continue label (for continue statements) */
			fdprintf(astFd, "(L %s_continue) ", st->label);
			/* Goto top to loop */
			fdprintf(astFd, "(G %s_top) ", st->label);
			/* Break label */
			fdprintf(astFd, "(L %s_break)", st->label);
		} else {
			/* Fallback: emit original WHILE format */
			fdprintf(astFd, "(W ");
			emitExpr(st->left);
			fdprintf(astFd, " ");
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, ")");
		}
		break;

	case DO:
		/* Emit as labeled sequence:
		 * Dxxx_top: (body) Dxxx_test: (if condition goto top) Dxxx_break: */
		if (st->label) {
			/* Top label */
			fdprintf(astFd, "(L %s_top) ", st->label);
			/* Body */
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, " ");
			/* Test label (continue target) */
			fdprintf(astFd, "(L %s_test) ", st->label);
			/* Test condition and loop back */
			fdprintf(astFd, "(I ");
			emitExpr(st->left);
			fdprintf(astFd, " (G %s_top) ()) ", st->label);
			/* Break label */
			fdprintf(astFd, "(L %s_break)", st->label);
		} else {
			/* Fallback: emit original DO format */
			fdprintf(astFd, "(D ");
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, " ");
			emitExpr(st->left);
			fdprintf(astFd, ")");
		}
		break;

	case FOR:
		/*
		 * Emit as labeled sequence:
		 * (init) Lxxx_top: (if !cond goto break) (body) Lxxx_continue:
		 * (increment) (goto top) Lxxx_break:
		 */
		if (st->label) {
			/* Init expression */
			if (st->left) {
				fdprintf(astFd, "(E ");
				emitExpr(st->left);
				fdprintf(astFd, ") ");
			}
			/* Top label */
			fdprintf(astFd, "(L %s_top) ", st->label);
			/* Test condition */
			if (st->middle) {
				fdprintf(astFd, "(I ");
				emitExpr(st->middle);
				fdprintf(astFd, " ");
				/* Body */
				if (st->chain) {
					emitStmt(st->chain);
				} else {
					fdprintf(astFd, "()");
				}
				fdprintf(astFd, " ()) ");  /* close IF */
			} else {
				/* No condition - always execute body */
				if (st->chain) {
					emitStmt(st->chain);
				} else {
					fdprintf(astFd, "()");
				}
				fdprintf(astFd, " ");
			}
			/* Continue label */
			fdprintf(astFd, "(L %s_continue) ", st->label);
			/* Increment expression */
			if (st->right) {
				fdprintf(astFd, "(E ");
				emitExpr(st->right);
				fdprintf(astFd, ") ");
			}
			/* Goto top */
			fdprintf(astFd, "(G %s_top) ", st->label);
			/* Break label */
			fdprintf(astFd, "(L %s_break)", st->label);
		} else {
			/* Fallback: emit original FOR format */
			fdprintf(astFd, "(F ");
			emitExpr(st->left);
			fdprintf(astFd, " ");
			emitExpr(st->middle);
			fdprintf(astFd, " ");
			emitExpr(st->right);
			fdprintf(astFd, " ");
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, ")");
		}
		break;

	case SWITCH:
		/* Emit as labeled sequence with break label:
		 * Sxxx_top: (switch expr body) Sxxx_break: */
		if (st->label) {
			/* Top label */
			fdprintf(astFd, "(L %s_top) ", st->label);
			/* Original switch structure */
			fdprintf(astFd, "(S ");
			emitExpr(st->left);
			fdprintf(astFd, " ");
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, ") ");
			/* Break label */
			fdprintf(astFd, "(L %s_break)", st->label);
		} else {
			/* Fallback: emit original SWITCH format */
			fdprintf(astFd, "(S ");
			emitExpr(st->left);
			fdprintf(astFd, " ");
			if (st->chain) {
				emitStmt(st->chain);
			} else {
				fdprintf(astFd, "()");
			}
			fdprintf(astFd, ")");
		}
		break;

	case CASE:
		fdprintf(astFd, "(C ");  /* Case */
		emitExpr(st->left);
		fdprintf(astFd, " ");
		if (st->chain) {
			emitStmt(st->chain);
		} else {
			fdprintf(astFd, "()");
		}
		fdprintf(astFd, ")");
		break;

	case DEFAULT:
		fdprintf(astFd, "(O ");  /* Default (O for default) */
		if (st->chain) {
			emitStmt(st->chain);
		} else {
			fdprintf(astFd, "()");
		}
		fdprintf(astFd, ")");
		break;

	case RETURN:
		fdprintf(astFd, "(R ");  /* Return */
		if (st->left) {
			emitExpr(st->left);
		}
		fdprintf(astFd, ")");
		break;

	case BREAK:
		fdprintf(astFd, "(K)");  /* Break (K for breaK) */
		break;

	case CONTINUE:
		fdprintf(astFd, "(N)");  /* Continue (N from CONTINUE token) */
		break;

	case GOTO:
		fdprintf(astFd, "(G %s)", st->label ? st->label : "?");
		break;

	case LABEL:
		fdprintf(astFd, "(L %s)", st->label ? st->label : "?");
		break;

	case EXPR:
		fdprintf(astFd, "(E ");  /* Expression statement */
		emitExpr(st->left);
		fdprintf(astFd, ")");
		break;

	case ';':
		fdprintf(astFd, "(;)");  /* Empty statement */
		break;

	case ASM:
		fdprintf(astFd, "(A ");  /* Assembly block */
		if (st->label) {
			/* Emit the assembly text as a quoted, escaped string */
			char escaped[8192];  /* Large buffer for escaped text */
			int i, j = 0;
			escaped[j++] = '\"';
			for (i = 0; st->label[i] && j < sizeof(escaped) - 4; i++) {
				unsigned char c = st->label[i];
				if (c == '\"' || c == '\\') {
					escaped[j++] = '\\';
					escaped[j++] = c;
				} else if (c == '\n') {
					escaped[j++] = '\\';
					escaped[j++] = 'n';
				} else if (c == '\t') {
					escaped[j++] = '\\';
					escaped[j++] = 't';
				} else if (c < 0x20 || c >= 0x7f) {
					/* Octal escape for non-printable characters */
					escaped[j++] = '\\';
					escaped[j++] = '0' + ((c >> 6) & 7);
					escaped[j++] = '0' + ((c >> 3) & 7);
					escaped[j++] = '0' + (c & 7);
				} else {
					escaped[j++] = c;
				}
			}
			escaped[j++] = '\"';
			escaped[j] = 0;
			fdprintf(astFd, "%s", escaped);
		} else {
			fdprintf(astFd, "\"\"");
		}
		fdprintf(astFd, ")");
		break;

	default:
		fdprintf(astFd, "(?%d)", st->op);
		break;
	}

	/* Output sibling statements */
	if (st->next) {
		fdprintf(astFd, " ");
		emitStmt(st->next);
	}
}

/*
 * Output function parameter list
 */
static void
emitParams(struct type *functype)
{
	struct name *param;
	int first = 1;

	fdprintf(astFd, "(");
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (!first) fdprintf(astFd, " ");
			first = 0;
			if (param->name && param->name[0] != '\0') {
				fdprintf(astFd, "%s", param->name);
			} else {
				fdprintf(astFd, "_");  /* anonymous parameter */
			}
			if (param->type && param->type->name) {
				fdprintf(astFd, ":%s", param->type->name);
			}
		}
	}
	fdprintf(astFd, ")");
}

/*
 * Output declaration nodes for function parameters and local variables
 * Called while variables are still in scope (before popScope)
 */
static void
emitDecls(struct name *func)
{
	extern struct name **names;
	extern int lastname;
	struct name *n;
	int i;
	int has_decls = 0;

	/* Iterate through names array looking for parameters and locals */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (!n)
			continue;

		/* Skip if not at function scope or deeper */
		if (n->level < 2)
			continue;

		/* Skip tags, typedefs, and function definitions */
		if (n->is_tag || n->kind == tdef || n->kind == fdef)
			continue;

		/*
		 * Emit declaration node: (d varname type) - only for function
		 * parameters. Local variables are emitted in their containing
		 * blocks, not at function level
		 */
		if (n->kind == funarg) {
			if (!has_decls) {
				fdprintf(astFd, "\n  ");
				has_decls = 1;
			} else {
				fdprintf(astFd, " ");
			}
			fdprintf(astFd, "(d %s", n->name);
			emitTypeInfo(n->type);
			fdprintf(astFd, ")");
		}
	}
}

/*
 * Output a function in AST format
 */
void
emitFunction(struct name *func)
{
	const char *func_name;

	if (!func || !func->u.body)
		return;

	/* Use mangled name for static functions, otherwise use original name */
	func_name = func->mangled_name ? func->mangled_name : func->name;

	fdprintf(astFd, "\n; Function: %s\n", func_name);
	fdprintf(astFd, "(f %s ", func_name);

	/* Output parameter list */
	if (func->type) {
		emitParams(func->type);
		/* Output return type */
		if (func->type->sub) {
			emitTypeInfo(func->type->sub);
		} else {
			fdprintf(astFd, " _void_");
		}
	} else {
		fdprintf(astFd, "() _void_");
	}

	/* Output declarations for parameters and local variables */
	emitDecls(func);

	/* Output function body */
	fdprintf(astFd, "\n  ");
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

	fdprintf(astFd, "([:%c", width);
	for (item = init; item; item = item->next) {
		fdprintf(astFd, " ");
		emitExpr(item);
	}
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

	/* Output: (s name "literal_data") */
	fdprintf(astFd, "\n; String literal: %s\n", strname->name);
	fdprintf(astFd, "(s %s \"", strname->name);
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
 * Format: (g varname type [init-expr])
 */
void
emitGv(struct name *var)
{
	if (!var || !var->type)
		return;

	fdprintf(astFd, "\n; Global variable: %s\n", var->name);
	fdprintf(astFd, "(g ");

	/* Output variable name with scope prefix */
	if (var->sclass & SC_STATIC) {
		/* Use mangled name for statics */
		if (var->mangled_name) {
			fdprintf(astFd, "$S%s", var->mangled_name);
		} else {
			fdprintf(astFd, "$S%s", var->name);
		}
	} else if (var->sclass & SC_EXTERN) {
		fdprintf(astFd, "$_%s", var->name);
	} else {
		/* Global variable (not extern, not static) */
		fdprintf(astFd, "$_%s", var->name);
	}

	/* Output type */
	emitTypeInfo(var->type);

	/* Output initializer if present */
	if (var->u.init) {
		fdprintf(astFd, " ");
		/* Check if this is an initializer list (has next pointers) */
		if (var->u.init->next) {
			/*
			 * For arrays, pass element type (var->type->sub)
			 * for width annotation
			 */
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
