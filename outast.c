/*
 * AST serialization for second pass
 * Outputs statements and expressions in a parseable S-expression format
 */
#include "cc1.h"

/* Forward declarations */
static void emit_type_info(struct type *type);

/*
 * Get size suffix for memory operations based on type
 * Returns: 'b' (byte), 's' (short/int), 'l' (long), 'p' (pointer), 'f' (float), 'd' (double)
 */
static char
get_size_suffix(struct type *t)
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
 * Output an expression in S-expression format
 * Constants: just the value (decimal)
 * Symbols: $name
 * Binary ops: (op left right)
 * Unary ops: (op operand)
 * Memory ops annotated with size: (M:b expr) (=:l lvalue rvalue)
 */
static void
emit_expr(struct expr *e)
{
	struct name *sym;

	if (!e) {
		fdprintf(ast_fd, "()");
		return;
	}

	switch (e->op) {
	case CONST:
		fdprintf(ast_fd, "%ld", e->v);
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
				if (sym->mangled_name) {
					fdprintf(ast_fd, "$S%s", sym->mangled_name);
				} else {
					/* Fallback if mangled name not set */
					fdprintf(ast_fd, "$S%s", sym->name);
				}
			} else if (sym->sclass & SC_EXTERN) {
				fdprintf(ast_fd, "$_%s", sym->name);
			} else if (sym->level == 1) {
				/* Global variable (not extern, not static) */
				fdprintf(ast_fd, "$_%s", sym->name);
			} else if (sym->kind == funarg) {
				/* Function argument */
				fdprintf(ast_fd, "$A%s", sym->name);
			} else {
				/* Local variable */
				fdprintf(ast_fd, "$%s", sym->name);
			}
		} else {
			fdprintf(ast_fd, "$?");
		}
		break;

	case STRING:
		/* String literals - output as reference to global synthetic name */
		if (e->var) {
			struct name *strname = (struct name *)e->var;
			/* Synthetic string names are global variables, use $_ prefix */
			fdprintf(ast_fd, "$_%s", strname->name);
		} else {
			/* Fallback to address if name not available */
			fdprintf(ast_fd, "S%ld", e->v);
		}
		break;

	case CALL:
		/* Function call: (@ func arg1 arg2 ...) */
		fdprintf(ast_fd, "(@");
		if (e->left) {
			fdprintf(ast_fd, " ");
			emit_expr(e->left);
		}
		/* Arguments are in e->right and linked via next */
		if (e->right) {
			struct expr *arg;
			for (arg = e->right; arg; arg = arg->next) {
				fdprintf(ast_fd, " ");
				emit_expr(arg);
			}
		}
		fdprintf(ast_fd, ")");
		break;

	case NARROW:
	case WIDEN:
	case SEXT:
		/* Cast operators with destination width annotation */
		{
			char size_suffix = get_size_suffix(e->type);
			char op_char = (e->op == NARROW) ? 'N' : (e->op == WIDEN) ? 'W' : 'X';
			fdprintf(ast_fd, "(%c:%c", op_char, size_suffix);
			if (e->left) {
				fdprintf(ast_fd, " ");
				emit_expr(e->left);
			}
			fdprintf(ast_fd, ")");
		}
		break;

	case COPY:
		/* Memory copy operator: (Y:length dest src) */
		fdprintf(ast_fd, "(Y:%ld", e->v);  /* v field contains byte count */
		if (e->left) {
			fdprintf(ast_fd, " ");
			emit_expr(e->left);
		}
		if (e->right) {
			fdprintf(ast_fd, " ");
			emit_expr(e->right);
		}
		fdprintf(ast_fd, ")");
		break;

	default:
		/* Operator - output in prefix notation */
		/* For DEREF (M) and ASSIGN (=), add size annotation */
		if (e->op == DEREF || e->op == ASSIGN) {
			char size_suffix = get_size_suffix(e->type);
			fdprintf(ast_fd, "(%c:%c", e->op, size_suffix);
		} else {
			fdprintf(ast_fd, "(%c", e->op);
		}
		if (e->left) {
			fdprintf(ast_fd, " ");
			emit_expr(e->left);
		}
		if (e->right) {
			fdprintf(ast_fd, " ");
			emit_expr(e->right);
		}
		fdprintf(ast_fd, ")");
		break;
	}
}

/*
 * Output type information for AST
 * For primitive types, outputs the type name
 * For aggregates (struct/union), outputs size
 */
static void
emit_type_info(struct type *type)
{
	if (!type)
		return;

	if (type->name && type->name[0]) {
		/* Primitive or named type */
		fdprintf(ast_fd, " %s", type->name);
	} else if (type->flags & TF_AGGREGATE) {
		/* Struct/union without type name - output size */
		fdprintf(ast_fd, " :struct:%d", type->size);
	} else if (type->flags & TF_POINTER) {
		fdprintf(ast_fd, " :ptr");
	} else if (type->flags & TF_ARRAY) {
		fdprintf(ast_fd, " :array:%d", type->count);
	} else {
		/* Unknown type - output size */
		fdprintf(ast_fd, " :size:%d", type->size);
	}
}

/*
 * Output a statement in S-expression format
 * Each statement type has its own format
 */
static void
emit_stmt(struct stmt *st)
{
	if (!st)
		return;

	/* Output this statement */
	switch (st->op) {
	case BEGIN:
		fdprintf(ast_fd, "(B");  /* Block */

		/* Emit declarations for local variables in this scope */
		if (st->locals) {
			struct name *local;
			for (local = st->locals; local; local = local->next) {
				fdprintf(ast_fd, " (d %s", local->name);
				emit_type_info(local->type);
				fdprintf(ast_fd, ")");
			}
		}

		if (st->chain) {
			fdprintf(ast_fd, " ");
			emit_stmt(st->chain);
		}
		fdprintf(ast_fd, ")");
		break;

	case IF:
		fdprintf(ast_fd, "(I ");  /* If */
		emit_expr(st->left);
		fdprintf(ast_fd, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		if (st->otherwise) {
			fdprintf(ast_fd, " ");
			emit_stmt(st->otherwise);
		}
		fdprintf(ast_fd, ")");
		break;

	case WHILE:
		fdprintf(ast_fd, "(W ");  /* While */
		emit_expr(st->left);
		fdprintf(ast_fd, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		fdprintf(ast_fd, ")");
		break;

	case DO:
		fdprintf(ast_fd, "(D ");  /* Do-while */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		fdprintf(ast_fd, " ");
		emit_expr(st->left);
		fdprintf(ast_fd, ")");
		break;

	case FOR:
		fdprintf(ast_fd, "(F ");  /* For */
		emit_expr(st->left);    /* init */
		fdprintf(ast_fd, " ");
		emit_expr(st->middle);  /* condition */
		fdprintf(ast_fd, " ");
		emit_expr(st->right);   /* increment */
		fdprintf(ast_fd, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		fdprintf(ast_fd, ")");
		break;

	case SWITCH:
		fdprintf(ast_fd, "(S ");  /* Switch */
		emit_expr(st->left);
		fdprintf(ast_fd, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		fdprintf(ast_fd, ")");
		break;

	case CASE:
		fdprintf(ast_fd, "(C ");  /* Case */
		emit_expr(st->left);
		fdprintf(ast_fd, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		fdprintf(ast_fd, ")");
		break;

	case DEFAULT:
		fdprintf(ast_fd, "(O ");  /* Default (O for default) */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fdprintf(ast_fd, "()");
		}
		fdprintf(ast_fd, ")");
		break;

	case RETURN:
		fdprintf(ast_fd, "(R ");  /* Return */
		if (st->left) {
			emit_expr(st->left);
		}
		fdprintf(ast_fd, ")");
		break;

	case BREAK:
		fdprintf(ast_fd, "(K)");  /* Break (K for breaK) */
		break;

	case CONTINUE:
		fdprintf(ast_fd, "(N)");  /* Continue (N from CONTINUE token) */
		break;

	case GOTO:
		fdprintf(ast_fd, "(G %s)", st->label ? st->label : "?");
		break;

	case LABEL:
		fdprintf(ast_fd, "(L %s)", st->label ? st->label : "?");
		break;

	case EXPR:
		fdprintf(ast_fd, "(E ");  /* Expression statement */
		emit_expr(st->left);
		fdprintf(ast_fd, ")");
		break;

	case ';':
		fdprintf(ast_fd, "(;)");  /* Empty statement */
		break;

	default:
		fdprintf(ast_fd, "(?%d)", st->op);
		break;
	}

	/* Output sibling statements */
	if (st->next) {
		fdprintf(ast_fd, " ");
		emit_stmt(st->next);
	}
}

/*
 * Output function parameter list
 */
static void
emit_params(struct type *functype)
{
	struct name *param;
	int first = 1;

	fdprintf(ast_fd, "(");
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (!first) fdprintf(ast_fd, " ");
			first = 0;
			if (param->name && param->name[0] != '\0') {
				fdprintf(ast_fd, "%s", param->name);
			} else {
				fdprintf(ast_fd, "_");  /* anonymous parameter */
			}
			if (param->type && param->type->name) {
				fdprintf(ast_fd, ":%s", param->type->name);
			}
		}
	}
	fdprintf(ast_fd, ")");
}

/*
 * Output declaration nodes for function parameters and local variables
 * Called while variables are still in scope (before pop_scope)
 */
static void
emit_declarations(struct name *func)
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

		/* Emit declaration node: (d varname type) - only for function parameters */
		/* Local variables are emitted in their containing blocks, not at function level */
		if (n->kind == funarg) {
			if (!has_decls) {
				fdprintf(ast_fd, "\n  ");
				has_decls = 1;
			} else {
				fdprintf(ast_fd, " ");
			}
			fdprintf(ast_fd, "(d %s", n->name);
			emit_type_info(n->type);
			fdprintf(ast_fd, ")");
		}
	}
}

/*
 * Output a function in AST format
 */
void
emit_function(struct name *func)
{
	if (!func || !func->body)
		return;

	fdprintf(ast_fd, "\n; Function: %s\n", func->name);
	fdprintf(ast_fd, "(f %s ", func->name);

	/* Output parameter list */
	if (func->type) {
		emit_params(func->type);
		/* Output return type */
		fdprintf(ast_fd, " ");
		if (func->type->sub && func->type->sub->name) {
			fdprintf(ast_fd, "%s", func->type->sub->name);
		} else {
			fdprintf(ast_fd, "void");
		}
	} else {
		fdprintf(ast_fd, "() void");
	}

	/* Output declarations for parameters and local variables */
	emit_declarations(func);

	/* Output function body */
	fdprintf(ast_fd, "\n  ");
	emit_stmt(func->body);
	fdprintf(ast_fd, ")\n");
}

/*
 * Emit an initializer list (linked via next pointers)
 * Used for array/struct initializers like {1, 2, 3}
 */
static void
emit_initializer_list(struct expr *init)
{
	struct expr *item;

	fdprintf(ast_fd, "(list");
	for (item = init; item; item = item->next) {
		fdprintf(ast_fd, " ");
		emit_expr(item);
	}
	fdprintf(ast_fd, ")");
}

/*
 * Emit string literals section
 * Outputs all string literal data for pass 2 to generate
 * Format: (literals (str addr "data")...)
 */
void
emit_literals(void)
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

		/* Look for synthetic string literal names (str0, str1, etc.) at any level */
		if (n->kind == var && n->init && n->init->op == STRING &&
		    n->name && strncmp(n->name, "str", 3) == 0 && n->name[3] >= '0' && n->name[3] <= '9') {
			found_any = 1;
			break;
		}
	}

	if (!found_any)
		return;

	/* Output literals section header */
	fdprintf(ast_fd, "\n; String literals\n");
	fdprintf(ast_fd, "(L\n");

	/* Second pass: output each string literal */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (!n)
			continue;

		/* Output string literal data (only synthetic str names at any level) */
		if (n->kind == var && n->init && n->init->op == STRING &&
		    n->name && strncmp(n->name, "str", 3) == 0 && n->name[3] >= '0' && n->name[3] <= '9') {
			cstring str = (cstring)n->init->v;
			if (str) {
				unsigned char len = (unsigned char)str[0];
				unsigned char *data = (unsigned char *)str + 1;
				int j;

				/* Output: (s name "literal_data") */
				fdprintf(ast_fd, "  (s %s \"", n->name);
				for (j = 0; j < len; j++) {
					unsigned char c = data[j];
					if (c == '"') {
						fdprintf(ast_fd, "\\\"");
					} else if (c == '\\') {
						fdprintf(ast_fd, "\\\\");
					} else if (c == '\n') {
						fdprintf(ast_fd, "\\n");
					} else if (c == '\t') {
						fdprintf(ast_fd, "\\t");
					} else if (c == '\r') {
						fdprintf(ast_fd, "\\r");
					} else if (c >= ' ' && c < 0x7f) {
						fdprintf(ast_fd, "%c", c);
					} else {
						fdprintf(ast_fd, "\\x%02x", c);
					}
				}
				fdprintf(ast_fd, "\")\n");
			}
		}
	}

	fdprintf(ast_fd, ")\n");
}

/*
 * Output a global variable declaration with optional initializer
 * Format: (g varname type [init-expr])
 */
void
emit_global_var(struct name *var)
{
	if (!var || !var->type)
		return;

	fdprintf(ast_fd, "\n; Global variable: %s\n", var->name);
	fdprintf(ast_fd, "(g ");

	/* Output variable name with scope prefix */
	if (var->sclass & SC_STATIC) {
		/* Use mangled name for statics */
		if (var->mangled_name) {
			fdprintf(ast_fd, "$S%s", var->mangled_name);
		} else {
			fdprintf(ast_fd, "$S%s", var->name);
		}
	} else if (var->sclass & SC_EXTERN) {
		fdprintf(ast_fd, "$_%s", var->name);
	} else {
		/* Global variable (not extern, not static) */
		fdprintf(ast_fd, "$_%s", var->name);
	}

	/* Output type */
	emit_type_info(var->type);

	/* Output initializer if present */
	if (var->init) {
		fdprintf(ast_fd, " ");
		/* Check if this is an initializer list (has next pointers) */
		if (var->init->next) {
			emit_initializer_list(var->init);
		} else {
			emit_expr(var->init);
		}
	}

	fdprintf(ast_fd, ")\n");
}

/*
 * Emit all global variables
 * Called after parsing completes with all names still in scope
 */
void
emit_global_vars(void)
{
	extern struct name **names;
	extern int lastname;
	struct name *n;
	int i;

	/* Iterate through names array looking for global variables */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (!n)
			continue;

		/* Emit global scope variables (level 1) and static locals (level > 1 with SC_STATIC) */
		if (n->level != 1 && n->sclass != SC_STATIC)
			continue;

		/* Skip tags, typedefs, and functions */
		if (n->is_tag || n->kind == tdef || n->kind == fdef)
			continue;

		/* Skip synthetic string literal names - they're in literals section */
		if (n->name && strncmp(n->name, "str", 3) == 0 && n->name[3] >= '0' && n->name[3] <= '9')
			continue;

		/* Emit global variable declaration */
		if (n->kind == var) {
			emit_global_var(n);
		}
	}
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
