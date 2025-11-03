/*
 * AST serialization for second pass
 * Outputs statements and expressions in a parseable S-expression format
 */
#include "cc1.h"

/*
 * Output an expression in S-expression format
 * Constants: just the value (decimal)
 * Symbols: $name
 * Binary ops: (op left right)
 * Unary ops: (op operand)
 */
static void
emit_expr(struct expr *e)
{
	struct name *sym;

	if (!e) {
		fprintf(ast_output, "()");
		return;
	}

	switch (e->op) {
	case CONST:
		fprintf(ast_output, "%ld", e->v);
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
					fprintf(ast_output, "$S%s", sym->mangled_name);
				} else {
					/* Fallback if mangled name not set */
					fprintf(ast_output, "$S%s", sym->name);
				}
			} else if (sym->sclass & SC_EXTERN) {
				fprintf(ast_output, "$_%s", sym->name);
			} else if (sym->level == 1) {
				/* Global variable (not extern, not static) */
				fprintf(ast_output, "$_%s", sym->name);
			} else if (sym->kind == funarg) {
				/* Function argument */
				fprintf(ast_output, "$A%s", sym->name);
			} else {
				/* Local variable */
				fprintf(ast_output, "$%s", sym->name);
			}
		} else {
			fprintf(ast_output, "$?");
		}
		break;

	case STRING:
		/* String literals - output as string index */
		fprintf(ast_output, "S%ld", e->v);
		break;

	case CALL:
		/* Function call: (@ func arg1 arg2 ...) */
		fprintf(ast_output, "(@");
		if (e->left) {
			fprintf(ast_output, " ");
			emit_expr(e->left);
		}
		/* Arguments are in e->right and linked via next */
		if (e->right) {
			struct expr *arg;
			for (arg = e->right; arg; arg = arg->next) {
				fprintf(ast_output, " ");
				emit_expr(arg);
			}
		}
		fprintf(ast_output, ")");
		break;

	default:
		/* Operator - output in prefix notation */
		fprintf(ast_output, "(%c", e->op);
		if (e->left) {
			fprintf(ast_output, " ");
			emit_expr(e->left);
		}
		if (e->right) {
			fprintf(ast_output, " ");
			emit_expr(e->right);
		}
		fprintf(ast_output, ")");
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
		fprintf(ast_output, " %s", type->name);
	} else if (type->flags & TF_AGGREGATE) {
		/* Struct/union without type name - output size */
		fprintf(ast_output, " :struct:%d", type->size);
	} else if (type->flags & TF_POINTER) {
		fprintf(ast_output, " :ptr");
	} else if (type->flags & TF_ARRAY) {
		fprintf(ast_output, " :array:%d", type->count);
	} else {
		/* Unknown type - output size */
		fprintf(ast_output, " :size:%d", type->size);
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
		fprintf(ast_output, "(B");  /* Block */

		/* Emit declarations for local variables in this scope */
		if (st->locals) {
			struct name *local;
			for (local = st->locals; local; local = local->next) {
				fprintf(ast_output, " (d %s", local->name);
				emit_type_info(local->type);
				fprintf(ast_output, ")");
			}
		}

		if (st->chain) {
			fprintf(ast_output, " ");
			emit_stmt(st->chain);
		}
		fprintf(ast_output, ")");
		break;

	case IF:
		fprintf(ast_output, "(I ");  /* If */
		emit_expr(st->left);
		fprintf(ast_output, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		if (st->otherwise) {
			fprintf(ast_output, " ");
			emit_stmt(st->otherwise);
		}
		fprintf(ast_output, ")");
		break;

	case WHILE:
		fprintf(ast_output, "(W ");  /* While */
		emit_expr(st->left);
		fprintf(ast_output, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		fprintf(ast_output, ")");
		break;

	case DO:
		fprintf(ast_output, "(D ");  /* Do-while */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		fprintf(ast_output, " ");
		emit_expr(st->left);
		fprintf(ast_output, ")");
		break;

	case FOR:
		fprintf(ast_output, "(F ");  /* For */
		emit_expr(st->left);    /* init */
		fprintf(ast_output, " ");
		emit_expr(st->middle);  /* condition */
		fprintf(ast_output, " ");
		emit_expr(st->right);   /* increment */
		fprintf(ast_output, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		fprintf(ast_output, ")");
		break;

	case SWITCH:
		fprintf(ast_output, "(S ");  /* Switch */
		emit_expr(st->left);
		fprintf(ast_output, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		fprintf(ast_output, ")");
		break;

	case CASE:
		fprintf(ast_output, "(C ");  /* Case */
		emit_expr(st->left);
		fprintf(ast_output, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		fprintf(ast_output, ")");
		break;

	case DEFAULT:
		fprintf(ast_output, "(O ");  /* Default (O for default) */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(ast_output, "()");
		}
		fprintf(ast_output, ")");
		break;

	case RETURN:
		fprintf(ast_output, "(R ");  /* Return */
		if (st->left) {
			emit_expr(st->left);
		}
		fprintf(ast_output, ")");
		break;

	case BREAK:
		fprintf(ast_output, "(K)");  /* Break (K for breaK) */
		break;

	case CONTINUE:
		fprintf(ast_output, "(N)");  /* Continue (N from CONTINUE token) */
		break;

	case GOTO:
		fprintf(ast_output, "(G %s)", st->label ? st->label : "?");
		break;

	case LABEL:
		fprintf(ast_output, "(L %s)", st->label ? st->label : "?");
		break;

	case EXPR:
		fprintf(ast_output, "(E ");  /* Expression statement */
		emit_expr(st->left);
		fprintf(ast_output, ")");
		break;

	case ';':
		fprintf(ast_output, "(;)");  /* Empty statement */
		break;

	default:
		fprintf(ast_output, "(?%d)", st->op);
		break;
	}

	/* Output sibling statements */
	if (st->next) {
		fprintf(ast_output, " ");
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

	fprintf(ast_output, "(");
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (!first) fprintf(ast_output, " ");
			first = 0;
			if (param->name && param->name[0] != '\0') {
				fprintf(ast_output, "%s", param->name);
			} else {
				fprintf(ast_output, "_");  /* anonymous parameter */
			}
			if (param->type && param->type->name) {
				fprintf(ast_output, ":%s", param->type->name);
			}
		}
	}
	fprintf(ast_output, ")");
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

		/* Emit declaration node: (d varname type) */
		if (n->kind == funarg || n->kind == var || n->kind == local) {
			if (!has_decls) {
				fprintf(ast_output, "\n  ");
				has_decls = 1;
			} else {
				fprintf(ast_output, " ");
			}
			fprintf(ast_output, "(d %s", n->name);
			emit_type_info(n->type);
			fprintf(ast_output, ")");
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

	fprintf(ast_output, "\n; Function: %s\n", func->name);
	fprintf(ast_output, "(f %s ", func->name);

	/* Output parameter list */
	if (func->type) {
		emit_params(func->type);
		/* Output return type */
		fprintf(ast_output, " ");
		if (func->type->sub && func->type->sub->name) {
			fprintf(ast_output, "%s", func->type->sub->name);
		} else {
			fprintf(ast_output, "void");
		}
	} else {
		fprintf(ast_output, "() void");
	}

	/* Output declarations for parameters and local variables */
	emit_declarations(func);

	/* Output function body */
	fprintf(ast_output, "\n  ");
	emit_stmt(func->body);
	fprintf(ast_output, ")\n");
}

/*
 * Emit an initializer list (linked via next pointers)
 * Used for array/struct initializers like {1, 2, 3}
 */
static void
emit_initializer_list(struct expr *init)
{
	struct expr *item;

	fprintf(ast_output, "(list");
	for (item = init; item; item = item->next) {
		fprintf(ast_output, " ");
		emit_expr(item);
	}
	fprintf(ast_output, ")");
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

	fprintf(ast_output, "\n; Global variable: %s\n", var->name);
	fprintf(ast_output, "(g ");

	/* Output variable name with scope prefix */
	if (var->sclass & SC_STATIC) {
		/* Use mangled name for statics */
		if (var->mangled_name) {
			fprintf(ast_output, "$S%s", var->mangled_name);
		} else {
			fprintf(ast_output, "$S%s", var->name);
		}
	} else if (var->sclass & SC_EXTERN) {
		fprintf(ast_output, "$_%s", var->name);
	} else {
		/* Global variable (not extern, not static) */
		fprintf(ast_output, "$_%s", var->name);
	}

	/* Output type */
	emit_type_info(var->type);

	/* Output initializer if present */
	if (var->init) {
		fprintf(ast_output, " ");
		/* Check if this is an initializer list (has next pointers) */
		if (var->init->next) {
			emit_initializer_list(var->init);
		} else {
			emit_expr(var->init);
		}
	}

	fprintf(ast_output, ")\n");
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

		/* Only emit variables at global scope (level 1) */
		if (n->level != 1)
			continue;

		/* Skip tags, typedefs, and functions */
		if (n->is_tag || n->kind == tdef || n->kind == fdef)
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
