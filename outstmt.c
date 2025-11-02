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
 * Output a function in AST format
 */
void
emit_function(struct name *func)
{
	if (!func || !func->body)
		return;

	fprintf(ast_output, "\n; Function: %s\n", func->name);
	fprintf(ast_output, "(func %s ", func->name);

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

	/* Output function body */
	fprintf(ast_output, "\n  ");
	emit_stmt(func->body);
	fprintf(ast_output, ")\n");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
