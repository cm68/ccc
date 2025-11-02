/*
 * AST serialization for second pass
 * Outputs statements and expressions in a parseable S-expression format
 */
#include "cc1.h"

/* Helper macro to output to ast_output or stdout */
#define OUT (ast_output ? ast_output : stdout)

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
		fprintf(OUT, "()");
		return;
	}

	switch (e->op) {
	case CONST:
		fprintf(OUT, "%ld", e->v);
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
					fprintf(OUT, "$S%s", sym->mangled_name);
				} else {
					/* Fallback if mangled name not set */
					fprintf(OUT, "$S%s", sym->name);
				}
			} else if (sym->sclass & SC_EXTERN) {
				fprintf(OUT, "$_%s", sym->name);
			} else if (sym->level == 1) {
				/* Global variable (not extern, not static) */
				fprintf(OUT, "$_%s", sym->name);
			} else if (sym->kind == funarg) {
				/* Function argument */
				fprintf(OUT, "$A%s", sym->name);
			} else {
				/* Local variable */
				fprintf(OUT, "$%s", sym->name);
			}
		} else {
			fprintf(OUT, "$?");
		}
		break;

	case STRING:
		/* String literals - output as string index */
		fprintf(OUT, "S%ld", e->v);
		break;

	case CALL:
		/* Function call: (@ func arg1 arg2 ...) */
		fprintf(OUT, "(@");
		if (e->left) {
			fprintf(OUT, " ");
			emit_expr(e->left);
		}
		/* Arguments are in e->right and linked via next */
		if (e->right) {
			struct expr *arg;
			for (arg = e->right; arg; arg = arg->next) {
				fprintf(OUT, " ");
				emit_expr(arg);
			}
		}
		fprintf(OUT, ")");
		break;

	default:
		/* Operator - output in prefix notation */
		fprintf(OUT, "(%c", e->op);
		if (e->left) {
			fprintf(OUT, " ");
			emit_expr(e->left);
		}
		if (e->right) {
			fprintf(OUT, " ");
			emit_expr(e->right);
		}
		fprintf(OUT, ")");
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
		fprintf(OUT, "(B");  /* Block */
		if (st->chain) {
			fprintf(OUT, " ");
			emit_stmt(st->chain);
		}
		fprintf(OUT, ")");
		break;

	case IF:
		fprintf(OUT, "(I ");  /* If */
		emit_expr(st->left);
		fprintf(OUT, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		if (st->otherwise) {
			fprintf(OUT, " ");
			emit_stmt(st->otherwise);
		}
		fprintf(OUT, ")");
		break;

	case WHILE:
		fprintf(OUT, "(W ");  /* While */
		emit_expr(st->left);
		fprintf(OUT, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		fprintf(OUT, ")");
		break;

	case DO:
		fprintf(OUT, "(D ");  /* Do-while */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		fprintf(OUT, " ");
		emit_expr(st->left);
		fprintf(OUT, ")");
		break;

	case FOR:
		fprintf(OUT, "(F ");  /* For */
		emit_expr(st->left);    /* init */
		fprintf(OUT, " ");
		emit_expr(st->middle);  /* condition */
		fprintf(OUT, " ");
		emit_expr(st->right);   /* increment */
		fprintf(OUT, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		fprintf(OUT, ")");
		break;

	case SWITCH:
		fprintf(OUT, "(S ");  /* Switch */
		emit_expr(st->left);
		fprintf(OUT, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		fprintf(OUT, ")");
		break;

	case CASE:
		fprintf(OUT, "(C ");  /* Case */
		emit_expr(st->left);
		fprintf(OUT, " ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		fprintf(OUT, ")");
		break;

	case DEFAULT:
		fprintf(OUT, "(O ");  /* Default (O for default) */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			fprintf(OUT, "()");
		}
		fprintf(OUT, ")");
		break;

	case RETURN:
		fprintf(OUT, "(R ");  /* Return */
		if (st->left) {
			emit_expr(st->left);
		}
		fprintf(OUT, ")");
		break;

	case BREAK:
		fprintf(OUT, "(K)");  /* Break (K for breaK) */
		break;

	case CONTINUE:
		fprintf(OUT, "(N)");  /* Continue (N from CONTINUE token) */
		break;

	case GOTO:
		fprintf(OUT, "(G %s)", st->label ? st->label : "?");
		break;

	case LABEL:
		fprintf(OUT, "(L %s)", st->label ? st->label : "?");
		break;

	case EXPR:
		fprintf(OUT, "(E ");  /* Expression statement */
		emit_expr(st->left);
		fprintf(OUT, ")");
		break;

	case ';':
		fprintf(OUT, "(;)");  /* Empty statement */
		break;

	default:
		fprintf(OUT, "(?%d)", st->op);
		break;
	}

	/* Output sibling statements */
	if (st->next) {
		fprintf(OUT, " ");
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

	fprintf(OUT, "(");
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (!first) fprintf(OUT, " ");
			first = 0;
			if (param->name && param->name[0] != '\0') {
				fprintf(OUT, "%s", param->name);
			} else {
				fprintf(OUT, "_");  /* anonymous parameter */
			}
			if (param->type && param->type->name) {
				fprintf(OUT, ":%s", param->type->name);
			}
		}
	}
	fprintf(OUT, ")");
}

/*
 * Output a function in AST format
 */
void
emit_function(struct name *func)
{
	if (!func || !func->body)
		return;

	fprintf(OUT, "\n; Function: %s\n", func->name);
	fprintf(OUT, "(func %s ", func->name);

	/* Output parameter list */
	if (func->type) {
		emit_params(func->type);
		/* Output return type */
		fprintf(OUT, " ");
		if (func->type->sub && func->type->sub->name) {
			fprintf(OUT, "%s", func->type->sub->name);
		} else {
			fprintf(OUT, "void");
		}
	} else {
		fprintf(OUT, "() void");
	}

	/* Output function body */
	fprintf(OUT, "\n  ");
	emit_stmt(func->body);
	fprintf(OUT, ")\n");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
