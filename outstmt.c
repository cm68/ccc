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
		printf("()");
		return;
	}

	switch (e->op) {
	case CONST:
		printf("%ld", e->v);
		break;

	case SYM:
		if (e->var) {
			sym = (struct name *)e->var;
			printf("$%s", sym->name);
		} else {
			printf("$?");
		}
		break;

	case STRING:
		/* String literals - output as string index */
		printf("S%ld", e->v);
		break;

	case CALL:
		/* Function call: (@ func arg1 arg2 ...) */
		printf("(@");
		if (e->left) {
			printf(" ");
			emit_expr(e->left);
		}
		/* Arguments are in e->right and linked via next */
		if (e->right) {
			struct expr *arg;
			for (arg = e->right; arg; arg = arg->next) {
				printf(" ");
				emit_expr(arg);
			}
		}
		printf(")");
		break;

	default:
		/* Operator - output in prefix notation */
		printf("(%c", e->op);
		if (e->left) {
			printf(" ");
			emit_expr(e->left);
		}
		if (e->right) {
			printf(" ");
			emit_expr(e->right);
		}
		printf(")");
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
		printf("(B");  /* Block */
		if (st->chain) {
			printf(" ");
			emit_stmt(st->chain);
		}
		printf(")");
		break;

	case IF:
		printf("(I ");  /* If */
		emit_expr(st->left);
		printf(" ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		if (st->otherwise) {
			printf(" ");
			emit_stmt(st->otherwise);
		}
		printf(")");
		break;

	case WHILE:
		printf("(W ");  /* While */
		emit_expr(st->left);
		printf(" ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		printf(")");
		break;

	case DO:
		printf("(D ");  /* Do-while */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		printf(" ");
		emit_expr(st->left);
		printf(")");
		break;

	case FOR:
		printf("(F ");  /* For */
		emit_expr(st->left);    /* init */
		printf(" ");
		emit_expr(st->middle);  /* condition */
		printf(" ");
		emit_expr(st->right);   /* increment */
		printf(" ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		printf(")");
		break;

	case SWITCH:
		printf("(S ");  /* Switch */
		emit_expr(st->left);
		printf(" ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		printf(")");
		break;

	case CASE:
		printf("(C ");  /* Case */
		emit_expr(st->left);
		printf(" ");
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		printf(")");
		break;

	case DEFAULT:
		printf("(O ");  /* Default (O for default) */
		if (st->chain) {
			emit_stmt(st->chain);
		} else {
			printf("()");
		}
		printf(")");
		break;

	case RETURN:
		printf("(R ");  /* Return */
		if (st->left) {
			emit_expr(st->left);
		}
		printf(")");
		break;

	case BREAK:
		printf("(K)");  /* Break (K for breaK) */
		break;

	case CONTINUE:
		printf("(N)");  /* Continue (N from CONTINUE token) */
		break;

	case GOTO:
		printf("(G %s)", st->label ? st->label : "?");
		break;

	case LABEL:
		printf("(L %s)", st->label ? st->label : "?");
		break;

	case EXPR:
		printf("(E ");  /* Expression statement */
		emit_expr(st->left);
		printf(")");
		break;

	case ';':
		printf("(;)");  /* Empty statement */
		break;

	default:
		printf("(?%d)", st->op);
		break;
	}

	/* Output sibling statements */
	if (st->next) {
		printf(" ");
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

	printf("(");
	if (functype && (functype->flags & TF_FUNC)) {
		for (param = functype->elem; param; param = param->next) {
			if (!first) printf(" ");
			first = 0;
			if (param->name && param->name[0] != '\0') {
				printf("%s", param->name);
			} else {
				printf("_");  /* anonymous parameter */
			}
			if (param->type && param->type->name) {
				printf(":%s", param->type->name);
			}
		}
	}
	printf(")");
}

/*
 * Output a function in AST format
 */
void
emit_function(struct name *func)
{
	if (!func || !func->body)
		return;

	printf("\n; Function: %s\n", func->name);
	printf("(func %s ", func->name);

	/* Output parameter list */
	if (func->type) {
		emit_params(func->type);
		/* Output return type */
		printf(" ");
		if (func->type->sub && func->type->sub->name) {
			printf("%s", func->type->sub->name);
		} else {
			printf("void");
		}
	} else {
		printf("() void");
	}

	/* Output function body */
	printf("\n  ");
	emit_stmt(func->body);
	printf(")\n");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
