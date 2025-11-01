/*
 * AST (Abstract Syntax Tree) output functions
 * Dump statement and expression trees for debugging
 */
#include "ccc.h"

#ifdef DEBUG

/*
 * Dump expression tree recursively for debugging
 */
static void
dump_expr_tree(struct expr *e, int indent)
{
	int i;
	struct name *n;

	if (!e)
		return;

	/* Print indentation */
	for (i = 0; i < indent; i++)
		printf("  ");

	/* Print expression info based on operator */
	switch (e->op) {
	case CONST:
		printf("CONST %ld (0x%lx)", e->v, e->v);
		break;
	case STRING:
		if (e->var) {
			n = (struct name *)e->var;
			printf("STRING \"%s\"", (char *)(e->v + 1));  /* skip length byte */
		} else {
			printf("STRING (no name)");
		}
		break;
	case SYM:
		if (e->var) {
			struct name *sym = (struct name *)e->var;
			/* Note: the name structure might have been freed by pop_scope */
			/* Display what we can safely, but be defensive */
			printf("SYM");
			if (sym->name) {
				printf(" '%s'", sym->name);
			}
			if (sym->type && sym->type->name) {
				printf(" (%s)", sym->type->name);
			}
			printf(" [level=%d]", sym->level);
			/* Also show the pointer for debugging */
			printf(" @%p", e->var);
		} else {
			printf("SYM (null)");
		}
		break;
	default:
		/* Use tokenname for operator display */
		if (e->op >= 0 && e->op < 256) {
			printf("%s", tokenname[(unsigned char)e->op]);
		} else {
			printf("OP_%d", e->op);
		}
		break;
	}

	/* Show constant flag if set */
	if (e->flags & E_CONST)
		printf(" [const]");

	printf("\n");

	/* Recursively dump left and right subtrees */
	if (e->left) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("left:\n");
		dump_expr_tree(e->left, indent + 1);
	}
	if (e->right) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("right:\n");
		dump_expr_tree(e->right, indent + 1);
	}
}

void
dump_expr(struct expr *e)
{
	if (!e) {
		printf("(null expr)\n");
		return;
	}
	dump_expr_tree(e, 0);
}

void
dump_expr_indent(struct expr *e, int indent)
{
	if (!e) {
		int i;
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("(null expr)\n");
		return;
	}
	dump_expr_tree(e, indent);
}

/*
 * Dump a statement tree recursively for debugging
 */
static void
dump_stmt_tree(struct stmt *st, int indent)
{
	int i;
	const char *op_name;

	if (!st)
		return;

	/* Print indentation */
	for (i = 0; i < indent; i++)
		printf("  ");

	/* Print statement info */
	switch (st->op) {
	case BEGIN: op_name = "BEGIN"; break;
	case IF: op_name = "IF"; break;
	case WHILE: op_name = "WHILE"; break;
	case FOR: op_name = "FOR"; break;
	case DO: op_name = "DO"; break;
	case SWITCH: op_name = "SWITCH"; break;
	case CASE: op_name = "CASE"; break;
	case DEFAULT: op_name = "DEFAULT"; break;
	case RETURN: op_name = "RETURN"; break;
	case BREAK: op_name = "BREAK"; break;
	case CONTINUE: op_name = "CONTINUE"; break;
	case GOTO: op_name = "GOTO"; break;
	case LABEL: op_name = "LABEL"; break;
	case EXPR: op_name = "EXPR"; break;
	case ';': op_name = "EMPTY"; break;
	default: op_name = "UNKNOWN"; break;
	}

	printf("%s", op_name);
	if (st->label)
		printf(" (%s)", st->label);
	printf("\n");

	/* Dump expressions attached to this statement */
	if (st->left) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("expr:\n");
		dump_expr_indent(st->left, indent + 1);
	}
	if (st->right) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("right:\n");
		dump_expr_indent(st->right, indent + 1);
	}
	if (st->middle) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("middle:\n");
		dump_expr_indent(st->middle, indent + 1);
	}

	/* Recursively dump child statements */
	if (st->chain) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("chain:\n");
		dump_stmt_tree(st->chain, indent + 1);
	}
	if (st->otherwise) {
		for (i = 0; i < indent; i++)
			printf("  ");
		printf("else:\n");
		dump_stmt_tree(st->otherwise, indent + 1);
	}

	/* Dump sibling statements */
	if (st->next)
		dump_stmt_tree(st->next, indent);
}

void
dump_stmt(struct stmt *st)
{
	if (!st) {
		printf("(null statement tree)\n");
		return;
	}
	dump_stmt_tree(st, 0);
}

#endif /* DEBUG */

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
