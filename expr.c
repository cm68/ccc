/*
 * generate expression trees
 */
#include "ccc.h"

#include "op_pri.h"

/*
 * operator precedence levels used in parse_expr
 * (from genop_pri.c - keep in sync)
 */
#define OP_PRI_PRIMARY     1   /* postfix/member access */
#define OP_PRI_MULT        3   /* * / % */
#define OP_PRI_ADD         4   /* + - */

/* PRI_ALL is defined in ccc.h as 0 - that's correct for "parse all operators" */

/*
 * counter for generating synthetic string literal names
 */
static int string_counter = 0;

struct expr *
makeexpr(char op, struct expr *left)
{
	struct expr *e;

	e = malloc(sizeof(*e));
	e->op = op;
	e->left = left;
	if (left) {
		e->cost = left->cost + 1;
	} else {
		e->cost = 1;
	}
	return e;
}

/*
 * deallocate an expression tree
 * this needs to not leak memory
 */
void
destroy_expr(struct expr *e)
{
	if (e->left) {
		destroy_expr(e->left);
	}
	if (e->right) {
		destroy_expr(e->left);
	}
	free(e);
}

char
lvalue(struct expr *e)
{
	if (e->op == DEREF) {
		return 1;
	}
	return 0;
}

/*
 * worker function to factor out common expr stuff for unary ops
 */
void
unop_set(struct expr *e)
{
    e->type = e->left->type;
    e->cost = e->left->cost;
    e->left->up = e;
}

/*
 * the operator priority table is indexed from OP_MIN to OP_MAX, and contains the encoded priority of the
 * binary operator.  zero values mean not an operator.
 */
char
binop_pri(char t)
{
    char po;
    char v;

    po = t - OP_MIN;
    if ((po < 0) || (t > OP_MAX)) {
        v = 0;
    } else {
	    v = op_pri[po];
	}
	printf("binop_pri 0x%02x %c -> %d\n", t, t > ' ' ? t : ' ', v);
    return v;
}

/*
 * parse an expression
 */
struct expr *
parse_expr(char pri, struct stmt *st)
{
	char op;
	struct expr *e;
    char p;

	tdump(cur.type);
	e = 0;  // initialize to avoid uninitialized use

	switch (cur.type) {   // prefix

    case NUMBER:
        e = makeexpr(CONST, 0);
        e->type = inttype;
        e->v = cur.v.numeric;
        e->flags = E_CONST;
        gettoken();
        break;

    case STRING: {
        struct name *strname;
        char namebuf[32];

        e = makeexpr(STRING, 0);
        /* string literals have type char* (pointer to char) */
        e->type = get_type(TF_POINTER, chartype, 0, 0);

        /* generate synthetic name for this string literal */
        sprintf(namebuf, "_str%d", string_counter++);

        /* create a name entry for this string literal at global scope (level 1) */
        strname = new_name(namebuf, var, e->type, 0);
        if (strname) {
            /* store pointer to counted string in the name's init field */
            strname->init = makeexpr(STRING, 0);
            strname->init->v = (unsigned long)cur.v.str;
            /* also store in expression for immediate use */
            e->v = (unsigned long)cur.v.str;
            /* store reference to the named string in the expression */
            e->var = (struct var *)strname;
        }
        e->flags = E_CONST;
        gettoken();
        break;
    }

    /* unary operators */
    case LPAR:      // parenthesized expression
        gettoken();
        e = parse_expr(0, st);  // parse inner expression with lowest precedence
        need(RPAR, RPAR, ER_E_SP);
        break;

    case MINUS:     // unary minus
        gettoken();
        e = makeexpr(NEG, parse_expr(OP_PRI_MULT - 1, st));  // higher precedence than mult
        if (e->left) {
            e->type = e->left->type;
            e->cost = e->left->cost;
            e->left->up = e;
        }
        e = cfold(e);
        break;

    case TWIDDLE:   // bitwise not
        gettoken();
        e = makeexpr(TWIDDLE, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->type = e->left->type;
            e->cost = e->left->cost;
            e->left->up = e;
        }
        e = cfold(e);
        break;

    case BANG:      // logical not
        gettoken();
        e = makeexpr(NOT, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->type = e->left->type;
            e->cost = e->left->cost;
            e->left->up = e;
        }
        e = cfold(e);
        break;

    case STAR:      // dereference (unary)
        gettoken();
        e = makeexpr(DEREF, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->cost = e->left->cost;
            e->left->up = e;
            // type will be determined later when we have full type info
            if (e->left->type && (e->left->type->flags & TF_POINTER) && e->left->type->sub) {
                e->type = e->left->type->sub;
            } else {
                e->type = e->left->type;
            }
        }
        break;

    case AND:       // address-of (unary)
        gettoken();
        e = makeexpr(AND, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            e->cost = e->left->cost;
            e->left->up = e;
            // create pointer type to operand's type
            if (e->left->type) {
                e->type = get_type(TF_POINTER, e->left->type, 0, 0);
            }
        }
        break;

	default:
		printf("unop default for token 0x%02x\n", cur.type);
		err(ER_E_UO);
		return 0;
    }
    /*
     * the recursive nature of this expression parser will have exhausted
     * the unary operators and terminals by this point. now we have postfix
     * and binary operators to deal with
     */
    while (1) { // binary operators
        tdump(cur.type);

        p = binop_pri(cur.type);
        if (p == 0) {
            // not a binary operator, we're done
            break;
        }
        if (pri != 0 && p >= pri) {
            // operator has same or lower precedence, stop parsing at this level
            // (pri == 0 means PRI_ALL, so we parse all operators regardless of precedence)
            break;
        }

        // we have a binary operator with higher precedence (lower p value)
        op = cur.type;
        gettoken();

        // for left-associative operators (most C operators), parse right side
        // with precedence p, which prevents same-precedence operators from
        // being pulled into the right subtree
        e = makeexpr(op, e);
        e->left->up = e;
        e->right = parse_expr(p, st);
        if (e->right) {
            e->right->up = e;
        }

        // compute cost and try to determine result type
        if (e->left && e->right) {
            e->cost = e->left->cost + e->right->cost;
            // for now, use left operand's type as result type
            // proper type resolution would go here
            e->type = e->left->type;
        } else if (e->left) {
            e->cost = e->left->cost;
            e->type = e->left->type;
        }

        e = cfold(e);
    }
    return e;
}

struct expr *
xreplace(struct expr *out, struct expr *in)
{
    in->next = out->next;
    in->prev = out->prev;
    if (out->prev) {
        out->prev->next = in;
    }
    if (out->next) {
        out->next->prev = in;
    }
    in->up = out->up;
    if (out->flags & E_FUNARG) {
        in->flags |= E_FUNARG;
    }
    free(out);
    return in;
}

struct expr *
cfold(struct expr *e) {
	long val;
	long vl, vr;

    switch (e->op) {
    case NEG:
        if (e->left->op == CONST) {
            val = -e->v;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case TWIDDLE:
        if (e->left->op == CONST) {
            val = ~e->v;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case NOT:
        if (e->left->op == CONST) {
            val = e->v ? 0 : 1;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    }
    if (!e->right) {
        err(ER_E_CF);
        return e;
    }
    if ((e->left->op != CONST) || (e->right->op != CONST)) {
        return e;
    }
    vl = e->left->v;
    vr = e->right->v;

    switch (e->op) {
    case PLUS:
        val = vl + vr;
        break;
    case MINUS:
        val = vl - vr;
        break;
    default:
        return e;
    }
    e = xreplace(e, e->left);
    e->v = val;  // Store the computed constant value
    return e;
}

/*
 * parse an expression that must yeild a constant. 
 * used for array declarations and CPP stuff
 */
int
parse_const(char token)
{
    struct expr *e;
    int val;

    e = parse_expr(PRI_ALL, 0);
    if (!(e->flags & E_CONST)) {
        err (ER_C_CE);
        return 0;
    }
    val = e->v;
    destroy_expr(e);
    return val;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
