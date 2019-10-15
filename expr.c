#ifdef notdef
/*
 * generate expression trees
 */
#include "ccc.h"

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
 * parse an expression
 */
struct expr *
expr(char pri, struct stmt *st)
{
	char op;

	switch (curtok) {   // prefix

	case SYM:
		e = makeexpr(DEREF, makeexpr(VAR, 0));
		e->left->var = findvar(symbuf, st);
		/* never seen the name and function call */
		if ((!(e->left->var)) && (nexttok == '('))
			e->left->var = makevar(strdup(symbuf),
                maketype(0, TK_FUNC, ptype[PT_INT]));
			e->left->var->flags |= V_GLOBAL;
			e->left->var->type->flags |= T_INCOMPLETE;
        }
        unop_set(e);
        e->left->type = normalizetype(maketype(0, TK_PTR, e->left->var->type));
        break;
    case NUMBER:
        e = makeexpr(CONST, 0);
        e->type = ptype[PTYPE_INT];
        e->v = numbervalue;
        gettoken();
        break;
    case STRING:
        e = makeexpr(VAR, 0);
        e->var = makevar(makelabel('S', (int)e),
            maketype(0, TK_ARRAY, ptype[PTYPE_CHAR]),
            V_STATIC|V_CONST|V_GLOBAL);
        e->var->type->len = strlen(symbuf);
        e->var->type->flags &= ~T_INCOMPLETE;
        e->var->init = makeexpr(BYTES,
            (struct expr *)strdup(symbuf));
        e->var->init->v = e->var->type->len;
        e->type = e->var->type;
        break;

    /* unary */
    case LPAR:
        gettoken();
        t = getbasetype();      // type cast
        if (t) {
            v = declare(&t);
            if (v) {
                err(ER_E_CS);
            }
            e = makeexpr(CAST, expr(pri, st));
            unop_set();
            e->type = t;
            e = cfold(e);
            need(RPAR, RPAR, ER_E_CP);
            break;
        }
        e = expr(PRI_PAREN, st);
        need(RPAR, RPAR, ER_E_SP);
        unop_set();
        e = cfold(e);
        break;
    case SIZEOF:
        gettoken();
        need(LPAR, LPAR, ER_E_SP);
        /* XXX - get a declaration */
        need(RPAR, RPAR, ER_E_SP);
        break;
    case INCR:
    case DECR:
        op = curtok();
        gettoken();
        e = makeexpr(op, expr(pri, st));
        if (!lvalue(e->left)) {
            err(ER_E_LV);
        }
        unop_set(e);
        break;
    case STAR:      // deref
        gettoken();
        e = makeexpr(DEREF, expr(pri, st));
        if (e->left->type->kind == TK_PTR) {
            unop_set(e);
            e->type = e->left->type->sub;
        } else {
            err(ER_E_DP);
        }
        break;
    case AND:       // addrof
        gettoken();
        e1 = expr(pri, st)
        if (!lvalue(e->left)) {
            err(ER_E_LV);
        }
        e = e1->left;
        free(e1);
        e->left->up = c;
        break;
    case BANG:      // unary not
        gettoken();
        e = makeexpr(NOT, expr(pri, st));
        unop_set();
        e->type = ptype[PT_BOOL];
        e = cfold(e);
        break;
    case MINUS:
        curtok = NEG;
        /* fall through */
    case TWIDDLE:
        op = curtok;
        gettoken();
        e = makeexpr(op, expr(pri, st));
        if (e->left->type->kind != TK_SCALAR) {
            err(ER_E_SC);
        }
        unop_set(e);
        e = cfold(e);
        break;
    }
    /*
     * the recursive nature of this expression parser will have exhausted
     * the unary operators and terminals by this point. now we have postfix
     * and binary operators to deal with
     */
    while (1) { // operators
        switch (curtok) {
        case INCR:
            if (!lvalue(e)) {
                err(ER_E_LV);
            }
            e = makeexpr(POSTINC, e);
            unop_set(e);
            continue;
        case DECR:
            if (!lvalue(e)) {
                err(ER_E_LV);
            }
            e = makeexpr(POSTDEC, e);
            unop_set(e);
            continue;
        case LBRACK:        // array reference
            gettoken();
            if (e->type->kind != TK_PTR) {
                err(ER_E_IT);
            }
            e = makeexpr(INDEX, e);
            e->right = expr(PRI_INDEX, st);
            e->type = e->left->type->sub;
            e->left->up = e->right->up = e;
            e->cost = e->left->cost + e->right->cost;
            need(RBRACK, RBRACK, ER_E_IB);
            e = cfold(e);
            continue;
        case LPAR:          // function call
            gettoken();
            if ((e->type->kind == TK_PTR) &&
                (e->left->type->kind == TK_FUNC)) {
                e = makeexpr(CALL, e);
                e->left->up = e;
                e->type = e->left->type->sub;
                e->cost = e->left->cost;
                while (curtok != E_O_F) {
                    e1 = expr(PRI_PAREN, st);
                    e->up = e;
                    if (e->next) {
                        e->next->prev = e1;
                    }
                    e1->next = e->next;
                    e->next = e1;
                    if (curtok == COMMA) {
                        gettoken();
                        continue;
                    }
                    if (curtok == RPAR) {
                        break;
                    }
                }
            } else {
                recover(RPAR, ER_E_NF);
            }
            need(RPAR, RPAR, ER_E_FA);
            continue;
        case DOT:
            gettoken();
            if ((curtok != SYM) || (e->type->kind != TK_PTR) || 
                (!e->type->sub->flags & T_AGGREGATE)) {
                err(ER_E_SM);
                continue;
            }
            v = varonlist(symbuf, e->type->sub->elem);
            if (!v) {
                err(ER_E_NT);
            }
            e = makeexpr(ADD, e);
            e->left->up = e;
            e->right = makeexpr(CONST, 0);
            e->right->up = e;
            e->right->type = ptype[PT_CHAR];
            e->type = v->type;
            e->right->v = v->offset;
            e = cfold(e);
            continue;
        case DEREF:             // ->
            gettoken();
            if ((curtok != SYM) || (e->type->kind != TK_PTR) ||
                (e->type->sub->kind != TK_PTR) ||
                (e->type->sub->sub->flags & T_AGGREGATE)) {
                err(ER_E_SM);
                continue;
            }
            v = varonlist(symbuf, e->type->sub->sub->elem);
            if (!v) {
                err(ER_E_NT);
            }
            e = makeexpr(ADD, e);
            e->left->up = e;
            e->right = makeexpr(CONST, 0);
            e->right->up = e;
            e->right->type = ptype[PT_CHAR];
            e->type = v->type;
            e = cfold(e);
            continue;
        default:
            printf("bzzt");
        } 
        p = binop_pri(curtok);
        if (p == 0) {
            err(ER_E_U0);
            return 0;
        }
        if (p > pri) {
            break;
        }

        e = makeexpr(curtok, e);
        e->left->up = e;
        gettoken();
        e->right = expr(p, st);
        e->right->up = e;
        e->type = opresult(e->op, e->left, e->right);

        if (lookupc(symops, e->op) != -1) {
            if (e->left->cost < e->right->cost) {
                e1 = e->left;
                e->left = e->right;
                e->right = e1;
            }
        }
        e->cost = e->left->cost + e->right->cost;
        e = cfold(e);
    }
    return e;
}

char
binop_pri(char t)
{
    char po;
    char v;

    po = t - MIN_OP;
    if ((po < 0) || (t > MAX_OP)) {
        return 0;
    }
    v = pritab[po / 2];
    if (po & 1) {
        v >>= 4;
    }
    v &= 0xf;
    return v;
}

#define PP(l,h) ((h << 4) | l)
char pritab[] = {
    PP(x,y),
};

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
    return e;
}
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
