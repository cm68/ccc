/*
 * generate expression trees
 */
#include "cc1.h"

#include "op_pri.h"

/*
 * counter for generating synthetic string literal names
 */
static int string_counter = 0;

struct expr *
makeexpr(char op, struct expr *left)
{
	struct expr *e;

	e = calloc(1, sizeof(*e));  // Zero-initialize all fields
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
	if (!e) {
		return;
	}
	if (e->left) {
		destroy_expr(e->left);
	}
	if (e->right) {
		destroy_expr(e->right);
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
        e->type = get_type(TF_POINTER, chartype, 0);

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

    case SYM: {
        /* Variable reference - SYM = address, wrap in DEREF to get value */
        struct name *n;
        struct expr *sym;

        n = lookup_name(cur.v.name, 0);
        if (!n) {
            err(ER_E_UO);
            e = makeexpr(CONST, 0);
            e->type = inttype;
            e->v = 0;
        } else {
            sym = makeexpr(SYM, 0);
            sym->var = (struct var *)n;
            sym->type = n->type;
            e = makeexpr(DEREF, 0);
            e->left = sym;
            e->type = n->type;
        }
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
            unop_set(e);
        }
        e = cfold(e);
        break;

    case TWIDDLE:   // bitwise not
        gettoken();
        e = makeexpr(TWIDDLE, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            unop_set(e);
        }
        e = cfold(e);
        break;

    case BANG:      // logical not
        gettoken();
        e = makeexpr(NOT, parse_expr(OP_PRI_MULT - 1, st));
        if (e->left) {
            unop_set(e);
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
        e = parse_expr(OP_PRI_MULT - 1, st);
        /* Optimize: &(DEREF x) = x, since SYM already gives address */
        if (e && e->op == DEREF) {
            e = e->left;
        } else {
            struct expr *addr = makeexpr(AND, e);
            if (e) {
                addr->cost = e->cost;
                e->up = addr;
                if (e->type) {
                    addr->type = get_type(TF_POINTER, e->type, 0);
                }
            }
            e = addr;
        }
        break;

    case SIZEOF:    // sizeof operator
        gettoken();
        // Check if it's sizeof(type) or sizeof expr
        if (cur.type == LPAR) {
            // Could be sizeof(type) or sizeof(expr)
            // Try to parse as type first
            struct type *t;
            gettoken();  // consume '('

            t = getbasetype();
            if (t) {
                // It's sizeof(type)
                // Handle pointer modifiers (e.g., sizeof(int *))
                while (cur.type == STAR) {
                    gettoken();
                    t = get_type(TF_POINTER, t, 0);
                }

                need(RPAR, RPAR, ER_E_SP);

                // Create constant expression with the size
                e = makeexpr(CONST, 0);
                e->type = inttype;
                e->v = t->size;
                e->flags = E_CONST;
            } else {
                // It's sizeof(expr) - parse as expression
                e = parse_expr(0, st);
                need(RPAR, RPAR, ER_E_SP);

                // Create constant expression with the size of the expression's type
                if (e && e->type) {
                    int size = e->type->size;
                    destroy_expr(e);  // we only needed it for the type
                    e = makeexpr(CONST, 0);
                    e->type = inttype;
                    e->v = size;
                    e->flags = E_CONST;
                } else {
                    err(ER_E_UO);  // couldn't determine type
                    e = makeexpr(CONST, 0);
                    e->type = inttype;
                    e->v = 0;
                    e->flags = E_CONST;
                }
            }
        } else {
            // sizeof expr (without parentheses)
            struct expr *operand = parse_expr(OP_PRI_MULT - 1, st);
            if (operand && operand->type) {
                int size = operand->type->size;
                destroy_expr(operand);
                e = makeexpr(CONST, 0);
                e->type = inttype;
                e->v = size;
                e->flags = E_CONST;
            } else {
                err(ER_E_UO);
                e = makeexpr(CONST, 0);
                e->type = inttype;
                e->v = 0;
                e->flags = E_CONST;
            }
        }
        break;

	default:
		err(ER_E_UO);
		return 0;
    }

    /*
     * Handle postfix operators: function calls, array subscripts, struct access
     */
    while (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT || cur.type == ARROW) {
        if (cur.type == LBRACK) {
            // Array subscript: arr[idx] = DEREF(ADD(base, idx * sizeof))
            struct expr *index, *scaled, *addr, *size_expr;
            int elem_size;

            gettoken();  // consume '['
            index = parse_expr(0, st);
            need(RBRACK, RBRACK, ER_E_SP);

            // Unwrap DEREF to get base address
            if (e && e->op == DEREF) {
                e = e->left;
            }

            // Get element size from type
            elem_size = 2;  // default to short/int size
            if (e && e->type) {
                if (e->type->flags & TF_POINTER && e->type->sub) {
                    elem_size = e->type->sub->size;
                } else if (e->type->flags & TF_ARRAY && e->type->sub) {
                    elem_size = e->type->sub->size;
                }
            }

            // Scale index by element size: idx * sizeof(elem)
            if (elem_size == 1) {
                scaled = index;
            } else {
                size_expr = makeexpr(CONST, 0);
                size_expr->v = elem_size;
                size_expr->type = inttype;
                size_expr->flags = E_CONST;

                scaled = makeexpr(STAR, index);
                scaled->right = size_expr;
                scaled->left->up = scaled;
                scaled->right->up = scaled;
                scaled->type = inttype;
                scaled = cfold(scaled);
            }

            // Add scaled offset to base: base + (idx * sizeof)
            addr = makeexpr(PLUS, e);
            addr->right = scaled;
            addr->left->up = addr;
            addr->right->up = addr;
            // The ADD result is a pointer to the element type
            if (e->type && (e->type->flags & TF_ARRAY) && e->type->sub) {
                addr->type = get_type(TF_POINTER, e->type->sub, 0);
            } else if (e->type && (e->type->flags & TF_POINTER)) {
                addr->type = e->type;  // pointer + offset = same pointer type
            } else {
                addr->type = e->type;
            }

            // Dereference to get element value
            e = makeexpr(DEREF, addr);
            e->left->up = e;
            if (e->left->type) {
                if ((e->left->type->flags & TF_POINTER) && e->left->type->sub) {
                    e->type = e->left->type->sub;
                } else if ((e->left->type->flags & TF_ARRAY) && e->left->type->sub) {
                    e->type = e->left->type->sub;
                }
            }
        } else if (cur.type == LPAR) {
            // Function call: expr(arg1, arg2, ...)
            struct expr *call, *arg, *lastarg;

            gettoken();  // consume '('

            // Create CALL node with function expression as left operand
            call = makeexpr(CALL, e);
            call->left->up = call;

            // Parse argument list
            lastarg = NULL;
            if (cur.type != RPAR) {
                // Parse first argument
                arg = parse_expr(OP_PRI_COMMA, st);
                if (arg) {
                    arg->flags |= E_FUNARG;
                    call->right = arg;
                    arg->up = call;
                    lastarg = arg;
                }

                // Parse remaining arguments
                while (cur.type == COMMA) {
                    gettoken();
                    arg = parse_expr(OP_PRI_COMMA, st);
                    if (arg) {
                        arg->flags |= E_FUNARG;
                        if (lastarg) {
                            lastarg->next = arg;
                            arg->prev = lastarg;
                        }
                        lastarg = arg;
                    }
                }
            }

            need(RPAR, RPAR, ER_E_SP);

            // Result type will be determined later from function signature
            call->cost = call->left->cost + 1;
            e = call;
        } else if (cur.type == DOT || cur.type == ARROW) {
            // Struct member access: s.x or p->x
            struct expr *base, *offset_expr, *addr;
            struct name *member;
            int is_arrow = (cur.type == ARROW);

            gettoken();  // consume '.' or '->'

            if (cur.type != SYM) {
                err(ER_E_UO);
                break;
            }

            // For s.x: e is DEREF(SYM s), unwrap to get SYM s
            // For p->x: e is DEREF(SYM p), keep as-is (pointer value)
            if (is_arrow) {
                base = e;  // pointer value
            } else {
                // Unwrap DEREF to get address
                if (e && e->op == DEREF) {
                    base = e->left;
                } else {
                    base = e;
                }
            }

            // Look up member in struct/union
            member = NULL;
            if (base && base->type) {
                struct type *t = base->type;
                // For both DOT and ARROW, if base type is pointer, get the pointed-to type
                // (DOT after array subscript produces pointer type)
                if (t->flags & TF_POINTER) {
                    t = t->sub;
                }
                if (t && (t->flags & TF_AGGREGATE) && t->elem) {
                    for (member = t->elem; member; member = member->next) {
                        if (strcmp(member->name, cur.v.name) == 0) {
                            break;
                        }
                    }
                }
            }

            if (!member) {
                err(ER_E_UO);
                gettoken();
                e = makeexpr(CONST, 0);
                e->v = 0;
                break;
            }

            // Generate: DEREF(ADD(base, offset))
            offset_expr = makeexpr(CONST, 0);
            offset_expr->v = member->offset;
            offset_expr->type = inttype;
            offset_expr->flags = E_CONST;

            addr = makeexpr(PLUS, base);
            addr->right = offset_expr;
            addr->left->up = addr;
            addr->right->up = addr;
            addr->type = base->type;

            e = makeexpr(DEREF, addr);
            e->left->up = e;
            e->type = member->type;

            gettoken();
        }
    }

    /*
     * the recursive nature of this expression parser will have exhausted
     * the unary operators and terminals by this point. now we have postfix
     * and binary operators to deal with
     */
    while (1) { // binary operators
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

        // for assignment, unwrap DEREF from left side to get lvalue address
        if (op == ASSIGN && e && e->op == DEREF) {
            e = e->left;  // unwrap to get address
        }

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
cfold(struct expr *e)
{
	long val;
	long vl, vr;

    switch (e->op) {
    case NEG:
        if (e->left->op == CONST) {
            val = -e->left->v;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case TWIDDLE:
        if (e->left->op == CONST) {
            val = ~e->left->v;
            e = xreplace(e, e->left);
            e->v = val;
        }
        return e;
    case NOT:
        if (e->left->op == CONST) {
            val = e->left->v ? 0 : 1;
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
    case STAR:  // multiplication
        val = vl * vr;
        break;
    case DIV:   // division
        if (vr == 0) {
            err(ER_E_CF);  // divide by zero - constant wont fold
            return e;
        }
        val = vl / vr;
        break;
    case MOD:   // modulo
        if (vr == 0) {
            err(ER_E_CF);  // modulo by zero - constant wont fold
            return e;
        }
        val = vl % vr;
        break;
    case AND:   // bitwise AND
        val = vl & vr;
        break;
    case OR:    // bitwise OR
        val = vl | vr;
        break;
    case XOR:   // bitwise XOR
        val = vl ^ vr;
        break;
    case LSHIFT:  // left shift
        val = vl << vr;
        break;
    case RSHIFT:  // right shift
        val = vl >> vr;
        break;
    case LAND:  // logical AND (&&)
        val = (vl && vr);
        break;
    case LOR:   // logical OR (||)
        val = (vl || vr);
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
    if (!e) {
        err(ER_C_CE);
        return 0;
    }
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
