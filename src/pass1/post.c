/*
 * the postfix arms, split from pfx.c: subscripts, calls, member
 * selection, and the postfix operators, one worker apiece - the
 * same frame-per-arm argument that split parsePrefix.
 */

#include "p1core.h"
#include "p1expr.h"
#include "p1type.h"
#include "p1name.h"
#include "p1lex.h"

/*
 * Convert an argument to the type the prototype declares for it.
 *
 * The argument list used to be handed to pass2 untouched, on the
 * theory that pass2 did the conversions.  Pass2 never sees the
 * prototype, so nothing did them, and an argument was pushed at
 * whatever width it happened to have.  For int-to-int that costs
 * nothing and hid the hole for a long time.  Crossing into a long
 * is where it shows: unistd.h declares
 *
 *	long lseek(unsigned char fd, long offset, int whence);
 *
 * and lexRewind's lseek(lexFd, 0, SEEK_SET) pushed the 0 as two
 * bytes.  The callee reads four, so its offset was the caller's
 * whence pasted onto the constant and its own whence came from a
 * slot nobody wrote - garbage, which fell through the switch to
 * default, set EINVAL and returned -1 having seeked nowhere.  The
 * return value is discarded there, so pass1 read its second pass
 * from wherever the first had stopped: end of file.  It emitted an
 * empty AST for every input and exited 0.
 *
 * Only widening is done here.  A narrower parameter is already
 * right: everything is pushed in two-byte slots, the callee reads
 * the low byte of one, and the machine is little-endian, so the
 * byte it wants is the byte that is there.
 */
struct expr *
argcvt(struct expr *a, struct type *pt)
{
	struct expr *w;

	if (!a || !pt || !a->type)
		return a;

	/* pointers, arrays, functions and aggregates keep their width */
	if ((pt->flags & (TF_POINTER | TF_ARRAY | TF_FUNC | TF_AGGREGATE)) ||
		(a->type->flags & (TF_POINTER | TF_ARRAY | TF_FUNC | TF_AGGREGATE)))
		return a;

	if (pt->size <= a->type->size)
		return a;

	/*
	 * A constant is emitted at the width of its type, so widening it
	 * is a relabel and costs no code.  Anything else has to be
	 * extended, and which extension depends on the source: signed
	 * sign-extends, unsigned pads with zeroes.
	 */
	if (a->flags & E_CONST) {
		a->type = pt;
		return a;
	}

	w = mkexpr((a->type->flags & TF_UNSIGNED) ? WIDEN : SEXT, a);
	w->type = pt;
	return w;
}

/*
 * The postfix arms, one worker apiece, on the same grounds as the
 * prefix ones: each arm's locals are its own, and this compiler does
 * no lifetime analysis by design, so the function boundary is what
 * keeps a subscript's scratch out of a call's registers.
 */

/* Array subscript: arr[idx] = DEREF(ADD(base, idx * sizeof)),
 * the '[' still current on entry. */
struct expr *
pfxIndex(struct expr *e)
{
    struct expr *e1, *e2, *e3, *e4;
    struct type *tp, *sub;
    unsigned short tf;
    int elem_size;

    gettoken();  // consume '['
    e1 = parseExpr(0);  /* index */
    expect(RBRACK, ER_E_SP);
    e1 = narrowidx(e1);         /* a subscript is address-wide */

    /*
     * An array's name is its address, so the load comes off
     * and the subscript is added to the address itself.  A
     * pointer's name is not: its value has to be read first
     * and the subscript added to that.
     *
     * Unwrapping both made "p[0]" the byte at the pointer
     * rather than the byte it points at - the low half of the
     * pointer itself.  "*p" was right all along, because that
     * path never unwrapped anything.
     */
    if (e->op == DEREF && e->type &&
        (e->type->flags & TF_ARRAY))
        tp = unwrapDeref(&e);
    else
        tp = e ? e->type : (struct type *)0;

    /* Get element size from type */
    tf = tp->flags;
    sub = tp->sub;
    elem_size = 2;  // default to short/int size
    if ((tf & (TF_POINTER | TF_ARRAY)) && sub)
        elem_size = sub->size;

    // Scale index by element size: idx * sizeof(elem)
    if (elem_size == 1) {
        e2 = e1;  /* scaled = index */
    } else {
        e4 = mkexprI(CONST, 0, inttype,
					elem_size, E_CONST);  /* size_expr */
        e2 = mkbin(STAR, e1, e4, inttype);  /* scaled */
    }

    // Add scaled offset to base: base + (idx * sizeof)
    /* The ADD result is a pointer to the element type */
    e3 = mkbin(PLUS, e, e2,
        ((tf & TF_ARRAY) && sub) ?
            getType(TF_POINTER, sub, 0) : tp);  /* addr */

    // Dereference to get element value
    e = mkexpr(DEREF, e3);
    tp = e3->type;
    if ((tp->flags & (TF_POINTER | TF_ARRAY)) && tp->sub)
        e->type = tp->sub;
    return e;
}

/* Function call: expr(arg1, arg2, ...), the '(' still current. */
struct expr *
pfxCall(struct expr *e)
{
    struct expr *e1, *e2, *e3;
    struct type *ft;
    unsigned char argi;

    gettoken();  // consume '('

    // Create CALL node with function expression as left operand
    e1 = mkexpr(CALL, e);  /* call */

    /*
     * The thing being called is either a function or a
     * pointer to one, and the return type is one step further
     * down in the second case.  Only the first was looked
     * for, so a call through a pointer had no type at all -
     * which nothing minded until the result was used.
     */
    ft = e->type;
    if (!(ft->flags & TF_FUNC) && (ft->flags & TF_POINTER) &&
        ft->sub && (ft->sub->flags & TF_FUNC))
        ft = ft->sub;

    // Set return type from function type
    if ((ft->flags & TF_FUNC) && ft->sub) {
        e1->type = ft->sub;
        /* a struct cannot come back by value either */
        if (isaggr(e1->type))
            gripe(ER_E_AG);
    }
    /*
     * Whatever was called, the call has a type.  Leaving it
     * unset let a null type reach everything downstream, and
     * calling something this could not read the return type
     * of died the moment the answer was used - while throwing
     * the answer away was fine, which is a poor way to find
     * out.  A function of unknown type returns int.
     */
    if (!e1->type)
        e1->type = inttype;

    /*
     * Parse the argument list, converting each argument to
     * the type the prototype declares for it.  A function
     * with no prototype, and the variadic tail past the
     * declared parameters, get no type back and are passed
     * as written.
     */
    argi = 0;
    e3 = NULL;
    if (cur.type != RPAR) {
        for (;;) {
            e2 = parseExpr(OP_PRI_COMMA);
            if (e2) {
                /* a struct cannot be passed - pass its address */
                if (isaggr(e2->type))
                    gripe(ER_E_AG);
                if (ft->flags & TF_FUNC)
                    e2 = argcvt(e2, fnArgType(ft, argi));
                e2->flags |= E_FUNARG;
                if (e3) {
                    e3->next = e2;
                } else {
                    e1->right = e2;
                }
                e3 = e2;
            }
            argi++;
            if (cur.type != COMMA)
                break;
            gettoken();
        }
    }

    expect(RPAR, ER_E_SP);

    // Result type will be determined later from function signature
    return e1;
}

/* Struct member access: s.x or p->x, the operator still current.
 * A malformed access sets *stop: the caller's postfix loop is done
 * with this expression, exactly as its break used to say. */
struct expr *
pfxMember(struct expr *e, unsigned char *stop)
{
    struct expr *e1, *e2, *e3;
    struct type *t;
    struct name *np;
    unsigned int uofs;
    unsigned char is_arrow;

    is_arrow = (cur.type == ARROW);

    gettoken();  // consume '.' or '->'

    if (cur.type != SYM) {
#ifdef DEBUG
        fdprintf(2, "bad op (member): %d\n", cur.type);
#endif
        gripe(ER_E_UO);
        *stop = 1;
        return e;
    }

    // For s.x: e is DEREF(SYM s), unwrap to get SYM s
    // For p->x: e is DEREF(SYM p), keep as-is (pointer value)
    if (is_arrow) {
        e1 = e;  /* base - pointer value */
        /* Track aggregate ref for register allocation */
        if (e1->op == DEREF && e1->left->op == SYM) {
            struct name *vn = (struct name *)e1->left->var;
            if (vn->level > 1 && vn->w.r.agg_refs < 255)
                vn->w.r.agg_refs++;
        }
    } else {
        // Unwrap DEREF to get address
        unwrapDeref(&e);
        e1 = e;  /* base */
        /* Track aggregate ref for register allocation */
        if (e1->op == SYM) {
            struct name *vn = (struct name *)e1->var;
            if (vn->level > 1 && vn->w.r.agg_refs < 255)
                vn->w.r.agg_refs++;
        }
    }

    // Look up member in struct/union
    np = NULL;  /* member */
    t = e1->type;
    /*
     * For both DOT and ARROW, if base type is pointer,
     * get the pointed-to type (DOT after array subscript
     * produces pointer type)
     */
    if (t->flags & TF_POINTER)
        t = t->sub;
    if (t && (t->flags & TF_AGGREGATE) && t->elem) {
        for (np = t->elem; np; np = np->next) {
            if (np->id == cur.v.id)
                break;
        }
    }

    if (!np) {
#ifdef DEBUG
        fdprintf(2, "bad op (no member): %s\n", nameOf(cur.v.id));
#endif
        gripe(ER_E_UO);
        gettoken();
        *stop = 1;
        return mkexprI(CONST, 0, NULL, 0, 0);
    }

    /*
     * Generate: DEREF(ADD(base, offset)) or BFEXTRACT
     * for bitfields.  uofs intermediate: zc3 miscompiles
     * the direct uchar -> ulong argument promotion.
     */
    uofs = np->w.m.offset;
    e2 = mkexprI(CONST, 0, inttype,
				uofs, E_CONST);  /* offset_expr */

    // addr is pointer to member, not pointer to base struct
    e3 = mkbin(PLUS, e1, e2, getType(TF_POINTER, np->type, 0));

    // Check if this is a bitfield access
    if (np->kind == kbitfield) {
        /*
         * Use BFEXTRACT operator with bitoff and
         * width stored in expr
         */
        e = mkexprI(BFEXTRACT, e3, np->type, 0, 0);
        /*
         * Store bitfield info in the var field (repurpose it)
         * We'll encode bitoff and width for the code generator
         * Keep reference to member for bitoff/width
         */
        e->var = (struct var *)np;
    } else if (np->type->flags & TF_ARRAY) {
        /* Array member: return address without DEREF */
        e = e3;
        e->type = np->type;
    } else {
        /* Non-array member: wrap in DEREF to get value */
        e = mkexprI(DEREF, e3, np->type, 0, 0);
    }

    gettoken();
    return e;
}

/*
 * Postfix operators on a parsed operand: function calls, array
 * subscripts, struct access, increment/decrement.
 */
struct expr *
parsePostfix(struct expr *e)
{
	unsigned char inc_op, stop;

    /*
     * Handle postfix operators: function calls, array subscripts,
     * struct access, increment/decrement
     */
    stop = 0;
    while (!stop &&
           (cur.type == LPAR || cur.type == LBRACK || cur.type == DOT ||
			cur.type == ARROW || cur.type == INCR || cur.type == DECR)) {
        if (cur.type == LBRACK) {
            e = pfxIndex(e);
        } else if (cur.type == LPAR) {
            e = pfxCall(e);
        } else if (cur.type == DOT || cur.type == ARROW) {
            e = pfxMember(e, &stop);
        } else {
            // Postfix increment/decrement: i++ or i--
            inc_op = cur.type;
            gettoken();
            e = mkIncDec(e, inc_op, 1);
        }
    }

    return e;
}

