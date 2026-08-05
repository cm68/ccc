/*
 * prefix and postfix expression parsing.
 *
 * Split from expr.c for the same reason fold.c was: the biggest
 * source in the tree has to fit through the compiler ON the target,
 * and cpp's tables for one translation unit are paid per unit.
 */
#include "cc1.h"

extern struct expr *mkexpr(unsigned char op, struct expr *left);
extern struct expr *mkbin(unsigned char op, struct expr *l,
    struct expr *r, struct type *t);
extern struct expr *mkIncDec(struct expr *operand, unsigned char inc_op,
    unsigned char is_postfix);
extern struct expr *skipExpr(unsigned char pri);
extern void freeNode(struct expr *e);
extern struct type *unwrapDeref(struct expr **ep);
extern int isaggr(struct type *t);

/* string literal: refer to the strN emitted in phase 1 */
struct expr *
pfxString(void)
{
    struct name *np;
    struct expr *e;

    np = (struct name *)galloc(sizeof(struct name));
    np->id = SYNTH + globalStrCtr++;
    np->type = getType(TF_POINTER, chartype, 0);
    np->kind = kvar;
    np->level = 1;
    e = mkexprI(STRING, 0, np->type, 0, E_CONST);
    e->var = (struct var *)np;
    gettoken();
    return e;
}

/* Symbol reference - SYM = address
 * For variables: wrap in DEREF to get value
 * For functions: return address (decay to function pointer)
 */
struct expr *
pfxSym(void)
{
    struct expr *e, *e1;
    struct type *tp;
    struct name *np;
    unsigned int uofs;
    unsigned short symid;

    /* Save the id before gettoken() overwrites cur.v.id */
    symid = cur.v.id;

    np = findName(symid, 0);

    /* Peek at next token to enable implicit function declarations */
    gettoken();

    if (!np) {
        /* Undefined symbol */
        /* K&R extension: if followed by '(', implicitly declare as
         * function returning int */
        if (cur.type == LPAR) {
            /* Create implicit function declaration: int name() */
            tp = (struct type *)permalloc(sizeof(struct type));
            tp->flags = TF_FUNC;
            tp->sub = inttype;  /* Return type: int */
            tp->elem = NULL;    /* No parameter info */

            np = (struct name *)galloc(sizeof(struct name));
            /* Initialize in struct field order */
            np->id = symid;
            np->type = tp;
            /* chain set by addName */
            np->kind = kvar;
            np->level = 1;  /* Global scope */
            /* is_tag = 0 from calloc */
            np->sclass = SC_EXTERN;

            np = addName(np);

#ifdef DEBUG
            if (VERBOSE(V_SYM)) {
                fdprintf(2, "Implicit declaration: int %s()\n", nameOf(symid));
            }
#endif
        } else {
            /* Not a function call - report error */
#ifdef DEBUG
            fdprintf(2, "bad op (not fn): %d sym=%s\n", cur.type,
                     nameOf(symid));
#endif
            gripe(ER_E_UO);
            return mkexprI(CONST, 0, inttype, 0, 0);
        }
    }

    if (np->kind == kelem) {
        /* Enum constant: treat as integer constant.
         * uofs intermediate: zc3 miscompiles the direct
         * uchar -> ulong argument promotion (clobbers the
         * loaded byte), yielding 0 for every enum. */
        uofs = np->w.m.offset;
        e = mkexprI(CONST, 0, inttype, uofs, E_CONST);
    } else {
        tp = np->type;
        e1 = mkexprI(SYM, 0, tp, 0, 0);
        e1->var = (struct var *)np;

        // Functions and arrays decay to pointers (addresses)
        // Only wrap non-functions in DEREF to get their value
        if (tp->flags & (TF_FUNC | TF_ARRAY))
            e = e1;  // address forms decay to the address
        else
            e = mkexprI(DEREF, e1, tp, 0, 0);  // Variable: wrap in DEREF
    }
    /* Note: gettoken() already called above for lookahead */
    return e;
}

/* parenthesized expression or type cast, the LPAR consumed */
struct expr *
pfxCast(void)
{
    struct expr *e, *e1;
    struct type *tp;

    /* Check if this is a type cast: (type)expr */
    if (isCastStart()) {
        /* Parse the type name */
        tp = parseTypeName();
        expect(RPAR, ER_E_SP);

        /* Parse the expression being cast */
        /* Cast has unary precedence */
        e1 = parseExpr(OP_PRI_MULT - 1);

        /*
         * A cast that only renames the type can change it in
         * place, and most do.  One that makes the value narrower
         * cannot: the value is somewhere, and where its low half
         * sits depends on how wide it was.  A long lives in HL:DE
         * with the low word in DE, so "(int)f()" that just
         * relabelled the call as an int left pass2 reading HL -
         * the high word - and there was nothing left in the tree
         * to say otherwise.
         *
         * NARROW says it explicitly.  It is unary and has been in
         * the opcode table and the pretty-printer all along; only
         * nobody emitted it.
         */
        if (e1) {
            int plain = e1->type && tp &&
                !(tp->flags & (TF_POINTER | TF_ARRAY | TF_FUNC |
                               TF_AGGREGATE)) &&
                !(e1->type->flags & (TF_POINTER | TF_ARRAY | TF_FUNC |
                                     TF_AGGREGATE)) &&
                !(e1->flags & E_CONST);

            if (plain && tp->size < e1->type->size) {
                e = mkexpr(NARROW, e1);
                e->type = tp;
            } else if (plain && tp->size > e1->type->size) {
                /*
                 * And the other direction, which is the same
                 * mistake read the other way: relabelling a byte
                 * as a long does not put anything in the three
                 * bytes above it.  Which of the two conversions it
                 * is depends on the *source*: a signed value
                 * sign-extends and an unsigned one zero-extends,
                 * and the instructions differ.
                 */
                e = mkexpr((e1->type->flags & TF_UNSIGNED) ?
                    WIDEN : SEXT, e1);
                e->type = tp;
            } else {
                /*
                 * A constant is relabelled, not wrapped - but a
                 * narrowing relabel has to truncate the VALUE too,
                 * here and now, or every later fold happily
                 * compares the wide value under the narrow name:
                 * (unsigned short)BIG != BIG&0xffff folded true.
                 * Signed targets sign-extend from the new width so
                 * the value still means what the type says.
                 */
                if ((e1->flags & E_CONST) && tp &&
                    !(tp->flags & (TF_POINTER | TF_ARRAY | TF_FUNC |
                                   TF_AGGREGATE))) {
                    if (tp->size == 1) {
                        e1->v &= 0xff;
                        if (!(tp->flags & TF_UNSIGNED) && (e1->v & 0x80))
                            e1->v |= 0xffffff00L;
                    } else if (tp->size == 2) {
                        e1->v &= 0xffff;
                        if (!(tp->flags & TF_UNSIGNED) && (e1->v & 0x8000L))
                            e1->v |= 0xffff0000L;
                    }
                }
                e1->type = tp;
                e = e1;
            }
        } else {
            e = mkexprI(CONST, 0, tp, 0, 0);
        }
    } else {
        /*
         * Parenthesized expression: (expr)
         * parse inner expression with lowest precedence
         */
        e = parseExpr(0);
        expect(RPAR, ER_E_SP);
    }
    return e;
}

/* unary minus, complement, logical not - the operator still in cur */
struct expr *
pfxUnary(void)
{
    struct expr *e, *e1;
    unsigned char uop;
    unsigned long uval;

    /*
     * The lexeme and the AST node are not the same thing: unary
     * minus becomes NEG, and "~" has to become NOT the same way.
     * Leaving it as the TWIDDLE lexeme meant pass2 never saw an
     * operator it recognised, so "~x" reduced to nothing at all -
     * no code, at either width, for the life of the compiler.
     */
    uop = (cur.type == MINUS) ? NEG :
          (cur.type == TWIDDLE) ? NOT : cur.type;
    gettoken();
    e1 = parseExpr(OP_PRI_MULT - 1);
    if (!e1)
        return 0;
    /* Fold unary ops on constants */
    if (e1->flags & E_CONST) {
        uval = e1->v;
        if (uop == NEG) uval = -uval;
        else if (uop == NOT) uval = ~uval;
        else if (uop == BANG) uval = !uval;
        e1->v = uval;
        return e1;
    }
    e = mkexpr(uop, e1);
    /*
     * Negation and complement give back what they were handed;
     * "!" gives an int whatever it was applied to.  Taking the
     * operand's type made "!lv" a long, so "!lv != 0" looked
     * like 32-bit work and went looking for a helper to
     * compare a truth value nothing had widened.
     */
    e->type = (uop == BANG) ? inttype : e1->type;
    /*
     * And these promote as the binary operators do: "-c" on a
     * char is the negation of an int, not of a byte.  Taking
     * the operand's width made it a byte negation widened
     * afterwards, so "-(unsigned char)5" came out as 251.
     */
    if ((uop == NEG || uop == NOT) && e->type &&
        e->type->size > 0 && e->type->size < inttype->size &&
        !(e->type->flags & (TF_POINTER | TF_ARRAY)))
        e->type = inttype;
    return e;
}

/* unary dereference, the STAR consumed */
struct expr *
pfxStar(void)
{
    struct expr *e, *e1;

    e1 = parseExpr(OP_PRI_MULT - 1);
    if (!e1)
        return 0;
    /*
     * Dereferencing a pointer to a function is a no-op: what it
     * yields is the function, and what you can do with that is
     * call it, which needs the address the pointer already holds.
     * Wrapping it in a load meant "(*fp)()" tried to fetch
     * through the pointer and call whatever it found.
     */
    if (e1->type && (e1->type->flags & TF_POINTER) &&
        e1->type->sub && (e1->type->sub->flags & TF_FUNC))
        return e1;
    e = mkexpr(DEREF, e1);
    if ((e1->type->flags & TF_POINTER) && e1->type->sub)
        e->type = e1->type->sub;
    else
        e->type = e1->type;
    return e;
}

/* address-of, the AND consumed */
struct expr *
pfxAddr(void)
{
    struct expr *e, *e1;

    e = parseExpr(OP_PRI_MULT - 1);
    if (!e)
        return 0;
    /*
     * Mark variable as address-taken (can't use register).
     *
     * Phase 1 has already done this, in skipExpr, and that is the
     * one that counts - by the time this runs the registers have
     * been handed out.  It is kept because the flag is also read
     * for the parameters, and it complains about nothing: taking
     * the address of a register variable is refused in phase 1,
     * which still has the right line number for it.
     */
    if (e->op == DEREF && e->left->op == SYM)
        ((struct name *)e->left->var)->w.r.addr_taken = 1;
    /* Optimize: &(DEREF x) = x, since SYM already gives address */
    if (e->op == DEREF) {
        e1 = e;
        e = e->left;
        if (e->op == SYM)
            e->type = getType(TF_POINTER, e->type, 0);
        e1->left = NULL;
        freeNode(e1);
    } else if (e->type->flags & TF_ARRAY) {
        e->type = getType(TF_POINTER, e->type, 0);
    } else {
        e1 = mkexpr(AND, e);
        e1->type = getType(TF_POINTER, e->type, 0);
        e = e1;
    }
    return e;
}

/* sizeof(type), sizeof(expr), or sizeof expr - the keyword consumed */
struct expr *
pfxSizeof(void)
{
    struct expr *e1;
    struct type *t;

    if (cur.type == LPAR) {
        gettoken();  /* consume '(' */
        if (isCastStart()) {
            /* sizeof(type) */
            t = parseTypeName();
            expect(RPAR, ER_E_SP);
        } else {
            /* sizeof(expr) */
            e1 = parseExpr(0);
            t = e1 ? e1->type : (struct type *)0;
            FreeExpr(e1);
            expect(RPAR, ER_E_SP);
        }
    } else {
        /* sizeof expr (no parens) */
        e1 = parseExpr(OP_PRI_MULT - 1);
        t = e1 ? e1->type : (struct type *)0;
        FreeExpr(e1);
    }
    /*
     * typesize, not t->size: the type node keeps its size in a
     * byte, so an array of 256 or more records zero.  "unsigned
     * char buf[512]" answered 0, and pass1's own
     * read(fd, lexBuf, sizeof lexBuf) asked the kernel for no
     * bytes and took the zero back for end of file.
     */
#ifdef DEBUG
    if (!t) fdprintf(2, "bad op (sizeof): no type\n");
#endif
    if (!t) gripe(ER_E_UO);
    /*
     * An array whose extent nothing has settled has no size to ask
     * for.  It is indeterminate until a later declaration says how
     * big it is, and until then sizeof cannot be answered - it used
     * to come back 2, the size of the pointer an array decays to,
     * which is a wrong answer rather than a refused one.
     *
     * So the declaration has to precede the use.  That is what C
     * requires in any case, and it is what lets the two phases run a
     * function at a time: nothing may depend on a declaration further
     * down the file.
     */
    if (t && (t->flags & TF_ARRAY) && t->count <= 0)
        gripe(ER_D_IA);
    return mkexprI(CONST, 0, inttype, t ? typesize(t) : 0, E_CONST);
}

struct expr *
parsePrefix(void)
{
    unsigned char inc_op;
    struct expr *e = 0;
    struct type *t;
    long sval;

    switch (cur.type) {   // prefix

    case LNUMBER:
        /* the source said L, so it is a long however small it is */
        e = mkexprI(CONST, 0, longtype, (unsigned long)cur.v.numeric,
                    E_CONST);
        gettoken();
        break;

    case INUMBER:
        /* folded upstream from an int-typed construct */
        e = mkexprI(CONST, 0, inttype, (unsigned long)cur.v.numeric,
                    E_CONST);
        gettoken();
        break;

    case NUMBER:
        sval = cur.v.numeric;
        /* Inline: determine smallest type for constant */
        if (sval < 0)
            t = (sval >= -128) ? chartype : (sval >= -32768) ? inttype : longtype;
        else if (sval <= 255)
            t = uchartype;
        else if (sval <= 65535)
            t = ushorttype;
        else
            /* sval >= 0 always fits in long here; spelling this as
             * sval <= 2147483647L trips a zc3 bug (rewritten to
             * < LONG_MIN, always false) */
            t = longtype;
        e = mkexprI(CONST, 0, t, (unsigned long)sval, E_CONST);
        gettoken();
        break;

    case STRING:
        e = pfxString();
        break;

    case SYM:
        e = pfxSym();
        break;

    case LPAR:
        gettoken();
        e = pfxCast();
        break;

    case MINUS:
    case TWIDDLE:
    case BANG:
        e = pfxUnary();
        break;

    case STAR:
        gettoken();
        e = pfxStar();
        break;

    case AND:
        gettoken();
        e = pfxAddr();
        break;

    case SIZEOF:
        gettoken();
        e = pfxSizeof();
        break;

    case INCR:      // prefix increment: ++i
    case DECR:      // prefix decrement: --i
        inc_op = cur.type;
        gettoken();
        e = mkIncDec(parseExpr(OP_PRI_MULT - 1), inc_op, 0);
        break;

	default:
#ifdef DEBUG
		fdprintf(2, "bad op (expr): %d\n", cur.type);
#endif
		gripe(ER_E_UO);
		return 0;
    }

    return e;
}

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
    if (e && e->op == DEREF && e->type &&
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
        if (e1->op == DEREF && e1->left && e1->left->op == SYM) {
            struct name *vn = (struct name *)e1->left->var;
            if (vn && vn->level > 1 && vn->w.r.agg_refs < 255)
                vn->w.r.agg_refs++;
        }
    } else {
        // Unwrap DEREF to get address
        unwrapDeref(&e);
        e1 = e;  /* base */
        /* Track aggregate ref for register allocation */
        if (e1->op == SYM) {
            struct name *vn = (struct name *)e1->var;
            if (vn && vn->level > 1 && vn->w.r.agg_refs < 255)
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

