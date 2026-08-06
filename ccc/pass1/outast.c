/*
 * AST serialization for second pass - binary format
 */
#include "cc1.h"

/* Forward declarations */
extern int analyzeFunc(struct name *func);  /* regalloc.c */
extern int frameSaveBase;                   /* regalloc.c */

/* Current function's locals from phase 1 - for frm_off/reg lookup */
struct local *curFuncLocals = NULL;

/*
 * Set while emitting the lvalue of an assignment, and consumed by the
 * first node emitted after that - see the DEREF case in emitExpr.
 */
static char inLvalue;


/*
 * Helper: emit child expression (if non-null)
 */
void
emitChild(struct expr *e)
{
	if (e)
		emitExpr(e);
}

/*
 * Emit an operand of an operator that works at width t, widening it
 * first if it is narrower.  Signed sources sign-extend and unsigned
 * ones zero-extend, which is precisely why the tree has to carry the
 * conversion rather than leaving pass2 to guess: the instructions
 * differ, and the node type is the only thing that knows which.
 */
void
emitOperand(struct expr *e, struct type *t)
{
	if (!e)
		return;
	/*
	 * Pointers, arrays, and functions keep their width: their value
	 * is an address, whatever their element size says.  An array
	 * member returned from a function otherwise picked up a SEXT of
	 * its first element's width, and pass2 dutifully loaded the two
	 * bytes the address pointed at instead of the address - which is
	 * how cpp's intern() returned the spelling's first characters as
	 * the canonical pointer.
	 */
	if (t && e->type && e->type->size < t->size &&
	    !(e->type->flags & (TF_POINTER | TF_ARRAY | TF_FUNC))) {
		if (e->op == CONST) {
			/*
			 * A constant is the same value at any width, so it
			 * just gets the wider type - wrapping it would break
			 * every rule that wants a literal operand.
			 */
			e->type = t;
		} else {
			emit1((e->type->flags & TF_UNSIGNED) ? WIDEN : SEXT);
			emit1(typeSfx(t));
		}
	}
	emitExpr(e);
}

/*
 * Output an expression in paren-free format
 * Constants: just the value (hex with dot)
 * Symbols: $name
 * Binary ops: op width left right
 * Unary ops: op width operand
 * Memory ops annotated with size: Mb expr, =l lvalue rvalue
 * Empty/null expression: _
 */
void
emitExpr(struct expr *e)
{
	/* Hoisted locals for stack reuse */
	struct name *np;
	struct expr *left, *right, *ep;
	struct type *type;
	unsigned char op, uc;
	char fullname[32], c, lval;
	/* everything n carries fits a byte: an argument count, an
	 * element size, an initializer count - and emit1 writes one
	 * byte of it regardless */
	unsigned char n;

	/* Fold constants before emitting */
	e = foldTree(e);

	if (!e) {
		emit1(AST_EMPTY);
		return;
	}

	op = e->op;
	left = e->left;
	right = e->right;
	type = e->type;

	/* consume the lvalue flag: it applies to this node only */
	lval = inLvalue;
	inLvalue = 0;

	switch (op) {
	case CONST:
		emit1(NUMBER);
		emit1(typeSfx(type));
		emit4(e->v);
		break;

	case SYM:
		np = (struct name *)e->var;
		/*
		 * Local variables: emit LOCALVAR/REGVAR directly.
		 *
		 * Not a static one.  It is inside a function, so its level
		 * is above one, but it is not in the frame: its storage is
		 * emitted with the globals under an S<n> label, which the
		 * name path below produces.  Coming here addressed it as a
		 * frame slot instead, so the value did not survive the
		 * call that set it and a static array read whatever was on
		 * the stack - which is how pass1's own sclassBit, a static
		 * table, made the c0 that ccc built reject every typedef.
		 */
		if (np->level > 1 && !(np->sclass & (SC_EXTERN | SC_STATIC))) {
			/* Look up frm_off/reg from phase 1 captured locals */
			struct local *local = findInLocals(np);
			char reg = local ? local->reg : np->w.r.reg;
			short off = local ? local->frm_off : np->w.r.frm_off;
			if (reg) {
				emit1(REGVAR);
				emit1(typeSfx(type));
				emit1(reg);
			} else {
				emit1(LOCALVAR);
				emit1(typeSfx(type));
				emit2((unsigned short)off);
			}
			break;
		}
		/* extern/global get underscore prefix */
		if ((np->sclass & SC_EXTERN) ||
		    (np->level == 1 && !(np->sclass & SC_STATIC)))
			fmtstr(fullname, "_%s", nameOf(np->id));
		else if (np->static_id)
			fmtstr(fullname,
			    "%c%d", np->sclass & SC_STATIC ? 'S' : 'L', np->static_id - 1);
		else
			fmtstr(fullname, "%s", nameOf(np->id));
		emit1(SYM);
		emitS(fullname);
		break;

	case STRING:
		/* String literals - reference by name (already emitted in phase 1) */
		np = (struct name *)e->var;
		emit1(SYM);
		emitS(nameOf(np->id));
		break;

	case CALL:
		/* Function call: CALL type count func arg1 arg2 ... */
		n = 0;
		c = typeSfx(type);
		/* Count arguments from the expression tree */
		for (ep = right; ep; ep = ep->next)
			n++;
		emit1(CALL);
		emit1(c);
		emit1(n);
		emitChild(left);
		for (ep = right; ep; ep = ep->next)
			emitChild(ep);
		break;


	case INCR:
	case DECR:
		/* Increment/decrement operators: emit with increment amount */
		n = 1;
		c = typeSfx(type);
		if (op == INCR)
			uc = (e->flags & E_POSTFIX) ? POSTINC : PREINC;
		else
			uc = (e->flags & E_POSTFIX) ? POSTDEC : PREDEC;
		if (type && (type->flags & TF_POINTER) && type->sub)
			n = type->sub->size;
		emit1(uc);
		emit1(c);
		/*
		 * What a step steps is a location, exactly as an assignment's
		 * left side is, so the DEREF case below has to keep itself:
		 * without this "(*p)++" on a register variable dropped the
		 * DEREF and stepped the pointer instead of what it points at.
		 * Clean code, no marker, wrong answer - and at both widths.
		 */
		inLvalue = 1;
		emitChild(left);
		emit2(n);
		break;

	case BFEXTRACT:
		/* Bitfield extract: offset width addr */
		np = (struct name *)e->var;
		emit1(BFEXTRACT);
		emit1(np ? np->w.m.bitoff : 0);
		emit1(np ? np->w.m.width : 0);
		emitChild(left);
		break;

	case BFASSIGN:
		/* Bitfield assign: offset width addr value */
		np = (struct name *)e->var;
		emit1(BFASSIGN);
		emit1(np ? np->w.m.bitoff : 0);
		emit1(np ? np->w.m.width : 0);
		emitChild(left);
		emitChild(right);
		break;

	case QUES:
		/* Ternary: QUES width cond then else - flatten the COLON node */
		emit1(QUES);
		emit1(typeSfx(type));
		emitChild(left);
		emitChild(right->left);
		emitChild(right->right);
		break;

	case INITLIST:
		/* Nested initializer list - emit contents */
		n = 0;
		for (ep = left; ep; ep = ep->next)
			n++;
		emit1(BEGIN);
		emit1(n);
		for (ep = left; ep; ep = ep->next)
			emitExpr(ep);
		emit1(END);
		break;

	case DEREF:
		/*
		 * DEREF(REGVAR) is just the value - skip the DEREF.
		 *
		 * Not on an assignment's lvalue though.  The assignment
		 * parser already unwrapped one DEREF to get the address, so
		 * what is left says "the register holds the address of the
		 * target".  Dropping it here emits the same REGVAR that
		 * "i = x" on a register variable emits, and pass2 then has no
		 * way to tell "*p = x" from "p = x".
		 */
		if (isRegvar(left) && !lval) {
			/*
			 * Carry this node's type down.  The name underneath
			 * is emitted in place of the DEREF, and it is
			 * emitted with its OWN type - so anything the DEREF
			 * had been relabelled with was thrown away.
			 *
			 * A cast is exactly that relabelling: "(unsigned
			 * int)p" on a register variable rewrites the DEREF
			 * and leaves the name alone, so the cast vanished
			 * and the comparison that followed ran signed.
			 * cpp's own null-pointer guard read a stack address
			 * as negative and took itself out that way.  The
			 * two types are the same whenever nothing was cast,
			 * which is nearly always, so this costs nothing.
			 */
			struct type *save = left->type;

			left->type = type;
			emitExpr(left);
			left->type = save;
			break;
		}
		/*
		 * A store through a pointer that is itself a member load:
		 * "*q->p = c" with p the first member.  The address here
		 * is DEREF(REGVAR) - load the word the register points at
		 * - and the child's DEREF has to survive, because bare
		 * "=(D(V))" is already taken: it is how "*q = c" spells
		 * "the register holds the address".  Without this the
		 * child collapsed to the same REGVAR a plain "*q" emits,
		 * and pass2 stored the character into the pointer member
		 * itself.  stdio's _flsbuf does exactly this - *f->_base
		 * = c - and every 513th byte through a buffered stream
		 * overwrote the buffer pointer's low byte instead of
		 * landing in the buffer.  dchainreg, not isRegvar: the
		 * flag rides the whole spine, so each level's DEREF makes
		 * the same choice and depth survives intact.
		 */
		if (lval && left->op == DEREF && dchainreg(left->left))
			inLvalue = 1;	/* the child DEREF keeps itself */
		/* Optimize: *++p -> (++p, *p) using comma operator */
		if ((left->op == INCR || left->op == DECR) &&
		    !(left->flags & E_POSTFIX)) {
			emit1(COMMA);
			emit1(typeSfx(type));
			emitExpr(left);
			emit1(DEREF);
			emit1(typeSfx(type));
			emitExpr(left->left);
			break;
		}
		/* fall through - standard unary */

	case NARROW:
	case WIDEN:
	case SEXT:
	case BANG:
	case NEG:
	case NOT:
		/* Unary operators */
		emit1(op);
		emit1(typeSfx(type));
		/*
		 * Negation and complement are done at the promoted width, so
		 * what they are applied to has to get there too.  emitChild
		 * hands the operand over as it found it, which left the
		 * operator working at a width its operand was not: a byte in
		 * A under a short negation, which no rule names.
		 *
		 * Not "!", whose answer is an int however narrow the thing
		 * tested, and not DEREF, which falls through to here and
		 * whose operand is an address rather than a value.
		 */
		if (op == NEG || op == NOT)
			emitOperand(left, type);
		else
			emitChild(left);
		break;

	case GT:
	case GE:
		/* Normalize GT/GE to LT/LE by swapping operands.  Still has
		 * to convert them: swapping is not a reason to skip the
		 * promotion the comparison would otherwise get. */
		{
			struct type *w = opwidth(e);
			emit1(op == GT ? LT : LE);
			emit1(typeSfx(type));
			emitOperand(right, w);
			emitOperand(left, w);
		}
		break;

	default:
		/* All operators get width suffix */
		emit1(op);
		emit1(typeSfx(type));
		if (isAssignOp(op)) {
			/*
			 * The result is about to be narrowed to the target, so
			 * compute it narrow where that cannot change the stored
			 * value.  This is the as-if rule standing in for the
			 * integer promotions: C says "c1 + c2" is int
			 * arithmetic, but if it lands in a char only the low
			 * byte was ever observable.
			 */
			if (right && candemote(right, type->size))
				demote(right, type);
			/* mark the lvalue so DEREF above knows to keep itself */
			inLvalue = 1;
			emitChild(left);		/* a location, never widened */
			emitOperand(right, type);	/* convert to the target */
		} else if (op == LAND || op == LOR) {
			/*
			 * The two sides of && and || are each tested against
			 * zero, separately, and short-circuited between - so
			 * there is no common width for them to meet at and
			 * nothing to convert them to.  The node's own type is
			 * uchar, because the answer is 0 or 1, and narrowing
			 * the operands to that drops exactly the bytes the
			 * zero test needs:
			 *
			 *	256 && 1	was false
			 *
			 * and a pointer whose low byte happened to be zero
			 * tested as null.  That last one is how it was found:
			 * cpp's conditional stack is a malloc'd list, and when
			 * the allocation landed on a 0x??00 address every
			 * "#if" in the file leaked its body.
			 */
			emitChild(left);
			emitChild(right);
		} else {
			struct type *w = opwidth(e);
			/*
			 * emitOperand widens a narrow operand and leaves a wide
			 * one alone, which is right for everything except an
			 * operand wider than the operator itself works at.
			 * "buf[pos + i]" with pos a long adds a long to a
			 * pointer and the sum is a pointer: only the low word
			 * can reach the address, and pass2 has no rule for
			 * adding the two widths together - it emitted nothing.
			 *
			 * So narrow it here, on the same terms the assignment
			 * above narrows what it stores: only where the operator
			 * cannot carry anything down from the bytes being
			 * dropped.
			 */
			if (left && left->type->size > w->size &&
			    candemote(left, w->size))
				demote(left, w);
			if (right && op != LSHIFT && op != RSHIFT &&
			    right->type->size > w->size &&
			    candemote(right, w->size))
				demote(right, w);
			emitOperand(left, w);
			/* a shift count is promoted on its own, not to the
			 * width of the value being shifted */
			if (op == LSHIFT || op == RSHIFT)
				emitChild(right);
			else
				emitOperand(right, w);
		}
		break;
	}
}
