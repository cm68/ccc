/*
 * AST serialization for second pass - binary format
 */
#include "cc1.h"
#include <unistd.h>

/* Forward declarations */
extern int analyzeFunc(struct name *func);  /* regalloc.c */
extern int frameSaveBase;                   /* regalloc.c */

/* Current function's locals from phase 1 - for frm_off/reg lookup */
static struct name *curFuncLocals = NULL;

/*
 * Look up a local variable in phase 1's captured locals, to get the
 * frame offset and register decided there.
 *
 * By name is not enough.  Every local of a function is in one list
 * now, so a name declared in a nested block sits beside the one it
 * shadows and both answer to the same string - the L<n> renaming is
 * for what gets emitted, not for what is looked up.  Matching on the
 * name alone returned whichever came first, so
 *
 *	short v; v = 1; { short v; v = 100; } return v;
 *
 * put both of them in the outer one's register.
 *
 * The level and the block say which is which: two variables of the
 * same name cannot be declared in the same block.
 */
struct name *
findInLocals(struct name *want)
{
	struct name *n;
	for (n = curFuncLocals; n; n = n->next) {
		if (n->id == want->id &&
		    n->level == want->level &&
		    n->w.r.blkid == want->w.r.blkid)
			return n;
	}
	return NULL;
}

/*
 * Assignment operators: plain = plus the ten compound forms.
 */
int
isAssignOp(unsigned char op)
{
	switch (op) {
	case ASSIGN:
	case PLUSEQ:
	case SUBEQ:
	case MULTEQ:
	case DIVEQ:
	case MODEQ:
	case RSHIFTEQ:
	case LSHIFTEQ:
	case ANDEQ:
	case OREQ:
	case XOREQ:
		return 1;
	}
	return 0;
}

/*
 * Set while emitting the lvalue of an assignment, and consumed by the
 * first node emitted after that - see the DEREF case in emitExpr.
 */
static char inLvalue;

/*
 * Check if expression is a SYM that maps to a REGVAR.
 * Returns the register number if so, 0 otherwise.
 */
char
isRegvar(struct expr *e)
{
	struct name *np, *local;
	if (!e || e->op != SYM)
		return 0;
	np = (struct name *)e->var;
	/* a static is never in a register - see canAlloc */
	if (np->level > 1 && !(np->sclass & (SC_EXTERN | SC_STATIC))) {
		local = findInLocals(np);
		return local ? local->w.r.reg : np->w.r.reg;
	}
	return 0;
}

/*
 * Truncation-transparent operators: the low n bits of the result
 * depend only on the low n bits of the operands, whatever the signs.
 * So if the value is about to be narrowed anyway, the whole
 * computation can be done narrow.
 *
 * / and % need the full value to pick a quotient, >> pulls down bits
 * from above, and a comparison yields int regardless of what its
 * operands were - none of them belong here.
 */
int
truncok(unsigned char op)
{
	switch (op) {
	case PLUS:
	case MINUS:
	/*
	 * STAR belongs here on the maths - the low n bits of a product
	 * depend only on the low n bits of the operands - but there is no
	 * 8-bit multiply helper, so demoting one only produces a shape
	 * nothing can generate.  Leave it wide and let the narrowing
	 * store take the low byte.
	 */
	case AND:
	case OR:
	case XOR:
	case LSHIFT:
	case NEG:
	case NOT:
		return 1;
	}
	return 0;
}

/*
 * Can this subtree be computed in 'size' bytes without changing the
 * low 'size' bytes of its value?
 */
int
candemote(struct expr *e, int size)
{
	if (!e)
		return 1;
	if (e->type->size <= size)
		return 1;
	if (e->op == CONST)
		return 1;
	if (e->op == DEREF) {
		/*
		 * A narrower read takes the low bytes, which come first.
		 * Not for a register variable though - it is emitted as the
		 * whole register, with no addressable low part.
		 */
		return !(e->left && e->left->op == SYM && isRegvar(e->left));
	}
	if (!truncok(e->op))
		return 0;
	/* a shift count keeps its own width; only the value narrows */
	if (e->op == LSHIFT)
		return candemote(e->left, size);
	return candemote(e->left, size) && candemote(e->right, size);
}

/*
 * Retype a subtree that candemote() has approved.
 */
void
demote(struct expr *e, struct type *t)
{
	if (!e || e->type->size <= t->size)
		return;
	e->type = t;
	if (e->op == CONST) {
		/* truncate to match, so the emitted value fits the width */
		if (t->size == 1)
			e->v &= 0xff;
		else if (t->size == 2)
			e->v &= 0xffff;
		return;
	}
	if (e->op == DEREF)
		return;			/* narrower load, address unchanged */
	demote(e->left, t);
	if (e->op != LSHIFT)
		demote(e->right, t);
}

int
iscmpop(unsigned char op)
{
	switch (op) {
	case EQ:
	case NEQ:
	case LT:
	case GT:
	case LE:
	case GE:
		return 1;
	}
	return 0;
}

/*
 * The width an operator actually works at.  For most that is the node
 * type, but a comparison yields int whatever it compared, so its own
 * type says nothing about the operands - they meet at their common
 * width instead.
 */
/*
 * The width an operand's VALUE occupies - an array or function
 * compares as its address, not its extent.  A comparison against a
 * forty-byte array otherwise chose forty as the common width and
 * sign-extended the pointer beside it into a long.
 */
unsigned char
valwidth(struct type *t)
{
	if (!t || (t->flags & (TF_POINTER | TF_ARRAY | TF_FUNC)))
		return 2;
	return t->size;
}

struct type *
opwidth(struct expr *e)
{
	if (!iscmpop(e->op))
		return e->type;
	if (!e->left)
		return e->type;
	if (!e->right)
		return e->left->type;
	return valwidth(e->left->type) >= valwidth(e->right->type) ?
	    e->left->type : e->right->type;
}

/*
 * Get size suffix for memory operations based on type
 * Returns: 'b' (byte), 's' (short/int), 'l' (long),
 * 'f' (float), 'd' (double), 'v' (void)
 * Uppercase B/S/L for unsigned types
 * Pointers use 's' since they're 16-bit on Z80
 */
char
typeSfx(struct type *t)
{
	char c;
	if (!t)
		return 's';  /* default to short */

	if (t->flags & (TF_POINTER | TF_ARRAY | TF_FUNC))
		return 's';  /* address-valued: 16 bits, whatever the extent */

	/* Check primitive types by size */
	if (t->size == 0)
		return 'v';  /* void */
	else if (t->size == 1)
		c = 'b';  /* char/byte */
	else if (t->size == 2)
		c = 's';  /* short/int */
	else if (t->size == 4)
		c = 'l';  /* long */
	else
		c = 's';  /* default to short */

	/* Uppercase for unsigned */
	if (t->flags & TF_UNSIGNED)
		c = c - 'a' + 'A';
	return c;
}

/* Helper: build label name from base+suffix */
static char lblBuf[16];

char *
mkLbl(char *base, char *suffix)
{
	fmtstr(lblBuf, "%s%s", base, suffix);
	return lblBuf;
}

void
emitLabel(char *base, char *suffix)
{
	emit1(LABEL);
	emitS(mkLbl(base, suffix));
}

void
emitGoto(char *base, char *suffix)
{
	emit1(GOTO);
	emitS(mkLbl(base, suffix));
}

void emitExpr(struct expr *e);  /* forward decl */

/*
 * Count intermediate labels needed for short-circuit && and || in condition.
 * Each && or || that's not at the top of its kind needs a label.
 */
int
cntCondLbls(struct expr *e)
{
	unsigned char cnt = 0;
	if (!e) return 0;
	/* Count in children first */
	cnt += cntCondLbls(e->left);
	cnt += cntCondLbls(e->right);
	/* && and || each need one label for short-circuit */
	if (e->op == LAND || e->op == LOR)
		cnt++;
	return cnt;
}

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
			struct name *local = findInLocals(np);
			char reg = local ? local->w.r.reg : np->w.r.reg;
			short off = local ? local->w.r.frm_off : np->w.r.frm_off;
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

/* Emit function parameter declarations */
void
emitPrmDecls(struct type *functype, struct name *locals)
{
	struct name *param, *local, *found;

	if (!functype || !(functype->flags & TF_FUNC))
		return;
	for (param = functype->elem; param; param = param->next) {
		if (param->type->size == 0)
			continue;
		found = NULL;
		if (param->id)
			for (local = locals; local; local = local->next)
				if (local->id == param->id) {
					found = local;
					break;
				}
		emit1(AST_DECL);
		emit1(typeSfx(param->type));
		emitS(param->id ? nameOf(param->id) : "_");
		emit1(found ? found->w.r.reg : 0);
		emit1(found ? (unsigned char)found->w.r.frm_off : 0);
	}
}

/* Emit local variable declarations */
void
emitLocals(struct name *locals)
{
	struct name *local;
	char lbuf[32];

	for (local = locals; local; local = local->next) {
		if (local->kind == kfunarg)
			continue;
		if (local->sclass & SC_STATIC)
			fmtstr(lbuf, "S%d", local->static_id - 1);
		else if (local->static_id)
			fmtstr(lbuf, "L%d", local->static_id - 1);
		else
			fmtstr(lbuf, "%s", nameOf(local->id));
		emit1(AST_DECL);
		emit1(typeSfx(local->type));
		emitS(lbuf);
		emit1(local->w.r.reg);
		emit2((unsigned short)local->w.r.frm_off);
	}
}

/*
 * Output a global asm block - write directly to assembly file
 */
void
emitGlobalAsm(char *text)
{
	/* Phase 1: don't emit */
	if (phase == 1)
		return;

	if (!text)
		return;
	setSeg(SEG_TEXT);
	asmLine(text);
}

/*
 * Output an asm block inside a function body.  Unlike a global asm
 * block this must stay in place relative to the generated code, so it
 * rides in the AST stream and pass2 copies it to the output.
 * Format: ASM len(2) text
 */
void
emitAsmStmt(char *text)
{
	unsigned short len;

	/* Phase 1: don't emit */
	if (phase == 1)
		return;

	len = text ? strlen(text) : 0;
	emit1(ASM);
	emit2(len);
	emitRaw(text, len);
}

/*
 * Output function header in AST format
 * Format: F rettype name param_count local_count frm_size(2) savebase
 *         params... locals...
 * savebase = scalar area size; the callee-save slots sit at
 * (iy-savebase-2)/(iy-savebase-4), with arrays below them.
 */
void
emitFuncPre(struct name *func)
{
	char func_name[32];
	int frm_size;
	char param_count, local_count;
	struct name *n;

	if (!func)
		return;
#ifdef DEBUG
	if (VERBOSE(V_EMIT))
		fdprintf(2, "EMIT func %s\n", nameOf(func->id));
#endif

	frm_size = analyzeFunc(func);
	curFuncLocals = func->u.locals;

	/* Count params and locals first */
	param_count = local_count = 0;
	if (func->type->flags & TF_FUNC)
		for (n = func->type->elem; n; n = n->next)
			if (n->type->size > 0)
				param_count++;
	for (n = func->u.locals; n; n = n->next)
		if (n->kind != kfunarg)
			local_count++;

	/* Emit function header */
	if (func->sclass & SC_STATIC)
		fmtstr(func_name, "S%d", func->static_id - 1);
	else
		fmtstr(func_name, "_%s", nameOf(func->id));
	emit1(AST_FUNC);
	emit1(func->type->sub ? typeSfx(func->type->sub) : 'v');
	emitS(func_name);
	emit1(param_count);
	emit1(local_count);
	emit2((unsigned short)frm_size);
	emit1((unsigned char)frameSaveBase);

	/* Emit declarations */
	emitPrmDecls(func->type, func->u.locals);
	emitLocals(func->u.locals);

	/* Block header */
	emit1(AST_BLOCK);
	emit1(0);
	emit1(popFuncCnt());
}

/*
 * Output an uninitialized global variable declaration
 * Initialized globals are handled by streaming in doInitlzr()
 */
void
emitGv(struct name *var)
{
	char fullname[32];
	int size;

	/* Phase 1: don't emit, just build symbol table */
	if (phase == 1)
		return;

	if (!var)
		return;

	/* Build label: globals get ::, statics get : */
	if (var->sclass & SC_STATIC)
		fmtstr(fullname, "S%d:", var->static_id - 1);
	else
		fmtstr(fullname, "_%s::", nameOf(var->id));

	/* Calculate total size */
	if (var->type->flags & TF_ARRAY)
		size = var->type->count * var->type->sub->size;
	else
		size = var->type->size;

	/* Uninitialized variable - use .bss */
	setSeg(SEG_BSS);
	asmLine(fullname);
	asmDs(size);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
