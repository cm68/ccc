/*
 * Register allocation and frame offset assignment for locals
 */
#include "cc1.h"

/* scalar area size of the current function: the callee-save slots sit
 * just below it (outast emits this as the FUNC header savebase) */
int frameSaveBase;

/*
 * Assign stack frame offsets to parameters and locals.
 * Params get positive offsets (above FP), locals get negative (below FP).
 * Register-allocated variables get frm_off=0 (not on stack).
 * Returns frame size (bytes needed for locals on stack).
 */
static int
assignFrmOff(struct name *func)
{
	struct name *n, *locals;
	int off, maxoff, i;
	int mark[MAXBLKLVL];
	unsigned char arrays, prevlvl, prevblk;

	frameSaveBase = 0;
	if (!func->type || !func->u.locals)
		return 0;
	locals = func->u.locals;

	/* Parameters: positive offsets starting at +4 (skip saved FP + ret addr) */
	off = 4;
	for (n = func->type->elem; n; n = n->next) {
		if (n->type->size == 0)
			continue;  /* skip void */
		if (n->id) {
			struct name *local;
			for (local = locals; local; local = local->next)
				if (local->id == n->id) {
					local->w.r.frm_off = off;
					break;
				}
		}
		off += n->type->size > 2 ? n->type->size : 2;
	}

	/*
	 * Locals: negative offsets for non-register vars.
	 * Scalars go first, nearest the frame pointer, so they stay
	 * inside the 7-bit (iy+d) window.  Arrays follow, below a
	 * 4-byte gap reserved for the callee-save slots: array bases
	 * are formed with 16-bit address arithmetic, so they may sit
	 * past the window.
	 */
	/*
	 * Locals in nested blocks share space with their siblings.  Two
	 * blocks at the same level cannot both be live, so "if (a) { int
	 * b; } else { int c; }" wants one slot and not two.
	 *
	 * C puts declarations at the head of a block, so capLocals hands
	 * them over in the order the blocks were entered: a block's own
	 * locals, then the blocks inside it.  That is a depth-first walk
	 * of the scope tree already, and one pass over it is enough.
	 * mark[] remembers where each level started; going deeper sets
	 * the mark, and a new block at the same level rewinds to it.
	 *
	 * The frame is the deepest the running offset ever reached, not
	 * where it ended.
	 */
	off = 0;
	maxoff = 0;
	prevlvl = 2;
	prevblk = 0;
	for (i = 0; i < MAXBLKLVL; i++)
		mark[i] = 0;
	for (n = locals; n; n = n->next) {
		if (n->kind == kfunarg)
			continue;
		if (n->w.r.reg) {
			n->w.r.frm_off = 0;
			continue;
		}
		if (n->type->flags & TF_ARRAY)
			continue;
		if (n->level < MAXBLKLVL &&
		    (n->level != prevlvl || n->w.r.blkid != prevblk)) {
			if (n->level > prevlvl)
				mark[n->level] = off;
			else
				off = mark[n->level];
			prevlvl = n->level;
			prevblk = n->w.r.blkid;
		}
		off += n->type->size;
		n->w.r.frm_off = -off;
		if (off > maxoff)
			maxoff = off;
	}
	off = maxoff;
	frameSaveBase = off;
	if (off > 120)
		gripe(ER_D_FL);
	arrays = 0;
	for (n = locals; n; n = n->next) {
		if (n->kind == kfunarg || n->w.r.reg)
			continue;
		if (!(n->type->flags & TF_ARRAY))
			continue;
		if (!arrays) {
			arrays = 1;
			off += 4;	/* callee-save slots */
		}
		off += n->type->size;
		n->w.r.frm_off = -off;
	}
	return off;  /* frame size = total local stack space */
}

/* Check if variable can be allocated to a register */
static int
canAlloc(struct name *n, int no_arg_regs)
{
	if (n->w.r.reg != REG_NONE || n->w.r.addr_taken)
		return 0;
	/*
	 * A static local lives at a label, not in the frame and not in
	 * a register: it has to survive the call, and a register does
	 * not.  It is a local by level, which is what let it through
	 * here - and being handed a register made a read of it come
	 * back as the address.
	 */
	if (n->sclass & SC_STATIC)
		return 0;
	if (no_arg_regs && n->kind == kfunarg)
		return 0;
	return 1;
}

/* Register allocation for local variables */
static void
allocRegs(struct name *locals)
{
	struct name *n, *best;
	char regs = 0;  /* bits: 1=IX, 2=BC, 4=B, 8=C */
	unsigned char no_arg_regs = 0;

	/* If any funarg has address taken, no funargs can use registers */
	for (n = locals; n; n = n->next)
		if (n->kind == kfunarg && n->w.r.addr_taken)
			no_arg_regs = 1;

	/* Allocate register-marked variables first */
	for (n = locals; n; n = n->next) {
		if (!(n->sclass & SC_REGISTER) || !canAlloc(n, no_arg_regs))
			continue;
		if ((n->type->flags & TF_POINTER) && !(regs & 1)) {
			n->w.r.reg = REG_IX;
			regs |= 1;
		} else if (n->type->size == 2 && !(regs & 14)) {
			/* the pair is free only if neither half is taken */
			n->w.r.reg = REG_BC;
			regs |= 2;
		} else if (n->type->size == 1 && !(regs & 2)) {
			if (!(regs & 4)) { n->w.r.reg = REG_B; regs |= 4; }
			else if (!(regs & 8)) { n->w.r.reg = REG_C; regs |= 8; }
		}
	}

	/* IX to pointer with highest agg_refs (only if used for field access) */
	if (!(regs & 1)) {
		best = NULL;
		for (n = locals; n; n = n->next) {
			if (!canAlloc(n, no_arg_regs))
				continue;
			if (!(n->type->flags & TF_POINTER))
				continue;
			/* Only use IX if pointer is used for field access */
			if (n->w.r.agg_refs > 0) {
				if (!best || n->w.r.agg_refs > best->w.r.agg_refs)
					best = n;
			}
		}
		if (best) { best->w.r.reg = REG_IX; regs |= 1; }
	}

	/*
	 * BC to word variable with highest ref_count - and word before
	 * byte is not an accident of ordering, it is the measured
	 * answer.  Three attempts to be cleverer all made the compiler
	 * bigger: weighting loop references 8x and 64x by depth cost
	 * 855 bytes (a variable with fifteen sites lost BC to one with
	 * two hot sites, and all fifteen got three bytes longer);
	 * weighting them 2x cost 323; and giving B and C to the two
	 * hottest bytes when their counts beat twice the best word's
	 * cost 264 even with no weighting at all, because a word in BC
	 * saves four bytes a site while a byte in B or C mostly reduces
	 * through A and saves one or two.  This allocator is buying
	 * bytes, not cycles: a reference site costs the same wherever
	 * it sits in the loop structure, so count sites and give the
	 * pair to the word.
	 */
	if (!(regs & 14)) {
		/*
		 * regs & 14, not regs & 2: B or C alone being taken - which
		 * a register-class byte parameter can now cause - leaves no
		 * pair to give.  Checking only the pair bit handed BC to a
		 * word local while B was carrying a parameter, and the
		 * initializer of one overwrote the staging of the other.
		 */
		best = NULL;
		for (n = locals; n; n = n->next) {
			if (!canAlloc(n, no_arg_regs))
				continue;
			if (n->type->flags & (TF_ARRAY | TF_AGGREGATE))
				continue;
			if (n->type->size == 2 && n->w.r.ref_count > 1)
				if (!best || n->w.r.ref_count > best->w.r.ref_count)
					best = n;
		}
		if (best) { best->w.r.reg = REG_BC; regs |= 2; }
	}

	/* B and C to byte variables */
	if (!(regs & 2)) {
		for (n = locals; n; n = n->next) {
			if (!canAlloc(n, no_arg_regs))
				continue;
			if (n->type->flags & (TF_ARRAY | TF_AGGREGATE))
				continue;
			if (n->type->size == 1 && n->w.r.ref_count > 1) {
				if (!(regs & 4)) { n->w.r.reg = REG_B; regs |= 4; }
				else if (!(regs & 8)) { n->w.r.reg = REG_C; regs |= 8; }
				if ((regs & 12) == 12) break;
			}
		}
	}
}

/* Entry point: analyze function and allocate registers, returns frame size */
int
analyzeFunc(struct name *func)
{
	struct name *n;

	if (!func)
		return 0;

	/* Locals are already captured (from phase 1) and ref_count is
	 * already populated during phase 1 parseExpr. Just reset reg. */
	for (n = func->u.locals; n; n = n->next) {
		n->w.r.reg = REG_NONE;
	}

	/* Allocate registers based on usage (ref_count from phase 1) */
	allocRegs(func->u.locals);

	/* Assign frame offsets to non-register vars, return frame size */
	return assignFrmOff(func);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
