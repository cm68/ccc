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
	int off;
	unsigned char arrays;

	frameSaveBase = 0;
	if (!func->type || !func->u.locals)
		return 0;
	locals = func->u.locals;

	/* Parameters: positive offsets starting at +4 (skip saved FP + ret addr) */
	off = 4;
	for (n = func->type->elem; n; n = n->next) {
		if (n->type->size == 0)
			continue;  /* skip void */
		if (n->name[0]) {
			struct name *local;
			for (local = locals; local; local = local->next)
				if (strcmp(local->name, n->name) == 0) {
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
	off = 0;
	for (n = locals; n; n = n->next) {
		if (n->kind == kfunarg)
			continue;
		if (n->w.r.reg) {
			n->w.r.frm_off = 0;
			continue;
		}
		if (n->type->flags & TF_ARRAY)
			continue;
		off += n->type->size;
		n->w.r.frm_off = -off;
	}
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
	int no_arg_regs = 0;

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
		} else if (n->type->size == 2 && !(regs & 2)) {
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

	/* BC to word variable with highest ref_count */
	if (!(regs & 2)) {
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
