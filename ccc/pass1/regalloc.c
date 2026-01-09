/*
 * Register allocation and frame offset assignment for locals
 */
#include "cc1.h"

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
					local->frm_off = off;
					break;
				}
		}
		off += n->type->size > 2 ? n->type->size : 2;
	}

	/* Locals: negative offsets for non-register vars */
	off = 0;
	for (n = locals; n; n = n->next) {
		if (n->kind == funarg)
			continue;
		if (n->reg)
			n->frm_off = 0;
		else {
			off += n->type->size;
			n->frm_off = -off;
		}
	}
	return off;  /* frame size = total local stack space */
}

/* Check if variable can be allocated to a register */
static int
canAlloc(struct name *n, int no_arg_regs)
{
	if (n->reg != REG_NONE || n->addr_taken)
		return 0;
	if (no_arg_regs && n->kind == funarg)
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
		if (n->kind == funarg && n->addr_taken)
			no_arg_regs = 1;

	/* Allocate register-marked variables first */
	for (n = locals; n; n = n->next) {
		if (!(n->sclass & SC_REGISTER) || !canAlloc(n, no_arg_regs))
			continue;
		if ((n->type->flags & TF_POINTER) && !(regs & 1)) {
			n->reg = REG_IX;
			regs |= 1;
		} else if (n->type->size == 2 && !(regs & 2)) {
			n->reg = REG_BC;
			regs |= 2;
		} else if (n->type->size == 1 && !(regs & 2)) {
			if (!(regs & 4)) { n->reg = REG_B; regs |= 4; }
			else if (!(regs & 8)) { n->reg = REG_C; regs |= 8; }
		}
	}

	/* IX to struct pointer with highest agg_refs */
	if (!(regs & 1)) {
		best = NULL;
		for (n = locals; n; n = n->next) {
			if (!canAlloc(n, no_arg_regs))
				continue;
			if ((n->type->flags & TF_POINTER) &&
			    n->agg_refs > 0 && n->ref_count > 1)
				if (!best || n->agg_refs > best->agg_refs)
					best = n;
		}
		if (best) { best->reg = REG_IX; regs |= 1; }
	}

	/* BC to word variable with highest ref_count */
	if (!(regs & 2)) {
		best = NULL;
		for (n = locals; n; n = n->next) {
			if (!canAlloc(n, no_arg_regs))
				continue;
			if (n->type->flags & (TF_ARRAY | TF_AGGREGATE))
				continue;
			if (n->type->size == 2 && n->ref_count > 1)
				if (!best || n->ref_count > best->ref_count)
					best = n;
		}
		if (best) { best->reg = REG_BC; regs |= 2; }
	}

	/* B and C to byte variables */
	if (!(regs & 2)) {
		for (n = locals; n; n = n->next) {
			if (!canAlloc(n, no_arg_regs))
				continue;
			if (n->type->flags & (TF_ARRAY | TF_AGGREGATE))
				continue;
			if (n->type->size == 1 && n->ref_count > 1) {
				if (!(regs & 4)) { n->reg = REG_B; regs |= 4; }
				else if (!(regs & 8)) { n->reg = REG_C; regs |= 8; }
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
		n->reg = REG_NONE;
	}

	/* Allocate registers based on usage (ref_count from phase 1) */
	allocRegs(func->u.locals);

	/* Assign frame offsets to non-register vars, return frame size */
	return assignFrmOff(func);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
