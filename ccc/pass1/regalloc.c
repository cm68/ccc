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
	struct name *n;
	struct stmt *body;
	int off;

	if (!func || !func->type || !func->u.body)
		return 0;
	body = func->u.body;

	/* Parameters: positive offsets starting at +4 (skip saved FP + ret addr) */
	off = 4;
	for (n = func->type->elem; n; n = n->next) {
		if (n->type && n->type->size == 0)
			continue;  /* skip void */
		/* Find matching local to set its frm_off */
		if (body->locals && n->name[0]) {
			struct name *local;
			for (local = body->locals; local; local = local->next) {
				if (strcmp(local->name, n->name) == 0) {
					/* Params always need frame offset (passed on stack) */
					local->frm_off = off;
					break;
				}
			}
		}
		off += (n->type && n->type->size > 2) ? n->type->size : 2;
	}

	/* Locals: negative offsets for non-register vars */
	off = 0;
	for (n = body->locals; n; n = n->next) {
		if (n->kind == funarg)
			continue;
		if (n->reg) {
			n->frm_off = 0;  /* in register, not on stack */
		} else {
			int sz = (n->type) ? n->type->size : 2;
			if (sz < 1) sz = 2;
			off += sz;
			n->frm_off = -off;
		}
	}
	return off;  /* frame size = total local stack space */
}

/*
 * Register allocation for local variables and parameters
 * ref_count is computed during phase 1 parseExpr
 *
 * Allocation priority:
 *   1. IX register: allocated to struct pointer with highest agg_refs
 *   2. BC register: allocated to word variable with highest ref_count
 *   3. B/C registers: allocated to byte variables by ref_count
 *
 * Variables excluded:
 *   - Arrays (must remain on stack for &arr[i])
 *   - Unused variables (ref_count == 0)
 *   - Single-use variables (ref_count == 1) - no benefit
 */
static void
allocRegs(struct stmt *body)
{
	struct name *n, *best;
	int bc_used = 0;  /* BC allocated? (precludes B and C) */
	int b_used = 0, c_used = 0;
	int ix_used = 0;
	int has_reg_hint = 0;
	int no_arg_regs = 0;  /* any arg addr taken? then no arg regs */

	/* If any funarg has address taken, no funargs can use registers */
	for (n = body->locals; n; n = n->next) {
		if (n->kind == funarg && n->addr_taken)
			no_arg_regs = 1;
	}

	/* Check if any locals have explicit 'register' storage class */
	for (n = body->locals; n; n = n->next) {
		if (n->sclass & SC_REGISTER)
			has_reg_hint = 1;
	}

	/* First: allocate register-marked variables with preferences */
	if (has_reg_hint) {
		int has_reg_byte = 0;
		/* Check if any register-marked bytes exist */
		for (n = body->locals; n; n = n->next) {
			if ((n->sclass & SC_REGISTER) && n->type &&
			    n->type->size == 1)
				has_reg_byte = 1;
		}
		/* Pass 1: pointers prefer IX */
		for (n = body->locals; n; n = n->next) {
			if (!(n->sclass & SC_REGISTER) || n->reg != REG_NONE ||
			    n->addr_taken || (no_arg_regs && n->kind == funarg))
				continue;
			if (n->type && (n->type->flags & TF_POINTER) && !ix_used) {
				n->reg = REG_IX;
				ix_used = 1;
			}
		}
		/* Pass 2: words - prefer IX if bytes need B/C, else BC */
		for (n = body->locals; n; n = n->next) {
			if (!(n->sclass & SC_REGISTER) || n->reg != REG_NONE ||
			    n->addr_taken || (no_arg_regs && n->kind == funarg))
				continue;
			if (n->type && n->type->size == 2) {
				if (has_reg_byte && !ix_used) {
					n->reg = REG_IX;
					ix_used = 1;
				} else if (!bc_used) {
					n->reg = REG_BC;
					bc_used = 1;
				} else if (!ix_used) {
					n->reg = REG_IX;
					ix_used = 1;
				}
			}
		}
		/* Pass 3: bytes get B then C */
		for (n = body->locals; n; n = n->next) {
			if (!(n->sclass & SC_REGISTER) || n->reg != REG_NONE ||
			    n->addr_taken || (no_arg_regs && n->kind == funarg))
				continue;
			if (n->type && n->type->size == 1 && !bc_used) {
				if (!b_used) {
					n->reg = REG_B;
					b_used = 1;
				} else if (!c_used) {
					n->reg = REG_C;
					c_used = 1;
				}
			}
		}
	}

	/* Second: allocate remaining registers to unmarked vars by usage */

	/* IX to struct pointer with highest agg_refs */
	if (!ix_used) {
		best = NULL;
		for (n = body->locals; n; n = n->next) {
			if (n->reg != REG_NONE ||
			    n->addr_taken || (no_arg_regs && n->kind == funarg))
				continue;
			if (n->type && (n->type->flags & TF_POINTER) &&
			    n->agg_refs > 0 && n->ref_count > 1) {
				if (!best || n->agg_refs > best->agg_refs)
					best = n;
			}
		}
		if (best) {
			best->reg = REG_IX;
			ix_used = 1;
		}
	}

	/* BC (or IX if BC taken) to word variable with highest ref_count */
	if (!bc_used || !ix_used) {
		best = NULL;
		for (n = body->locals; n; n = n->next) {
			if (n->reg != REG_NONE ||
			    n->addr_taken || (no_arg_regs && n->kind == funarg))
				continue;
			if (n->type && (n->type->flags & (TF_ARRAY | TF_AGGREGATE)))
				continue;
			if (n->ref_count <= 1)
				continue;
			if (n->type && n->type->size == 2) {
				if (!best || n->ref_count > best->ref_count)
					best = n;
			}
		}
		if (best) {
			if (!bc_used) {
				best->reg = REG_BC;
				bc_used = 1;
			} else {
				best->reg = REG_IX;
				ix_used = 1;
			}
		}
	}

	/* B and C to byte variables (if BC not used as word) */
	if (!bc_used) {
		for (n = body->locals; n; n = n->next) {
			if (n->reg != REG_NONE ||
			    n->addr_taken || (no_arg_regs && n->kind == funarg))
				continue;
			if (n->type && (n->type->flags & (TF_ARRAY | TF_AGGREGATE)))
				continue;
			if (n->ref_count <= 1)
				continue;
			if (n->type && n->type->size == 1) {
				if (!b_used) {
					n->reg = REG_B;
					b_used = 1;
				} else if (!c_used) {
					n->reg = REG_C;
					c_used = 1;
				}
				if (b_used && c_used)
					break;
			}
		}
	}
}

/* Entry point: analyze function and allocate registers, returns frame size */
int
analyzeFunc(struct name *func)
{
	struct name *n;

	if (!func || !func->u.body)
		return 0;

	/* Locals are already captured (from phase 1) and ref_count is
	 * already populated during phase 1 parseExpr. Just reset reg. */
	for (n = func->u.body->locals; n; n = n->next) {
		n->reg = REG_NONE;
	}

	/* Allocate registers based on usage (ref_count from phase 1) */
	allocRegs(func->u.body);

	/* Assign frame offsets to non-register vars, return frame size */
	return assignFrmOff(func);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
