/*
 * match.c - Expression tree pattern matcher interpreter
 */
#include "match.h"

/*
 * Match expression against bytecode patterns.
 * Returns SP_* result on match, 0 (SP_NONE) on no match.
 *
 * Pattern format: [len] [instructions...] [P_MATCH result]
 * Terminated by len=0.
 */
int
exprMatch(struct expr *e, unsigned char *patterns)
{
	struct expr *stack[8];
	struct expr *cur;
	unsigned char *pc, *pat;
	int sp;

	pat = patterns;
	while (*pat) {
		unsigned char len = *pat++;
		pc = pat;
		pat += len;  /* advance to next pattern */

		cur = e;
		sp = 0;

		while (pc < pat) {
			unsigned char inst = *pc++;
			switch (inst) {
			case P_OP:
				if (cur->op != *pc++) goto next;
				break;
			case P_OP2:
				if (cur->op != pc[0] && cur->op != pc[1]) goto next;
				pc += 2;
				break;
			case P_SIZE:
				if (cur->size != *pc++) goto next;
				break;
			case P_SIZLE:
				if (cur->size > *pc++) goto next;
				break;
			case P_AUX:
				if (cur->aux != *pc++) goto next;
				break;
			case P_AUX2:
				if (cur->aux != pc[0] && cur->aux != pc[1]) goto next;
				pc += 2;
				break;
			case P_SYM:
				if (!cur->sym) goto next;
				break;
			case P_POW2:
				{
					long n = cur->v.l;
					if (n <= 0 || (n & (n - 1)) != 0) goto next;
				}
				break;
			case P_CNT4:
				if (cur->aux2 > 4) goto next;
				break;
			case P_LEFT:
				if (!cur->left) goto next;
				stack[sp++] = cur;
				cur = cur->left;
				break;
			case P_RIGHT:
				if (!cur->right) goto next;
				stack[sp++] = cur;
				cur = cur->right;
				break;
			case P_UP:
				cur = stack[--sp];
				break;
			case P_MATCH:
				return *pc;  /* success! */
			}
		}
	next:
		continue;
	}
	return 0;  /* no match (SP_NONE) */
}

/*
 * Apply action table after successful match.
 * Sets e->special and copies fields based on action flags.
 */
void
applyAction(struct expr *e, int result)
{
	struct action *a = exprActions;

	while (a->result) {
		if (a->result == result) {
			e->special = result;
			if (a->flags & ACT_SYM_L) {
				e->sym = e->left->sym;
				e->left->sym = 0;
			}
			if (a->flags & ACT_SYM_LL) {
				e->sym = e->left->left->sym;
				e->left->left->sym = 0;
			}
			if (a->flags & ACT_OFS_RV)
				e->offset = e->right->v.s;
			if (a->flags & ACT_OFS_LRV)
				e->offset = e->left->right->v.s;
			if (a->flags & ACT_OFS_LA2)
				e->offset = e->left->aux2;
			if (a->flags & ACT_INCR_A2)
				e->incr = e->aux2;
			if (a->flags & ACT_DEST_LA)
				e->dest = e->left->aux;
			if (a->flags & ACT_DEST_IYO)
				e->dest = R_IYO;
			return;
		}
		a++;
	}
	/* Fallback: just set special, no field copying */
	e->special = result;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
