/*
 * match.h - Expression tree pattern matcher
 */
#ifndef MATCH_H
#define MATCH_H

#include "cc2.h"

/* Navigation instructions */
#define P_LEFT   0x10  /* push current, move to e->left */
#define P_RIGHT  0x11  /* push current, move to e->right */
#define P_UP     0x12  /* pop, restore parent */

/* Checks with 1-byte operand */
#define P_OP     0x20  /* +op: check cur->op == op */
#define P_SIZE   0x21  /* +sz: check cur->size == sz */
#define P_SIZLE  0x22  /* +sz: check cur->size <= sz */
#define P_AUX    0x23  /* +aux: check cur->aux == aux */

/* Checks with 2-byte operands */
#define P_OP2    0x30  /* +op1,op2: check op == op1 || op == op2 */
#define P_AUX2   0x31  /* +a1,a2: check aux == a1 || aux == a2 */

/* Flag checks (no operand) */
#define P_SYM    0x40  /* check cur->sym != NULL */
#define P_POW2   0x41  /* check cur->v.l is power of 2 */
#define P_CNT4   0x42  /* check cur->aux2 <= 4 */

/* Control */
#define P_MATCH  0xF0  /* +result: return SP_* result */
#define P_END    0xFF  /* end of all patterns (len=0) */

/* Action flags - which fields to copy from matched nodes */
#define ACT_SYM_L     0x01  /* e->sym = e->left->sym (steal it) */
#define ACT_SYM_LL    0x02  /* e->sym = e->left->left->sym */
#define ACT_OFS_RV    0x04  /* e->offset = e->right->v.s */
#define ACT_OFS_LRV   0x08  /* e->offset = e->left->right->v.s */
#define ACT_OFS_LA2   0x10  /* e->offset = e->left->aux2 */
#define ACT_INCR_A2   0x20  /* e->incr = e->aux2 */
#define ACT_DEST_LA   0x40  /* e->dest = e->left->aux */
#define ACT_DEST_IYO  0x80  /* e->dest = R_IYO */

/* Action table entry */
struct action {
	unsigned char result;  /* SP_* value */
	unsigned char flags;   /* ACT_* flags to apply */
};

/* Pattern matcher - returns SP_* result or 0 (SP_NONE) */
int exprMatch(struct expr *e, unsigned char *patterns);

/* Apply action table after match */
void applyAction(struct expr *e, int result);

/* Pattern bytecode table (defined in patterns.c) */
extern unsigned char exprPatterns[];

/* Action table (defined in patterns.c) */
extern struct action exprActions[];

#endif /* MATCH_H */
