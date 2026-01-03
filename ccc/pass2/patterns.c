/*
 * patterns.c - Expression pattern bytecode tables
 */
#include "match.h"
#include "../../cpp/lexeme.h"

/*
 * Pattern bytecode table
 * Format: [len] [instructions...] [P_MATCH result]
 * Terminated by len=0 (P_END)
 *
 * Byte counts: P_OP=2, P_OP2=3, P_SIZE=2, P_SIZLE=2, P_AUX=2, P_AUX2=3,
 *              P_LEFT=1, P_RIGHT=1, P_UP=1, P_SYM=1, P_POW2=1, P_CNT4=1,
 *              P_MATCH=2
 *
 * Patterns are ordered most-specific first.
 */
unsigned char exprPatterns[] = {
	/* ASSIGN REGVAR(B|C) AST_CONST (size==1) -> SP_STCONST */
	/* P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_AUX2(3)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=16 */
	16,
	P_OP, ASSIGN, P_SIZE, 1,
	P_LEFT, P_OP, REGVAR, P_AUX2, R_B, R_C,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_STCONST,

	/* ASSIGN SYM AST_CONST (size<=2) -> SP_STCONST */
	/* P_OP(2)+P_SIZLE(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=13 */
	13,
	P_OP, ASSIGN, P_SIZLE, 2,
	P_LEFT, P_OP, SYM,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_STCONST,

	/* ASSIGN LOCALVAR AST_CONST -> SP_STCONST */
	/* P_OP(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=11 */
	11,
	P_OP, ASSIGN,
	P_LEFT, P_OP, LOCALVAR,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_STCONST,

	/* ASSIGN DEREF AST_CONST -> SP_STCONST */
	/* P_OP(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=11 */
	11,
	P_OP, ASSIGN,
	P_LEFT, P_OP, DEREF,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_STCONST,

	/* ASSIGN PLUS(sz=2) AST_CONST -> SP_STCONST */
	/* P_OP(2)+P_LEFT(1)+P_OP(2)+P_SIZE(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=13 */
	13,
	P_OP, ASSIGN,
	P_LEFT, P_OP, PLUS, P_SIZE, 2,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_STCONST,

	/* PREINC/PREDEC REGVAR (cnt<=4) -> SP_INCR (caller distinguishes) */
	/* P_OP2(3)+P_LEFT(1)+P_OP(2)+P_CNT4(1)+P_MATCH(2)=9 */
	9,
	P_OP2, PREINC, PREDEC,
	P_LEFT, P_OP, REGVAR,
	P_CNT4,
	P_MATCH, SP_INCR,

	/* PREINC/PREDEC LOCALVAR (size==1, cnt<=4) -> SP_INCR */
	/* P_OP2(3)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_CNT4(1)+P_MATCH(2)=11 */
	11,
	P_OP2, PREINC, PREDEC, P_SIZE, 1,
	P_LEFT, P_OP, LOCALVAR,
	P_CNT4,
	P_MATCH, SP_INCR,

	/* PREINC/PREDEC SYM (size==2, cnt<=4) -> SP_INCGLOB */
	/* P_OP2(3)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_CNT4(1)+P_MATCH(2)=11 */
	11,
	P_OP2, PREINC, PREDEC, P_SIZE, 2,
	P_LEFT, P_OP, SYM,
	P_CNT4,
	P_MATCH, SP_INCGLOB,

	/* PLUS SYM AST_CONST (size==2) -> SP_SYMOFS */
	/* P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_SYM(1)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=14 */
	14,
	P_OP, PLUS, P_SIZE, 2,
	P_LEFT, P_OP, SYM, P_SYM,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_SYMOFS,

	/* PLUS DEREF[REGVAR(BC)] AST_CONST (size==2) -> SP_ADDBC */
	/* P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_LEFT(1)+P_OP(2)+P_AUX(2)+P_UP(1)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=19 */
	19,
	P_OP, PLUS, P_SIZE, 2,
	P_LEFT, P_OP, DEREF,
	P_LEFT, P_OP, REGVAR, P_AUX, R_BC,
	P_UP, P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_ADDBC,

	/* DEREF[PLUS SYM AST_CONST] -> SP_SYMOFD */
	/* P_OP(2)+P_LEFT(1)+P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_SYM(1)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=17 */
	17,
	P_OP, DEREF,
	P_LEFT, P_OP, PLUS, P_SIZE, 2,
	P_LEFT, P_OP, SYM, P_SYM,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_SYMOFD,

	/* DEREF[SYM] -> SP_MSYM */
	/* P_OP(2)+P_LEFT(1)+P_OP(2)+P_SYM(1)+P_MATCH(2)=8 */
	8,
	P_OP, DEREF,
	P_LEFT, P_OP, SYM, P_SYM,
	P_MATCH, SP_MSYM,

	/* STAR AST_CONST (pow2) -> SP_MUL2 */
	/* P_OP(2)+P_RIGHT(1)+P_OP(2)+P_POW2(1)+P_MATCH(2)=8 */
	8,
	P_OP, STAR,
	P_RIGHT, P_OP, AST_CONST, P_POW2,
	P_MATCH, SP_MUL2,

	/* EQ/NEQ word AST_CONST -> SP_CMPEQ (caller checks 0/1/-1) */
	/* P_OP2(3)+P_SIZE(2)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=10 */
	10,
	P_OP2, EQ, NEQ, P_SIZE, 2,
	P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_CMPEQ,

	/* LT/EQ LOCALVAR AST_CONST (size==1) -> SP_CMPV */
	/* P_OP2(3)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=14 */
	14,
	P_OP2, LT, EQ, P_SIZE, 1,
	P_LEFT, P_OP, LOCALVAR,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_CMPV,

	/* NEQ LOCALVAR AST_CONST (size==1) -> SP_CMPV (separate for NEQ) */
	/* P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=13 */
	13,
	P_OP, NEQ, P_SIZE, 1,
	P_LEFT, P_OP, LOCALVAR,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_CMPV,

	/* LT/EQ REGVAR AST_CONST (size==1) -> SP_CMPR */
	/* P_OP2(3)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=14 */
	14,
	P_OP2, LT, EQ, P_SIZE, 1,
	P_LEFT, P_OP, REGVAR,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_CMPR,

	/* NEQ REGVAR AST_CONST (size==1) -> SP_CMPR */
	/* P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_UP(1)+P_RIGHT(1)+P_OP(2)+P_MATCH(2)=13 */
	13,
	P_OP, NEQ, P_SIZE, 1,
	P_LEFT, P_OP, REGVAR,
	P_UP, P_RIGHT, P_OP, AST_CONST,
	P_MATCH, SP_CMPR,

	/* LT/EQ DEREF[...] ... (size==1) -> SP_CMPHL */
	/* This is a partial match - caller checks isSimpleByte(right) */
	/* P_OP2(3)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_MATCH(2)=10 */
	10,
	P_OP2, LT, EQ, P_SIZE, 1,
	P_LEFT, P_OP, DEREF,
	P_MATCH, SP_CMPHL,

	/* NEQ DEREF[...] ... (size==1) -> SP_CMPHL */
	/* P_OP(2)+P_SIZE(2)+P_LEFT(1)+P_OP(2)+P_MATCH(2)=9 */
	9,
	P_OP, NEQ, P_SIZE, 1,
	P_LEFT, P_OP, DEREF,
	P_MATCH, SP_CMPHL,

	/* End of patterns */
	0
};

/*
 * Action table - specifies which fields to copy after a match
 */
struct action exprActions[] = {
	{ SP_SYMOFS,  ACT_SYM_L | ACT_OFS_RV },
	{ SP_SYMOFD,  ACT_SYM_LL | ACT_OFS_LRV },
	{ SP_MSYM,    ACT_SYM_L },
	{ SP_INCR,    ACT_INCR_A2 | ACT_DEST_LA },
	{ SP_DECR,    ACT_INCR_A2 | ACT_DEST_LA },
	{ SP_INCGLOB, ACT_SYM_L | ACT_INCR_A2 },
	{ SP_ADDBC,   ACT_OFS_RV },
	{ 0, 0 }  /* end */
};

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
