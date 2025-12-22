#include "c0.h"
/*
 * Operator dope table.
 * Each entry encodes precedence and flags for one operator.
 *
 * Flag bits (low 9 bits):
 *   BINARY  (001) - binary operator
 *   LVALUE  (002) - left operand must be lvalue
 *   RELAT   (004) - relational operator
 *   ASSGOP  (010) - assignment operator
 *   LWORD   (020) - left operand must be integer
 *   RWORD   (040) - right operand must be integer
 *   COMMUTE (0100) - commutative
 *   RASSOC  (0200) - right associative
 *   LEAF    (0400) - leaf node (constant, name)
 *
 * Precedence (bits 9-13): use PREC(level) macro
 * PCVOK (bit 14): pointer conversion allowed
 */
int opdope[] = {
	/* 0: EOFC */	0,
	/* 1: ; */	0,
	/* 2: { */	0,
	/* 3: } */	0,
	/* 4: [ */	PREC(PREC_POSTFX) | BINARY,
	/* 5: ] */	0,
	/* 6: ( */	PREC(PREC_POSTFX),
	/* 7: ) */	0,
	/* 8: : */	PREC(PREC_COND) | RASSOC | BINARY,
	/* 9: , */	PREC(PREC_COMMA) | BINARY,
	/* 10: FSEL */	BINARY,
	/* 11: CAST */	RASSOC | BINARY,
	/* 12: ETYPE */	0,
	/* 13: ITOP */	BINARY,
	/* 14: PTOI */	BINARY,
	/* 15: LTOP */	BINARY,
	/* 16 */	0,
	/* 17 */	0,
	/* 18 */	0,
	/* 19 */	0,
	/* 20: NAME */	LEAF,
	/* 21: CON */	LEAF,
	/* 22: STRING */	LEAF,
	/* 23: FCON */	LEAF,
	/* 24: SFCON */	LEAF,
	/* 25: LCON */	LEAF,
	/* 26: SLCON */	LEAF,
	/* 27 */	0,
	/* 28 */	0,
	/* 29: NULLOP */	LEAF,
	/* 30: INCBEF */	PREC(PREC_UNARY) | PCVOK | RASSOC | LVALUE | BINARY,
	/* 31: DECBEF */	PREC(PREC_UNARY) | PCVOK | RASSOC | LVALUE | BINARY,
	/* 32: INCAFT */	PREC(PREC_POSTFX) | PCVOK | LVALUE | BINARY,
	/* 33: DECAFT */	PREC(PREC_POSTFX) | PCVOK | LVALUE | BINARY,
	/* 34: EXCLA */	PREC(PREC_UNARY) | RASSOC,
	/* 35: AMPER */	PREC(PREC_UNARY) | RASSOC | LVALUE,
	/* 36: STAR */	PREC(PREC_UNARY) | RASSOC | LWORD,
	/* 37: NEG */	PREC(PREC_UNARY) | RASSOC,
	/* 38: COMPL */	PREC(PREC_UNARY) | RASSOC | LWORD,
	/* 39: DOT */	PREC(PREC_POSTFX) | BINARY,
	/* 40: PLUS */	PREC(PREC_ADD) | PCVOK | COMMUTE | BINARY,
	/* 41: MINUS */	PREC(PREC_ADD) | PCVOK | BINARY,
	/* 42: TIMES */	PREC(PREC_MUL) | COMMUTE | BINARY,
	/* 43: DIVIDE */	PREC(PREC_MUL) | BINARY,
	/* 44: MOD */	PREC(PREC_MUL) | BINARY,
	/* 45: RSHIFT */	PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/* 46: LSHIFT */	PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/* 47: AND */	PREC(PREC_BAND) | COMMUTE | RWORD | LWORD | BINARY,
	/* 48: OR */	PREC(PREC_BOR) | COMMUTE | RWORD | LWORD | BINARY,
	/* 49: EXOR */	PREC(PREC_BOR) | COMMUTE | RWORD | LWORD | BINARY,
	/* 50: ARROW */	PREC(PREC_POSTFX) | BINARY,
	/* 51: ITOF */	0,
	/* 52: FTOI */	0,
	/* 53: LOGAND */	PREC(PREC_LAND) | BINARY,
	/* 54: LOGOR */	PREC(PREC_LOR) | BINARY,
	/* 55: ANDN */	PREC(PREC_ADD) | BINARY,
	/* 56: FTOL */	0,
	/* 57: LTOF */	0,
	/* 58: ITOL */	0,
	/* 59: LTOI */	0,
	/* 60: EQUAL */	PREC(PREC_EQ) | RELAT | BINARY,
	/* 61: NEQUAL */	PREC(PREC_EQ) | RELAT | BINARY,
	/* 62: LESSEQ */	PREC(PREC_REL) | RELAT | BINARY,
	/* 63: LESS */	PREC(PREC_REL) | RELAT | BINARY,
	/* 64: GREATEQ */	PREC(PREC_REL) | RELAT | BINARY,
	/* 65: GREAT */	PREC(PREC_REL) | RELAT | BINARY,
	/* 66: LESSEQP */	PREC(PREC_REL) | RELAT | BINARY,
	/* 67: LESSP */	PREC(PREC_REL) | RELAT | BINARY,
	/* 68: GREATQP */	PREC(PREC_REL) | RELAT | BINARY,
	/* 69: GREATP */	PREC(PREC_REL) | RELAT | BINARY,
	/* 70: ASPLUS */	PREC(PREC_ASGN) | PCVOK | RASSOC | ASSGOP | LVALUE | BINARY,
	/* 71: ASMINUS */	PREC(PREC_ASGN) | PCVOK | RASSOC | ASSGOP | LVALUE | BINARY,
	/* 72: ASTIMES */	PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/* 73: ASDIV */	PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/* 74: ASMOD */	PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/* 75: ASRSH */	PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/* 76: ASLSH */	PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/* 77: ASSAND */	PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/* 78: ASOR */	PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/* 79: ASXOR */	PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/* 80: ASSIGN */	PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/* 81 */	0,
	/* 82 */	0,
	/* 83 */	0,
	/* 84 */	0,
	/* 85 */	0,
	/* 86 */	0,
	/* 87 */	0,
	/* 88 */	0,
	/* 89 */	0,
	/* 90: QUEST */	PREC(PREC_COND) | RASSOC | BINARY,
	/* 91: SIZEOF */	PREC(PREC_UNARY) | RASSOC,
	/* 92 */	0,
	/* 93 */	0,
	/* 94 */	0,
	/* 95 */	0,
	/* 96 */	0,
	/* 97: SEQNC */	PREC(PREC_COMMA) | BINARY,
	/* 98 */	0,
	/* 99 */	0,
	/* 100: CALL */	PREC(PREC_POSTFX) | BINARY,
	/* 101: MCALL */	PREC(PREC_POSTFX) | BINARY,
	/* 102: JUMP */	0,
	/* 103: CBRANCH */	0,
	/* 104: INIT */	0,
	/* 105: SETREG */	0,
	/* 106 */	0,
	/* 107 */	0,
	/* 108 */	0,
	/* 109: ITOC */	0,
	/* 110: RFORCE */	0,
	/* 111: BRANCH */	0,
	/* 112: LABEL */	0,
	/* 113: NLABEL */	0,
	/* 114: RLABEL */	0,
	/* 115: STRASG */	0,
	/* 116: LINENO */	0,
	/* 117: NEWLINE */	0,
	/* 118: ASMSTR */	0,
	/* 119 */	0,
	/* 120 */	0,
	/* 121 */	0,
	/* 122 */	0,
	/* 123 */	0,
	/* 124 */	0,
	/* 125 */	0,
	/* 126 */	0,
	/* 127 */	0,
	/* 128: ULLSHIFT */	PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/* 129: UASLSHL */	PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
};

/*
 * conversion table:
 * FTI: float (or double) to integer
 * ITF: integer to float
 * ITP: integer to pointer
 * ITL: integer to long
 * LTI: long to integer
 * LTF: long to float
 * FTL: float to long
 * PTI: pointer to integer
 * LTP: long to ptr (ptr[long])
 * XX: usually illegal
 * When FTI, LTI, FTL are added in they specify
 * that it is the left operand that should be converted.
 * For + this is done and the conversion is turned back into
 * ITF, ITL, LTF.
 * For = however the left operand can't be converted
 * and the specified conversion is applied to the rhs.
 */
char cvtab[4][4] = {
/*		int	double		long		ptr */
/* int */	0,	(FTI<<4)+ITF,	(LTI<<4)+ITL,	(ITP<<4)+ITP,	
/* double */	ITF,	0,		LTF,		XX,
/* long */	ITL,	(FTL<<4)+LTF,	0,		(LTP<<4)+LTP,
/* ptr */	ITP,	XX,		LTP,		PTI,
};

/*
 * relate conversion numbers to operators
 */
char	cvntab[] = {
	0, ITOF, ITOL, LTOF, ITOP, PTOI, FTOI, LTOI, FTOL, LTOP,
};

