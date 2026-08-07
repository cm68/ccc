/*
 * C second pass -- tables
 * Z80 target version
 */

#include "c1.h"
#include "z80.h"
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
 *   CNVRT   (01000) - conversion operator
 *
 * Precedence (bits 9-13): use PREC(level) macro
 */
int opdope[] = {
	/*
	 * 0: EOFC 
	 */ 0,
	/*
	 * 1: SEMI 
	 */ 0,
	/*
	 * 2: LBRACE 
	 */ 0,
	/*
	 * 3: RBRACE 
	 */ 0,
	/*
	 * 4: LBRACK 
	 */ PREC(PREC_POSTFX),
	/*
	 * 5: RBRACK 
	 */ 0,
	/*
	 * 6: LPARN 
	 */ PREC(PREC_POSTFX),
	/*
	 * 7: RPARN 
	 */ 0,
	/*
	 * 8: COLON 
	 */ PREC(PREC_COND) | RASSOC | BINARY,
	/*
	 * 9: COMMA 
	 */ PREC(PREC_COMMA) | BINARY,
	/*
	 * 10: FSEL 
	 */ 0,
	/*
	 * 11: FSELR 
	 */ 0,
	/*
	 * 12: FSELT 
	 */ BINARY,
	/*
	 * 13: ITOP 
	 */ BINARY,
	/*
	 * 14: PTOI 
	 */ BINARY,
	/*
	 * 15: LTOP 
	 */ BINARY,
	/*
	 * 16: FSELA 
	 */ BINARY,
	/*
	 * 17: ULSH 
	 */ BINARY,
	/*
	 * 18: ASULSH 
	 */ BINARY,
	/*
	 * 19: KEYW 
	 */ 0,
	/*
	 * 20: NAME 
	 */ LEAF,
	/*
	 * 21: CON 
	 */ LEAF,
	/*
	 * 22: STRING 
	 */ LEAF,
	/*
	 * 23: FCON 
	 */ LEAF,
	/*
	 * 24: SFCON 
	 */ LEAF,
	/*
	 * 25: LCON 
	 */ LEAF,
	/*
	 * 26: SLCON 
	 */ LEAF,
	/*
	 * 27: AUTOI 
	 */ LEAF,
	/*
	 * 28: AUTOD 
	 */ LEAF,
	/*
	 * 29: NULLOP 
	 */ LEAF,
	/*
	 * 30: INCBEF 
	 */ RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 31: DECBEF 
	 */ RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 32: INCAFT 
	 */ RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 33: DECAFT 
	 */ RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 34: EXCLA 
	 */ RASSOC | LWORD,
	/*
	 * 35: AMPER 
	 */ RASSOC | LVALUE,
	/*
	 * 36: STAR 
	 */ RASSOC | LWORD,
	/*
	 * 37: NEG 
	 */ RASSOC,
	/*
	 * 38: COMPL 
	 */ RASSOC | LWORD,
	/*
	 * 39: DOT 
	 */ PREC(PREC_POSTFX) | BINARY,
	/*
	 * 40: PLUS 
	 */ PREC(PREC_ADD) | COMMUTE | BINARY,
	/*
	 * 41: MINUS 
	 */ PREC(PREC_ADD) | BINARY,
	/*
	 * 42: TIMES 
	 */ PREC(PREC_MUL) | COMMUTE | BINARY,
	/*
	 * 43: DIVIDE 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 44: MOD 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 45: RSHIFT 
	 */ PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/*
	 * 46: LSHIFT 
	 */ PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/*
	 * 47: AND 
	 */ PREC(PREC_BAND) | COMMUTE | RWORD | LWORD | BINARY,
	/*
	 * 48: OR 
	 */ PREC(PREC_BOR) | COMMUTE | RWORD | LWORD | BINARY,
	/*
	 * 49: EXOR 
	 */ PREC(PREC_BOR) | COMMUTE | RWORD | LWORD | BINARY,
	/*
	 * 50: ARROW 
	 */ PREC(PREC_POSTFX) | BINARY,
	/*
	 * 51: ITOF 
	 */ CNVRT,
	/*
	 * 52: FTOI 
	 */ CNVRT,
	/*
	 * 53: LOGAND 
	 */ BINARY,
	/*
	 * 54: LOGOR 
	 */ BINARY,
	/*
	 * 55: ANDN 
	 */ PREC(PREC_ADD) | BINARY,
	/*
	 * 56: FTOL 
	 */ CNVRT,
	/*
	 * 57: LTOF 
	 */ CNVRT,
	/*
	 * 58: ITOL 
	 */ CNVRT,
	/*
	 * 59: LTOI 
	 */ 0,
	/*
	 * 60: EQUAL 
	 */ PREC(PREC_EQ) | RELAT | BINARY,
	/*
	 * 61: NEQUAL 
	 */ PREC(PREC_EQ) | RELAT | BINARY,
	/*
	 * 62: LESSEQ 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 63: LESS 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 64: GREATEQ 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 65: GREAT 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 66: LESSEQP 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 67: LESSP 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 68: GREATQP 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 69: GREATP 
	 */ PREC(PREC_REL) | RELAT | BINARY,
	/*
	 * 70: ASPLUS 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 71: ASMINUS 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 72: ASTIMES 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 73: ASDIV 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 74: ASMOD 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 75: ASRSH 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 76: ASLSH 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 77: ASAND 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 78: ASOR 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 79: ASXOR 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 80: ASSIGN 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 81: TAND 
	 */ PREC(PREC_ADD) | BINARY,
	/*
	 * 82: LTIMES 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 83: LDIV 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 84: LMOD 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 85: ASANDN 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 86: LASTIMES 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 87: LASDIV 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 88: LASMOD 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 89 
	 */ 0,
	/*
	 * 90: QUEST 
	 */ PREC(PREC_COND) | RASSOC | BINARY,
	/*
	 * 91: LLSHIFT 
	 */ PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/*
	 * 92: ASLSHL 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
	/*
	 * 93 
	 */ 0,
	/*
	 * 94 
	 */ 0,
	/*
	 * 95 
	 */ 0,
	/*
	 * 96 
	 */ 0,
	/*
	 * 97: SEQNC 
	 */ BINARY,
	/*
	 * 98: CALL1 
	 */ 0,
	/*
	 * 99: CALL2 
	 */ 0,
	/*
	 * 100: CALL 
	 */ PREC(PREC_POSTFX) | BINARY,
	/*
	 * 101: MCALL 
	 */ PREC(PREC_POSTFX),
	/*
	 * 102: JUMP 
	 */ 0,
	/*
	 * 103: CBRANCH 
	 */ 0,
	/*
	 * 104: INIT 
	 */ 0,
	/*
	 * 105: SETREG 
	 */ LEAF,
	/*
	 * 106: LOAD 
	 */ 0,
	/*
	 * 107: PTOI1 
	 */ PREC(PREC_ADD) | BINARY,
	/*
	 * 108 
	 */ 0,
	/*
	 * 109: ITOC 
	 */ 0,
	/*
	 * 110: RFORCE 
	 */ 0,
	/*
	 * 111: BRANCH 
	 */ 0,
	/*
	 * 112: LABEL 
	 */ 0,
	/*
	 * 113: NLABEL 
	 */ 0,
	/*
	 * 114: RLABEL 
	 */ 0,
	/*
	 * 115: STRASG 
	 */ 0,
	/*
	 * 116: STRSET 
	 */ BINARY,
	/*
	 * 117: UDIV 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 118: UMOD 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 119: ASUDIV 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 120: ASUMOD 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 121: ULTIMES 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 122: ULDIV 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 123: ULMOD 
	 */ PREC(PREC_MUL) | BINARY,
	/*
	 * 124: ULASTIMES 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 125: ULASDIV 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 126: ULASMOD 
	 */ PREC(PREC_ASGN) | RASSOC | ASSGOP | LVALUE | BINARY,
	/*
	 * 127: ULTOF 
	 */ CNVRT,
	/*
	 * 128: ULLSHIFT 
	 */ PREC(PREC_SHIFT) | RWORD | LWORD | BINARY,
	/*
	 * 129: UASLSHL 
	 */ PREC(PREC_ASGN) | RASSOC | RWORD | ASSGOP | LVALUE | BINARY,
};

#ifdef DEBUG
char *opntab[] = {
	0,							/* 0 */
	0,
	0,
	0,
	0,
	0,
	0,
	0,
	":",
	",",
	"field select",				/* 10 */
	0,
	0,
	"int->ptr",
	"ptr->int",
	"long->ptr",
	"field assign",
	">>",
	">>=",
	"keyword",
	"name",						/* 20 */
	"short constant",
	"string",
	"float",
	"double",
	"long constant",
	"long constant",
	"*r++",
	"*--r",
	"()",
	"++pre",					/* 30 */
	"--pre",
	"++post",
	"--post",
	"!un",
	"&",
	"*",
	"-",
	"~",
	".",
	"+",						/* 40 */
	"-",
	"*",
	"/",
	"%",
	">>",
	"<<",
	"&",
	"|",
	"^",
	"->",						/* 50 */
	"int->double",
	"double->int",
	"&&",
	"||",
	"&~",
	"double->long",
	"long->double",
	"integer->long",
	"long->integer",
	"==",						/* 60 */
	"!=",
	"<=",
	"<",
	">=",
	">",
	"<p",
	"<=p",
	">p",
	">=p",
	"+=",						/* 70 */
	"-=",
	"*=",
	"/=",
	"%=",
	">>=",
	"<<=",
	"&=",
	"|=",
	"^=",
	"=",						/* 80 */
	"& for tests",
	"*",
	"/",
	"%",
	"&= ~",
	"*=",
	"/=",
	"%=",
	0,
	"?",						/* 90 */
	"<<",
	"<<=",
	0,
	0,
	0,
	0,
	",",
	"call1",
	"call2",
	"call",						/* 100 */
	"mcall",
	"goto",
	"jump cond",
	"branch cond",
	"set nregs",
	"load value",
	"ptr->integer",
	0,
	"int->char",
	"force register",			/* 110 */
	"branch",
	"label",
	"nlabel",
	"rlabel",
	"=structure",
	"= (struct setup)",
	"/",
	"%",
	"/=",
	"%=",						/* 120 */
	"*",						/* unsigned long */
	"/",						/* unsigned long */
	"%",						/* unsigned long */
	"*=",						/* unsigned long */
	"/=",						/* unsigned long */
	"%=",						/* unsigned long */
	"u_long->double",			/* unsigned long */
	">>",						/* unsigned long */
	">>=",						/* 129 unsigned long */
};
#endif

/*
 * Strings for instruction tables.
 * Z80 target mnemonics
 */
char ld[] = "ld";
char clr[] = "ld hl,0";
char cmp[] = "call cmp16";		/* compare HL vs DE */
char tst[] = "ld a,h\n\tor l";	/* test HL for zero */
char add[] = "add hl,de";
char sub[] = "or a\n\tsbc hl,de";
char inc[] = "inc hl";
char dec[] = "dec hl";
char mul[] = "call mul16";		/* HL = HL * DE */
char idiv[] = "call div16";		/* HL = HL / DE */
char asr[] = "sra h\n\trr l";	/* arithmetic shift right */
char shl[] = "add hl,hl";		/* shift left */
char andn[] = "call and16";		/* HL = HL & DE */
char tand[] = "call tand16";	/* test HL & DE */
char or[] = "call or16";		/* HL = HL | DE */
char xor[] = "call xor16";		/* HL = HL ^ DE */
char neg16[] = "call neg16";	/* HL = -HL */
char com16[] = "call com16";	/* HL = ~HL */
char stdol[] = "call (hl)";		/* indirect call */
char ashc[] = "call lshl";		/* long shift */
char slmul[] = "call lmul";		/* long multiply */
char sldiv[] = "call ldiv";		/* long divide */
char slrem[] = "call lmod";		/* long modulo */
char ladd[] = "call ladd";		/* long add */
char lsub[] = "call lsub";		/* long sub */
char lor[] = "call lor";		/* long or */
char land[] = "call land";		/* long and */
char lxor[] = "call lxor";		/* long xor */
char uldiv[] = "call uldiv";	/* unsigned long divide */
char ulrem[] = "call ulmod";	/* unsigned long modulo */
char ualdiv[] = "call ualdiv";
char ualrem[] = "call ualmod";
char ultof[] = "call ultof";
char ulsh[] = "call lushl";
char ualsh[] = "call ualsh";
char almul[] = "call almul";
char aldiv[] = "call aldiv";
char alrem[] = "call almod";
char udiv[] = "call udiv16";
char urem[] = "call umod16";

/*
 * Z80 branch instructions 
 */
char jeq[] = "jp z,";
char jne[] = "jp nz,";
char jle[] = "call jle";		/* signed <= needs helper */
char jgt[] = "call jgt";		/* signed > needs helper */
char jlt[] = "jp m,";			/* signed < */
char jge[] = "jp p,";			/* signed >= */
char jlos[] = "call jlos";		/* unsigned <= needs helper */
char jhi[] = "call jhi";		/* unsigned > needs helper */
char jlo[] = "jp c,";			/* unsigned < */
char jhis[] = "jp nc,";			/* unsigned >= */
char nop[] = "; nop";
char jbr[] = "jp ";
char jpl[] = "jp p,";
char jmi[] = "jp m,";
char jmijne[] = "jp m,L%d\n\tjp nz,";
char jmijeq[] = "jp m,L%d\n\tjp z,";

/*
 * Instruction tables, accessed by
 * I (first operand) or I' (second) macros.
 */

struct instab instab[] = {
	LOAD, ld, tst,
	ASSIGN, ld, clr,
	EQUAL, cmp, tst,
	NEQUAL, cmp, tst,
	LESSEQ, cmp, tst,
	LESS, cmp, tst,
	GREATEQ, cmp, tst,
	GREAT, cmp, tst,
	LESSEQP, cmp, tst,
	LESSP, cmp, tst,
	GREATQP, cmp, tst,
	GREATP, cmp, tst,
	PLUS, add, inc,
	ASPLUS, add, inc,
	MINUS, sub, dec,
	ASMINUS, sub, dec,
	INCBEF, add, inc,
	DECBEF, sub, dec,
	INCAFT, add, inc,
	DECAFT, sub, dec,
	TIMES, mul, mul,
	ASTIMES, mul, mul,
	DIVIDE, idiv, idiv,
	ASDIV, idiv, idiv,
	MOD, idiv, idiv,
	ASMOD, idiv, idiv,
	PTOI, idiv, idiv,
	RSHIFT, asr, asr,
	LSHIFT, shl, shl,
	AND, andn, andn,
	TAND, tand, tand,
	OR, or, or,
	EXOR, xor, xor,
	NEG, neg16, neg16,
	COMPL, com16, com16,
	CALL1, stdol, stdol,
	CALL2, "", "",
	LLSHIFT, ashc, ashc,
	ASLSHL, ashc, ashc,
	LTIMES, slmul, slmul,
	LDIV, sldiv, sldiv,
	LMOD, slrem, slrem,
	LASTIMES, almul, almul,
	LASDIV, aldiv, aldiv,
	LASMOD, alrem, alrem,
	ULSH, ashc, ashc,
	ASULSH, ashc, ashc,
	UDIV, udiv, udiv,
	UMOD, urem, urem,
	ASUDIV, udiv, udiv,
	ASUMOD, urem, urem,
	ULTIMES, slmul, slmul,		/* symmetry */
	ULDIV, uldiv, uldiv,
	ULMOD, ulrem, ulrem,
	ULASTIMES, almul, almul,	/* symmetry */
	ULASDIV, ualdiv, ualdiv,
	ULASMOD, ualrem, ualrem,
	ULTOF, ultof, ultof,
	ULLSHIFT, ulsh, ulsh,
	UASLSHL, ualsh, ualsh,
	LPLUS, ladd, ladd,
	LMINUS, lsub, lsub,
	LOR, lor, lor,
	LAND, land, land,
	LXOR, lxor, lxor,
	LASPLUS, ladd, ladd,
	LASMINUS, lsub, lsub,
	LASOR, lor, lor,
	LASAND, land, land,
	LASXOR, lxor, lxor,
	0, 0, 0
};

/*
 * Similar table for relationals.
 * The first string is for the positive
 * test, the second for the inverted one.
 * The '200+' entries are 
 * used in tests against 0 where a 'tst'
 * instruction is used; it clears the c-bit
 * the c-bit so ptr tests are funny.
 */
struct instab branchtab[] = {
	EQUAL, jeq, jne,
	NEQUAL, jne, jeq,
	LESSEQ, jle, jgt,
	LESS, jlt, jge,
	GREATEQ, jge, jlt,
	GREAT, jgt, jle,
	LESSEQP, jlos, jhi,
	LESSP, jlo, jhis,
	GREATQP, jhis, jlo,
	GREATP, jhi, jlos,
	200 + EQUAL, jeq, jne,
	200 + NEQUAL, jne, jeq,
	200 + LESSEQ, jmijeq, jmijne,
	200 + LESS, jmi, jpl,
	200 + GREATEQ, jpl, jmi,
	200 + GREAT, jmijne, jmijeq,
	200 + LESSEQP, jeq, jne,
	200 + LESSP, nop, jbr,
	200 + GREATQP, jbr, nop,
	200 + GREATP, jne, jeq,
	0, 0, 0
};

/*
 * vim: set tabstop=4 shiftwidth=4 noexpandtab: 
 */
