/*
 *
 * The op.c file is part of the restored P1.COM program
 * from the Hi-Tech CP/M Z80 C v3.09
 *
 * Not a commercial goal of this laborious work is to popularize among
 * potential fans of 8-bit computers the old HI-TECH Z80 C compiler V3.09
 * (HI-TECH Software) and extend its life, outside of the CP/M environment
 * for full operation in windows 32/64 and Unix-like operating systems
 *
 * The HI-TECH Z80 C cross compiler V3.09 is provided free of charge for any use,
 * private or commercial, strictly as-is. No warranty or product support
 * is offered or implied including merchantability, fitness for a particular
 * purpose, or non-infringement. In no event will HI-TECH Software or its
 * corporate affiliates be liable for any direct or indirect damages.
 *
 * You may use this software for whatever you like, providing you acknowledge
 * that the copyright to this software remains with HI-TECH Software and its
 * corporate affiliates.
 *
 * All copyrights to the algorithms used, binary code, trademarks, etc.
 * belong to the legal owner - Microchip Technology Inc. and its subsidiaries.
 * Commercial use and distribution of recreated source codes without permission
 * from the copyright holderis strictly prohibited.
 *
 * Mark Ogden
 * 09-Jul-2022
 */
#include "p1.h"

/*
 * the pass1 operator/operand table
 */

op_t opTable[68] = {
/*   name    prec  opnf  oprf    type	*/
	{ "",    0x06, 0x00, 0x0200, 7 }, /* 60 T_OPBOT */
	{ "(",   0x1e, 0x02, 0x0200, 3 }, /* 61 */
	{ "",    0x1f, 0x00, 0x0000, 0 }, /* 62 T_LPAREN */
	{ "",    0x02, 0x00, 0x0000, 0 }, /* 63 T_RPAREN */
	{ "",    0x1e, 0x02, 0x0000, 0 }, /* 64 T_ARRAYIDX */
	{ "",    0x1e, 0x00, 0x0000, 0 }, /* 65 T_LBRACK */
	{ "",    0x02, 0x00, 0x0000, 0 }, /* 66 T_RBRACK */
	{ ".",   0x1e, 0x02, 0x0a00, 2 }, /* 67 T_DOT */
	{ "",    0x1e, 0x02, 0x0000, 0 }, /* 68 T_POINTER */
	{ "*U",  0x1c, 0x1c, 0x4200, 3 }, /* 69 D_DEREF */
	{ "&U",  0x1c, 0x1c, 0x2200, 4 }, /* 70 D_ADDRESSOF */
	{ "-U",  0x1c, 0x1c, 0x0030, 1 }, /* 71 D_NEG */
	{ "!",   0x1c, 0x08, 0x0002, 6 }, /* 72 T_LNOT */
	{ "~",   0x1c, 0x18, 0x0020, 1 }, /* 73 T_BNOT */
	{ "",    0x1c, 0x08, 0x0000, 0 }, /* 74 T_PREINC */
	{ "",    0x1c, 0x08, 0x0000, 0 }, /* 75 T_POSTINC */
	{ "",    0x1c, 0x08, 0x0000, 0 }, /* 76 T_PREDEC */
	{ "",    0x1c, 0x08, 0x0000, 0 }, /* 77 T_POSTDEC */
	{ "#",   0x1c, 0x18, 0x0300, 5 }, /* 78 T_SIZEOF */
	{ "()",  0x1c, 0x0a, 0x0000, 0 }, /* 79 T_CAST */
	{ "*",   0x1a, 0x16, 0x0070, 1 }, /* 80 T_MUL */
	{ "&",   0x11, 0x16, 0x0060, 1 }, /* 81 T_BAND */
	{ "-",   0x18, 0x16, 0x007c, 1 }, /* 82 T_MINUS */
	{ "/",   0x1a, 0x12, 0x0070, 1 }, /* 83 T_DIV */
	{ "%",   0x1a, 0x12, 0x0060, 1 }, /* 84 T_MOD */
	{ "+",   0x18, 0x12, 0x0074, 1 }, /* 85 T_PLUS */
	{ ">>",  0x16, 0x12, 0x0060, 1 }, /* 86 T_SHR */
	{ "<<",  0x16, 0x12, 0x0060, 1 }, /* 87 T_SHL */
	{ "<",   0x14, 0x12, 0x10f8, 6 }, /* 88 T_LT */
	{ ">",   0x14, 0x12, 0x10f8, 6 }, /* 89 T_GT */
	{ "<=",  0x14, 0x12, 0x10f8, 6 }, /* 90 T_LE */
	{ ">=",  0x14, 0x12, 0x10f8, 6 }, /* 91 T_GE */
	{ "==",  0x12, 0x12, 0x90f8, 6 }, /* 92 T_EQEQ */
	{ "!=",  0x12, 0x12, 0x90f8, 6 }, /* 93 T_NE */
	{ "^",   0x10, 0x12, 0x0060, 1 }, /* 94 T_XOR */
	{ "|",   0x0f, 0x12, 0x0060, 1 }, /* 95 T_BOR */
	{ "&&",  0x0e, 0x02, 0x0003, 6 }, /* 96 T_LAND */
	{ "||",  0x0d, 0x02, 0x0003, 6 }, /* 97 T_LOR */
	{ "?",   0x0c, 0x1a, 0x0002, 2 }, /* 98 T_QUEST */
	{ ":",   0x0c, 0x1a, 0x12f8, 1 }, /* 99 T_COLON */
	{ "=",   0x0a, 0x0a, 0x2600, 1 }, /* 100 T_EQ */
	{ "=+",  0x0a, 0x0a, 0x2434, 1 }, /* 101 P1_EQPLUS */
	{ "++",  0x00, 0x0a, 0x2434, 1 }, /* 102 P1_POSTINC */
	{ "=-",  0x0a, 0x0a, 0x2434, 1 }, /* 103 P1_EQMINUS */
	{ "--",  0x00, 0x0a, 0x2434, 1 }, /* 104 P1_POSTDEC */
	{ "=*",  0x0a, 0x0a, 0x2430, 1 }, /* 105 P1_EQMUL */
	{ "=/",  0x0a, 0x0a, 0x2430, 1 }, /* 106 P1_EQDIV */
	{ "=%",  0x0a, 0x0a, 0x2420, 1 }, /* 107 P1_EQMOD */
	{ "=>>", 0x0a, 0x0a, 0x2020, 1 }, /* 108 P1_EQSHR */
	{ "=<<", 0x0a, 0x0a, 0x2020, 1 }, /* 109 P1_EQSHL */
	{ "=&",  0x0a, 0x0a, 0x2420, 1 }, /* 110 P1_EQAND */
	{ "=^",  0x0a, 0x0a, 0x2420, 1 }, /* 111 P1_EQXOR */
	{ "=|",  0x0a, 0x0a, 0x2420, 1 }, /* 112 P1_EQOR */
	{ ",",   0x07, 0x02, 0x0200, 7 }, /* 113 T_COMMA */
	{ ";",   0x07, 0x02, 0x0200, 2 }, /* 114 T_SEMICOLON */
	{ "",    0x00, 0x01, 0x0000, 0 }, /* 115 T_ID */
	{ "",    0x00, 0x11, 0x0000, 0 }, /* 116 T_ICONST */
	{ "",    0x00, 0x11, 0x0000, 0 }, /* 117 T_LCONST */
	{ "",    0x00, 0x11, 0x0000, 0 }, /* 118 T_SCONST */
	{ "",    0x00, 0x11, 0x0000, 0 }, /* 119 S_TYPE */
	{ "..",  0x00, 0x00, 0x0000, 0 }, /* 120 */
	{ ")",   0x00, 0x00, 0x0200, 7 }, /* 121 */
	{ "$U",  0x00, 0x00, 0x0200, 7 }, /* 122 P1_COND */
	{ "$",   0x00, 0x02, 0x0002, 7 }, /* 123 */
	{ "->",  0x00, 0x12, 0x0880, 2 }, /* 124 P1_CONVERT */
	{ "@",   0x00, 0x02, 0x0480, 7 }, /* 125 */
	{ "",    0x00, 0x01, 0x0000, 0 }, /* 126 D_MEMBERID */
	{ "",    0x00, 0x11, 0x0000, 0 }, /* 127 T_FCONST */
};

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
