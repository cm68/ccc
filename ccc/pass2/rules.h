/*
 * rules.h - Rewrite rule definitions
 */
#ifndef RULES_H
#define RULES_H

/* Replacement flags */
#define RF_POW2  0x01    /* transform constant through log2 */
#define RF_IXIY  0x02    /* require reg is IX or IY */
#define RF_NOTEQ 0x04    /* NEQ->BANG(EQ): wrap children in EQ node */
#define RF_INC1  0x08    /* increment right constant by 1 */
#define RF_BC    0x10    /* require reg is BC */
#define RF_DE    0x20    /* require reg is DE */
#define RF_HL    0x40    /* require reg is HL */
#define RF_IX    0x80    /* require reg is IX */
#define RF_C     0x100   /* require reg is C (low byte of BC) */
#define RF_B     0x200   /* require reg is B (high byte of BC) */
#define RF_TDE   0x400   /* require TARGET is DE (for RHS of binary ops) */

/*
 * Rewrite rule
 */
struct rule {
	char *pat;      /* pattern string */
	char *rep;      /* replacement: I=INDEX, <=LSHIFT, etc */
	char *lsrc;     /* left child source path */
	char *rsrc;     /* right child source path */
	char *dsrc;     /* data source path (for reg/off) */
	unsigned short flags;
	char *asmtpl;   /* asm template: $L/$R/$LL/etc for interpolation */
	unsigned char destval; /* result location: R_HL, R_A, etc (0=none) */
};

/* Rule table (defined in rules.c) */
extern struct rule rules[];

/* Preserve patterns - subtrees matching these are not reduced */
extern char *preserve[];

#endif /* RULES_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
