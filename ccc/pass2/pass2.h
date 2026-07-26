/*
 * pass2.h - Code generator header
 */
#ifndef PASS2_H
#define PASS2_H

#include "../cpp/lexeme.h"

/* Type suffixes from AST */
#define T_BYTE    'b'
#define T_UBYTE   'B'
#define T_SHORT   's'
#define T_USHORT  'S'
#define T_LONG    'l'
#define T_ULONG   'L'
#define T_FLOAT   'f'
#define T_VOID    'v'

/* Type size helpers */
#define ISBYTE(t)  ((t) == T_BYTE || (t) == T_UBYTE)
#define ISWORD(t)  ((t) == T_SHORT || (t) == T_USHORT)
#define ISLONG(t)  ((t) == T_LONG || (t) == T_ULONG || (t) == T_FLOAT)
#define TSIZE(t)   (ISBYTE(t) ? 1 : ISWORD(t) ? 2 : ISLONG(t) ? 4 : 0)

/* Register codes */
#define R_B     1
#define R_C     2
#define R_BC    3
#define R_IX    4
#define R_DE    5
#define R_HL    6
#define R_A     7
#define R_IY    8
#define R_E     9       /* low byte of DE */
#define R_D     10      /* high byte of DE */
#define R_L     11      /* low byte of HL */
#define R_H     12      /* high byte of HL */

/* Flag codes (for comparison results) */
#define F_Z     16      /* zero flag */
#define F_NZ    17      /* not zero */
#define F_C     18      /* carry flag */
#define F_NC    19      /* not carry */

/* Global state */
extern int infd;
extern int in2fd;
extern int outfd;
extern void initOpTab(void);	/* rewrite.c: fill op_table */

/* AST I/O */
unsigned char read1(void);
void unread1(unsigned char c);
unsigned short read2(void);
unsigned long read4(void);
void readS(char *buf);

/* Output */
void out(char *s);
void outc(char c);
void outd(int n);
void copyinit(void);

/* Parser */
void parse(void);

/* debug options */
#ifdef DEBUG
#define VERBOSE(x) (verbose & (x))
extern short verbose;
#else
#define VERBOSE(x) (0)
#endif

#endif /* PASS2_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
