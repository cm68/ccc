/*
 * Z80 instruction encoding
 * extracted from asm.c
 */
#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#else
#include <stdio.h>
#endif

#include "asm.h"

/* instruction type codes */
#define IBASIC      1
#define IBASIC_EXT  2
#define IARITH      3
#define IINCR       4
#define IBITSH      5
#define ISTACK      6
#define IRET        7
#define IJMP        8
#define IJRL        9
#define ICALL       10
#define IRST        11
#define IIN         12
#define IOUT        13
#define IEXCH       14
#define IINTMODE    15
#define ILOAD       16
#define IINW        17
#define IOUTW       18
#define ITSTI       19
#define IMULDIV     20
#define IADDW       21
#define IEI         22
#define ILDCTL      23
#define ILDA        24
#define INEG        25
#define ILDUD       26
#define IEXTS       27
#define IEND        0

/* arithmetic sub-types */
#define ADD     0
#define UNARY   1
#define CARRY   2

struct instruct {
	unsigned char type;
	char *mnem;
	unsigned char opcode;
	unsigned char arg;
};

struct instruct isr_table[] = {
	/* basic instructions */
	{ IBASIC, "nop", 0x00, 0 },
	{ IBASIC, "rlca", 0x07, 0 },
	{ IBASIC, "rrca", 0x0F, 0 },
	{ IBASIC, "rla", 0x17, 0 },
	{ IBASIC, "rra", 0x1F, 0 },
	{ IBASIC, "daa", 0x27, 0 },
	{ IBASIC, "cpl", 0x2F, 0 },
	{ IBASIC, "scf", 0x37, 0 },
	{ IBASIC, "ccf", 0x3F, 0 },
	{ IBASIC, "halt", 0x76, 0 },
	{ IBASIC, "exx", 0xD9, 0 },
	/* di/ei take an optional Z280 interrupt mask: opcode = plain,
	 * arg = the ED second byte for the masked form */
	{ IEI, "di", 0xF3, 0x7E },
	{ IEI, "ei", 0xFB, 0x7F },

	/* extended basic instructions */
	{ INEG, "neg", 0x44, 0xED },
	{ IBASIC_EXT, "retn", 0x45, 0xED },
	{ IBASIC_EXT, "reti", 0x4D, 0xED },
	{ IBASIC_EXT, "retil", 0x55, 0xED },
	{ IBASIC_EXT, "pcache", 0x65, 0xED },
	{ IBASIC_EXT, "rrd", 0x67, 0xED },
	{ IBASIC_EXT, "rld", 0x6F, 0xED },
	{ IBASIC_EXT, "ldi", 0xA0, 0xED },
	{ IBASIC_EXT, "cpi", 0xA1, 0xED },
	{ IBASIC_EXT, "ini", 0xA2, 0xED },
	{ IBASIC_EXT, "outi", 0xA3, 0xED },
	{ IBASIC_EXT, "ldd", 0xA8, 0xED },
	{ IBASIC_EXT, "cpd", 0xA9, 0xED },
	{ IBASIC_EXT, "ind", 0xAA, 0xED },
	{ IBASIC_EXT, "outd", 0xAB, 0xED },
	{ IBASIC_EXT, "ldir", 0xB0, 0xED },
	{ IBASIC_EXT, "cpir", 0xB1, 0xED },
	{ IBASIC_EXT, "inir", 0xB2, 0xED },
	{ IBASIC_EXT, "otir", 0xB3, 0xED },
	{ IBASIC_EXT, "lddr", 0xB8, 0xED },
	{ IBASIC_EXT, "cpdr", 0xB9, 0xED },
	{ IBASIC_EXT, "indr", 0xBA, 0xED },
	{ IBASIC_EXT, "otdr", 0xBB, 0xED },
	{ IBASIC_EXT, "iniw", 0x82, 0xED },
	{ IBASIC_EXT, "outiw", 0x83, 0xED },
	{ IBASIC_EXT, "indw", 0x8A, 0xED },
	{ IBASIC_EXT, "outdw", 0x8B, 0xED },
	{ IBASIC_EXT, "inirw", 0x92, 0xED },
	{ IBASIC_EXT, "otirw", 0x93, 0xED },
	{ IBASIC_EXT, "indrw", 0x9A, 0xED },
	{ IBASIC_EXT, "otdrw", 0x9B, 0xED },
	
	/* arithmetic */
	{ IARITH, "add", 0x80, ADD },
	{ IARITH, "adc", 0x88, CARRY },
	{ IARITH, "sub", 0x90, UNARY },
	{ IARITH, "sbc", 0x98, CARRY },
	{ IARITH, "and", 0xA0, UNARY },
	{ IARITH, "xor", 0xA8, UNARY },
	{ IARITH, "or", 0xB0, UNARY },
	{ IARITH, "cp", 0xB8, UNARY },
	
	/* inc / dec */
	{ IINCR, "inc", 0x04, 0x03 },
	{ IINCR, "dec", 0x05, 0x0B },
	
	/* bit / shift */
	{ IBITSH, "rlc", 0x00, 0 },
	{ IBITSH, "rrc", 0x08, 0 },
	{ IBITSH, "rl", 0x10, 0 },
	{ IBITSH, "rr", 0x18, 0 },
	{ IBITSH, "sla", 0x20, 0 },
	{ IBITSH, "sra", 0x28, 0 },
	{ IBITSH, "sll", 0x30, 0 },
	{ IBITSH, "srl", 0x38, 0 },
	{ IBITSH, "bit", 0x40, 1 },
	{ IBITSH, "res", 0x80, 1 },
	{ IBITSH, "set", 0xC0, 1 },
	
	/* stack ops */
	{ ISTACK, "pop", 0xC1, 0 },
	{ ISTACK, "push", 0xC5, 0 },
	
	/* return */
	{ IRET, "ret", 0xC0, 0xC9 },
	
	/* jump */
	{ IJMP, "jp", 0xC2, 0xE9 },
	
	/* jump relative */
	{ IJRL, "jr", 0x18, 1 },
	{ IJRL, "djnz", 0x10, 0},
	
	/* call */
	{ ICALL, "call", 0xC4, 0xCD },
	
	/* rst */
	{ IRST, "rst", 0xC7, 0 },
	
	/* in */
	{ IIN, "in", 0xDB, 0x40 },
	
	/* out */
	{ IOUT, "out", 0xD3, 0x41 },
	
	/* exchange */
	{ IEXCH, "ex", 0xE3, 0x08 },
	
	/* interrupt mode */
	{ IINTMODE, "im", 0x46, 0x5E },
	
	/* load instructions */
	{ ILOAD, "ld", 0x00, 0x00 },

	/* Z280 word input/output */
	{ IINW, "inw", 0xB7, 0 },
	{ IOUTW, "outw", 0xBF, 0 },
	{ ITSTI, "tsti", 0x70, 0 },

	/* Z280 multiply/divide: base opcode; arg says which optional
	 * prefix - 0 for "A," (multiply), 1 for "HL," (divide) */
	{ IMULDIV, "mult", 0xC0, 0 },
	{ IMULDIV, "multu", 0xC1, 0 },
	{ IMULDIV, "div", 0xC4, 1 },
	{ IMULDIV, "divu", 0xC5, 1 },

	/* Z280 16-bit arithmetic: base opcode, destination is HL */
	{ IADDW, "addw", 0xC6, 0 },
	{ IADDW, "subw", 0xCE, 0 },
	{ IADDW, "cpw", 0xC7, 0 },

	/* Z280 32-bit multiply/divide: same encoding family, result DEHL */
	{ IADDW, "multw", 0xC2, 0 },
	{ IADDW, "multuw", 0xC3, 0 },
	{ IADDW, "divw", 0xCA, 0 },
	{ IADDW, "divuw", 0xCB, 0 },

	/* Z280 user-space load; arg distinguishes ldud from ldup */
	{ ILDUD, "ldud", 0, 0 },
	{ ILDUD, "ldup", 0, 1 },

	/* Z280 sign extend */
	{ IEXTS, "exts", 0, 0 },

	/* Z280 test-and-set (the manual's name for the SLL slot) */
	{ IBITSH, "tset", 0x30, 0 },

	/* Z280 load control register */
	{ ILDCTL, "ldctl", 0, 0 },

	/* Z280 load effective address: dst = HL, IX or IY */
	{ ILDA, "lda", 0, 0 },

	{ IEND, "", 0x00, 0x00}
};

/* Token definitions needed */
#define T_BIAS  0x80

#define T_B     (T_BIAS + 0)
#define T_C     (T_BIAS + 1)
#define T_D     (T_BIAS + 2)
#define T_E     (T_BIAS + 3)
#define T_H     (T_BIAS + 4)
#define T_L     (T_BIAS + 5)
#define T_HL_I  (T_BIAS + 6)
#define T_A     (T_BIAS + 7)

#define T_BC    (T_BIAS + 8)
#define T_DE    (T_BIAS + 9)
#define T_HL    (T_BIAS + 10)
#define T_SP    (T_BIAS + 11)
#define T_AF    (T_BIAS + 12)
#define T_IX    (T_BIAS + 13)
#define T_IY    (T_BIAS + 14)

#define T_NZ    (T_BIAS + 15)
#define T_Z     (T_BIAS + 16)
#define T_NC    (T_BIAS + 17)
#define T_CR    (T_BIAS + 18)
#define T_PO    (T_BIAS + 19)
#define T_PE    (T_BIAS + 20)
#define T_P     (T_BIAS + 21)
#define T_M     (T_BIAS + 22)

#define T_IXH   (T_BIAS + 23)
#define T_IXL   (T_BIAS + 24)
#define T_IX_D  (T_BIAS + 25)
#define T_IYH   (T_BIAS + 26)
#define T_IYL   (T_BIAS + 27)
#define T_IY_D  (T_BIAS + 28)

#define T_PLAIN (T_BIAS + 29)
#define T_INDIR (T_BIAS + 30)

#define T_SP_I  (T_BIAS + 31)
#define T_BC_I  (T_BIAS + 32)
#define T_DE_I  (T_BIAS + 33)
#define T_IX_I  (T_BIAS + 34)
#define T_IY_I  (T_BIAS + 35)

#define T_I     (T_BIAS + 40)
#define T_R     (T_BIAS + 41)

/* External functions from asm.c */
extern void need();
extern unsigned char operand();
extern void emitbyte();
extern void emit_exp();
extern void emit_imm();
extern unsigned char tok2reg();
extern char match();
extern void gripe();
extern unsigned short cur_address;
extern char pass;
extern char no_relax;
extern void add_jump();
extern int is_relaxed();   /* the jump add_jump has just counted */
extern unsigned char peekchar();
extern unsigned char skipwhite();

#define T_NUM   (T_BIAS + 43)
#define T_C_I   (T_BIAS + 36)
#define T_SP_D  (T_BIAS + 44)	/* Z280 (SP + dd): stack relative */
#define T_PC_D  (T_BIAS + 45)	/* Z280 (PC + expr): PC relative (RA) */
#define T_HL_IX (T_BIAS + 46)	/* Z280 (HL + IX): base index */
#define T_HL_IY (T_BIAS + 47)	/* Z280 (HL + IY): base index */
#define T_IX_IY (T_BIAS + 48)	/* Z280 (IX + IY): base index */
#define T_HL_D  (T_BIAS + 49)	/* Z280 (HL + dd): indexed, 16-bit */
#define T_USP   (T_BIAS + 50)	/* Z280 user stack pointer */
#define T_DEHL  (T_BIAS + 51)	/* Z280 DE:HL pair (divw/divuw) */

/*
 * store indirect
 * load indirect
 */
int
do_stax(vp)
struct expval *vp;
{
	unsigned char arg;
	struct expval value;

	need(',');
	arg = operand(&value);

	switch (arg) {
	case T_HL:					/* ld (nn), hl */
		emitbyte(0x22);
		break;

	case T_A:					/* ld (nn), a */
		emitbyte(0x32);
		break;

	case T_IX:					/* ld (nn), ix */
		emitbyte(0xDD);
		emitbyte(0x22);
		break;

	case T_IY:					/* ld (nn), iy */
		emitbyte(0xFD);
		emitbyte(0x22);
		break;

	case T_BC:					/* ld (nn), bc */
	case T_DE:					/* ld (nn), de */
	case T_SP:					/* ld (nn), sp */
		emitbyte(0xED);
		emitbyte(0x43 + ((arg - T_BC) << 4));
		break;

	case T_PLAIN:				/* ld (nn), n = DD 3E nn16 n8 */
		emitbyte(0xDD);
		emitbyte(0x3E);
		emit_exp(2, vp);
		emitbyte(value.num.b);
		return 0;

	default:
		return 1;
	}
	emit_exp(2, vp);
	return 0;
}

/*
 * 16 bit load
 */
int
do_16i(reg)
unsigned char reg;
{
	unsigned char arg;
	struct expval value;

	/*
	 * correct for ix,iy into hl
	 */
	if (reg == T_IX) {
		emitbyte(0xDD);
		reg = T_HL;
	} else if (reg == T_IY) {
		emitbyte(0xFD);
		reg = T_HL;
	}

	/*
	 * grab a direct or deferred word
	 */
	need(',');
	arg = operand(&value);

	if (arg == T_PLAIN) {
		/*
		 * ld bc|de|hl|sp, nn
		 */
		emitbyte(0x01 + ((reg - T_BC) << 4));
		emit_exp(2, &value);
	} else if (arg == T_INDIR) {
		if (reg == T_HL) {
			emitbyte(0x2A);
		} else {
			/*
			 * ld bc|de|sp, (nn)
			 */
			emitbyte(0xED);
			emitbyte(0x4B + ((reg - T_BC) << 4));
		}
		emit_exp(2, &value);
	} else if (arg == T_HL_I) {
		/* ld rr,(hl): ED 06/16/26/36 */
		emitbyte(0xED);
		emitbyte(0x06 + ((reg - T_BC) << 4));
	} else if (arg == T_IX_D || arg == T_IY_D) {
		short disp = (short)value.num.w;
		if (disp >= -128 && disp <= 127) {
			/* SX: DD/FD ED 06/16/26/36 d8 */
			emitbyte(arg == T_IX_D ? 0xDD : 0xFD);
			emitbyte(0xED);
			emitbyte(0x06 + ((reg - T_BC) << 4));
			emitbyte(disp & 0xff);
		} else if (reg == T_HL) {
			/* X: ED 2C/34 dd16 (HL/IX/IY only) */
			emitbyte(0xED);
			emitbyte(arg == T_IX_D ? 0x2C : 0x34);
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		} else
			return 1;
	} else if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY) {
		/* BX: ED 0C/14/1C (HL/IX/IY only) */
		if (reg != T_HL)
			return 1;
		emitbyte(0xED);
		emitbyte(arg == T_HL_IX ? 0x0C : arg == T_HL_IY ? 0x14 : 0x1C);
	} else if (arg == T_PC_D) {
		/* RA: ED 24 dd16 (HL/IX/IY only) */
		if (reg != T_HL)
			return 1;
		emitbyte(0xED);
		emitbyte(0x24);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_SP_D && reg == T_HL) {
		/*
		 * Z280 SR mode: ld hl|ix|iy,(sp+dd) = [DD/FD] ED 04 dd16.
		 * Only HL (and its IX/IY forms) has a word load; BC/DE do
		 * not, so reg==T_HL is the gate.
		 */
		emitbyte(0xED);
		emitbyte(0x04);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_HL_D && reg == T_HL) {
		/* Z280 X mode with HL as base: ld hl,(hl+dd) = ED 3C dd16 */
		emitbyte(0xED);
		emitbyte(0x3C);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (reg == T_SP) {
		/*
		 * ld sp,hl|ix|iy specials
		 */
		switch (arg) {
		case T_HL:
			emitbyte(0xF9);
			break;
		case T_IX:
			emitbyte(0xDD);
			emitbyte(0xF9);
			break;
		case T_IY:
			emitbyte(0xFD);
			emitbyte(0xF9);
			break;
		default:
			return 1;
		}
	} else
		return 1;
	return 0;
}

/*
 * if there is a passed in expval, it's a displacement for the first arg
 * cases:
 * ld a|b|c|d|e|h|l|(hl)|(ix+d)|(iy+d), a|b|c|d|e|h|l|(hl)|(ix+d)|(iy+d)
 * ld a,(bc)|(de)|(nnnn)|i|r
 */
int
do_ldr8(arg, disp)
unsigned char arg;
struct expval *disp;
{
	unsigned char reg;
    struct expval value;
	struct expval *disp_ptr;
	unsigned char arg_reg, reg_reg;
    value.sym = 0;
    disp->sym = 0;

	disp_ptr = 0;

	if (arg == T_IX_D || arg == T_IY_D) {
        disp_ptr = disp;
	}
	need(',');

	reg = operand(&value);

	/* Z280 word store through HL: ld (hl),bc/de/hl/sp = ED 0E/1E/2E/3E */
	if (arg == T_HL_I && reg >= T_BC && reg <= T_SP) {
		emitbyte(0xED);
		emitbyte(0x0E + ((reg - T_BC) << 4));
		return 0;
	}

	/* Z280 word store to BX/X/RA/SR: ld (hl+ix)/...,hl/ix/iy */
	if ((arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY ||
	     arg == T_HL_D || arg == T_PC_D || arg == T_SP_D) &&
	    (reg == T_HL || reg == T_IX || reg == T_IY)) {
		if (reg == T_IX)
			emitbyte(0xDD);
		else if (reg == T_IY)
			emitbyte(0xFD);
		emitbyte(0xED);
		switch (arg) {
		case T_HL_IX: emitbyte(0x0D); return 0;
		case T_HL_IY: emitbyte(0x15); return 0;
		case T_IX_IY: emitbyte(0x1D); return 0;
		case T_HL_D: emitbyte(0x3D); break;
		case T_PC_D: emitbyte(0x25); break;
		case T_SP_D: emitbyte(0x05); break;
		}
		emitbyte(disp->num.w & 0xff);
		emitbyte((disp->num.w >> 8) & 0xff);
		return 0;
	}

	/* Z280 word store to SX/X: ld (ix+d)/(iy+d),rr */
	if ((arg == T_IX_D || arg == T_IY_D) && reg >= T_BC && reg <= T_IY) {
		short d = (short)disp->num.w;
		if (d >= -128 && d <= 127) {
			/* SX: ld (ix+d8),bc/de/hl/sp = DD/FD ED 0E/1E/2E/3E d8 */
			if (reg < T_BC || reg > T_SP)
				return 1;
			emitbyte(arg == T_IX_D ? 0xDD : 0xFD);
			emitbyte(0xED);
			emitbyte(0x0E + ((reg - T_BC) << 4));
			emitbyte(d & 0xff);
		} else {
			/* X: ld (ix+dd16),hl/ix/iy = [DD/FD] ED 2D/35 dd16 */
			if (reg != T_HL && reg != T_IX && reg != T_IY)
				return 1;
			if (reg == T_IX)
				emitbyte(0xDD);
			else if (reg == T_IY)
				emitbyte(0xFD);
			emitbyte(0xED);
			emitbyte(arg == T_IX_D ? 0x2D : 0x35);
			emitbyte(disp->num.w & 0xff);
			emitbyte((disp->num.w >> 8) & 0xff);
		}
		return 0;
	}

	/* Z280 X-mode byte store: ld (ix+dd16)/(iy+dd16),a or n */
	if (arg == T_IX_D || arg == T_IY_D) {
		short d = (short)disp->num.w;
		if (d < -128 || d > 127) {
			if (reg == T_A) {
				emitbyte(0xED);
				emitbyte(arg == T_IX_D ? 0x2B : 0x33);
				emitbyte(disp->num.w & 0xff);
				emitbyte((disp->num.w >> 8) & 0xff);
				return 0;
			}
			if (reg == T_PLAIN) {
				emitbyte(0xFD);
				emitbyte(arg == T_IX_D ? 0x0E : 0x16);
				emitbyte(disp->num.w & 0xff);
				emitbyte((disp->num.w >> 8) & 0xff);
				emitbyte(value.num.b);
				return 0;
			}
		}
	}

	/* Z280 byte store to BX/X/RA/DA: ld (hl+ix)/...,a or n */
	if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY ||
	    arg == T_HL_D || arg == T_PC_D || arg == T_SP_D || arg == T_INDIR) {
		unsigned char op = 0;
		switch (arg) {
		case T_HL_IX: op = 0x0B; break;      /* ED 0B */
		case T_HL_IY: op = 0x13; break;
		case T_IX_IY: op = 0x1B; break;
		case T_HL_D: op = 0x3B; break;
		case T_PC_D: op = 0x23; break;
		case T_SP_D: op = 0x03; break;
		case T_INDIR: op = 0; break;          /* DA: DD 3E addr16 */
		default: return 1;
		}
		if (arg == T_INDIR) {
			if (reg != T_PLAIN)
				return 1;
			emitbyte(0xDD);
			emitbyte(0x3E);
			emit_exp(2, &value);
			emitbyte(value.num.b);
			return 0;
		}
		if (reg == T_A) {
			emitbyte(0xED);
			emitbyte(op);
			if (arg != T_HL_IX && arg != T_HL_IY && arg != T_IX_IY) {
				emitbyte(disp->num.w & 0xff);
				emitbyte((disp->num.w >> 8) & 0xff);
			}
			return 0;
		}
		if (reg == T_PLAIN) {
			if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY) {
				/* immediate store to BX: DD 0E/16/1E n */
				emitbyte(0xDD);
				emitbyte(arg == T_HL_IX ? 0x0E : arg == T_HL_IY ? 0x16 : 0x1E);
				emitbyte(value.num.b);
			} else {
				/* immediate store to X/RA/SR: FD/DD 0E..06 dd16 n */
				emitbyte(arg == T_SP_D ? 0xDD : 0xFD);
				emitbyte(arg == T_HL_D ? 0x1E : 0x06);
				emitbyte(disp->num.w & 0xff);
				emitbyte((disp->num.w >> 8) & 0xff);
				emitbyte(value.num.b);
			}
			return 0;
		}
		return 1;
	}

	/* Z280 byte load from BX/X/RA: ld a,(hl+ix)/... */
	if (arg == T_A && (reg == T_HL_IX || reg == T_HL_IY || reg == T_IX_IY ||
	    reg == T_HL_D || reg == T_IX_D || reg == T_IY_D || reg == T_PC_D)) {
		unsigned char op = 0;
		short has_disp = 0;
		switch (reg) {
		case T_HL_IX: op = 0x79; emitbyte(0xDD); break;  /* DD 79 */
		case T_HL_IY: op = 0x7A; emitbyte(0xDD); break;
		case T_IX_IY: op = 0x7B; emitbyte(0xDD); break;
		case T_HL_D: op = 0x7B; has_disp = 1; emitbyte(0xFD); break;
		case T_IX_D:
		case T_IY_D: {
			short d = (short)value.num.w;
			if (d >= -128 && d <= 127) {
				/* SX: DD/FD 7E d8 */
				emitbyte(reg == T_IX_D ? 0xDD : 0xFD);
				emitbyte(0x7E);
				emitbyte(d & 0xff);
				return 0;
			}
			/* X: FD 79/7A dd16 */
			emitbyte(0xFD);
			emitbyte(reg == T_IX_D ? 0x79 : 0x7A);
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
			return 0;
		}
		case T_PC_D: op = 0x78; has_disp = 1; emitbyte(0xFD); break;
		default: return 1;
		}
		emitbyte(op);
		if (has_disp) {
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		}
		return 0;
	}

	/* Z280 SR mode: only the accumulator has a byte form.
	 * ld a,(sp+dd) = DD 78 dd16 */
	if (reg == T_SP_D) {
		if (arg != T_A)
			return 1;
		emitbyte(0xDD);
		emitbyte(0x78);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
		return 0;
	}

	if (arg >= T_IXH && arg <= T_IY_D) {
		if (arg <= T_IX_D) {
			emitbyte(0xDD);
            /* lose on ld ix*, iy* or ld ix[hl], (ix+d) */
            if (reg >= T_IYH && reg <= T_IY_D)
                return 1;
            if (arg != T_IX_D && reg == T_IX_D)
                return 1;
		} else {
			emitbyte(0xFD);
            /* lose on ld iy*, ix* or ld iy[hl], (iy+d) */
            if (reg >= T_IXH && reg <= T_IX_D)
                return 1;
            if (arg != T_IY_D && reg == T_IY_D)
                return 1;
		}
	} else if (reg >= T_IXH && reg <= T_IY_D) {
		if (arg == T_HL_I)
			return 1;

		if (reg <= T_IX_D) {
			emitbyte(0xDD);
		} else {
			emitbyte(0xFD);
		}
		if (reg == T_IX_D || reg == T_IY_D) {
            disp_ptr = &value;
		} else if (tok2reg(arg) == 4 || tok2reg(arg) == 5)
            /* lose on ld [hl], ix[hl] */
			return 1;
	}

	/*
	 * no (hl),(hl)
	 */
	if (arg == T_HL_I && reg == T_HL_I)
		return 1;

	/* Convert tokens to register numbers for opcode calculation */
	arg_reg = tok2reg(arg);
	reg_reg = tok2reg(reg);

	if (arg_reg <= 7 && reg_reg <= 7) {
		/* reg8->reg8 */
		emitbyte(0x40 + (arg_reg << 3) + reg_reg);
		if (disp_ptr)
			emit_imm(disp_ptr);
	} else if (arg_reg <= 7 && (reg == T_PLAIN)) {
		/* ld reg8, n */
		emitbyte(0x06 + (arg_reg << 3));
		if (disp_ptr)
			emit_imm(disp_ptr);
		emit_imm(&value);
	} else if (arg == T_A) {
		/*
		 * special a loads
		 */
		switch (reg) {
		case T_BC_I:
			emitbyte(0x0A);
			break;

		case T_DE_I:
			emitbyte(0x1A);
			break;

		case T_INDIR:
			emitbyte(0x3A);
			emit_exp(2, &value);
			break;

		case T_I:
			emitbyte(0xED);
			emitbyte(0x57);
			break;

		case T_R:
			emitbyte(0xED);
			emitbyte(0x5F);
			break;

		default:
			return 1;
		}
	} else
		return 1;
	return 0;
}

static char
do_basic(isr)
struct instruct *isr;
{
	emitbyte(isr->opcode);
	return 0;
}

static char
do_basic_ext(isr)
struct instruct *isr;
{
	emitbyte(isr->arg);
	emitbyte(isr->opcode);
	return 0;
}

/* neg a (bare "neg") or neg hl ("neg hl") - the operand is optional */
static char
do_neg(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	emitbyte(0xED);
	if (arg == 255)
		emitbyte(isr->opcode);
	else if (arg == T_HL)
		emitbyte(0x4C);
	else
		return 1;
	return 0;
}

static char
do_arith(isr)
struct instruct *isr;
{
	unsigned char prim = 0, arg, reg;
	struct expval value;

	arg = operand(&value);

	if (isr->arg == CARRY) {
		if (arg == T_HL || arg == T_IX || arg == T_IY) {
			prim = 1;
			reg = arg;
		} else if (arg != T_A)
			return 1;

		need(',');
		arg = operand(&value);
	} else if (isr->arg == ADD) {
		if (arg == T_HL) {
			prim = 2;
		} else if (arg == T_IX || arg == T_IY) {
			prim = 3;
			reg = arg;
		} else if (arg != T_A)
			return 1;

		need(',');
		arg = operand(&value);

		if (prim == 3 && arg == T_HL)
			return 1;

		if (prim == 3 && arg == reg)
			arg = T_HL;
	} else if (skipwhite() == ',') {
		/*
		 * sub, and, xor, or and cp take one operand - the
		 * accumulator is implicit in Z80 syntax.  "cp a,32" is
		 * the 8080-ish spelling and is not accepted, but it used
		 * to be read as "cp a" with the 32 dropped in silence,
		 * which assembles cleanly and compares the accumulator
		 * with itself.  Two of those were sitting in this tree:
		 * one made every character look like a space to the CP/M
		 * argv parser, the other took the bound off perror's
		 * error-table lookup.  Say so instead.
		 */
		return 1;
	}

	if (prim == 0) {
		if (arg <= T_A) {
			emitbyte(isr->opcode + (arg - T_B));
		} else if (arg == T_IXH || arg == T_IXL || arg == T_IYH || arg == T_IYL) {
			emitbyte(arg == T_IXH || arg == T_IXL ? 0xDD : 0xFD);
			emitbyte(isr->opcode + (arg == T_IXH || arg == T_IYH ? 4 : 5));
		} else if (arg == T_IX_D || arg == T_IY_D) {
			short disp = (short)value.num.w;
			if (disp >= -128 && disp <= 127) {
				/* SX: DD/FD (op+6) d8 */
				emitbyte(arg == T_IX_D ? 0xDD : 0xFD);
				emitbyte(isr->opcode + 6);
				emitbyte(disp & 0xff);
			} else {
				/* X: FD (op+1/2) dd16 */
				emitbyte(0xFD);
				emitbyte(isr->opcode + (arg == T_IX_D ? 1 : 2));
				emitbyte(value.num.w & 0xff);
				emitbyte((value.num.w >> 8) & 0xff);
			}
		} else if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY) {
			/* BX: DD (op+1/2/3) */
			emitbyte(0xDD);
			emitbyte(isr->opcode + (arg == T_HL_IX ? 1 : arg == T_HL_IY ? 2 : 3));
		} else if (arg == T_HL_D) {
			/* X mode with HL base: FD (op+3) dd16 */
			emitbyte(0xFD);
			emitbyte(isr->opcode + 3);
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		} else if (arg == T_INDIR) {
			/* DA: DD (op+7) addr16 */
			emitbyte(0xDD);
			emitbyte(isr->opcode + 7);
			emit_exp(2, &value);
		} else if (arg == T_PC_D) {
			/* RA: FD (op+0) dd16 */
			emitbyte(0xFD);
			emitbyte(isr->opcode);
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		} else if (arg == T_SP_D) {
			/* SR: DD (op+0) dd16 */
			emitbyte(0xDD);
			emitbyte(isr->opcode);
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		} else if (arg == T_PLAIN) {
			emitbyte(isr->opcode + 0x46);
			emitbyte(value.num.b);
		} else
			return 1;
	} else if (prim == 1) {
		if (arg == reg)
			arg = T_HL;   /* adc ix,ix is adc ix,hl in the field */
		if (arg >= T_BC && arg <= T_SP) {
			if (reg == T_IX)
				emitbyte(0xDD);
			else if (reg == T_IY)
				emitbyte(0xFD);
			emitbyte(0xED);
			emitbyte((0x42 + (isr->opcode == 0x88 ? 8 : 0)) +
					 ((arg - 8) << 4));
		} else
			return 1;
	} else if (prim == 2) {
		if (arg == T_A) {
			/* add hl,a = ED 6D */
			emitbyte(0xED);
			emitbyte(0x6D);
		} else if (arg >= T_BC && arg <= T_SP) {
			emitbyte(0x09 + ((arg - 8) << 4));
		} else
			return 1;
	} else if (prim == 3) {
		if (arg == T_HL)
			arg = reg;
		if (arg == reg)
			arg = T_HL;

		if (reg == T_IX)
			emitbyte(0xDD);
		else
			emitbyte(0xFD);

		if (arg == T_A) {
			/* add ix/iy,a = DD/FD ED 6D */
			emitbyte(0xED);
			emitbyte(0x6D);
		} else if (arg >= T_BC && arg <= T_SP) {
			emitbyte(0x09 + ((arg - 8) << 4));
		} else
			return 1;
	}
	return 0;
}

static char
do_incr(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg <= T_A) {
		emitbyte(isr->opcode + (arg << 3));
	} else if (arg <= T_SP) {
		emitbyte(isr->arg + ((arg - T_BC) << 4));
	} else if (arg == T_IX) {
		emitbyte(0xDD);
		emitbyte(isr->arg + 0x20);
	} else if (arg == T_IY) {
		emitbyte(0xFD);
		emitbyte(isr->arg + 0x20);
	} else if (arg == T_IXH || arg == T_IXL || arg == T_IYH || arg == T_IYL) {
		emitbyte(arg == T_IXH || arg == T_IXL ? 0xDD : 0xFD);
		emitbyte(isr->opcode + ((arg == T_IXH || arg == T_IYH ? 4 : 5) << 3));
	} else if (arg == T_IX_D || arg == T_IY_D) {
		short disp = (short)value.num.w;
		if (disp >= -128 && disp <= 127) {
			/* SX: DD/FD (op + 6<<3) d8 */
			emitbyte(arg == T_IX_D ? 0xDD : 0xFD);
			emitbyte(isr->opcode + (6 << 3));
			emitbyte(disp & 0xff);
		} else {
			/* X: FD (op + 1/2<<3) dd16 */
			emitbyte(0xFD);
			emitbyte(isr->opcode + ((arg == T_IX_D ? 1 : 2) << 3));
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		}
	} else if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY) {
		/* BX: DD (op + 1/2/3<<3) */
		emitbyte(0xDD);
		emitbyte(isr->opcode + ((arg == T_HL_IX ? 1 : arg == T_HL_IY ? 2 : 3) << 3));
	} else if (arg == T_HL_D) {
		/* X mode with HL base: FD (op + 3<<3) dd16 */
		emitbyte(0xFD);
		emitbyte(isr->opcode + (3 << 3));
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_INDIR) {
		/* DA: DD (op + 7<<3) addr16 */
		emitbyte(0xDD);
		emitbyte(isr->opcode + (7 << 3));
		emit_exp(2, &value);
	} else if (arg == T_PC_D) {
		/* RA: FD op dd16 */
		emitbyte(0xFD);
		emitbyte(isr->opcode);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_SP_D) {
		/* SR: DD op dd16 */
		emitbyte(0xDD);
		emitbyte(isr->opcode);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else
		return 1;
	return 0;
}

static char
do_bitsh(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	reg = 0;
	if (isr->arg) {
		if (arg != T_PLAIN || value.sym)
			return 1;

		if (value.num.b > 7)
			return 1;

		reg = value.num.b;

		need(',');
		arg = operand(&value);
	}

	if (arg == T_IX_D || arg == T_IY_D) {

		if (arg == T_IX_D)
			emitbyte(0xDD);
		else
			emitbyte(0xFD);

		emitbyte(0xCB);

		emitbyte(value.num.b);

		arg = T_HL_I;
		if (peekchar() == ',') {
			need(',');
			arg = operand(&value);

			if (arg == 6)
				arg = 8;
		}
	} else
		emitbyte(0xCB);

	/* Convert register token to register code (0-7) */
	if (arg >= T_B && arg <= T_A)
		arg -= T_B;
	else if (arg == T_HL_I)
		arg = 6;
	else if (arg > 7)
		return 1;

	emitbyte(isr->opcode + arg + (reg << 3));
	return 0;
}

static char
do_stack(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg == T_AF)
		arg = T_SP;

	if (arg >= T_BC && arg <= T_SP) {
		emitbyte(isr->opcode + ((arg - T_BC) << 4));
	} else if (arg == T_IX) {
		emitbyte(0xDD);
		emitbyte(isr->opcode + 0x20);
	} else if (arg == T_IY) {
		emitbyte(0xFD);
		emitbyte(isr->opcode + 0x20);
	} else if (arg == T_PLAIN) {
		/* IM: push nn = FD <base+0x30> <imm16> */
		emitbyte(0xFD);
		emitbyte(isr->opcode + 0x30);
		emit_exp(2, &value);
	} else if (arg == T_INDIR) {
		/* DA: push (nn) = DD <base+0x10> <addr16> */
		emitbyte(0xDD);
		emitbyte(isr->opcode + 0x10);
		emit_exp(2, &value);
	} else if (arg == T_PC_D) {
		/* RA: push (pc+expr) = DD <base+0x30> <disp16> */
		int dist = value.num.w;
		emitbyte(0xDD);
		emitbyte(isr->opcode + 0x30);
		if (value.sym)
			dist = value.sym->value + value.num.w - (cur_address + 2);
		emitbyte(dist & 0xff);
		emitbyte((dist >> 8) & 0xff);
	} else if (arg == T_HL_I) {
		/* IR: push (hl) = DD <base> */
		emitbyte(0xDD);
		emitbyte(isr->opcode);
	} else
		return 1;
	return 0;
}

static char
do_ret(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_C) arg = T_CR;  /* 'c' means carry, not register C */
	if (arg >= T_NZ && arg <= T_M) {
		emitbyte(isr->opcode + ((arg - T_NZ) << 3));
	} else if (arg == 255) {
		emitbyte(isr->arg);
	} else
		return 1;
	return 0;
}

static char
do_jmp(isr)
struct instruct *isr;
{
	unsigned char arg, cond;
	struct expval value;
	unsigned short addr;
	int target, dist;

	arg = operand(&value);

	if (arg == T_C) arg = T_CR;
	if (arg >= T_NZ && arg <= T_M) {
		cond = arg;
		need(',');
		arg = operand(&value);

		/* indirect forms are fixed length, not relaxed */
		if (arg == T_HL_I) {
			emitbyte(0xDD);
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
			return 0;
		}
		if (arg == T_PC_D) {
			emitbyte(0xFD);
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
			return 0;
		}

		/* record jump for relaxation */
		addr = cur_address;
		add_jump(addr, value.sym, value.num.w, cond);

		/* check if relaxed to jr */
		if (is_relaxed()) {
			/* emit jr cc, offset */
			/* jr nz=20, z=28, nc=30, c=38 */
			emitbyte(0x20 + ((cond - T_NZ) << 3));
			/* calculate relative offset */
			if (value.sym)
				target = value.sym->value + value.num.w;
			else
				target = value.num.w;
			dist = target - (cur_address + 1);
			emitbyte(dist & 0xff);
		} else {
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
			emit_exp(2, &value);
		}
	} else if (arg == T_NUM || arg == T_PLAIN) {
		/* unconditional jp */
		addr = cur_address;
		add_jump(addr, value.sym, value.num.w, 0);

		if (is_relaxed()) {
			/* emit jr offset */
			emitbyte(0x18);
			if (value.sym)
				target = value.sym->value + value.num.w;
			else
				target = value.num.w;
			dist = target - (cur_address + 1);
			emitbyte(dist & 0xff);
		} else {
			emitbyte(isr->opcode + 1);
			emit_exp(2, &value);
		}
	} else if (arg == T_HL_I) {
		emitbyte(isr->arg);
	} else if (arg == T_IX_I) {
		emitbyte(0xDD);
		emitbyte(isr->arg);
	} else if (arg == T_IY_I) {
		emitbyte(0xFD);
		emitbyte(isr->arg);
	} else if (arg == T_PC_D) {
		/* jp (pc+dd) = FD C3 dd16 */
		emitbyte(0xFD);
		emitbyte(isr->opcode + 1);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else
		return 1;
	return 0;
}

static char
do_jrl(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;
	int target, dist;

	arg = operand(&value);

	reg = 0;
	if (isr->arg) {
		if (arg == T_C) arg = T_CR;  /* 'c' means carry, not register C */
		if (arg >= T_NZ && arg <= T_CR) {
			/* conditional jr: base opcode 0x20 + (cond * 8) */
			reg = 0x08 + ((arg - T_NZ) << 3);
			need(',');
			arg = operand(&value);
		} else if (arg != T_NUM && arg != T_PLAIN)
			return 1;
	}

	if (arg != T_PLAIN)
		return 1;

	emitbyte(isr->opcode + reg);
	/* compute PC-relative offset: target - (PC after 2-byte jr) */
	if (value.sym)
		target = value.sym->value + value.num.w;
	else
		target = value.num.w;
	dist = target - (cur_address + 1);
	if (pass == 1 && (dist < -128 || dist > 127))
		gripe("relative jump out of range");
	emitbyte(dist & 0xff);
	return 0;
}

static char
do_call(isr)
struct instruct *isr;
{
	unsigned char arg, cond;
	struct expval value;

	arg = operand(&value);

	if (arg == T_C) arg = T_CR;  /* 'c' means carry, not register C */
	if (arg == 1) arg = T_CR;
	if (arg >= T_NZ && arg <= T_M) {
		cond = arg;
		need(',');
		arg = operand(&value);
		if (arg == T_HL_I) {
			/* call cc,(hl) = DD (op + cc*8) */
			emitbyte(0xDD);
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
		} else if (arg == T_PC_D) {
			/* call cc,(pc+dd) = FD (op + cc*8) dd16 */
			emitbyte(0xFD);
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		} else {
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
			emit_exp(2, &value);
		}
	} else if (arg == T_HL_I) {
		/* call (hl) = DD CD */
		emitbyte(0xDD);
		emitbyte(isr->arg);
	} else if (arg == T_PC_D) {
		/* call (pc+dd) = FD CD dd16 */
		emitbyte(0xFD);
		emitbyte(isr->arg);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_PLAIN) {
		emitbyte(isr->arg);
		emit_exp(2, &value);
	} else
		return 1;
	return 0;
}

static char
do_rst(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg != T_PLAIN || value.num.b & 0x7 || value.num.b > 0x38)
		return 1;

	emitbyte(isr->opcode + value.num.b);
	return 0;
}

static char
do_in(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value, dval;

	arg = operand(&value);

	if (arg == T_C_I) {
		emitbyte(0xED);
		emitbyte(0x70);
		return 0;
	}

	if (arg == T_HL_I)
		return 1;

	/* memory-mode destination: in (ix+d),(c) etc. */
	if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY ||
	    arg == T_IX_D || arg == T_IY_D || arg == T_HL_D ||
	    arg == T_PC_D || arg == T_SP_D || arg == T_INDIR ||
	    arg == T_IXH || arg == T_IXL || arg == T_IYH || arg == T_IYL) {
		dval = value;
		need(',');
		if (operand(&value) != T_C_I)
			return 1;
		switch (arg) {
		case T_HL_IX: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x48); break;
		case T_HL_IY: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x50); break;
		case T_IX_IY: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x58); break;
		case T_IXH: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x60); break;
		case T_IXL: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x68); break;
		case T_IYH: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x60); break;
		case T_IYL: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x68); break;
		case T_HL_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x58);
			emitbyte(dval.num.w & 0xff); emitbyte((dval.num.w >> 8) & 0xff); break;
		case T_IX_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x48);
			emitbyte(dval.num.w & 0xff); emitbyte((dval.num.w >> 8) & 0xff); break;
		case T_IY_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x50);
			emitbyte(dval.num.w & 0xff); emitbyte((dval.num.w >> 8) & 0xff); break;
		case T_PC_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x40);
			emitbyte(dval.num.w & 0xff); emitbyte((dval.num.w >> 8) & 0xff); break;
		case T_SP_D: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x40);
			emitbyte(dval.num.w & 0xff); emitbyte((dval.num.w >> 8) & 0xff); break;
		case T_INDIR: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x78);
			emit_exp(2, &dval); break;
		default: return 1;
		}
		return 0;
	}

	if (arg > T_A)
		return 1;

	reg = arg;
	need(',');
	arg = operand(&value);

	if (reg == T_A && arg == T_INDIR) {
		emitbyte(isr->opcode);
		emitbyte(value.num.b);
	} else if (arg == T_C_I) {
		emitbyte(0xED);
		emitbyte(0x40 + (reg << 3));
	} else
		return 1;
	return 0;
}

static char
do_out(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_INDIR) {
		reg = value.num.b;
		need(',');
		arg = operand(&value);

		if (arg != T_A)
			return 1;

		emitbyte(isr->opcode);
		emitbyte(reg);
	} else if (arg == T_C_I) {
		need(',');
		arg = operand(&value);

		if (arg == T_HL_I)
			return 1;
		if (arg == T_PLAIN && !value.num.w)
			arg = T_HL_I;

		/* memory-mode source: out (c),(ix+d) = [DD/FD] ED (41+code*8) */
		if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY ||
		    arg == T_IX_D || arg == T_IY_D || arg == T_HL_D ||
		    arg == T_PC_D || arg == T_SP_D || arg == T_INDIR ||
		    arg == T_IXH || arg == T_IXL || arg == T_IYH || arg == T_IYL) {
			switch (arg) {
			case T_HL_IX: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x49); break;
			case T_HL_IY: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x51); break;
			case T_IX_IY: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x59); break;
			case T_IXH: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x61); break;
			case T_IXL: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x69); break;
			case T_IYH: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x61); break;
			case T_IYL: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x69); break;
			case T_HL_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x59);
				emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
			case T_IX_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x49);
				emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
			case T_IY_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x51);
				emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
			case T_PC_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x41);
				emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
			case T_SP_D: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x41);
				emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
			case T_INDIR: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x79);
				emit_exp(2, &value); break;
			default: return 1;
			}
			return 0;
		}

		if (arg > T_A)
			return 1;

		emitbyte(0xED);
		emitbyte(0x41 + (arg << 3));
	} else
		return 1;
	return 0;
}

/*
 * Z280 word input: INW HL,(C)
 */
static char
do_inw(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg != T_HL)
		return 1;
	need(',');
	arg = operand(&value);
	if (arg != T_C_I)
		return 1;
	emitbyte(0xED);
	emitbyte(isr->opcode);
	return 0;
}

/*
 * Z280 word output: OUTW (C),HL
 */
static char
do_outw(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg != T_C_I)
		return 1;
	need(',');
	arg = operand(&value);
	if (arg != T_HL)
		return 1;
	emitbyte(0xED);
	emitbyte(isr->opcode);
	return 0;
}

/*
 * Z280 test input: TSTI (C)
 */
static char
do_tsti(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg != T_C_I)
		return 1;
	emitbyte(0xED);
	emitbyte(isr->opcode);
	return 0;
}

/*
 * Z280 multiply/divide: MULT/MULTU A,r -> HL, DIV/DIVU HL,r -> HL.
 * The destination (A for multiply, HL for divide) may be spelled out,
 * followed by a comma; otherwise the source is the only operand.  The
 * source is an 8-bit register, whose field goes in the low three bits
 * of the second ED byte; (HL) is field 6.
 */
static char
do_muldiv(isr)
struct instruct *isr;
{
	unsigned char arg, want;
	struct expval value;

	arg = operand(&value);

	/* "MULT A,src" / "DIV HL,src" — the destination is spelled out */
	want = isr->arg ? T_HL : T_A;
	if (arg == want && peekchar() == ',') {
		need(',');
		arg = operand(&value);
	}

	if (arg >= T_B && arg <= T_A) {
		emitbyte(0xED);
		emitbyte(isr->opcode + ((arg - T_B) << 3));
	} else if (arg == T_SP_D) {
		/* SR mode: DD ED <base> <disp16> */
		emitbyte(0xDD);
		emitbyte(0xED);
		emitbyte(isr->opcode);
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_PC_D) {
		/* RA mode: FD ED <base> <disp16>.  A symbol is swizzled
		 * to sym - (next instruction); a number is the literal
		 * displacement.  The next instruction is two bytes past
		 * the FD ED <base> already emitted. */
		int dist = value.num.w;
		emitbyte(0xFD);
		emitbyte(0xED);
		emitbyte(isr->opcode);
		if (value.sym)
			dist = value.sym->value + value.num.w - (cur_address + 2);
		emitbyte(dist & 0xff);
		emitbyte((dist >> 8) & 0xff);
	} else if (arg == T_INDIR) {
		/* DA mode: DD ED <F8+delta> <addr16>, absolute with a
		 * relocation for a forward symbol */
		emitbyte(0xDD);
		emitbyte(0xED);
		emitbyte(0xF8 + (isr->opcode - 0xC0));
		emit_exp(2, &value);
	} else if (arg == T_HL_IX || arg == T_HL_IY || arg == T_IX_IY) {
		/* BX mode: DD ED <C8/D0/D8 + delta>, register + register */
		unsigned char op = arg == T_HL_IX ? 0xC8 :
		                  arg == T_HL_IY ? 0xD0 : 0xD8;
		emitbyte(0xDD);
		emitbyte(0xED);
		emitbyte(op + (isr->opcode - 0xC0));
	} else if (arg == T_IX_D || arg == T_IY_D) {
		/* SX (8-bit) when the displacement fits in a signed byte,
		 * else X (16-bit).  The displacement is a constant - the
		 * parser rejects forward symbols - so the choice is the same
		 * in both passes and no relaxation is needed. */
		short disp = (short)value.num.w;
		if (disp >= -128 && disp <= 127) {
			/* SX mode: <DD/FD> ED <F0/F1 + delta> <disp8> */
			if (arg == T_IX_D) {
				emitbyte(0xDD);
				emitbyte(0xED);
				emitbyte(0xF0 + (isr->opcode - 0xC0));
			} else {
				emitbyte(0xFD);
				emitbyte(0xED);
				emitbyte(0xF0 + (isr->opcode - 0xC0));
			}
			emitbyte(disp & 0xff);
		} else {
			/* X mode: FD ED <C8/D0 + delta> <disp16> */
			emitbyte(0xFD);
			emitbyte(0xED);
			emitbyte((arg == T_IX_D ? 0xC8 : 0xD0) +
			         (isr->opcode - 0xC0));
			emitbyte(value.num.w & 0xff);
			emitbyte((value.num.w >> 8) & 0xff);
		}
	} else if (arg == T_HL_D) {
		/* X mode with HL: FD ED <D8 + delta> <disp16> */
		emitbyte(0xFD);
		emitbyte(0xED);
		emitbyte(0xD8 + (isr->opcode - 0xC0));
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_IXH || arg == T_IXL || arg == T_IYH || arg == T_IYL) {
		/* RX mode: index half-register.  The field is the same 4/5
		 * as H/L, with a DD/FD prefix naming the index register. */
		unsigned char field = (arg == T_IXH || arg == T_IYH) ? 4 : 5;
		emitbyte((arg == T_IXH || arg == T_IXL) ? 0xDD : 0xFD);
		emitbyte(0xED);
		emitbyte(isr->opcode + (field << 3));
	} else if (arg == T_PLAIN) {
		/* IM mode: FD ED <F8 + delta> <imm8> (F8, same base as DA) */
		emitbyte(0xFD);
		emitbyte(0xED);
		emitbyte(0xF8 + (isr->opcode - 0xC0));
		emitbyte(value.num.w & 0xff);
	} else
		return 1;
	return 0;
}

/*
 * Z280 16-bit arithmetic: ADDW/SUBW/CPW.  The destination is always
 * HL and may be spelled out.  Register and (hl) forms only; the
 * memory modes (IM/DA/X/RA) take their own arms later.
 */
static char
do_addw(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	/* "ADDW HL,src" (or "DIVW DEHL,src") — the destination spelled out */
	if ((arg == T_HL || arg == T_DEHL) && peekchar() == ',') {
		need(',');
		arg = operand(&value);
	}

	if (arg >= T_BC && arg <= T_SP) {
		/* R RR: ED <base + rr*16>, rr = BC/DE/HL/SP */
		emitbyte(0xED);
		emitbyte(isr->opcode + ((arg - T_BC) << 4));
	} else if (arg == T_IX || arg == T_IY) {
		/* R XY: DD/FD ED <base + 0x20> */
		emitbyte(arg == T_IX ? 0xDD : 0xFD);
		emitbyte(0xED);
		emitbyte(isr->opcode + 0x20);
	} else if (arg == T_HL_I) {
		/* IR (HL): DD ED <base> */
		emitbyte(0xDD);
		emitbyte(0xED);
		emitbyte(isr->opcode);
	} else if (arg == T_PLAIN) {
		/* IM: FD ED <base + 0x30> <imm16> */
		emitbyte(0xFD);
		emitbyte(0xED);
		emitbyte(isr->opcode + 0x30);
		emit_exp(2, &value);
	} else if (arg == T_INDIR) {
		/* DA: DD ED <base + 0x10> <addr16> */
		emitbyte(0xDD);
		emitbyte(0xED);
		emitbyte(isr->opcode + 0x10);
		emit_exp(2, &value);
	} else if (arg == T_IX_D || arg == T_IY_D) {
		/* X: FD ED <base + 0x00/0x10> <disp16> (always 16-bit) */
		emitbyte(0xFD);
		emitbyte(0xED);
		emitbyte(isr->opcode + (arg == T_IX_D ? 0x00 : 0x10));
		emitbyte(value.num.w & 0xff);
		emitbyte((value.num.w >> 8) & 0xff);
	} else if (arg == T_PC_D) {
		/* RA: DD ED <base + 0x30> <disp16>, symbol swizzled */
		int dist = value.num.w;
		emitbyte(0xDD);
		emitbyte(0xED);
		emitbyte(isr->opcode + 0x30);
		if (value.sym)
			dist = value.sym->value + value.num.w - (cur_address + 2);
		emitbyte(dist & 0xff);
		emitbyte((dist >> 8) & 0xff);
	} else
		return 1;
	return 0;
}

/*
 * Z280 EI/DI with an optional interrupt mask.  A bare ei/di is the
 * Z80 form; with a mask it is ED 7F/7E <mask>.
 */
static char
do_ei(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg == 255) {
		/* no mask: plain ei/di */
		emitbyte(isr->opcode);
	} else if (arg == T_PLAIN) {
		emitbyte(0xED);
		emitbyte(isr->arg);
		emitbyte(value.num.w & 0xff);
	} else
		return 1;
	return 0;
}

/*
 * Z280 LDCTL dst,src: control-register transfer among (C), USP and
 * HL/IX/IY.  The register names the control register; the second ED
 * byte (6E/66/87/8F) distinguishes the direction.
 */
static char
do_ldctl(isr)
struct instruct *isr;
{
	unsigned char dst, src, reg, opcode;
	struct expval value;

	dst = operand(&value);
	need(',');
	src = operand(&value);

	if (dst == T_C_I && (src == T_HL || src == T_IX || src == T_IY)) {
		opcode = 0x6E; reg = src;
	} else if ((dst == T_HL || dst == T_IX || dst == T_IY) &&
	           src == T_C_I) {
		opcode = 0x66; reg = dst;
	} else if ((dst == T_HL || dst == T_IX || dst == T_IY) &&
	           src == T_USP) {
		opcode = 0x87; reg = dst;
	} else if (dst == T_USP &&
	           (src == T_HL || src == T_IX || src == T_IY)) {
		opcode = 0x8F; reg = src;
	} else
		return 1;

	if (reg == T_HL) {
		emitbyte(0xED);
		emitbyte(opcode);
	} else {
		emitbyte(reg == T_IX ? 0xDD : 0xFD);
		emitbyte(0xED);
		emitbyte(opcode);
	}
	return 0;
}

static char
do_exch(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	reg = operand(&value);
	need(',');
	arg = operand(&value);

	if (reg == T_AF) {
		if (arg == T_AF) {
			need('\'');
			emitbyte(isr->arg);
		} else
			return 1;
	}
	else if (reg == T_DE) {
		if (arg == T_HL)
			emitbyte(isr->opcode + 0x08);
		else
			return 1;
	}
	else if (reg == T_SP_I) {
		switch (arg) {
		case T_HL:
			break;
		case T_IX:
			emitbyte(0xDD);
			break;
		case T_IY:
			emitbyte(0xFD);
			break;
		default:
			return 1;
		}
		emitbyte(isr->opcode);
	}
	else if ((reg == T_IX || reg == T_IY) && arg == T_HL) {
		/* ex ix/iy,hl = DD/FD EB */
		emitbyte(reg == T_IX ? 0xDD : 0xFD);
		emitbyte(0xEB);
	}
	else if (reg == T_H && arg == T_L) {
		/* ex h,l = ED EF */
		emitbyte(0xED);
		emitbyte(0xEF);
	}
	else if (reg == T_A) {
		/* ex a,src : [DD/FD] ED (07 + code*8) [+ operand] */
		switch (arg) {
		case T_B: emitbyte(0xED); emitbyte(0x07); break;
		case T_C: emitbyte(0xED); emitbyte(0x0F); break;
		case T_D: emitbyte(0xED); emitbyte(0x17); break;
		case T_E: emitbyte(0xED); emitbyte(0x1F); break;
		case T_H: emitbyte(0xED); emitbyte(0x27); break;
		case T_L: emitbyte(0xED); emitbyte(0x2F); break;
		case T_A: emitbyte(0xED); emitbyte(0x3F); break;
		case T_HL_I: emitbyte(0xED); emitbyte(0x37); break;
		case T_IXH: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x27); break;
		case T_IXL: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x2F); break;
		case T_IYH: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x27); break;
		case T_IYL: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x2F); break;
		case T_HL_IX: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x0F); break;
		case T_HL_IY: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x17); break;
		case T_IX_IY: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x1F); break;
		case T_IX_D:
		case T_IY_D: {
			short disp = (short)value.num.w;
			if (disp >= -128 && disp <= 127) {
				emitbyte(arg == T_IX_D ? 0xDD : 0xFD);
				emitbyte(0xED); emitbyte(0x37);
				emitbyte(disp & 0xff);
			} else {
				emitbyte(0xFD); emitbyte(0xED);
				emitbyte(arg == T_IX_D ? 0x0F : 0x17);
				emitbyte(value.num.w & 0xff);
				emitbyte((value.num.w >> 8) & 0xff);
			}
			break;
		}
		case T_HL_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x1F);
			emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
		case T_PC_D: emitbyte(0xFD); emitbyte(0xED); emitbyte(0x07);
			emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
		case T_SP_D: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x07);
			emitbyte(value.num.w & 0xff); emitbyte((value.num.w >> 8) & 0xff); break;
		case T_INDIR: emitbyte(0xDD); emitbyte(0xED); emitbyte(0x3F);
			emit_exp(2, &value); break;
		default:
			return 1;
		}
	}
	else
		return 1;
	return 0;
}

static char
do_intmode(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg != T_PLAIN)
		return 1;

	emitbyte(0xED);
	switch (value.num.w) {
	case 0:
	case 1:
		emitbyte(isr->opcode + (value.num.b << 4));
		break;

	case 2:
		emitbyte(isr->arg);
		break;

	case 3:
		emitbyte(0x4E);
		break;

	default:
		return 1;
	}
	return 0;
}

static char
do_load(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	/* Z280 SR mode: (sp+dd) as the destination */
	if (arg == T_SP_D) {
		int disp = value.num.w;
		need(',');
		reg = operand(&value);
		if (reg == T_A) {
			/* ld (sp+dd),a = ED 03 dd16 */
			emitbyte(0xED);
			emitbyte(0x03);
		} else if (reg == T_HL) {
			/* ld (sp+dd),hl = ED 05 dd16 */
			emitbyte(0xED);
			emitbyte(0x05);
		} else if (reg == T_IX || reg == T_IY) {
			/* ld (sp+dd),ix/iy = DD/FD ED 05 dd16 */
			emitbyte(reg == T_IX ? 0xDD : 0xFD);
			emitbyte(0xED);
			emitbyte(0x05);
		} else if (reg == T_PLAIN) {
			/* ld (sp+dd),n = DD 06 dd16 n */
			emitbyte(0xDD);
			emitbyte(0x06);
			emitbyte(disp & 0xff);
			emitbyte((disp >> 8) & 0xff);
			emitbyte(value.num.w & 0xff);
			return 0;
		} else
			return 1;
		emitbyte(disp & 0xff);
		emitbyte((disp >> 8) & 0xff);
		return 0;
	}

	if (arg == T_INDIR) {
		return do_stax(&value);
	}

	if (arg <= T_A || (arg >= T_IXH && arg <= T_IY_D) ||
	    arg == T_PC_D || arg == T_HL_IX || arg == T_HL_IY ||
	    arg == T_IX_IY || arg == T_HL_D) {
		return do_ldr8(arg, &value);
	}

	if ((arg >= T_BC && arg <= T_SP) || (arg == T_IX || arg == T_IY)) {
		return do_16i(arg);
	}

	if (arg >= T_BC_I && arg <= T_R) {
		need(',');
		reg = operand(&value);
		if (reg != T_A)
			return 1;

		switch (arg) {
		case T_BC_I:
			emitbyte(0x02);
			break;

		case T_DE_I:
			emitbyte(0x12);
			break;

		case T_I:
			emitbyte(0xED);
			emitbyte(0x47);
			break;

		case T_R:
			emitbyte(0xED);
			emitbyte(0x4F);
			break;
		}
	} else
		return 1;
	return 0;
}

/*
 * Z280 load effective address: the address of the source operand lands
 * in HL, IX or IY.  The code generator reaches two modes - SR, the
 * frame-slot address, and X, the struct member - and DA is the absolute
 * one, whose encoding is the same 21 nn as ld hl,nn.
 */
static char
do_lda(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg == T_IX) {
		emitbyte(0xDD);
	} else if (arg == T_IY) {
		emitbyte(0xFD);
	} else if (arg != T_HL)
		return 1;

	need(',');
	arg = operand(&value);

	switch (arg) {
	case T_SP_D:			/* lda hl,(sp+dd) = ED 02 dd16 */
		emitbyte(0xED);
		emitbyte(0x02);
		break;
	case T_IX_D:			/* lda hl,(ix+dd) = ED 2A dd16 */
		emitbyte(0xED);
		emitbyte(0x2A);
		break;
	case T_IY_D:			/* lda hl,(iy+dd) = ED 32 dd16 */
		emitbyte(0xED);
		emitbyte(0x32);
		break;
	case T_HL_D:			/* lda hl,(hl+dd) = ED 3A dd16 */
		emitbyte(0xED);
		emitbyte(0x3A);
		break;
	case T_PC_D:			/* lda hl,(pc+dd) = ED 22 dd16 */
		emitbyte(0xED);
		emitbyte(0x22);
		break;
	case T_HL_IX:			/* lda hl,(hl+ix) = ED 0A */
		emitbyte(0xED);
		emitbyte(0x0A);
		return 0;
	case T_HL_IY:			/* lda hl,(hl+iy) = ED 12 */
		emitbyte(0xED);
		emitbyte(0x12);
		return 0;
	case T_IX_IY:			/* lda hl,(ix+iy) = ED 1A */
		emitbyte(0xED);
		emitbyte(0x1A);
		return 0;
	case T_INDIR:			/* lda hl,(nn) = 21 nn */
		emitbyte(0x21);
		emit_exp(2, &value);
		return 0;
	default:
		return 1;
	}
	emitbyte(value.num.w & 0xff);
	emitbyte((value.num.w >> 8) & 0xff);
	return 0;
}

/* Z280 load user data / program: ldud/ldup a,(hl)/(ix+d) and the store */
static char
do_ldud(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value, dval;

	arg = operand(&value);
	dval = value;

	if (arg == T_A) {
		need(',');
		reg = operand(&value);
		if (reg == T_HL_I) {
			emitbyte(0xED);
			emitbyte(isr->arg ? 0x96 : 0x86);
			return 0;
		}
		if (reg == T_IX_D || reg == T_IY_D) {
			emitbyte(reg == T_IX_D ? 0xDD : 0xFD);
			emitbyte(0xED);
			emitbyte(isr->arg ? 0x96 : 0x86);
			emitbyte(value.num.b);
			return 0;
		}
		return 1;
	}
	if (arg == T_HL_I || arg == T_IX_D || arg == T_IY_D) {
		need(',');
		reg = operand(&value);
		if (reg != T_A)
			return 1;
		if (arg == T_HL_I) {
			emitbyte(0xED);
			emitbyte(isr->arg ? 0x9E : 0x8E);
			return 0;
		}
		emitbyte(arg == T_IX_D ? 0xDD : 0xFD);
		emitbyte(0xED);
		emitbyte(isr->arg ? 0x9E : 0x8E);
		emitbyte(dval.num.b);
		return 0;
	}
	return 1;
}

/* Z280 sign extend: exts a / exts hl */
static char
do_exts(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	emitbyte(0xED);
	if (arg == 255 || arg == T_A)
		emitbyte(0x64);
	else if (arg == T_HL)
		emitbyte(0x6C);
	else
		return 1;
	return 0;
}

static char (*isr_handlers[])() = {
	0,
	do_basic,
	do_basic_ext,
	do_arith,
	do_incr,
	do_bitsh,
	do_stack,
	do_ret,
	do_jmp,
	do_jrl,
	do_call,
	do_rst,
	do_in,
	do_out,
	do_exch,
	do_intmode,
	do_load,
	do_inw,
	do_outw,
	do_tsti,
	do_muldiv,
	do_addw,
	do_ei,
	do_ldctl,
	do_lda,
	do_neg,
	do_ldud,
	do_exts
};

/*
 * attempts to assemble an instruction assuming a symbol has just been tokenized
 *
 * in = pointer to string
 * returns 0 if an instruction is not matched, 1 if it is
 */
char
asm_instr(in)
char *in;
{
	register struct instruct *isr;

	/*
	 * The first character decides sixty-eight of the sixty-nine.
	 * This walked the whole table for every instruction in the
	 * file and called match on each entry to be told no; the test
	 * match makes first is the one made here, without the call.
	 */
	for (isr = isr_table; isr->type != IEND; isr++) {
		if (*in != *isr->mnem)
			continue;
		if (match(in, isr->mnem)) {
			if ((*isr_handlers[isr->type])(isr))
				gripe("invalid operand");
			return 1;
		}
	}
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
