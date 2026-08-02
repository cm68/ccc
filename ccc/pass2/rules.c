/*
 * rules.c - Code generation pattern rules
 */
#include <stddef.h>
#include "pass2.h"
#include "expr.h"
#include "opcodes.h"
#include "rules.h"

/* Common assembly templates for deduplication */
/*
 * Common instruction sequences are stored once, here, and named in a
 * template by a single byte with the high bit set - the index into
 * fragtab.  emitasm expands them before it interpolates anything, so
 * nothing downstream has to know.
 *
 * This is worth about eight kilobytes.  The templates are the largest
 * single thing in the compiler and they repeat heavily: "or a" alone
 * appeared a hundred times, and nothing pools identical literals.
 */
#define F_ORA          "\201"	/*  or a */
#define F_SBCHLDE      "\202"	/*  sbc hl,de */
#define F_LDLC         "\203"	/*  ld l,c */
#define F_JPPO5        "\204"	/*  jp po,$$+5 */
#define F_INCHL        "\205"	/*  inc hl */
#define F_LDHB         "\206"	/*  ld h,b */
#define F_EXDEHL       "\207"	/*  ex de,hl */
#define F_LDDER        "\210"	/*  ld de,$R */
#define F_ADDHLDE      "\211"	/*  add hl,de */
#define F_LDAH         "\212"	/*  ld a,h */
#define F_XOR80H       "\213"	/*  xor 80h */
#define F_LDLA         "\214"	/*  ld l,a */
#define F_LDALL        "\215"	/*  ld a,($LL) */
#define F_ADDHLHL      "\216"	/*  add hl,hl */
#define F_LDAHL        "\217"	/*  ld a,(hl) */
#define F_LDHA         "\220"	/*  ld h,a */
#define F_LDLA1        "\221"	/*  ld ($L),a */
#define F_PUSHBC       "\222"	/*  push bc */
#define F_JR3          "\223"	/*  jr $$+3 */
#define F_LDAL         "\224"	/*  ld a,l */
#define F_LDDH         "\225"	/*  ld d,h */
#define F_LDEL         "\226"	/*  ld e,l */
#define F_LDHLRH       "\227"	/*  ld (hl),$Rh */
#define F_LDHLRL       "\230"	/*  ld (hl),$Rl */
#define F_LDHLE        "\231"	/*  ld (hl),e */
#define F_POPBC        "\232"	/*  pop bc */
#define F_XORA         "\233"	/*  xor a */
#define F_LDHLL        "\234"	/*  ld hl,$L */
#define F_CPR          "\235"	/*  cp $R */
#define F_JPM5         "\236"	/*  jp m,$$+5 */
#define F_LDHLD        "\237"	/*  ld (hl),d */
#define F_LDDELO       "\240"	/*  ld de,$Lo */
#define F_LDLHL        "\241"	/*  ld ($L),hl */
#define F_SUBR         "\242"	/*  sub $R */
#define F_PUSHLR       "\243"	/*  push $Lr */
#define F_LDLL         "\244"	/*  ld ($L),l */
#define F_LDAL1        "\245"	/*  ld a,($L) */
#define F_LDHHL        "\246"	/*  ld h,(hl) */
#define F_SBCHLBC      "\247"	/*  sbc hl,bc */
#define F_LDAB         "\250"	/*  ld a,b */
#define F_POPHL        "\251"	/*  pop hl */
#define F_CALLLADEC    "\252"	/*  call ladec */
#define F_CALLLAINC    "\253"	/*  call lainc */
#define F_JRNZ3        "\254"	/*  jr nz,$$+3 */
#define F_LDLH         "\255"	/*  ld ($L+),h */
#define F_LDHLR        "\256"	/*  ld hl,$R */
#define F_DECHL        "\257"	/*  dec hl */
#define F_PUSHHL       "\260"	/*  push hl */
#define F_SBCAA        "\261"	/*  sbc a,a */
#define F_LDHLRL1      "\262"	/*  ld hl,$RL */
#define F_LDDB         "\263"	/*  ld d,b */
#define F_LDEC         "\264"	/*  ld e,c */
#define F_POPDE        "\265"	/*  pop de */
#define F_LDHLR2       "\266"	/*  ld (hl),$R2 */
#define F_LDHLR3       "\267"	/*  ld (hl),$R3 */
#define F_LDHLL1       "\270"	/*  ld h,($LL+) */
#define F_LDDEL        "\271"	/*  ld de,($L) */
#define F_LDHLL2       "\272"	/*  ld hl,($L) */
#define F_LDLLL        "\273"	/*  ld l,($LL) */
#define F_SUBE         "\274"	/*  sub e */
#define F_JPM7         "\275"	/*  jp m,$$+7 */
#define F_LDHLA        "\276"	/*  ld (hl),a */
#define F_LDA0         "\277"	/*  ld a,0 */
#define F_LDAC         "\300"	/*  ld a,c */
#define F_LDH0         "\301"	/*  ld h,0 */
#define F_CPE          "\302"	/*  cp e */
#define F_LDHLL3       "\303"	/*  ld hl,($L++) */
#define F_ORHL         "\304"	/*  or (hl) */
#define F_LDUL         "\305"	/*  ld $u,($L+) */
#define F_RLA          "\306"	/*  rla */

char *fragtab[] = {
	0,
	"\tor a\n",	/* F_ORA */
	"\tsbc hl,de\n",	/* F_SBCHLDE */
	"\tld l,c\n",	/* F_LDLC */
	"\tjp po,$$+5\n",	/* F_JPPO5 */
	"\tinc hl\n",	/* F_INCHL */
	"\tld h,b\n",	/* F_LDHB */
	"\tex de,hl\n",	/* F_EXDEHL */
	"\tld de,$R\n",	/* F_LDDER */
	"\tadd hl,de\n",	/* F_ADDHLDE */
	"\tld a,h\n",	/* F_LDAH */
	"\txor 80h\n",	/* F_XOR80H */
	"\tld l,a\n",	/* F_LDLA */
	"\tld a,($LL)\n",	/* F_LDALL */
	"\tadd hl,hl\n",	/* F_ADDHLHL */
	"\tld a,(hl)\n",	/* F_LDAHL */
	"\tld h,a\n",	/* F_LDHA */
	"\tld ($L),a\n",	/* F_LDLA1 */
	"\tpush bc\n",	/* F_PUSHBC */
	"\tjr $$+3\n",	/* F_JR3 */
	"\tld a,l\n",	/* F_LDAL */
	"\tld d,h\n",	/* F_LDDH */
	"\tld e,l\n",	/* F_LDEL */
	"\tld (hl),$Rh\n",	/* F_LDHLRH */
	"\tld (hl),$Rl\n",	/* F_LDHLRL */
	"\tld (hl),e\n",	/* F_LDHLE */
	"\tpop bc\n",	/* F_POPBC */
	"\txor a\n",	/* F_XORA */
	"\tld hl,$L\n",	/* F_LDHLL */
	"\tcp $R\n",	/* F_CPR */
	"\tjp m,$$+5\n",	/* F_JPM5 */
	"\tld (hl),d\n",	/* F_LDHLD */
	"\tld de,$Lo\n",	/* F_LDDELO */
	"\tld ($L),hl\n",	/* F_LDLHL */
	"\tsub $R\n",	/* F_SUBR */
	"\tpush $Lr\n",	/* F_PUSHLR */
	"\tld ($L),l\n",	/* F_LDLL */
	"\tld a,($L)\n",	/* F_LDAL1 */
	"\tld h,(hl)\n",	/* F_LDHHL */
	"\tsbc hl,bc\n",	/* F_SBCHLBC */
	"\tld a,b\n",	/* F_LDAB */
	"\tpop hl\n",	/* F_POPHL */
	"\tcall ladec\n",	/* F_CALLLADEC */
	"\tcall lainc\n",	/* F_CALLLAINC */
	"\tjr nz,$$+3\n",	/* F_JRNZ3 */
	"\tld ($L+),h\n",	/* F_LDLH */
	"\tld hl,$R\n",	/* F_LDHLR */
	"\tdec hl\n",	/* F_DECHL */
	"\tpush hl\n",	/* F_PUSHHL */
	"\tsbc a,a\n",	/* F_SBCAA */
	"\tld hl,$RL\n",	/* F_LDHLRL1 */
	"\tld d,b\n",	/* F_LDDB */
	"\tld e,c\n",	/* F_LDEC */
	"\tpop de\n",	/* F_POPDE */
	"\tld (hl),$R2\n",	/* F_LDHLR2 */
	"\tld (hl),$R3\n",	/* F_LDHLR3 */
	"\tld h,($LL+)\n",	/* F_LDHLL1 */
	"\tld de,($L)\n",	/* F_LDDEL */
	"\tld hl,($L)\n",	/* F_LDHLL2 */
	"\tld l,($LL)\n",	/* F_LDLLL */
	"\tsub e\n",	/* F_SUBE */
	"\tjp m,$$+7\n",	/* F_JPM7 */
	"\tld (hl),a\n",	/* F_LDHLA */
	"\tld a,0\n",	/* F_LDA0 */
	"\tld a,c\n",	/* F_LDAC */
	"\tld h,0\n",	/* F_LDH0 */
	"\tcp e\n",	/* F_CPE */
	"\tld hl,($L++)\n",	/* F_LDHLL3 */
	"\tor (hl)\n",	/* F_ORHL */
	"\tld $u,($L+)\n",	/* F_LDUL */
	"\trla\n",	/* F_RLA */
};

#define T_SAVE_HL	F_LDDH F_LDEL
#define T_ADD_HL_DE	F_ADDHLDE
#define T_ADD_HL_HL	F_ADDHLHL
#define T_IDX_S_ST	F_LDLL F_LDLH
#define T_IDX_S_LD	"\tld l,($R)\n\tld h,($R+)\n"
#define T_IX_TEST	"\tld a,ixl\n\tor ixh\n"
#define T_BC_TEST	F_LDAC "\tor b\n"
#define T_HL_TEST	F_LDAL "\tor h\n"
/* a long lives in HL:DE, so all four bytes have to be in the test */
#define T_HLDE_TEST	F_LDAH "\tor l\n\tor d\n\tor e\n"
#define T_BC_HL	F_LDLC F_LDHB
/*
 * Load a word through HL into HL.  A carries the low byte while HL is
 * walked to the high one, because the pointer and the result share the
 * register - which is also why anything that has to write back has to
 * save the address first.
 *
 * CLOBBERS A.  ld e,(hl) / inc hl / ld d,(hl) / ex de,hl is the same
 * four bytes and leaves A alone, but takes DE instead - and DE holds
 * the second operand of a word expression far more often than A holds
 * anything, so this is the safer default.  Anywhere A is live across a
 * word load, that is the form to reach for.
 */
#define T_LD_IHL	F_LDAHL F_INCHL F_LDHHL F_LDLA
#define T_SUB_DE	F_ORA F_SBCHLDE
#define T_SUB_BC	F_ORA F_SBCHLBC
/*
 * Turn the result of a signed subtraction into a sign flag: the answer
 * is sign exclusive-or overflow, so flip the top bit of the high byte
 * when P/V says the subtraction overflowed, then let or a set S from
 * it.  M means less than, P means greater or equal.
 */
#define T_SXORV	F_LDAH F_JPPO5 F_XOR80H F_ORA
/*
 * The same correction for a byte, where the difference is already in A
 * so there is nothing to fetch first.  Note the subtraction has to be
 * sub rather than cp: cp keeps A, but the top bit has to be flipped in
 * the value for or a to read the corrected sign back out.  Nothing
 * needs A afterwards - these only ever produce a flag.
 */
#define T_SXORA	F_JPPO5 F_XOR80H F_ORA
/*
 * "greater than" and "less or equal" are not one flag the way "less
 * than" is: they need the sign and the zero together.  Given S and Z
 * set from a corrected difference, send the negative side into an
 * xor a so that both paths arrive with Z meaning false.
 *
 *   J+0  jp m   3   negative
 *   J+3  jr     2   otherwise Z already says whether it was equal
 *   J+5  xor a  1
 *   J+6
 */
#define T_SZTAIL	F_JPM5 F_JR3 F_XORA
/* the address of a frame slot, worked out into HL - left path */
#define T_IDX_ADDR	F_PUSHLR F_POPHL F_LDDELO F_ADDHLDE
/* the same for the right operand, which is where address-of puts it */
#define T_IDX_R_ADDR	"\tpush $Rr\n" F_POPHL "\tld de,$Ro\n" F_ADDHLDE
/* four bytes of a constant written through the address in HL */
#define T_ST_IHL_N	F_LDHLRL F_INCHL F_LDHLRH F_INCHL F_LDHLR2 F_INCHL F_LDHLR3
/* address on the stack, value in HL -> address in HL, value in DE */
#define T_SWAP_ADDR	F_POPDE F_EXDEHL
/* store DE through HL, then bring the value back to HL */
#define T_ST_IHL	F_LDHLE F_INCHL F_LDHLD F_EXDEHL
#define T_DE_TEST	"\tld a,e\n\tor d\n"

#define R(pat, rep, l, r, d, f, tpl, dest) \
	{pat, tpl, rep, (l) | ((r) << 2) | ((d) << 4), f, dest}

struct rule rules[] = {
	/* LOCALVAR -> INDEX */
	R("L", INDEX, P_NONE, P_NONE, P_NONE, 0, NULL, 0),

	/* LOCALVAR past the 7-bit (iy+d) window (big-array bases live
	 * below the callee-save slots): form the address with 16-bit
	 * arithmetic (special-cased in tryrule).  Only reached when the
	 * INDEX rule above refuses. */
	R("L", CODE, P_NONE, P_NONE, P_NONE, 0, NULL, 0),

	/* bare SYM -> SYMREF+0, so the address rules below can see it */
	R("S", SYMREF, P_NONE, P_NONE, P_NONE, 0, NULL, 0),

	/* REGVAR -> IN* (value is in register) */
	R("V", INBC, P_NONE, P_NONE, P_NONE, RF_BC, NULL, 0),
	R("V", INDE, P_NONE, P_NONE, P_NONE, RF_DE, NULL, 0),
	R("V", INHL, P_NONE, P_NONE, P_NONE, RF_HL, NULL, 0),

	/* REGVAR IX in flag context: test for zero */
	R("V:F", REGVAR, P_NONE, P_NONE, P_NONE, RF_IX, T_IX_TEST, F_NZ),

	/* INBC in flag context: test for zero */
	R("B:F", INBC, P_NONE, P_NONE, P_NONE, 0, T_BC_TEST, F_NZ),

	/* REGVAR byte C/B in flag context */
	R("V:bF", REGVAR, P_NONE, P_NONE, P_NONE, RF_C, F_LDAC F_ORA, F_NZ),
	R("V:bF", REGVAR, P_NONE, P_NONE, P_NONE, RF_B, F_LDAB F_ORA, F_NZ),

	/* assign constant/A/HL to REGVAR C/B */
	R("=(V,N):b", ASSIGN, P_L, P_R, P_L, RF_C, "\tld c,$R\n", R_A),
	R("=(V,N):b", ASSIGN, P_L, P_R, P_L, RF_B, "\tld b,$R\n", R_A),
	R("=(V,A):b", ASSIGN, P_L, P_R, P_L, RF_C, "\tld c,a\n", R_A),
	R("=(V,A):b", ASSIGN, P_L, P_R, P_L, RF_B, "\tld b,a\n", R_A),
	R("=(V,H):b", ASSIGN, P_L, P_R, P_L, RF_C, "\tld c,l\n", R_HL),
	R("=(V,H):b", ASSIGN, P_L, P_R, P_L, RF_B, "\tld b,l\n", R_HL),

	/* load REGVAR C/B to HL (zero-extended) */
	R("=(H,V):b", ASSIGN, P_L, P_R, P_R, RF_C, F_LDLC F_LDH0, R_HL),
	R("=(H,V):b", ASSIGN, P_L, P_R, P_R, RF_B, "\tld l,b\n" F_LDH0, R_HL),

	/* REGVAR C/B -> INA (value in C/B, byte context) */
	R("V:b", INA, P_NONE, P_NONE, P_NONE, RF_C, F_LDAC, R_A),
	R("V:b", INA, P_NONE, P_NONE, P_NONE, RF_B, F_LDAB, R_A),

	/*
	 * INHL/INDE/INA in flag context: test for zero.
	 *
	 * The long has to come first.  "H:F" carries no width, so it
	 * matches one too - and testing HL alone tests the high word,
	 * which is zero for every long that fits in an int.  "if (v)"
	 * on a long was false for 1 and true for 65536.
	 */
	R("H:lF", INHL, P_NONE, P_NONE, P_NONE, 0, T_HLDE_TEST, F_NZ),
	R("H:F", INHL, P_NONE, P_NONE, P_NONE, 0, T_HL_TEST, F_NZ),
	R("E:F", INDE, P_NONE, P_NONE, P_NONE, 0, T_DE_TEST, F_NZ),
	R("A:F", INA, P_NONE, P_NONE, P_NONE, 0, F_ORA, F_NZ),
	/*
	 * The same for a value that ended up in IX.  HL, DE, BC, A and E
	 * each become a typed node before the table is reached; IX does
	 * not, so it arrives as CODE and matched nothing at all.  No rule
	 * matching normally leaves a marker, but this is a condition
	 * rather than a value - the caller wanted flags and took whatever
	 * was in them:
	 *
	 *	while ((top = fstack_top(&fs)) != NULL)
	 *
	 * with top in IX emitted push hl / pop ix, which sets no flags,
	 * and then branched on what the call had left behind.  cpp's
	 * filtbrace loops forever on any brace-less if, while or for.
	 */
	R("C:F", CODE, P_NONE, P_NONE, P_NONE, RF_IX, T_IX_TEST, F_NZ),

	/* copy IX to HL/BC/DE */
	R("=(H,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tpush ix\n" F_POPHL, R_HL),
	R("=(B,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tld c,ixl\n\tld b,ixh\n", R_BC),
	R("=(E,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tld e,ixl\n\tld d,ixh\n", R_DE),

	/* Address rules: IX+offset -> INDEX */
	R("+(V,N)", INDEX, P_NONE, P_NONE, P_L, RF_IX, NULL, 0),
	R("+(D(V),N)", INDEX, P_NONE, P_NONE, P_LL, RF_IXIY, NULL, 0),
	R("+(I,N)", INDEX, P_NONE, P_NONE, P_L, 0, NULL, 0),
	/*
	 * Array element with a variable subscript: the base is a frame
	 * slot and the scaled index is already in HL, so form the address
	 * rather than let (ix+d) do it.  A constant subscript never gets
	 * here - +(I,N) above folds it straight into the offset.
	 */
	R("+(I,H)", PLUS, P_L, P_R, P_NONE, 0,
		F_PUSHLR F_POPDE F_ADDHLDE F_LDDELO F_ADDHLDE, R_HL),
	/* the same with the subscript already in DE, which is where the
	 * reorder leaves it when the index is the costlier side */
	R("+(I,E)", PLUS, P_L, P_R, P_NONE, 0,
		F_PUSHLR F_POPHL F_ADDHLDE F_LDDELO F_ADDHLDE, R_HL),
	/* and in BC, which is where a register variable subscript sits */
	R("+(I,B)", PLUS, P_L, P_R, P_NONE, 0,
		F_PUSHLR F_POPHL "\tadd hl,bc\n" F_LDDELO F_ADDHLDE, R_HL),

	/* symbol + constant offset folds into the SYMREF */
	R("+(O,N)", SYMREF, P_NONE, P_NONE, P_NONE, 0, NULL, 0),
	/*
	 * The same for a global array, where the base is a link-time
	 * constant and the scaled subscript is in a register - one add,
	 * with the base loaded into whichever half is free.  A constant
	 * subscript never reaches here: +(O,N) above folds it away.
	 */
	R("+(O,H)", PLUS, P_L, P_R, P_NONE, 0, "\tld de,$L\n" F_ADDHLDE, R_HL),
	R("+(O,E)", PLUS, P_L, P_R, P_NONE, 0, F_LDHLL F_ADDHLDE, R_HL),
	/*
	 * The same with the subscript in BC, which a register variable
	 * puts it in - "buf[i]" over a global array with i in a register.
	 * There were forms for HL and DE and not for BC, so the address
	 * was never worked out and nothing was emitted for it.
	 */
	R("+(O,B)", PLUS, P_L, P_R, P_NONE, 0, F_LDHLL "\tadd hl,bc\n", R_HL),

	/* strength reduction */
	R("*(_,P)", LSHIFT, P_L, P_R, P_NONE, RF_POW2, NULL, 0),

	/* STAR by small constants */
	R("*(H,3)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE, R_HL),
	R("*(H,5)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_DE, R_HL),
	R("*(H,6)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL, R_HL),
	R("*(H,7)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_DE, R_HL),
	R("*(H,9)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_DE, R_HL),
	R("*(H,x)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL, R_HL),
	R("*(H,e)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_DE, R_HL),
	R("*(H,w)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_HL, R_HL),
	R("*(H,f)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL, R_HL),
	R("*(H,n)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_DE, R_HL),
	R("*(H,y)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_HL, R_HL),
	R("*(H,q)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_HL, R_HL),
	R("*(H,z)", STAR, P_L, P_NONE, P_NONE, 0,
		T_SAVE_HL T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_DE T_ADD_HL_HL T_ADD_HL_HL T_ADD_HL_HL, R_HL),

	/* runtime calls */
	/*
	 * The 16-bit helpers take the left operand in HL and the right in
	 * DE and return in HL.  a- is the signed form and l- the logical
	 * one - not long, whatever the letter suggests elsewhere.
	 *
	 * These were named __mul16, __div16 and __mod16, which the
	 * library has never defined.  Nothing noticed until a program was
	 * linked and run, because a call to a symbol that does not exist
	 * assembles perfectly well.
	 */
	R("*(H,E)", STAR, P_L, P_R, P_NONE, 0, "$[\tcall amul\n$]", R_HL),
	/* the multiplier in BC: amul wants it in DE, and clobbers BC on
	 * the way, so the save the $[ $] pair makes is what puts a
	 * register variable back */
	R("*(H,B)", STAR, P_L, P_R, P_NONE, 0,
		"\tld e,c\n\tld d,b\n$[\tcall amul\n$]", R_HL),
	/*
	 * Any other constant, which has to go through the helper like a
	 * variable would.  The shift-and-add forms above are cheaper and
	 * come first; this is what catches everything they do not name.
	 * There was nothing here at all, so "v * 100" emitted no multiply
	 * - and said so only after the marker learned to look below the
	 * root of a statement.
	 */
	R("*(H,N)", STAR, P_L, P_R, P_NONE, 0,
		F_LDDER "$[\tcall amul\n$]", R_HL),
	/*
	 * The same with the left operand in BC, which a register variable
	 * puts it in.  There was a form for BC times a constant and none
	 * for BC times a value, so "i * a" with i in a register emitted
	 * no multiply at all - and no marker either, the store above it
	 * having matched.
	 *
	 * BC is saved across the call because amul takes its second
	 * operand off the stack with a pop bc and does not put it back.
	 * The register variable that was the left operand is still wanted
	 * afterwards, and when it was the subscript of the loop doing the
	 * multiplying the loop did not end.
	 */
	R("*(B,E)", STAR, P_L, P_R, P_NONE, 0,
		"$[" T_BC_HL "\tcall amul\n$]", R_HL),
	R("/(H,E)", DIV, P_L, P_R, P_NONE, RF_SIGNL, "$[\tcall adiv\n$]", R_HL),
	R("/(H,E)", DIV, P_L, P_R, P_NONE, 0, "$[\tcall ldiv\n$]", R_HL),
	/* and with the left operand in BC, as a register variable puts it */
	R("/(B,E)", DIV, P_L, P_R, P_NONE, RF_SIGNL,
		"$[" T_BC_HL "\tcall adiv\n$]", R_HL),
	R("/(B,E)", DIV, P_L, P_R, P_NONE, 0,
		"$[" T_BC_HL "\tcall ldiv\n$]", R_HL),
	R("%(B,E)", MOD, P_L, P_R, P_NONE, RF_SIGNL,
		"$[" T_BC_HL "\tcall amod\n$]", R_HL),
	R("%(B,E)", MOD, P_L, P_R, P_NONE, 0,
		"$[" T_BC_HL "\tcall lmod\n$]", R_HL),
	/* by a constant, which is what dividing a pointer difference by
	 * the element size looks like */
	R("/(H,N)", DIV, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER "$[\tcall adiv\n$]", R_HL),
	R("/(H,N)", DIV, P_L, P_R, P_NONE, 0,
		F_LDDER "$[\tcall ldiv\n$]", R_HL),
	R("%(H,N)", MOD, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER "$[\tcall amod\n$]", R_HL),
	R("%(H,N)", MOD, P_L, P_R, P_NONE, 0,
		F_LDDER "$[\tcall lmod\n$]", R_HL),
	R("%(H,E)", MOD, P_L, P_R, P_NONE, RF_SIGNL, "$[\tcall amod\n$]", R_HL),
	R("%(H,E)", MOD, P_L, P_R, P_NONE, 0, "$[\tcall lmod\n$]", R_HL),

	/*
	 * Store to a frame slot.  A constant goes straight into the slot
	 * without touching a register, which is the right thing for a
	 * statement and the wrong thing for "i = k = 5": the assignment
	 * has a value, and there has to be one somewhere for the outer
	 * assignment to copy.  The :V forms pay for a register because
	 * something is going to read it.
	 */
	R("=(I,N):bV", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n" F_LDLA1, R_A),
	R("=(I,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),$R\n", 0),
	R("=(I,N):sV", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLR T_IDX_S_ST, R_HL),
	R("=(I,N):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),$Rl\n\tld ($L+),$Rh\n", 0),
	R("=(I,H):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLL, R_HL),
	/*
	 * These stored from a register and left the value in it, so they
	 * name it.  Claiming whatever register the node was aimed at
	 * would hand the parent one that was never written.
	 */
	R("=(I,H):s", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_S_ST, R_HL),
	/*
	 * A frame slot assigned a frame slot's address - "p = &v".  A
	 * bare index is a place, not a value; reading one is a DEREF
	 * around it, and a copy between two locals comes through here as
	 * =(I,D(I)) with the load already done.  So this form only ever
	 * arises from address-of, and loading through the right operand
	 * made "p = &v" mean "p = v".
	 */
	R("=(I,I):s", ASSIGN, P_L, P_R, P_NONE, 0,
		T_IDX_R_ADDR T_IDX_S_ST, R_HL),
	R("=(I,E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),e\n\tld ($L+),d\n", R_DE),
	R("=(I,B):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),c\n\tld ($L+),b\n", R_BC),

	/* store to symref */
	R("=(O,A):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLA1, R_A),
	R("=(O,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n" F_LDLA1, R_A),
	R("=(O,H):s", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLHL, R_HL),
	/* narrowing store: a word result keeps only its low byte */
	R("=(O,H):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAL F_LDLA1, R_A),
	R("=(O,B):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAC F_LDLA1, R_A),
	/*
	 * A register variable stored to a global.  ld (nn),bc is four
	 * bytes and there was no rule for it at all, so "g = r" emitted
	 * nothing and said so in a marker nobody had run into.
	 */
	R("=(O,B):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),bc\n", R_BC),
	R("=(I,B):bV", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAC F_LDLA1, R_A),
	R("=(I,B):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),c\n", 0),
	R("=(O,N):s", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLR F_LDLHL, R_HL),
	/*
	 * A byte in A stored to a word.  This is what a truth test or a
	 * comparison used for its value comes to: the flag becomes a
	 * nought or a one in A, and what it goes into is wider than
	 * that.  Unsigned by construction, so the top half is zero;
	 * anything genuinely signed arrives wrapped in a SEXT.
	 */
	R("=(O,A):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLA F_LDH0 F_LDLHL, R_HL),
	R("=(I,A):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLA F_LDH0 T_IDX_S_ST, R_HL),

	/* load constant to register variable */
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tld ix,$R\n", R_IX),
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_BC, "\tld bc,$R\n", R_BC),
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_DE, F_LDDER, R_DE),
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_HL, F_LDHLR, R_HL),

	/* load constant to register (already converted) */
	/*
	 * A 32-bit constant first, since the rule below carries no width
	 * and would otherwise take it and keep the low half.  That is how
	 * a long constant passed as an argument arrived as its bottom two
	 * bytes with DE left holding whatever was there before.
	 */
	R("=(H,N):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld e,$Rl\n\tld d,$Rh\n\tld l,$R2\n\tld h,$R3\n", R_HL),
	R("=(B,N)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld bc,$R\n", R_BC),
	R("=(E,N)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDDER, R_DE),
	R("=(H,N)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLR, R_HL),

	/* assign to IX register variable */
	/*
	 * The index register holds a pointer that is used for field
	 * access, which is what it is chosen for - but the pointer is
	 * still a value, and gets assigned, compared and passed like any
	 * other.  Almost none of that had a rule.
	 *
	 * The index registers cannot be compared or arithmetic'd against
	 * anything except through HL, so those go via the stack.  Loading
	 * and storing them whole does not: ld ix,nn and ld (nn),ix exist.
	 */
	R("=(V,O)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tld ix,$R\n", R_IX),
	R("=(O,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tld ($L),ix\n", R_IX),
	/*
	 * The same once the operand has been reduced.  IX has no register
	 * node of its own - HL, DE, BC and the byte registers each do -
	 * so a value in it stays a CODE node, and the rule above, which
	 * asks for a REGVAR, no longer matches.
	 */
	R("=(O,C)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tld ($L),ix\n", R_IX),
	R("=(I,V):s", ASSIGN, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n" F_POPHL T_IDX_S_ST, R_HL),
	/*
	 * "p + n" folds into an indexed location, so assigning one back
	 * is how a pointer step arrives here.  The base register of that
	 * location need not be the one being assigned to, so the address
	 * is worked out rather than added in place.
	 */
	R("=(V,I)", ASSIGN, P_L, P_R, P_L, RF_IX,
		"\tpush $Rr\n" F_POPHL "\tld de,$Ro\n" F_ADDHLDE
		F_PUSHHL "\tpop ix\n", R_IX),
	/*
	 * Arithmetic on it comes out in HL, which "=(V,H)" then puts
	 * back.  Only addition of a constant folds into an indexed
	 * location; subtraction has to be done.
	 */
	R("-(V,N)", MINUS, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_LDDER F_ORA F_SBCHLDE, R_HL),
	R("-(V,O)", MINUS, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_LDDER F_ORA F_SBCHLDE, R_HL),
	R("+(V,O)", PLUS, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_LDDER F_ADDHLDE, R_HL),
	/*
	 * The index register plus a value rather than a constant or a
	 * symbol - "p[i]" where p is a pointer register variable and i is
	 * worked out.  A constant offset folds into an INDEX and never
	 * reaches here, which is why this was missing: the shape only
	 * turns up when the subscript is not known.  Eleven places in the
	 * tools, counting the ones that then sign-extend the result.
	 */
	R("+(V,E)", PLUS, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_ADDHLDE, R_HL),
	/*
	 * Against zero, which is what "p == 0" on a pointer register
	 * variable comes to and had no form: testing the halves is
	 * shorter than loading nought into DE to subtract it.
	 */
	R("Q(V,Z)", EQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL T_HL_TEST, F_Z),
	R("U(V,Z)", NEQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL T_HL_TEST, F_NZ),
	/* and against any other constant, the same as against a symbol */
	R("Q(V,N)", EQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL "\tld de,$R\n" F_ORA F_SBCHLDE, F_Z),
	R("U(V,N)", NEQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL "\tld de,$R\n" F_ORA F_SBCHLDE, F_NZ),
	R("Q(V,O)", EQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL "\tld de,$R\n" F_ORA F_SBCHLDE, F_Z),
	R("U(V,O)", NEQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL "\tld de,$R\n" F_ORA F_SBCHLDE, F_NZ),
	R("Q(V,B)", EQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_ORA "\tsbc hl,bc\n", F_Z),
	R("U(V,B)", NEQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_ORA "\tsbc hl,bc\n", F_NZ),
	R("Q(V,H)", EQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n\tpop de\n" F_ORA F_SBCHLDE, F_Z),
	R("U(V,H)", NEQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n\tpop de\n" F_ORA F_SBCHLDE, F_NZ),
	/*
	 * And against DE, which had no form at all.  A pointer register
	 * variable compared with a local pointer emitted no code and the
	 * branch after it went wherever the flags happened to point.
	 */
	R("Q(V,E)", EQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_ORA F_SBCHLDE, F_Z),
	R("U(V,E)", NEQ, P_L, P_R, P_L, RF_IX,
		"\tpush ix\n" F_POPHL F_ORA F_SBCHLDE, F_NZ),
	/* the same with the index register on the other side, which
	 * normalize does not swap because equality is not a relation it
	 * reorders by operand kind */
	R("Q(H,V)", EQ, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_ORA F_SBCHLDE, F_Z),
	R("U(H,V)", NEQ, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_ORA F_SBCHLDE, F_NZ),
	/*
	 * The orderings, which arrive when a register pointer is
	 * compared against a symbol the rewriter staged into HL.
	 * Unsigned: IX holds pointers by allocation policy, and
	 * addresses order by carry, not sign.
	 */
	R("T(H,V)", LT, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_ORA F_SBCHLDE, F_C),
	R("Y(H,V)", GE, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_ORA F_SBCHLDE, F_NC),
	R("W(H,V)", LE, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_EXDEHL F_ORA F_SBCHLDE, F_NC),
	R("G(H,V)", GT, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_EXDEHL F_ORA F_SBCHLDE, F_C),

	R("=(V,H)", ASSIGN, P_L, P_R, P_L, RF_IX, F_PUSHHL "\tpop ix\n", R_IX),
	R("=(V,E)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tpush de\n\tpop ix\n", R_IX),
	R("=(V,B)", ASSIGN, P_L, P_R, P_L, RF_IX, F_PUSHBC "\tpop ix\n", R_IX),

	/* register-to-register moves */
	R("=(B,H)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,l\n\tld b,h\n", R_BC),
	R("=(E,H)", ASSIGN, P_L, P_R, P_NONE, 0, F_EXDEHL, R_DE),
	R("=(H,E)", ASSIGN, P_L, P_R, P_NONE, 0, F_EXDEHL, R_HL),
	R("=(H,B)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLC F_LDHB, R_HL),
	R("=(B,E)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,e\n\tld b,d\n", R_BC),
	R("=(E,B)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDEC F_LDDB, R_DE),
	R("=(B,B)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_BC),
	R("=(E,E)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_DE),
	R("=(H,H)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_HL),

	/*
	 * Comma: both sides have already emitted their code, in order,
	 * and the value is the right one - so there is nothing left to do
	 * but say where it ended up.  ';' rather than ',' because the
	 * pattern parser uses the comma to separate children.
	 */
	R(";(_,H)", COMMA, P_L, P_R, P_NONE, 0, "", R_HL),
	R(";(_,E)", COMMA, P_L, P_R, P_NONE, 0, "", R_DE),
	R(";(_,B)", COMMA, P_L, P_R, P_NONE, 0, "", R_BC),
	R(";(_,A)", COMMA, P_L, P_R, P_NONE, 0, "", R_A),

	/* assign to CODE result */
	R("=(C,H)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_HL),
	R("=(C,E)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_DE),
	R("=(C,B)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_BC),
	R("=(C,A)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_A),

	/*
	 * Widening a byte to a word.  Unsigned zero-extends; signed puts
	 * bit 7 into carry with rla, then sbc a,a turns that into 00 or
	 * ff.  Both honour the target, since the widened value is as
	 * often the right operand (DE) as the left (HL).
	 */
	R("J(A):s", WIDEN, P_L, P_NONE, P_NONE, 0, "\tld $t,a\n\tld $u,0\n", 0),
	/*
	 * A byte that is already in HL rather than in A - which is where
	 * a ternary leaves its value, both arms having been landed in the
	 * one register so the expression has a value whichever way the
	 * branch went.  Only the low half is meaningful, so the high half
	 * is cleared or filled with the sign.
	 *
	 * The width has to be pinned on the operand, not just the result:
	 * a SEXT to short whose operand is already a short is a widening
	 * of a pointer, and filling H with the sign of L destroys it.
	 */
	R("J(H:b):s", WIDEN, P_L, P_NONE, P_NONE, 0, F_LDH0, R_HL),
	/* widening a word to a word, which the usual conversions ask for
	 * between two pointers of the same size: nothing to do, except
	 * where the word is in the index register and has to come out */
	R("X(V):s", SEXT, P_L, P_NONE, P_L, RF_IX, "\tpush ix\n" F_POPHL, R_HL),
	R("J(V):s", WIDEN, P_L, P_NONE, P_L, RF_IX, "\tpush ix\n" F_POPHL, R_HL),
	R("X(H:s):s", SEXT, P_L, P_NONE, P_NONE, 0, "", R_HL),
	/*
	 * The same for a word already in BC, where there is still nothing
	 * to extend but it does have to come over.  Without these, a
	 * pointer difference or a subscript on a register variable ran
	 * into a sign extension from short to short that no rule named,
	 * and stopped there.
	 */
	R("X(B:s):s", SEXT, P_L, P_NONE, P_NONE, 0, T_BC_HL, R_HL),
	R("J(B:s):s", WIDEN, P_L, P_NONE, P_NONE, 0, T_BC_HL, R_HL),
	/* and from a frame slot, where it is the same load D(I):s makes */
	R("X(I:s):s", SEXT, P_L, P_NONE, P_NONE, 0, "\tld $t,($L)\n" F_LDUL, 0),
	R("J(I:s):s", WIDEN, P_L, P_NONE, P_NONE, 0, "\tld $t,($L)\n" F_LDUL, 0),
	/* a symbol's address widened to a word, which is what taking a
	 * function's address for a function pointer comes to */
	R("X(O):s", SEXT, P_L, P_NONE, P_NONE, 0, "\tld $T,$L\n", 0),
	R("J(O):s", WIDEN, P_L, P_NONE, P_NONE, 0, "\tld $T,$L\n", 0),
	R("J(H:s):s", WIDEN, P_L, P_NONE, P_NONE, 0, "", R_HL),
	R("X(H:b):s", SEXT, P_L, P_NONE, P_NONE, 0,
		F_LDAL F_RLA F_SBCAA F_LDHA, R_HL),
	R("X(A):s", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tld $t,a\n" F_RLA F_SBCAA "\tld $u,a\n", 0),

	/*
	 * Narrowing, which is the other direction and is only ever a
	 * question of where the low half already is.
	 *
	 * From a long it is in DE, so the exchange is the whole of the
	 * work; the byte rules that follow take L, which is then the
	 * lowest byte of the value.  From anything else the low half is
	 * already where a narrower reader would look for it, and the
	 * rule is there so that the cast emits nothing rather than
	 * matching nothing.
	 *
	 * pass1 used to write the cast's type over the node and leave no
	 * trace, so "(int)f()" on a long-returning f read HL - the high
	 * word.  Every numeric escape in cpp came back zero, because
	 * escint() is "(int)getint(base)".
	 */
	R("R(H:l):s", NARROW, P_L, P_NONE, P_NONE, 0, F_EXDEHL, R_HL),
	R("R(H:l):b", NARROW, P_L, P_NONE, P_NONE, 0, F_EXDEHL F_LDAL, R_A),
	R("R(H):b", NARROW, P_L, P_NONE, P_NONE, 0, F_LDAL, R_A),
	R("R(B):b", NARROW, P_L, P_NONE, P_NONE, 0, F_LDAC, R_A),
	R("R(E):b", NARROW, P_L, P_NONE, P_NONE, 0, "\tld a,e\n", R_A),
	R("R(A)", NARROW, P_L, P_NONE, P_NONE, 0, "", R_A),
	R("R(K)", NARROW, P_L, P_NONE, P_NONE, 0, "", R_E),
	R("R(H)", NARROW, P_L, P_NONE, P_NONE, 0, "", R_HL),

	/*
	 * 32-bit values live in HL:DE, high word in HL.
	 *
	 * Sign-extending into that means the value becomes the low word
	 * and the high word becomes all sign bits - rla puts bit 15 (or
	 * bit 7 for a byte) into carry and sbc a,a spreads it.
	 */
	R("X(H):l", SEXT, P_L, P_NONE, P_NONE, 0,
		F_EXDEHL "\tld a,d\n" F_RLA F_SBCAA F_LDHA F_LDLA, R_HL),
	R("X(B):l", SEXT, P_L, P_NONE, P_NONE, 0,
		F_LDEC F_LDDB F_LDAB F_RLA F_SBCAA
		F_LDHA F_LDLA, R_HL),
	R("X(A):l", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tld e,a\n" F_RLA F_SBCAA "\tld d,a\n" F_LDHA F_LDLA, R_HL),
	/* an unsigned word widened: it becomes the low half and the high
	 * half is nothing, which is the whole difference from X(H) */
	R("J(H):l", WIDEN, P_L, P_NONE, P_NONE, 0,
		F_EXDEHL "\tld hl,0\n", R_HL),
	R("J(B):l", WIDEN, P_L, P_NONE, P_NONE, 0,
		"\tld e,c\n\tld d,b\n\tld hl,0\n", R_HL),
	R("J(A):l", WIDEN, P_L, P_NONE, P_NONE, 0,
		"\tld e,a\n\tld d,0\n\tld h,d\n\tld l,d\n", R_HL),

	/* storing one: a pair at a time to a global, a byte at a time to
	 * a local, since only (hl) takes an immediate */
	R("=(O,H):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld ($L),de\n\tld ($L++),hl\n", 0),
	R("=(I,H):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld ($L),e\n\tld ($L+),d\n\tld ($L++),l\n\tld ($L+++),h\n", 0),
	/* the long helpers hand back a CODE that never passed through the
	 * step() loop that would have made it an INHL */
	R("=(I,C):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld ($L),e\n\tld ($L+),d\n\tld ($L++),l\n\tld ($L+++),h\n", 0),
	R("=(O,C):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld ($L),de\n\tld ($L++),hl\n", 0),
	R("=(I,N):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld ($L),$Rl\n\tld ($L+),$Rh\n"
		"\tld ($L++),$R2\n\tld ($L+++),$R3\n", 0),
	/* no ld (nn),n, so point HL at the global and walk it */
	R("=(O,N):l", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDHLL F_LDHLRL F_INCHL F_LDHLRH
		F_INCHL F_LDHLR2 F_INCHL F_LDHLR3, 0),
	/* a long already in HL:DE is the return value as it stands */
	R("=(H,C):l", ASSIGN, P_L, P_R, P_NONE, 0, "", R_HL),

	/* test a long in memory for zero */
	R("D(O):lF", DEREF, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_LDAHL F_INCHL F_ORHL F_INCHL
		F_ORHL F_INCHL F_ORHL, F_NZ),

	/*
	 * Loading one, the mirror of the stores above: the low word lives
	 * at the lower address, so it comes back in DE and the high word
	 * in HL.  A global can move a pair at a time; a frame slot goes a
	 * byte at a time, since (iy+d) is all that reaches it.
	 */
	R("D(O):l", DEREF, P_L, P_NONE, P_NONE, 0,
		F_LDDEL F_LDHLL3, R_HL),
	R("D(I):l", DEREF, P_L, P_NONE, P_NONE, 0,
		"\tld e,($L)\n\tld d,($L+)\n\tld l,($L++)\n\tld h,($L+++)\n", R_HL),
	/* through a pointer already in HL */
	R("D(H):l", DEREF, P_L, P_NONE, P_NONE, 0, "\tcall lld\n", R_HL),

	/*
	 * Stepping a long in memory.  The helper takes the address in HL,
	 * updates the value in place and hands back what was there before
	 * - which is what a postfix wants and a prefix does not, so a
	 * prefix that is used for its value reads the new one back.  As a
	 * statement, which is nearly always, there is nothing to read.
	 */
	R("j(O):l", POSTINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("m(O):l", POSTDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(O):lS", PREINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("k(O):lS", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(O):l", PREINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_PUSHBC F_CALLLAINC F_POPBC F_LDDEL F_LDHLL3, R_HL),
	R("k(O):l", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_PUSHBC F_CALLLADEC F_POPBC F_LDDEL F_LDHLL3, R_HL),
	/*
	 * The same for a frame slot, where the address has to be worked
	 * out: (iy+d) reaches a byte at a time, and the helper wants the
	 * whole address in HL.
	 */
	R("j(I):l", POSTINC, P_L, P_NONE, P_NONE, 0,
		T_IDX_ADDR F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("m(I):l", POSTDEC, P_L, P_NONE, P_NONE, 0,
		T_IDX_ADDR F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(I):lS", PREINC, P_L, P_NONE, P_NONE, 0,
		T_IDX_ADDR F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("k(I):lS", PREDEC, P_L, P_NONE, P_NONE, 0,
		T_IDX_ADDR F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(I):l", PREINC, P_L, P_NONE, P_NONE, 0,
		T_IDX_ADDR F_PUSHBC F_CALLLAINC F_POPBC T_IDX_ADDR "\tcall lld\n", R_HL),
	R("k(I):l", PREDEC, P_L, P_NONE, P_NONE, 0,
		T_IDX_ADDR F_PUSHBC F_CALLLADEC F_POPBC T_IDX_ADDR "\tcall lld\n", R_HL),
	/*
	 * And through a pointer, where the address is in HL already and
	 * there is nothing to work out - "(*lp)++", which the short forms
	 * had and these did not, so stepping a long through a pointer
	 * emitted nothing at all.
	 *
	 * A prefix used for its value has to read the long back, and the
	 * helper leaves the old one in HL:DE with the address gone, so the
	 * address is kept on the stack under the saved BC.  lld preserves
	 * BC, which is why it can come after the pop.
	 */
	R("j(H):l", POSTINC, P_L, P_NONE, P_NONE, 0,
		F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("m(H):l", POSTDEC, P_L, P_NONE, P_NONE, 0,
		F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(H):lS", PREINC, P_L, P_NONE, P_NONE, 0,
		F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("k(H):lS", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(H):l", PREINC, P_L, P_NONE, P_NONE, 0,
		F_PUSHBC F_PUSHHL F_CALLLAINC F_POPHL F_POPBC
		"\tcall lld\n", R_HL),
	R("k(H):l", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_PUSHBC F_PUSHHL F_CALLLADEC F_POPHL F_POPBC
		"\tcall lld\n", R_HL),
	/*
	 * And through a pointer kept in a register, where the DEREF is
	 * still standing because rewrite1 left it there - see the step
	 * branch that explains why.  The address is in BC and the helper
	 * wants it in HL, which is the only difference from the forms
	 * above.
	 */
	R("j(D(B)):l", POSTINC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("m(D(B)):l", POSTDEC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(D(B)):lS", PREINC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHBC F_CALLLAINC F_POPBC, R_HL),
	R("k(D(B)):lS", PREDEC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHBC F_CALLLADEC F_POPBC, R_HL),
	R("i(D(B)):l", PREINC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHBC F_CALLLAINC F_POPBC T_BC_HL
		"\tcall lld\n", R_HL),
	R("k(D(B)):l", PREDEC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHBC F_CALLLADEC F_POPBC T_BC_HL
		"\tcall lld\n", R_HL),

	/*
	 * Storing a long constant through an address, which the four
	 * (hl) writes reach whether it came from a pointer variable or
	 * was worked out.  There is no ld (nn),n for any width, so the
	 * address has to be in HL either way.
	 */
	/*
	 * Storing through a pointer that is itself a global: load the
	 * pointer, then write through it.  Only the long form of this
	 * existed, so "p[0] = c" had nowhere to go once the subscript
	 * stopped being folded into the pointer's own address.
	 *
	 * The word form has to get the value out of HL first, since that
	 * is where the address has to end up.
	 */
	R("=(D(O),N):b", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld hl,($LL)\n\tld (hl),$R\n", 0),
	R("=(D(O),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld hl,($LL)\n\tld (hl),$Rl\n" F_INCHL "\tld (hl),$Rh\n", 0),
	R("=(D(O),A):b", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld hl,($LL)\n\tld (hl),a\n", R_A),
	R("=(D(O),H):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDAL "\tld hl,($LL)\n\tld (hl),a\n", R_A),
	R("=(D(O),E):s", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld hl,($LL)\n" F_LDHLE F_INCHL F_LDHLD, R_DE),
	R("=(D(O),H):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_EXDEHL "\tld hl,($LL)\n" F_LDHLE F_INCHL F_LDHLD F_EXDEHL, R_HL),
	/*
	 * Storing a symbol's own address - which is what a string
	 * literal is, since pass1 turns one into a SYMREF for the label
	 * it emitted.  A SYMREF is left unreduced so the load and store
	 * rules can use it as an address, so where its value is wanted
	 * it has to be loaded, and these are the store shapes that had
	 * no form for it:
	 *
	 *	arr[i] = "lit";		nothing stored, marker left
	 *	sp->f  = "lit";		the same
	 *
	 * A constant subscript folds to a plain symbol store and was
	 * always right, and any non-literal value is in a register
	 * already, so this only ever showed with a literal and a
	 * subscript that had to be worked out.
	 */
	R("=(D(O),O):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDDER "\tld hl,($LL)\n" F_LDHLE F_INCHL F_LDHLD, R_DE),
	R("=(D(H),O):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDDER F_LDHLE F_INCHL F_LDHLD, R_DE),
	R("=(D(I),O):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDDER "\tld l,($LL)\n\tld h,($LL+)\n"
		F_LDHLE F_INCHL F_LDHLD, R_DE),

	R("=(D(O),N):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld hl,($LL)\n" T_ST_IHL_N, 0),
	R("=(D(H),N):l", ASSIGN, P_L, P_R, P_NONE, 0, T_ST_IHL_N, 0),
	/*
	 * A long value stored through a pointer.  The value fills HL:DE,
	 * so there is nowhere to put the address except the stack - and
	 * that is exactly how lstde wants it: value in the pair, address
	 * pushed, which it consumes.
	 *
	 * ex (sp),hl does the swap without a spare register: push the
	 * high word, load the pointer over it, then trade.
	 */
	/*
	 * lstde carries the return address in BC across the stack
	 * juggling and returns with it still there, so it needs the
	 * same guard the arithmetic helpers have.  It did not have it:
	 * a long stored through a pointer left a register variable
	 * holding the address of the instruction after the call.
	 *
	 * The save goes outside the whole sequence rather than around
	 * the call alone: lstde takes its destination off the stack, so
	 * a push bc after the pointer was put there is what it stores
	 * through.
	 */
	R("=(D(O),H):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"$[" F_PUSHHL "\tld hl,($LL)\n\tex (sp),hl\n\tcall lstde\n$]",
		R_HL),
	R("=(D(I),H):l", ASSIGN, P_L, P_R, P_NONE, 0,
		"$[" F_PUSHHL "\tld l,($LL)\n\tld h,($LL+)\n\tex (sp),hl\n"
		"\tcall lstde\n$]", R_HL),
	R("=(D(I),N):l", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 T_ST_IHL_N, 0),

	/* complement of a word; the long form is handled in rewrite.c,
	 * beside the long negation it shares its shape with */
	R("~(H):s", NOT, P_L, P_NONE, P_NONE, 0,
		F_LDAL "\tcpl\n" F_LDLA F_LDAH "\tcpl\n" F_LDHA, R_HL),
	R("~(A):b", NOT, P_L, P_NONE, P_NONE, 0, "\tcpl\n", R_A),
	/*
	 * The truth test.  "!x" is true when x is zero, so the answer is
	 * the zero flag once the value has been tested - and testing is
	 * all it takes, whatever the value is sitting in.
	 *
	 * A comparison already produces a flag and is handled before the
	 * rules run, by inverting it; this is the other half, where the
	 * operand is a value and nothing has set the flags.  It had no
	 * rule at any width but long, so every "!x" on an ordinary value
	 * reduced to nothing.
	 *
	 * The width that matters is the operand's: "!" yields an int
	 * whatever it was applied to.
	 */
	R("!(H:l)", BANG, P_L, P_NONE, P_NONE, 0,
		F_LDAH "\tor l\n\tor d\n\tor e\n", F_Z),
	R("!(H:s)", BANG, P_L, P_NONE, P_NONE, 0, T_HL_TEST, F_Z),
	R("!(H:b)", BANG, P_L, P_NONE, P_NONE, 0, F_LDAL F_ORA, F_Z),
	R("!(A)", BANG, P_L, P_NONE, P_NONE, 0, F_ORA, F_Z),
	R("!(B:s)", BANG, P_L, P_NONE, P_NONE, 0, T_BC_TEST, F_Z),
	R("!(B:b)", BANG, P_L, P_NONE, P_NONE, 0, F_LDAC F_ORA, F_Z),
	R("!(E:s)", BANG, P_L, P_NONE, P_NONE, 0, T_DE_TEST, F_Z),
	R("!(K)", BANG, P_L, P_NONE, P_NONE, 0, "\tld a,e\n" F_ORA, F_Z),
	R("!(V)", BANG, P_L, P_NONE, P_L, RF_IX, T_IX_TEST, F_Z),

	/* zero-extended loads */
	R("=(B,A)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,a\n\tld b,0\n", R_BC),
	R("=(H,A)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLA F_LDH0, R_HL),
	R("=(E,A)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld e,a\n\tld d,0\n", R_DE),

	/* register base address calculations */
	R("+(B,N)", PLUS, P_L, P_R, P_NONE, 0, F_LDLC F_LDHB F_LDDER F_ADDHLDE, R_HL),
	R("+(B,M)", PLUS, P_L, P_R, P_NONE, 0, F_LDLC F_LDHB "%(\tinc hl\n)", R_HL),
	R("+(E,N)", PLUS, P_L, P_R, P_NONE, 0, F_EXDEHL F_LDDER F_ADDHLDE, R_HL),
	R("+(E,M)", PLUS, P_L, P_R, P_NONE, 0, F_EXDEHL "%(\tinc hl\n)", R_HL),

	/* negation */
	R("g(B)", NEG, P_L, P_NONE, P_NONE, 0, F_LDA0 "\tsub c\n" F_LDLA F_LDA0 "\tsbc a,b\n" F_LDHA, R_HL),
	R("g(H)", NEG, P_L, P_NONE, P_NONE, 0, F_XORA "\tsub l\n" F_LDLA F_LDA0 "\tsbc a,h\n" F_LDHA, R_HL),
	R("g(E)", NEG, P_L, P_NONE, P_NONE, 0, F_LDA0 F_SUBE F_LDLA F_LDA0 "\tsbc a,d\n" F_LDHA, R_HL),

	/*
	 * Steps whose value nobody reads, for everything register-homed.
	 * The forms below materialise the answer in HL or A, and a bare
	 * "p++;" was paying those bytes to throw the answer away.  These
	 * must sit above the context-less forms: first match wins, and a
	 * pattern with no context suffix matches statement context too.
	 */
	R("i(B):S", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc bc\n", 0),
	R("k(B):S", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec bc\n", 0),
	R("j(B):S", POSTINC, P_L, P_NONE, P_NONE, 0, "\tinc bc\n", 0),
	R("m(B):S", POSTDEC, P_L, P_NONE, P_NONE, 0, "\tdec bc\n", 0),
	R("i(V):bS", PREINC, P_L, P_NONE, P_L, RF_B, "\tinc b\n", 0),
	R("i(V):bS", PREINC, P_L, P_NONE, P_L, RF_C, "\tinc c\n", 0),
	R("k(V):bS", PREDEC, P_L, P_NONE, P_L, RF_B, "\tdec b\n", 0),
	R("k(V):bS", PREDEC, P_L, P_NONE, P_L, RF_C, "\tdec c\n", 0),
	R("j(V):bS", POSTINC, P_L, P_NONE, P_L, RF_B, "\tinc b\n", 0),
	R("j(V):bS", POSTINC, P_L, P_NONE, P_L, RF_C, "\tinc c\n", 0),
	R("m(V):bS", POSTDEC, P_L, P_NONE, P_L, RF_B, "\tdec b\n", 0),
	R("m(V):bS", POSTDEC, P_L, P_NONE, P_L, RF_C, "\tdec c\n", 0),

	/* pre-inc/dec */
	R("i(B)", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc bc\n" F_LDLC F_LDHB, R_HL),
	R("k(B)", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec bc\n" F_LDLC F_LDHB, R_HL),
	/* inc/dec a word global in place */
	R("i(O):s", PREINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL2 F_INCHL F_LDLHL, R_HL),
	R("k(O):s", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL2 F_DECHL F_LDLHL, R_HL),
	R("j(O):s", POSTINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL2 F_INCHL F_LDLHL F_DECHL, R_HL),
	R("m(O):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL2 F_DECHL F_LDLHL F_INCHL, R_HL),

	/*
	 * inc/dec through an address in HL.  Reading the word costs the
	 * pointer, so it goes on the stack first and comes back to do the
	 * store.  Postfix then undoes the update to get its old value,
	 * the same trick the memory forms use.
	 */
	R("i(H):s", PREINC, P_L, P_NONE, P_NONE, 0,
		F_PUSHHL T_LD_IHL F_INCHL T_SWAP_ADDR T_ST_IHL, R_HL),
	R("k(H):s", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_PUSHHL T_LD_IHL F_DECHL T_SWAP_ADDR T_ST_IHL, R_HL),
	R("j(H):s", POSTINC, P_L, P_NONE, P_NONE, 0,
		F_PUSHHL T_LD_IHL F_INCHL T_SWAP_ADDR T_ST_IHL
		F_DECHL, R_HL),
	R("m(H):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
		F_PUSHHL T_LD_IHL F_DECHL T_SWAP_ADDR T_ST_IHL
		F_INCHL, R_HL),
	/*
	 * The same through a pointer kept in a register, where the DEREF
	 * survives reduction and the address is in BC.  Without these,
	 * "(*p)++" on a register pointer loaded the pointer, took that
	 * for an address and stepped what it pointed at.
	 */
	/*
	 * The byte forms, which had never existed: "(*p)++" on a byte
	 * through a register pointer emitted a marker, and through a
	 * frame pointer too.  ld a,(bc) is one of the Z80's two
	 * one-byte indirect loads, so the BC forms cost nothing extra;
	 * the HL forms read and write through (hl) directly.  Postfix
	 * parks the old value in E, which nothing owns here.
	 */
	R("i(D(B)):b", PREINC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(bc)\n\tinc a\n\tld (bc),a\n", R_A),
	R("k(D(B)):b", PREDEC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(bc)\n\tdec a\n\tld (bc),a\n", R_A),
	R("j(D(B)):b", POSTINC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(bc)\n\tld e,a\n\tinc a\n\tld (bc),a\n\tld a,e\n", R_A),
	R("m(D(B)):b", POSTDEC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(bc)\n\tld e,a\n\tdec a\n\tld (bc),a\n\tld a,e\n", R_A),
	R("i(D(H)):b", PREINC, P_L, P_NONE, P_NONE, 0,
		"\tinc (hl)\n\tld a,(hl)\n", R_A),
	R("k(D(H)):b", PREDEC, P_L, P_NONE, P_NONE, 0,
		"\tdec (hl)\n\tld a,(hl)\n", R_A),
	R("j(D(H)):b", POSTINC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tinc (hl)\n", R_A),
	R("m(D(H)):b", POSTDEC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tdec (hl)\n", R_A),
	R("i(D(B)):s", PREINC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHHL T_LD_IHL F_INCHL T_SWAP_ADDR T_ST_IHL, R_HL),
	R("k(D(B)):s", PREDEC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHHL T_LD_IHL F_DECHL T_SWAP_ADDR T_ST_IHL, R_HL),
	R("j(D(B)):s", POSTINC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHHL T_LD_IHL F_INCHL T_SWAP_ADDR T_ST_IHL
		F_DECHL, R_HL),
	R("m(D(B)):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
		T_BC_HL F_PUSHHL T_LD_IHL F_DECHL T_SWAP_ADDR T_ST_IHL
		F_INCHL, R_HL),

	/* postfix yields the old value, so read before updating */
	R("j(B)", POSTINC, P_L, P_NONE, P_NONE, 0, F_LDLC F_LDHB "\tinc bc\n", R_HL),
	R("m(B)", POSTDEC, P_L, P_NONE, P_NONE, 0, F_LDLC F_LDHB "\tdec bc\n", R_HL),
	/*
	 * Stepping a pointer the allocator homed in IX.  The BC pointer
	 * has had these all along; through-IX steps existed only fused
	 * with a deref, so "p++" standing alone matched nothing, and a
	 * scan loop over a register pointer read one byte forever - a
	 * marker in the listing, an infinite loop on the machine.  The
	 * statement forms come first: a step nobody wants the value of
	 * is inc ix and not a byte more.
	 */
	R("i(V):S", PREINC, P_L, P_NONE, P_L, RF_IX, "\tinc ix\n", 0),
	R("k(V):S", PREDEC, P_L, P_NONE, P_L, RF_IX, "\tdec ix\n", 0),
	R("j(V):S", POSTINC, P_L, P_NONE, P_L, RF_IX, "\tinc ix\n", 0),
	R("m(V):S", POSTDEC, P_L, P_NONE, P_L, RF_IX, "\tdec ix\n", 0),
	R("i(V)", PREINC, P_L, P_NONE, P_L, RF_IX,
		"\tinc ix\n\tpush ix\n" F_POPHL, R_HL),
	R("k(V)", PREDEC, P_L, P_NONE, P_L, RF_IX,
		"\tdec ix\n\tpush ix\n" F_POPHL, R_HL),
	R("j(V)", POSTINC, P_L, P_NONE, P_L, RF_IX,
		"\tpush ix\n" F_POPHL "\tinc ix\n", R_HL),
	R("m(V)", POSTDEC, P_L, P_NONE, P_L, RF_IX,
		"\tpush ix\n" F_POPHL "\tdec ix\n", R_HL),
	/*
	 * Postfix on a word in memory.  The old value is wanted as the
	 * result and the new one in store, and rather than hold both, the
	 * update is undone afterwards - one byte, against a push/pop pair
	 * or a shuffle through DE.
	 */
	R("j(I):s", POSTINC, P_L, P_NONE, P_NONE, 0,
	  "\tld $t,($L)\n" F_LDUL "\tinc $T\n"
	  "\tld ($L),$t\n\tld ($L+),$u\n\tdec $T\n", 0),
	R("m(I):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
	  "\tld $t,($L)\n" F_LDUL "\tdec $T\n"
	  "\tld ($L),$t\n\tld ($L+),$u\n\tinc $T\n", 0),
	/* unary: operand is the LEFT child (T_IDX_S_LD reads $R) */
	R("i(I):s", PREINC, P_L, P_NONE, P_NONE, 0,
	  "\tld l,($L)\n\tld h,($L+)\n" F_INCHL T_IDX_S_ST, R_HL),
	R("k(I):s", PREDEC, P_L, P_NONE, P_NONE, 0,
	  "\tld l,($L)\n\tld h,($L+)\n" F_DECHL T_IDX_S_ST, R_HL),
	/*
	 * A byte step on a frame slot needs no register at all: inc/dec
	 * (iy+d) is one read-modify-write and it sets Z itself.  Where
	 * the value is thrown away that is the whole job, and where only
	 * the flags are wanted the prefix forms answer in NZ directly -
	 * "while (--n)" on a frame byte is dec (iy+d) / jp z, not the
	 * eleven-byte load, dec a, store, or a dance the value rules
	 * make.  A postfix in flag context answers with the value from
	 * before the step, so the load still happens and or a asks it.
	 */
	R("i(I):bS", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc ($L)\n", 0),
	R("k(I):bS", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec ($L)\n", 0),
	R("j(I):bS", POSTINC, P_L, P_NONE, P_NONE, 0, "\tinc ($L)\n", 0),
	R("m(I):bS", POSTDEC, P_L, P_NONE, P_NONE, 0, "\tdec ($L)\n", 0),
	R("i(I):bF", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc ($L)\n", F_NZ),
	R("k(I):bF", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec ($L)\n", F_NZ),
	R("j(I):bF", POSTINC, P_L, P_NONE, P_NONE, 0,
	  F_LDAL1 "\tinc ($L)\n" F_ORA, F_NZ),
	R("m(I):bF", POSTDEC, P_L, P_NONE, P_NONE, 0,
	  F_LDAL1 "\tdec ($L)\n" F_ORA, F_NZ),
	R("i(I):b", PREINC, P_L, P_NONE, P_NONE, 0, F_LDAL1 "\tinc a\n" F_LDLA1, R_A),
	R("k(I):b", PREDEC, P_L, P_NONE, P_NONE, 0, F_LDAL1 "\tdec a\n" F_LDLA1, R_A),
	/* a postfix wants the value from before, so the step happens in
	 * memory and the load beats it there */
	R("j(I):b", POSTINC, P_L, P_NONE, P_NONE, 0, F_LDAL1 "\tinc ($L)\n", R_A),
	R("m(I):b", POSTDEC, P_L, P_NONE, P_NONE, 0, F_LDAL1 "\tdec ($L)\n", R_A),

	/*
	 * The same through a pointer held in IX, where the member is the
	 * first one and so needs no offset to add - "m->parmcount++"
	 * with m in IX.  A member further in becomes +(D(V),N) and folds
	 * to an INDEX, which the two rules above already take; only the
	 * offset-free one arrived as a bare DEREF and matched nothing.
	 *
	 * It emitted no code and no marker, because the store above it
	 * matched anyway - which is how cpp's macdefine came to write a
	 * macro parameter through a stale HL.
	 */
	R("j(D(V)):b", POSTINC, P_L, P_NONE, P_LL, RF_IX,
		"\tld a,(ix+0)\n\tinc (ix+0)\n", R_A),
	R("m(D(V)):b", POSTDEC, P_L, P_NONE, P_LL, RF_IX,
		"\tld a,(ix+0)\n\tdec (ix+0)\n", R_A),
	R("i(D(V)):b", PREINC, P_L, P_NONE, P_LL, RF_IX,
		"\tinc (ix+0)\n\tld a,(ix+0)\n", R_A),
	R("k(D(V)):b", PREDEC, P_L, P_NONE, P_LL, RF_IX,
		"\tdec (ix+0)\n\tld a,(ix+0)\n", R_A),

	/*
	 * Stepping a byte at a global.  There is no inc (nn), but there
	 * is inc (hl), so the address goes in HL and the step happens in
	 * memory - four bytes against the seven that loading, adding and
	 * storing would take.
	 *
	 * Which side of the step the load falls on is the whole
	 * difference between prefix and postfix, and a statement wants
	 * neither: it is only the step.
	 */
	R("i(O):bF", PREINC, P_L, P_NONE, P_NONE, 0, F_LDHLL "\tinc (hl)\n", F_NZ),
	R("k(O):bF", PREDEC, P_L, P_NONE, P_NONE, 0, F_LDHLL "\tdec (hl)\n", F_NZ),
	R("i(O):bS", PREINC, P_L, P_NONE, P_NONE, 0, F_LDHLL "\tinc (hl)\n", 0),
	R("k(O):bS", PREDEC, P_L, P_NONE, P_NONE, 0, F_LDHLL "\tdec (hl)\n", 0),
	R("j(O):bS", POSTINC, P_L, P_NONE, P_NONE, 0, F_LDHLL "\tinc (hl)\n", 0),
	R("m(O):bS", POSTDEC, P_L, P_NONE, P_NONE, 0, F_LDHLL "\tdec (hl)\n", 0),
	R("j(O):b", POSTINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_LDAHL "\tinc (hl)\n", R_A),
	R("m(O):b", POSTDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL F_LDAHL "\tdec (hl)\n", R_A),
	R("i(O):b", PREINC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL "\tinc (hl)\n" F_LDAHL, R_A),
	R("k(O):b", PREDEC, P_L, P_NONE, P_NONE, 0,
		F_LDHLL "\tdec (hl)\n" F_LDAHL, R_A),

	/* byte stores */
	R("=(I,A)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLA1, R_A),
	R("=(H,N)", ASSIGN, P_L, P_R, P_NONE, 0, NULL, 0),
	R("=(H,A):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLA, R_HL),
	R("=(H,V):b", ASSIGN, P_L, P_R, P_R, RF_BC, "\tld (hl),c\n", 0),

	/* loads from register addresses */
	R("D(H):b", DEREF, P_L, P_NONE, P_NONE, 0, F_LDAHL, R_A),
	/*
	 * Load a word through HL.  Where the result is wanted in DE it
	 * can go straight there - three bytes instead of four, no
	 * exchange, and A is left alone.  Only when it has to land back
	 * in HL does A get used as the carrier, because then the pointer
	 * and the result are the same register.
	 */
	R("D(H):s", DEREF, P_L, P_NONE, P_NONE, RF_TDE,
		"\tld e,(hl)\n" F_INCHL "\tld d,(hl)\n", R_DE),
	R("D(H):s", DEREF, P_L, P_NONE, P_NONE, 0, T_LD_IHL, R_HL),
	R("D(B):b", DEREF, P_L, P_NONE, P_NONE, 0, F_LDLC F_LDHB F_LDAHL, R_A),
	/*
	 * Reading through BC when the answer is wanted in DE, which is
	 * where the right operand of a binary node is asked to go.  The
	 * rule below moves the pointer into HL to read through it and
	 * lands there, whatever it was asked for - so as the right
	 * operand of a comparison it overwrote the left one, and then
	 * nothing matched a comparison of HL against HL.  The label was
	 * right all along; the rule simply could not do as it was told.
	 *
	 * ld a,(bc) reads without HL at all.  BC steps forward for the
	 * high byte and back again, so the pointer is as it was.
	 */
	R("D(B):s", DEREF, P_L, P_NONE, P_NONE, RF_TDE,
		"\tld a,(bc)\n\tld e,a\n\tinc bc\n"
		"\tld a,(bc)\n\tld d,a\n\tdec bc\n", R_DE),
	R("D(B):s", DEREF, P_L, P_NONE, P_NONE, 0, F_LDLC F_LDHB F_LDAHL F_INCHL F_LDHHL F_LDLA, R_HL),
	R("D(E):b", DEREF, P_L, P_NONE, P_NONE, 0, F_EXDEHL F_LDAHL, R_A),
	R("D(E):s", DEREF, P_L, P_NONE, P_NONE, 0, F_EXDEHL "\tld e,(hl)\n" F_INCHL "\tld d,(hl)\n" F_EXDEHL, R_HL),

	/* indirect stores via registers */
	R("=(D(B),A):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLC F_LDHB F_LDHLA, 0),
	R("=(D(E),A):b", ASSIGN, P_L, P_R, P_NONE, 0, F_EXDEHL F_LDHLA F_EXDEHL, 0),
	R("=(D(B),H):s", ASSIGN, P_L, P_R, P_NONE, 0, F_PUSHHL F_LDLC F_LDHB F_POPDE F_LDHLE F_INCHL F_LDHLD, 0),
	R("=(D(E),H):s", ASSIGN, P_L, P_R, P_NONE, 0, F_EXDEHL "\tpush de\n" F_LDHLE F_INCHL F_LDHLD F_POPHL, 0),

	/*
	 * Store through a struct pointer in IX.  Only offset zero lands
	 * here - a non-zero member offset folds into an INDEX first via
	 * +(V,N), which is why these are the forms that were missing.
	 */
	R("=(D(V),N):s", ASSIGN, P_L, P_R, P_LL, RF_IX,
		"\tld (ix+0),$Rl\n\tld (ix+1),$Rh\n", 0),
	R("=(D(V),H):s", ASSIGN, P_L, P_R, P_LL, RF_IX,
		"\tld (ix+0),l\n\tld (ix+1),h\n", 0),
	R("=(D(V),B):s", ASSIGN, P_L, P_R, P_LL, RF_IX,
		"\tld (ix+0),c\n\tld (ix+1),b\n", 0),
	R("=(D(V),N):b", ASSIGN, P_L, P_R, P_LL, RF_IX, "\tld (ix+0),$R\n", 0),
	R("=(D(V),A):b", ASSIGN, P_L, P_R, P_LL, RF_IX, "\tld (ix+0),a\n", 0),
	/* a word in HL narrowed on its way through the index register */
	R("=(D(V),H):b", ASSIGN, P_L, P_R, P_LL, RF_IX,
		F_LDAL "\tld (ix+0),a\n", R_A),
	/*
	 * And the same from BC, which is where a register variable
	 * lives - the third form these rules keep needing.  Without it
	 *
	 *	t->size = off;
	 *
	 * with size a byte member and off a local word left a marker
	 * and stored nothing, so type.c's member offsets stayed zero
	 * and every struct came out either empty or "too big".
	 */
	R("=(D(V),B):b", ASSIGN, P_L, P_R, P_LL, RF_IX,
		F_LDAC "\tld (ix+0),a\n", R_A),
	R("=(D(V),E):b", ASSIGN, P_L, P_R, P_LL, RF_IX,
		"\tld a,e\n\tld (ix+0),a\n", R_A),

	/*
	 * The index register stored through a pointer, rather than used
	 * as one.  It goes out through DE.
	 *
	 * The half-register forms are fine to use - they work on every
	 * Z80 - and this file uses them wherever they win: ld a,ixl for
	 * a single byte, and ld c,ixl / ld e,ixl to reach BC and DE.
	 * What they cannot do is reach HL, because the DD prefix renames
	 * H and L to IXH and IXL for the whole instruction, so "ld l,ixl"
	 * has no encoding at all - the assembler rejects it.  HL can only
	 * be had through the stack.
	 *
	 * For a pair the stack is smaller anyway: push ix / pop de and
	 * the three stores is six bytes, against seven for ld e,ixl /
	 * ld d,ixh, each half costing two for its prefix.  Going through
	 * A a byte at a time is seven as well.
	 */
	R("=(D(H),V):s", ASSIGN, P_L, P_R, P_R, RF_IX,
		"\tpush ix\n\tpop de\n" F_LDHLE F_INCHL F_LDHLD, 0),
	R("=(D(H),V):b", ASSIGN, P_L, P_R, P_R, RF_IX,
		"\tld a,ixl\n" F_LDHLA, R_A),
	R("=(D(B),V):s", ASSIGN, P_L, P_R, P_R, RF_IX,
		T_BC_HL "\tpush ix\n\tpop de\n" F_LDHLE F_INCHL F_LDHLD, 0),
	R("=(D(I),V):s", ASSIGN, P_L, P_R, P_R, RF_IX,
		F_LDLLL F_LDHLL1 "\tpush ix\n\tpop de\n"
		F_LDHLE F_INCHL F_LDHLD, 0),
	R("=(D(O),V):s", ASSIGN, P_L, P_R, P_R, RF_IX,
		"\tld hl,($LL)\n\tpush ix\n\tpop de\n"
		F_LDHLE F_INCHL F_LDHLD, 0),

	/*
	 * Store through a pointer that itself lives in memory - a pointer
	 * parameter, say.  Load it first; the HL form has to shuffle
	 * through DE because HL is holding the value.
	 */
	R("=(D(I),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 F_LDHLRL F_INCHL F_LDHLRH, 0),
	R("=(D(I),H):s", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld e,($LL)\n\tld d,($LL+)\n" F_EXDEHL F_LDHLE F_INCHL F_LDHLD, 0),
	R("=(D(I),N):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 "\tld (hl),$R\n", 0),
	R("=(D(I),A):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 F_LDHLA, 0),
	/*
	 * The same with the value in HL, narrowing to its low byte.  The
	 * word form above is here and the byte one was not, so storing a
	 * byte through a pointer in a frame slot emitted nothing.  Take
	 * the byte out of L before loading the pointer, which wants HL.
	 */
	R("=(D(I),H):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDAL F_LDLLL F_LDHLL1 F_LDHLA, R_A),
	/*
	 * A register variable stored through a pointer in a frame slot,
	 * narrowing on the way - "where[0] = v" with v in BC.  Load the
	 * value out of BC before the pointer, since the pointer wants HL.
	 */
	R("=(D(I),B):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDAC F_LDLLL F_LDHLL1 F_LDHLA, R_A),
	R("=(D(I),B):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 "\tld (hl),c\n" F_INCHL "\tld (hl),b\n", R_BC),
	R("=(D(I),E):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 F_LDHLE, R_DE),
	R("=(D(I),E):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDLLL F_LDHLL1 F_LDHLE F_INCHL F_LDHLD, R_DE),

	/* indirect stores via HL */
	R("=(D(H),N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),$R\n", 0),
	R("=(D(H),E):s", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLE F_INCHL F_LDHLD, 0),
	R("=(D(H),E):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLE, 0),
	/* a byte worked out in A, stored through an address in HL - which
	 * is where a compound assignment through a computed address ends
	 * up, the value in A and the address recovered from the stack */
	R("=(D(H),A):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLA, R_A),
	/* a word narrowed to a byte on its way through an address */
	R("=(D(H),B):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAC F_LDHLA, R_A),
	R("=(D(H),H):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAL F_LDHLA, R_A),
	/*
	 * Stepping what an address in HL points at - "p[i]++" once the
	 * subscript has been worked out.  A postfix wants the value from
	 * before, which is what is already in A after the load, so it can
	 * step memory directly and is a byte shorter.
	 */
	R("j(H):b", POSTINC, P_L, P_NONE, P_NONE, 0, "\tld a,(hl)\n\tinc (hl)\n", R_A),
	R("m(H):b", POSTDEC, P_L, P_NONE, P_NONE, 0, "\tld a,(hl)\n\tdec (hl)\n", R_A),
	R("i(H):b", PREINC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tinc a\n" F_LDHLA, R_A),
	R("k(H):b", PREDEC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tdec a\n" F_LDHLA, R_A),
	/*
	 * The word forms carry between the halves, so they go through A
	 * rather than inc (hl) and a branch.  They leave HL on the high
	 * byte and so say nothing about the value: statements only.
	 */
	R("i(H):sS", PREINC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tadd a,1\n" F_LDHLA F_INCHL
		"\tld a,(hl)\n\tadc a,0\n" F_LDHLA, 0),
	R("k(H):sS", PREDEC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tsub 1\n" F_LDHLA F_INCHL
		"\tld a,(hl)\n\tsbc a,0\n" F_LDHLA, 0),
	R("j(H):sS", POSTINC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tadd a,1\n" F_LDHLA F_INCHL
		"\tld a,(hl)\n\tadc a,0\n" F_LDHLA, 0),
	R("m(H):sS", POSTDEC, P_L, P_NONE, P_NONE, 0,
		"\tld a,(hl)\n\tsub 1\n" F_LDHLA F_INCHL
		"\tld a,(hl)\n\tsbc a,0\n" F_LDHLA, 0),
	R("=(D(B),N):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDLC F_LDHB "\tld (hl),$R\n", 0),
	/* the value in HL, which has to be taken out of the way before the
	 * pointer comes over on top of it */
	R("=(D(B),H):b", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDAL F_LDLC F_LDHB F_LDHLA, R_A),
	R("=(D(B),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDHLRL F_INCHL F_LDHLRH, 0),
	R("=(D(E),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		F_EXDEHL F_LDHLRL F_INCHL F_LDHLRH, 0),
	R("=(D(H),N):s", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLRL F_INCHL F_LDHLRH, 0),
	R("=(D(H),B):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),c\n" F_INCHL "\tld (hl),b\n", 0),

	/* pointer testing */
	R("D(H):pF", DEREF, P_L, P_NONE, P_NONE, 0, F_LDAHL "\tor (hl)\n", F_NZ),

	/* structured loads to BC/DE/HL */
	R("=(B,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,(hl)\n" F_INCHL "\tld b,(hl)\n", R_BC),
	/*
	 * A register given a frame slot's address - the other half of
	 * "p = &v", where p happens to live in a register.  As above, a
	 * bare index is a place: these loaded through it and turned every
	 * address-of into a read of what was there.
	 */
	R("=(B,I)", ASSIGN, P_L, P_R, P_NONE, 0,
		T_IDX_R_ADDR "\tld c,l\n\tld b,h\n", R_BC),
	R("=(H,I)", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_R_ADDR, R_HL),
	R("=(E,I)", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_R_ADDR F_EXDEHL, R_DE),
	R("=(I,O)", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDHLR T_IDX_S_ST, R_HL),
	/* one symbol's address into another symbol's storage - "lp = &g" */
	R("=(O,O)", ASSIGN, P_L, P_R, P_NONE, 0,
		F_LDHLR F_LDLHL, R_HL),
	R("=(B,O)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld bc,$R\n", R_BC),
	R("=(H,O)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDHLR, R_HL),
	R("=(E,O)", ASSIGN, P_L, P_R, P_NONE, 0, F_LDDER, R_DE),
	R("=(B,D(O)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n\tld c,a\n\tld a,($RL+)\n\tld b,a\n", R_BC),
	R("=(E,D(O)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld de,($RL)\n", R_DE),
	R("=(H,D(O)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld hl,($RL)\n", R_HL),
	R("=(E,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld e,(hl)\n" F_INCHL "\tld d,(hl)\n", R_DE),
	R("=(H,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAHL F_INCHL F_LDHHL F_LDLA, R_HL),
	R("=(I,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAHL F_LDLA1 F_INCHL F_LDAHL "\tld ($L+),a\n", 0),

	/* arithmetic/logical on indexed */
	R("o(H,N):b", OREQ, P_L, P_R, P_NONE, 0, F_LDAHL "\tor $R\n" F_LDHLA, R_A),
	R("o(I,K):b", OREQ, P_L, P_R, P_NONE, 0, F_LDAL1 "\tor e\n" F_LDLA1, R_A),
	/*
	 * A frame variable as a truth value.  The flag named here is the
	 * one that means true, and true is "not zero" - these said Z, so
	 * "if (local)" ran its body exactly when the local was zero.
	 * The register-variable rule above has always said NZ, which is
	 * what made the disagreement visible.
	 */
	R("D(I):bF", DEREF, P_L, P_NONE, P_NONE, 0, F_LDAL1 F_ORA, F_NZ),
	R("D(I):sF", DEREF, P_L, P_NONE, P_NONE, 0, F_LDAL1 "\tor ($L+)\n", F_NZ),
	R("D(I):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tld $t,($L)\n" F_LDUL, 0),
	/*
	 * Of the 8-bit registers only A can load from an absolute address,
	 * but the pairs all can, so ld de,(nn) reaches E without touching
	 * A or HL - either of which may hold the left operand.  It reads
	 * the following byte into D as well; D is dead here, and one byte
	 * of over-read is harmless in a flat memory model.
	 */
	R("D(O):b", DEREF, P_L, P_NONE, P_NONE, RF_TDE, F_LDDEL, R_E),
	R("D(O):b", DEREF, P_L, P_NONE, P_NONE, 0, F_LDAL1, R_A),
	R("D(I):b", DEREF, P_L, P_NONE, P_NONE, RF_TDE, "\tld e,($L)\n", R_E),
	/* the plain load: a byte local reaching A, as D(O):b does for a
	 * global.  Without it a byte local could only be read by the rules
	 * that match its parent too, so SEXT of one had nothing to widen */
	R("D(I):b", DEREF, P_L, P_NONE, P_NONE, 0, F_LDAL1, R_A),
	/* a byte through a struct pointer in IX at offset zero - a
	 * non-zero member offset folds into an INDEX first */
	R("D(V):b", DEREF, P_L, P_NONE, P_L, RF_IX, "\tld a,(ix+0)\n", R_A),
	R("D(V):s", DEREF, P_L, P_NONE, P_L, RF_IX,
		"\tld $t,(ix+0)\n\tld $u,(ix+1)\n", 0),
	/* honour the target: as the right operand of a compare this has to
	 * land in DE, or it overwrites the left operand in HL */
	R("D(O):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tld $T,($L)\n", 0),

	/* 16-bit binary arithmetic */
	R("+(H,E)", PLUS, P_L, P_R, P_NONE, 0, T_ADD_HL_DE, R_HL),
	R("+(H,B)", PLUS, P_L, P_R, P_NONE, 0, "\tadd hl,bc\n", R_HL),
	R("-(H,B)", MINUS, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLBC, R_HL),
	/* a constant less a register variable - "5 - n".  The subtraction
	 * is not commutative, so normalize leaves the constant on the
	 * left and there was no form with it there. */
	R("-(N,B)", MINUS, P_L, P_R, P_NONE, 0,
		"\tld hl,$L\n" F_ORA F_SBCHLBC, R_HL),
	/*
	 * And with the value in DE or HL, which is what "0 - (n % 10)"
	 * reduces to: the helper's answer moved aside, the constant on
	 * the left where normalize keeps it.  outd() spells its digits
	 * exactly that way, so the self-built c1 printed every operand
	 * of every instruction as an empty string.
	 */
	R("-(N,E)", MINUS, P_L, P_R, P_NONE, 0,
		"\tld hl,$L\n" F_ORA F_SBCHLDE, R_HL),
	R("-(N,H)", MINUS, P_L, P_R, P_NONE, 0,
		"\tex de,hl\n\tld hl,$L\n" F_ORA F_SBCHLDE, R_HL),
	R("-(N,K):b", MINUS, P_L, P_R, P_NONE, 0,
		"\tld a,$L\n" F_SUBE, R_A),
	R("-(N,A):b", MINUS, P_L, P_R, P_NONE, 0,
		"\tld e,a\n\tld a,$L\n" F_SUBE, R_A),
	R("+(B,E)", PLUS, P_L, P_R, P_NONE, 0, T_BC_HL T_ADD_HL_DE, R_HL),
	R("-(B,E)", MINUS, P_L, P_R, P_NONE, 0, T_BC_HL F_ORA F_SBCHLDE, R_HL),
	/*
	 * A pointer in BC less the address of an array - "s - buf", the
	 * ordinary pointer difference, when the array is declared with no
	 * size.  Given a size, pass1 puts a conversion over the symbol
	 * and this arrives as -(B,E) above; an array of unknown size has
	 * size zero, the conversion is not inserted because nothing looks
	 * wider than nothing, and the symbol reaches here bare.
	 */
	R("-(B,O)", MINUS, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE, R_HL),
	R("-(B,N)", MINUS, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE, R_HL),
	R("<(B,N)", LSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(" T_ADD_HL_HL ")", R_HL),
	R("/(B,N)", DIV, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_LDDER "$[\tcall adiv\n$]", R_HL),
	R("/(B,N)", DIV, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER "$[\tcall ldiv\n$]", R_HL),
	R("%(B,N)", MOD, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_LDDER "$[\tcall amod\n$]", R_HL),
	R("%(B,N)", MOD, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER "$[\tcall lmod\n$]", R_HL),
	R("*(B,N)", STAR, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER "$[\tcall amul\n$]", R_HL),
	R("+(H,M)", PLUS, P_L, P_R, P_NONE, 0, "%(\tinc hl\n)", R_HL),
	R("-(H,M)", MINUS, P_L, P_R, P_NONE, 0, "%(\tdec hl\n)", R_HL),
	R("+(A,M)", PLUS, P_L, P_R, P_NONE, 0, "%(\tinc a\n)", R_A),
	R("-(A,M)", MINUS, P_L, P_R, P_NONE, 0, "%(\tdec a\n)", R_A),
	R("+(H,N)", PLUS, P_L, P_R, P_NONE, 0, F_LDDER T_ADD_HL_DE, R_HL),
	/*
	 * A byte in A against a constant, once it is too big for the inc
	 * and dec runs above.  Only at byte width: at word width A holds
	 * the low half and the carry would have nowhere to go.
	 */
	R("+(A,N):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,$R\n", R_A),
	R("+(D(I),N):b", PLUS, P_L, P_R, P_NONE, 0, F_LDALL "\tadd a,$R\n", R_A),
	R("-(A,N):b", MINUS, P_L, P_R, P_NONE, 0, F_SUBR, R_A),
	R("-(D(I),N):b", MINUS, P_L, P_R, P_NONE, 0, F_LDALL F_SUBR, R_A),
	R("-(H,E)", MINUS, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLDE, R_HL),
	/* less a symbol's address, which is one half of a pointer
	 * difference once the other half is in HL */
	R("-(H,O)", MINUS, P_L, P_R, P_NONE, 0,
		F_LDDER F_ORA F_SBCHLDE, R_HL),
	R("+(H,O)", PLUS, P_L, P_R, P_NONE, 0, F_LDDER F_ADDHLDE, R_HL),
	R("-(H,N)", MINUS, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, R_HL),
	/* and against a frame slot, which the other four widths of this
	 * had and the subtraction did not */
	R("-(H,I)", MINUS, P_L, P_R, P_NONE, 0,
		"\tld e,($R)\n\tld d,($R+)\n" F_ORA F_SBCHLDE, R_HL),

	/* shifts */
	R("<(H,N)", LSHIFT, P_L, P_R, P_NONE, 0, "%(" T_ADD_HL_HL ")", R_HL),
	R("<(A,N):b", LSHIFT, P_L, P_R, P_NONE, 0, "%(\tsla a\n)", R_A),
	/*
	 * A byte shifted by a count only known at runtime - "1 << n",
	 * the ordinary way to make a mask.  Every other shift here
	 * spells itself out, which only works for a constant count.
	 *
	 * The count is in E and the value in A, so B does the counting
	 * and djnz walks it: one more than the count, then a jump
	 * straight to the djnz, so a count of nought runs the body no
	 * times rather than once.  Displacements are from each jump
	 * itself - sla a is two bytes, so the entry jump clears it and
	 * the djnz reaches back over it.
	 */
	R("<(N,K):b", LSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,e\n\tld a,$L\n\tinc b\n"
		"\tjr $$+4\n\tsla a\n\tdjnz $$-2\n$]", R_A),
	/*
	 * The same count arriving as a word, which is what an expression
	 * count reduces to: "1 << (idx & 7)" works the AND out in HL and
	 * the target machinery may swap it to DE.  Only the low byte
	 * counts - a shift past eight of a byte is zero anyway, and the
	 * loop delivers exactly that.  Without these two forms the shift
	 * matched nothing, and matched nothing SILENTLY: the compound
	 * assignment above it stored the count itself through the
	 * address, so "map[i] |= 1 << (n & 7)" wrote n's low byte into
	 * the bitmap and pass1's else-if bookkeeping read garbage.
	 */
	R("<(N,E):b", LSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,e\n\tld a,$L\n\tinc b\n"
		"\tjr $$+4\n\tsla a\n\tdjnz $$-2\n$]", R_A),
	R("<(N,H):b", LSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,l\n\tld a,$L\n\tinc b\n"
		"\tjr $$+4\n\tsla a\n\tdjnz $$-2\n$]", R_A),
	/*
	 * A byte VARIABLE shifted by a runtime count - the value already
	 * reduced into A, the count into E, DE or HL.  The docompound
	 * marker found these missing: "m[i++] >>= n" reduced its value
	 * and its count and then had no rule to put them together.
	 * Right shifts come in two kinds and the signed one must be
	 * first, RF_SIGNL deciding: sra copies the sign bit back in
	 * where srl feeds zero.
	 */
	R("<(A,K):b", LSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,e\n\tinc b\n"
		"\tjr $$+4\n\tsla a\n\tdjnz $$-2\n$]", R_A),
	R("<(A,E):b", LSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,e\n\tinc b\n"
		"\tjr $$+4\n\tsla a\n\tdjnz $$-2\n$]", R_A),
	R("<(A,H):b", LSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,l\n\tinc b\n"
		"\tjr $$+4\n\tsla a\n\tdjnz $$-2\n$]", R_A),
	R(">(A,K):b", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		"$[\tld b,e\n\tinc b\n"
		"\tjr $$+4\n\tsra a\n\tdjnz $$-2\n$]", R_A),
	R(">(A,E):b", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		"$[\tld b,e\n\tinc b\n"
		"\tjr $$+4\n\tsra a\n\tdjnz $$-2\n$]", R_A),
	R(">(A,H):b", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		"$[\tld b,l\n\tinc b\n"
		"\tjr $$+4\n\tsra a\n\tdjnz $$-2\n$]", R_A),
	R(">(A,K):b", RSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,e\n\tinc b\n"
		"\tjr $$+4\n\tsrl a\n\tdjnz $$-2\n$]", R_A),
	R(">(A,E):b", RSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,e\n\tinc b\n"
		"\tjr $$+4\n\tsrl a\n\tdjnz $$-2\n$]", R_A),
	R(">(A,H):b", RSHIFT, P_L, P_R, P_NONE, 0,
		"$[\tld b,l\n\tinc b\n"
		"\tjr $$+4\n\tsrl a\n\tdjnz $$-2\n$]", R_A),
	/*
	 * A signed right shift keeps the sign: sra copies bit 7 back into
	 * itself where srl feeds in a zero.  The signed rule has to come
	 * first, since the unsigned pattern matches either width.
	 */
	R(">(A,N):b", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL, "%(\tsra a\n)", R_A),
	R(">(A,N):b", RSHIFT, P_L, P_R, P_NONE, 0, "%(\tsrl a\n)", R_A),
	/*
	 * A shift by a whole byte is a register move, not a loop - two
	 * bytes against the thirty-two the repeated form would emit.  The
	 * signed right shift has to put the sign back, since the byte
	 * that moved down carries it.
	 */
	R(">(H,8)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld l,h\n" F_LDAH F_RLA F_SBCAA F_LDHA, R_HL),
	R(">(H,8)", RSHIFT, P_L, P_R, P_NONE, 0, "\tld l,h\n" F_LDH0, R_HL),
	R("<(H,8)", LSHIFT, P_L, P_R, P_NONE, 0, "\tld h,l\n\tld l,0\n", R_HL),
	R(">(H,M)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL, "%(\tsra h\n\trr l\n)", R_HL),
	R(">(H,M)", RSHIFT, P_L, P_R, P_NONE, 0, "%(\tsrl h\n\trr l\n)", R_HL),
	R(">(B,M)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL "%(\tsra h\n\trr l\n)", R_HL),
	R(">(B,M)", RSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(\tsrl h\n\trr l\n)", R_HL),
	/*
	 * By any other count.  M is one to four and 8 has a form of its
	 * own, so a shift by five, six, seven or more than eight matched
	 * nothing at all - the left shifts have taken any count all
	 * along, and these stopped at four.
	 */
	R(">(H,N)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL, "%(\tsra h\n\trr l\n)", R_HL),
	R(">(H,N)", RSHIFT, P_L, P_R, P_NONE, 0, "%(\tsrl h\n\trr l\n)", R_HL),
	R(">(B,N)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL "%(\tsra h\n\trr l\n)", R_HL),
	R(">(B,N)", RSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(\tsrl h\n\trr l\n)", R_HL),

	/* stores/loads with indexed/symref */
	R("=(A,D(I)):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n", R_A),
	R("=(A,D(O)):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n", R_A),
	R("=(A,A)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_A),
	R("=(A,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n", R_A),
	/*
	 * A byte wanted in A that came back in HL, which is where the
	 * wrapper that lands a value puts it.  Only the low half is
	 * meaningful at this width, so it is one instruction - and it
	 * was the single most repeated thing the compiler could not do.
	 */
	R("=(A,H):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAL, R_A),
	R("=(A,B):b", ASSIGN, P_L, P_R, P_NONE, 0, F_LDAC, R_A),
	R("=(A,E):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,e\n", R_A),
	/* a byte in A stepped in place */
	/*
	 * Stepping a byte that lives in B or C, in place.
	 *
	 * These have to come before the A forms below.  A byte register
	 * variable reduces into A to be worked on, and "k(A):b" then
	 * steps it there and calls that the answer - but A is a copy, and
	 * the variable never changed.  "while (--n)" on one of these did
	 * not terminate.
	 *
	 * dec b and dec c set Z themselves, so the flag forms need
	 * nothing after them.  That is also what djnz is: dec b and a
	 * relative jump, in one instruction and one byte less.
	 */
	R("i(V):bF", PREINC, P_L, P_NONE, P_L, RF_B, "\tinc b\n", F_NZ),
	R("i(V):bF", PREINC, P_L, P_NONE, P_L, RF_C, "\tinc c\n", F_NZ),
	R("k(V):bF", PREDEC, P_L, P_NONE, P_L, RF_B, "\tdec b\n", F_NZ),
	R("k(V):bF", PREDEC, P_L, P_NONE, P_L, RF_C, "\tdec c\n", F_NZ),
	R("i(V):b", PREINC, P_L, P_NONE, P_L, RF_B, "\tinc b\n\tld a,b\n", R_A),
	R("i(V):b", PREINC, P_L, P_NONE, P_L, RF_C, "\tinc c\n\tld a,c\n", R_A),
	R("k(V):b", PREDEC, P_L, P_NONE, P_L, RF_B, "\tdec b\n\tld a,b\n", R_A),
	R("k(V):b", PREDEC, P_L, P_NONE, P_L, RF_C, "\tdec c\n\tld a,c\n", R_A),
	/* postfix wants the old value, so take a copy before stepping */
	R("j(V):b", POSTINC, P_L, P_NONE, P_L, RF_B, "\tld a,b\n\tinc b\n", R_A),
	R("j(V):b", POSTINC, P_L, P_NONE, P_L, RF_C, "\tld a,c\n\tinc c\n", R_A),
	R("m(V):b", POSTDEC, P_L, P_NONE, P_L, RF_B, "\tld a,b\n\tdec b\n", R_A),
	R("m(V):b", POSTDEC, P_L, P_NONE, P_L, RF_C, "\tld a,c\n\tdec c\n", R_A),
	R("i(A):b", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc a\n", R_A),
	R("k(A):b", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec a\n", R_A),
	/* storing a word already in DE to a global, so a nested
	 * assignment can be used for its value */
	R("=(O,E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),de\n", R_DE),

	/*
	 * Bit testing.  A single bit out of a byte is what bit does, in
	 * two bytes and without touching A or the carry - against and,
	 * which needs the byte in A first, costs two bytes itself and
	 * then a third to set the flags, because and leaves Z meaning
	 * the whole result rather than the bit.
	 *
	 * Only for a mask that is one bit: P matches a power of two, and
	 * RF_POW2 turns the mask into the bit number the instruction
	 * wants.  Neither admits 1, so bit 0 is still tested the long
	 * way - ispow2 answers 0 for it and both guards read that as no.
	 *
	 * The first two test the byte where it lies, through an index
	 * register or through HL, and are a byte shorter again than
	 * loading it into A first.  Reaching them takes more than a rule:
	 * an AND reduces its left operand before it is itself looked at,
	 * which loads the byte into A and leaves the address nowhere to
	 * be seen, so rewrite1 has a case that reduces the address and
	 * leaves the DEREF standing.  Without it the indexed rule sat
	 * here for a long time matching nothing at all.
	 *
	 * A global keeps the third form.  "ld a,(nn)" is the only direct
	 * absolute load the Z80 has and bit has no absolute form, so
	 * pointing HL at it first would cost what it saved.
	 */
	R("&(D(I),P):bF", AND, P_L, P_R, P_NONE, RF_POW2, "\tbit $R,($LL)\n", F_NZ),
	R("&(D(V),P):bF", AND, P_L, P_R, P_LL, RF_POW2 | RF_IX,
		"\tbit $R,(ix+0)\n", F_NZ),
	R("&(D(H),P):bF", AND, P_L, P_R, P_NONE, RF_POW2, "\tbit $R,(hl)\n", F_NZ),
	R("&(A,P):bF", AND, P_L, P_R, P_NONE, RF_POW2, "\tbit $R,a\n", F_NZ),
	R("&(D(I),N):b", AND, P_L, P_R, P_NONE, 0, F_LDALL "\tand $R\n", R_A),
	R("|(D(I),N):b", OR, P_L, P_R, P_NONE, 0, F_LDALL "\tor $R\n", R_A),
	R("^(D(I),N):b", XOR, P_L, P_R, P_NONE, 0, F_LDALL "\txor $R\n", R_A),
	/*
	 * Byte arithmetic against a memory operand.  These match on the
	 * parent so that A is known to hold the left operand, which makes
	 * HL free to point at the right one - a rule matching the DEREF
	 * alone cannot know that, and would clobber a word left operand.
	 * The Z80 operates directly on (hl) and (iy+d), so no temporary
	 * register is needed at all.
	 */
	R("+(A,D(O)):b", PLUS, P_L, P_R, P_NONE, 0, F_LDHLRL1 "\tadd a,(hl)\n", R_A),
	R("-(A,D(O)):b", MINUS, P_L, P_R, P_NONE, 0, F_LDHLRL1 "\tsub (hl)\n", R_A),
	R("&(A,D(O)):b", AND, P_L, P_R, P_NONE, 0, F_LDHLRL1 "\tand (hl)\n", R_A),
	R("|(A,D(O)):b", OR, P_L, P_R, P_NONE, 0, F_LDHLRL1 F_ORHL, R_A),
	R("^(A,D(O)):b", XOR, P_L, P_R, P_NONE, 0, F_LDHLRL1 "\txor (hl)\n", R_A),
	R("+(A,D(I)):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,($RL)\n", R_A),
	R("-(A,D(I)):b", MINUS, P_L, P_R, P_NONE, 0, "\tsub ($RL)\n", R_A),
	R("&(A,D(I)):b", AND, P_L, P_R, P_NONE, 0, "\tand ($RL)\n", R_A),
	R("|(A,D(I)):b", OR, P_L, P_R, P_NONE, 0, "\tor ($RL)\n", R_A),
	R("^(A,D(I)):b", XOR, P_L, P_R, P_NONE, 0, "\txor ($RL)\n", R_A),

	/* byte arithmetic with both operands live: left in A, right in E */
	R("+(A,K):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,e\n", R_A),
	R("-(A,K):b", MINUS, P_L, P_R, P_NONE, 0, F_SUBE, R_A),
	R("&(A,K):b", AND, P_L, P_R, P_NONE, 0, "\tand e\n", R_A),
	R("|(A,K):b", OR, P_L, P_R, P_NONE, 0, "\tor e\n", R_A),
	R("^(A,K):b", XOR, P_L, P_R, P_NONE, 0, "\txor e\n", R_A),
	/*
	 * The same operators when the right operand arrived as a word in
	 * DE - a call result moved aside, mostly.  A byte operation only
	 * reads E, which is the low byte, which is the byte.  Without
	 * these "cnt += rec(n)" on a byte counter had no rule, and c0's
	 * cntCondLbls answered 0 under the self-build: every IF in the
	 * stream was emitted claiming no short-circuit labels.
	 */
	R("+(A,E):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,e\n", R_A),
	R("-(A,E):b", MINUS, P_L, P_R, P_NONE, 0, F_SUBE, R_A),
	R("&(A,E):b", AND, P_L, P_R, P_NONE, 0, "\tand e\n", R_A),
	R("|(A,E):b", OR, P_L, P_R, P_NONE, 0, "\tor e\n", R_A),
	R("^(A,E):b", XOR, P_L, P_R, P_NONE, 0, "\txor e\n", R_A),
	/* the flag form first: and sets Z itself, so a test that only
	 * wants the flag must not pay for a result register */
	R("&(A,N):bF", AND, P_L, P_R, P_NONE, 0, "\tand $R\n", F_NZ),
	R("&(A,N):b", AND, P_L, P_R, P_NONE, 0, "\tand $R\n", R_A),
	R("&(A,K):bF", AND, P_L, P_R, P_NONE, 0, "\tand e\n", F_NZ),
	R("|(A,N):b", OR, P_L, P_R, P_NONE, 0, "\tor $R\n", R_A),
	R("^(A,N):b", XOR, P_L, P_R, P_NONE, 0, "\txor $R\n", R_A),
	/* no 16-bit and/or/xor on the Z80 - do it a byte at a time */
	R("&(H,N)", AND, P_L, P_R, P_NONE, 0,
		F_LDAL "\tand $Rl\n" F_LDLA F_LDAH "\tand $Rh\n" F_LDHA, R_HL),
	R("|(H,N)", OR, P_L, P_R, P_NONE, 0,
		F_LDAL "\tor $Rl\n" F_LDLA F_LDAH "\tor $Rh\n" F_LDHA, R_HL),
	R("^(H,N)", XOR, P_L, P_R, P_NONE, 0,
		F_LDAL "\txor $Rl\n" F_LDLA F_LDAH "\txor $Rh\n" F_LDHA, R_HL),
	R("&(B,N)", AND, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDAL "\tand $Rl\n" F_LDLA F_LDAH "\tand $Rh\n" F_LDHA, R_HL),
	R("|(B,N)", OR, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDAL "\tor $Rl\n" F_LDLA F_LDAH "\tor $Rh\n" F_LDHA, R_HL),
	R("^(B,N)", XOR, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDAL "\txor $Rl\n" F_LDLA F_LDAH "\txor $Rh\n" F_LDHA, R_HL),
	R("&(H,E)", AND, P_L, P_R, P_NONE, 0, F_LDAL "\tand e\n" F_LDLA F_LDAH "\tand d\n" F_LDHA, R_HL),
	R("|(H,E)", OR, P_L, P_R, P_NONE, 0, F_LDAL "\tor e\n" F_LDLA F_LDAH "\tor d\n" F_LDHA, R_HL),
	R("^(H,E)", XOR, P_L, P_R, P_NONE, 0, F_LDAL "\txor e\n" F_LDLA F_LDAH "\txor d\n" F_LDHA, R_HL),
	/*
	 * The same with BC on one side or the other.  A word bitwise
	 * operator had a form for HL against DE and no other, so one of
	 * these on a register variable emitted nothing.  Against BC the
	 * accumulator can take b and c directly; with the value in BC it
	 * comes over to HL first, which is what the constant-count shifts
	 * do a few rules down.
	 */
	R("&(H,B)", AND, P_L, P_R, P_NONE, 0, F_LDAL "\tand c\n" F_LDLA F_LDAH "\tand b\n" F_LDHA, R_HL),
	R("|(H,B)", OR, P_L, P_R, P_NONE, 0, F_LDAL "\tor c\n" F_LDLA F_LDAH "\tor b\n" F_LDHA, R_HL),
	R("^(H,B)", XOR, P_L, P_R, P_NONE, 0, F_LDAL "\txor c\n" F_LDLA F_LDAH "\txor b\n" F_LDHA, R_HL),
	R("&(B,E)", AND, P_L, P_R, P_NONE, 0, T_BC_HL F_LDAL "\tand e\n" F_LDLA F_LDAH "\tand d\n" F_LDHA, R_HL),
	R("|(B,E)", OR, P_L, P_R, P_NONE, 0, T_BC_HL F_LDAL "\tor e\n" F_LDLA F_LDAH "\tor d\n" F_LDHA, R_HL),
	R("^(B,E)", XOR, P_L, P_R, P_NONE, 0, T_BC_HL F_LDAL "\txor e\n" F_LDLA F_LDAH "\txor d\n" F_LDHA, R_HL),

	/*
	 * Signed compare against zero is just the sign bit, and it has to
	 * be: sbc hl,de sets carry on an unsigned borrow, so the generic
	 * form below says "x < 0" is false for every x.  Must precede the
	 * T/Y(H,N) rules - zero is a subset of NUMBER and first match wins.
	 */
	R("T(H,Z)", LT, P_L, P_R, P_NONE, RF_SIGNL, F_LDAH F_ORA, F_M),
	R("Y(H,Z)", GE, P_L, P_R, P_NONE, RF_SIGNL, F_LDAH F_ORA, F_P),
	R("T(B,Z)", LT, P_L, P_R, P_NONE, RF_SIGNL, F_LDAB F_ORA, F_M),
	R("Y(B,Z)", GE, P_L, P_R, P_NONE, RF_SIGNL, F_LDAB F_ORA, F_P),
	/*
	 * "> 0" and "<= 0" are not a single flag the way "< 0" is - they
	 * need the value to be non-negative AND non-zero.  Test the sign,
	 * and on the negative side fall into an xor a that forces Z, so
	 * both paths arrive with Z meaning false:
	 *
	 *   J+0  ld a,h   1
	 *   J+1  or a     1   sign of the high byte
	 *   J+2  jp m     3   negative, so false
	 *   J+5  ld a,h   1
	 *   J+6  or l     1   Z here means the whole value was zero
	 *   J+7  jr       2   past the forced-false
	 *   J+9  xor a    1
	 *   J+10
	 */
	R("G(H,Z)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDAH F_ORA F_JPM7 F_LDAH "\tor l\n"
		F_JR3 F_XORA, F_NZ),
	R("W(H,Z)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDAH F_ORA F_JPM7 F_LDAH "\tor l\n"
		F_JR3 F_XORA, F_Z),
	R("G(B,Z)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDAB F_ORA F_JPM7 F_LDAB "\tor c\n"
		F_JR3 F_XORA, F_NZ),
	R("W(B,Z)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDAB F_ORA F_JPM7 F_LDAB "\tor c\n"
		F_JR3 F_XORA, F_Z),

	/*
	 * Signed relational comparison.  Carry answers the unsigned
	 * question, so it cannot be used here: with a = -1 and b = 1 the
	 * subtraction does not borrow, and carry would report that -1 is
	 * not less than 1.
	 *
	 * The signed answer is sign exclusive-or overflow.  sbc hl,de
	 * leaves the sign in bit 7 of H and the overflow in P/V, so take
	 * the high byte, flip its top bit when the subtraction overflowed,
	 * and let or a set the sign from the result.  M is then "less
	 * than" and P is "greater or equal".
	 *
	 * Ten bytes against the three carry costs, which is why the
	 * unsigned forms below keep using it, and why comparing against
	 * zero stays on the sign-bit rules above - those are exact and
	 * cheaper.
	 */
	R("T(H,E)", LT, P_L, P_R, P_NONE, RF_SIGNL, T_SUB_DE T_SXORV, F_M),
	R("Y(H,E)", GE, P_L, P_R, P_NONE, RF_SIGNL, T_SUB_DE T_SXORV, F_P),
	R("W(H,E)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("G(H,E)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_EXDEHL T_SUB_DE T_SXORV, F_M),
	/*
	 * A symbol compared against a register.  A SYMREF is left
	 * unreduced so the store and load rules can use it as an
	 * address, so where its *value* is wanted it has to be loaded -
	 * and here that is the whole difference.
	 *
	 * It only shows up on the left because "a > b" is canonicalised
	 * to "b < a", which is what puts the symbol there.  "s == buf"
	 * kept the symbol on the right, where it becomes DE and the
	 * ordinary rules match, so equality worked and ordering did not:
	 *
	 *	while (s > macbuffer && ...)	never ran
	 *
	 * which is macro.c's trailing-whitespace trim, so a macro body
	 * kept its trailing blanks - and an empty one walked off the end
	 * of the definition and ate the next line.
	 */
	R("T(O,E)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL T_SUB_DE T_SXORV, F_M),
	R("Y(O,E)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL T_SUB_DE T_SXORV, F_P),
	R("W(O,E)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("G(O,E)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL F_EXDEHL T_SUB_DE T_SXORV, F_M),

	/*
	 * The same symbol-on-the-left shapes with the register operand
	 * living in BC - a register variable compared against a global
	 * array's address arrives exactly here, and the table stopping
	 * at (O,E) left "if (s > buf)" unreduced.
	 */
	R("T(O,B)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL T_SUB_BC T_SXORV, F_M),
	R("Y(O,B)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL T_SUB_BC T_SXORV, F_P),
	R("W(O,B)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL F_LDEC F_LDDB F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("G(O,B)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDHLL F_LDEC F_LDDB F_EXDEHL T_SUB_DE T_SXORV, F_M),

	/* and with the symbol on the other side, where it becomes DE */
	R("T(H,O)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER T_SUB_DE T_SXORV, F_M),
	R("Y(H,O)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER T_SUB_DE T_SXORV, F_P),
	R("W(H,O)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("G(H,O)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER F_EXDEHL T_SUB_DE T_SXORV, F_M),

	R("T(H,B)", LT, P_L, P_R, P_NONE, RF_SIGNL, T_SUB_BC T_SXORV, F_M),
	R("Y(H,B)", GE, P_L, P_R, P_NONE, RF_SIGNL, T_SUB_BC T_SXORV, F_P),
	R("W(H,B)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDEC F_LDDB F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("G(H,B)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDEC F_LDDB F_EXDEHL T_SUB_DE T_SXORV, F_M),
	R("T(H,N)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER T_SUB_DE T_SXORV, F_M),
	R("Y(H,N)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER T_SUB_DE T_SXORV, F_P),
	R("W(H,N)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("G(H,N)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDDER F_EXDEHL T_SUB_DE T_SXORV, F_M),

	/* comparisons */
	R("Q(H,E)", EQ, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLDE, F_Z),
	/* LE/GT have no cheap flag of their own: swap the operands so the
	 * borrow from sbc answers the reversed question. */
	R("W(H,E)", LE, P_L, P_R, P_NONE, 0, F_EXDEHL F_ORA F_SBCHLDE, F_NC),
	R("G(H,E)", GT, P_L, P_R, P_NONE, 0, F_EXDEHL F_ORA F_SBCHLDE, F_C),
	R("U(H,E)", NEQ, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLDE, F_NZ),
	R("T(H,E)", LT, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLDE, F_C),
	R("Y(H,E)", GE, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLDE, F_NC),
	/* the same four with a symbol on the left - see the signed set */
	R("W(O,E)", LE, P_L, P_R, P_NONE, 0,
		F_LDHLL F_EXDEHL F_ORA F_SBCHLDE, F_NC),
	R("G(O,E)", GT, P_L, P_R, P_NONE, 0,
		F_LDHLL F_EXDEHL F_ORA F_SBCHLDE, F_C),
	R("T(O,E)", LT, P_L, P_R, P_NONE, 0, F_LDHLL F_ORA F_SBCHLDE, F_C),
	R("Y(O,E)", GE, P_L, P_R, P_NONE, 0, F_LDHLL F_ORA F_SBCHLDE, F_NC),
	R("W(H,O)", LE, P_L, P_R, P_NONE, 0,
		F_LDDER F_EXDEHL F_ORA F_SBCHLDE, F_NC),
	R("G(H,O)", GT, P_L, P_R, P_NONE, 0,
		F_LDDER F_EXDEHL F_ORA F_SBCHLDE, F_C),
	R("T(H,O)", LT, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_C),
	R("Y(H,O)", GE, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_NC),
	/* BC operands: the Z80 has add/sbc hl,bc, so no shuffle needed */
	R("Q(H,B)", EQ, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLBC, F_Z),
	R("U(H,B)", NEQ, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLBC, F_NZ),
	R("T(H,B)", LT, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLBC, F_C),
	R("Y(H,B)", GE, P_L, P_R, P_NONE, 0, F_ORA F_SBCHLBC, F_NC),
	/*
	 * LE and GT answer the reversed question, and there is no
	 * ex bc,hl - so copy BC into DE and swap that instead.
	 */
	R("W(H,B)", LE, P_L, P_R, P_NONE, 0,
		F_LDEC F_LDDB F_EXDEHL F_ORA F_SBCHLDE, F_NC),
	R("G(H,B)", GT, P_L, P_R, P_NONE, 0,
		F_LDEC F_LDDB F_EXDEHL F_ORA F_SBCHLDE, F_C),
	R("Q(B,E)", EQ, P_L, P_R, P_NONE, 0, T_BC_HL F_ORA F_SBCHLDE, F_Z),
	R("U(B,E)", NEQ, P_L, P_R, P_NONE, 0, T_BC_HL F_ORA F_SBCHLDE, F_NZ),
	/*
	 * A register variable compared, signed.  The rules below answer
	 * with carry, which is the unsigned question - the same fault the
	 * HL forms had, in the register that fix did not reach.  A
	 * variable that lives in BC and goes negative compared as though
	 * it were large: "i < 2" was false for i = -1.
	 *
	 * Greater-than and at-or-below have no flag of their own, so the
	 * operands are handed over the other way round, which is what the
	 * ex de,hl is doing.
	 */
	R("T(B,E)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL T_SUB_DE T_SXORV, F_M),
	R("Y(B,E)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL T_SUB_DE T_SXORV, F_P),
	R("G(B,E)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_EXDEHL T_SUB_DE T_SXORV, F_M),
	R("W(B,E)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("T(B,E)", LT, P_L, P_R, P_NONE, 0, T_BC_HL F_ORA F_SBCHLDE, F_C),
	R("Y(B,E)", GE, P_L, P_R, P_NONE, 0, T_BC_HL F_ORA F_SBCHLDE, F_NC),
	R("Q(B,N)", EQ, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE, F_Z),
	R("U(B,N)", NEQ, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE, F_NZ),
	R("T(B,N)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_LDDER T_SUB_DE T_SXORV, F_M),
	R("Y(B,N)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_LDDER T_SUB_DE T_SXORV, F_P),
	R("G(B,N)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_LDDER F_EXDEHL T_SUB_DE T_SXORV, F_M),
	R("W(B,N)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL F_LDDER F_EXDEHL T_SUB_DE T_SXORV, F_P),
	R("T(B,N)", LT, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE, F_C),
	R("Y(B,N)", GE, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE, F_NC),

	/* against a symbol's address, which is what comparing a pointer
	 * with "&thing" comes to */
	R("Q(H,O)", EQ, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_Z),
	R("U(H,O)", NEQ, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_NZ),
	R("Q(H,N)", EQ, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_Z),
	R("U(H,N)", NEQ, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_NZ),
	R("T(H,N)", LT, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_C),
	R("Y(H,N)", GE, P_L, P_R, P_NONE, 0, F_LDDER F_ORA F_SBCHLDE, F_NC),

	/*
	 * Signed byte comparisons.  These have to come before the
	 * unsigned forms below, which match either signedness and answer
	 * the unsigned question: cp sets carry on a borrow, and nothing
	 * borrows against zero, so "c < 0" was false for every char in
	 * the language.  Equality needs no signed form - the bits are
	 * either equal or they are not.
	 *
	 * Against zero the sign bit is the whole answer, and or a puts it
	 * in S for free.  Zero is a subset of NUMBER, so these must also
	 * precede the T/Y(A,N) rules: first match wins.
	 *
	 * None of these ask for flag context, unlike the unsigned rules
	 * below, which is what let a byte comparison used for its value
	 * fall through to no rule at all.  A flag becomes a number by the
	 * same path a word comparison uses.
	 */
	R("T(A,Z)", LT, P_L, P_R, P_NONE, RF_SIGNL, F_ORA, F_M),
	R("Y(A,Z)", GE, P_L, P_R, P_NONE, RF_SIGNL, F_ORA, F_P),
	/* or a sets S and Z together, which is what > 0 and <= 0 need */
	R("G(A,Z)", GT, P_L, P_R, P_NONE, RF_SIGNL, F_ORA T_SZTAIL, F_NZ),
	R("W(A,Z)", LE, P_L, P_R, P_NONE, RF_SIGNL, F_ORA T_SZTAIL, F_Z),
	/*
	 * Against anything else, the same sign-exclusive-or-overflow the
	 * word rules use.  Seven bytes against cp's two, which is why the
	 * unsigned forms below keep cp and why zero stays on the rules
	 * above.
	 *
	 * > and <= go the long way round rather than becoming >= and <
	 * against the constant plus one, the way the unsigned rules do.
	 * That trick has nowhere to go at 127, where the increment wraps
	 * to -128 and turns a test that is always false into one that is
	 * always true.
	 */
	R("T(A,K)", LT, P_L, P_R, P_NONE, RF_SIGNL, F_SUBE T_SXORA, F_M),
	R("Y(A,K)", GE, P_L, P_R, P_NONE, RF_SIGNL, F_SUBE T_SXORA, F_P),
	R("G(A,K)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_SUBE T_SXORA T_SZTAIL, F_NZ),
	R("W(A,K)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_SUBE T_SXORA T_SZTAIL, F_Z),
	R("T(A,N)", LT, P_L, P_R, P_NONE, RF_SIGNL, F_SUBR T_SXORA, F_M),
	R("Y(A,N)", GE, P_L, P_R, P_NONE, RF_SIGNL, F_SUBR T_SXORA, F_P),
	R("G(A,N)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_SUBR T_SXORA T_SZTAIL, F_NZ),
	R("W(A,N)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_SUBR T_SXORA T_SZTAIL, F_Z),
	R("T(D(I),N):b", LT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDALL F_SUBR T_SXORA, F_M),
	R("Y(D(I),N):b", GE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDALL F_SUBR T_SXORA, F_P),
	R("G(D(I),N):b", GT, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDALL F_SUBR T_SXORA T_SZTAIL, F_NZ),
	R("W(D(I),N):b", LE, P_L, P_R, P_NONE, RF_SIGNL,
		F_LDALL F_SUBR T_SXORA T_SZTAIL, F_Z),

	/* byte comparisons */
	/* byte comparison against another byte, in E */
	R("Q(A,K)", EQ, P_L, P_R, P_NONE, 0, F_CPE, F_Z),
	R("U(A,K)", NEQ, P_L, P_R, P_NONE, 0, F_CPE, F_NZ),
	R("T(A,K)", LT, P_L, P_R, P_NONE, 0, F_CPE, F_C),
	R("Y(A,K)", GE, P_L, P_R, P_NONE, 0, F_CPE, F_NC),
	R("Q(A,N)", EQ, P_L, P_R, P_NONE, 0, F_CPR, F_Z),
	R("U(A,N)", NEQ, P_L, P_R, P_NONE, 0, F_CPR, F_NZ),
	/*
	 * Against a byte register variable, which lives in B or C.  cp
	 * takes either directly; there were forms for E and for a
	 * constant and none for these.
	 */
	R("Q(A,V)", EQ, P_L, P_R, P_R, RF_B, "\tcp b\n", F_Z),
	R("U(A,V)", NEQ, P_L, P_R, P_R, RF_B, "\tcp b\n", F_NZ),
	R("Q(A,V)", EQ, P_L, P_R, P_R, RF_C, "\tcp c\n", F_Z),
	R("U(A,V)", NEQ, P_L, P_R, P_R, RF_C, "\tcp c\n", F_NZ),
	R("T(A,N)", LT, P_L, P_R, P_NONE, 0, F_CPR, F_C),
	R("Y(A,N)", GE, P_L, P_R, P_NONE, 0, F_CPR, F_NC),
	R("Q(D(I),N):b", EQ, P_L, P_R, P_NONE, 0, F_LDALL F_CPR, F_Z),
	R("U(D(I),N):b", NEQ, P_L, P_R, P_NONE, 0, F_LDALL F_CPR, F_NZ),
	R("T(D(I),N):b", LT, P_L, P_R, P_NONE, 0, F_LDALL F_CPR, F_C),
	R("Y(D(I),N):b", GE, P_L, P_R, P_NONE, 0, F_LDALL F_CPR, F_NC),

	/*
	 * Unsigned > and <=.  cp leaves the answer spread over two flags -
	 * carry says below, zero says equal - and "at or below" wants
	 * both.  Rather than branch twice, fold equality into the carry:
	 * when the two were equal, set it.  Carry then means "at or
	 * below" on its own, and its complement means "above".
	 *
	 *   J+0  cp     1   C = below, Z = equal
	 *   J+1  jr nz  2   not equal, so carry already answers
	 *   J+3  scf    1   equal, so make carry say so
	 *   J+4
	 *
	 * This replaces turning "> n" into ">= n+1", which has nowhere to
	 * go at 255: the increment wraps to zero and a test that is never
	 * true becomes one that always is.  Two bytes more, and right at
	 * both ends of the range.
	 */
	R("G(A,K)", GT, P_L, P_R, P_NONE, 0, F_CPE F_JRNZ3 "\tscf\n", F_NC),
	R("W(A,K)", LE, P_L, P_R, P_NONE, 0, F_CPE F_JRNZ3 "\tscf\n", F_C),
	R("G(A,N)", GT, P_L, P_R, P_NONE, 0, F_CPR F_JRNZ3 "\tscf\n", F_NC),
	R("W(A,N)", LE, P_L, P_R, P_NONE, 0, F_CPR F_JRNZ3 "\tscf\n", F_C),
	R("G(D(I),N):b", GT, P_L, P_R, P_NONE, 0,
		F_LDALL F_CPR F_JRNZ3 "\tscf\n", F_NC),
	R("W(D(I),N):b", LE, P_L, P_R, P_NONE, 0,
		F_LDALL F_CPR F_JRNZ3 "\tscf\n", F_C),

	/*
	 * The same fold at word width.  These turned "> n" into ">= n+1"
	 * until now, which the note above says has nowhere to go at the
	 * top of the range - it was fixed for bytes at 255 and left here,
	 * where the increment wraps at 65535 instead.  "u <= 0xffff" is
	 * true of every unsigned short and came out false for all of
	 * them.
	 */
	R("G(H,N)", GT, P_L, P_R, P_NONE, 0,
		F_LDDER F_ORA F_SBCHLDE F_JRNZ3 "\tscf\n", F_NC),
	R("W(H,N)", LE, P_L, P_R, P_NONE, 0,
		F_LDDER F_ORA F_SBCHLDE F_JRNZ3 "\tscf\n", F_C),
	/* the same for a register variable, which had neither */
	R("G(B,N)", GT, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE F_JRNZ3 "\tscf\n", F_NC),
	R("W(B,N)", LE, P_L, P_R, P_NONE, 0,
		T_BC_HL F_LDDER F_ORA F_SBCHLDE F_JRNZ3 "\tscf\n", F_C),
	R("G(B,E)", GT, P_L, P_R, P_NONE, 0,
		T_BC_HL F_EXDEHL F_ORA F_SBCHLDE, F_C),
	R("W(B,E)", LE, P_L, P_R, P_NONE, 0,
		T_BC_HL F_EXDEHL F_ORA F_SBCHLDE, F_NC),

	/* NEQ -> BANG(EQ) */
	R("U(_,N)", 0, P_NONE, P_NONE, P_NONE, RF_NOTEQ, NULL, 0),
	R("U", 0, P_NONE, P_NONE, P_NONE, RF_NOTEQ, NULL, 0),

	/*
	 * Storing a register-relative address to a global: the INDEX
	 * form the address-combining rules mint is the value here, not
	 * a location - "g = a + 2" with a in IX.  Every other context
	 * (frame store, argument push, comparison) materializes it on
	 * some other path; this one had no rule at all, fell to the
	 * incomplete-rewrite marker, and the store was silently
	 * dropped - which is how cpp's -I list vanished on the Z80.
	 * add hl takes only bc/de/hl/sp, so the register goes through
	 * the stack.
	 */
	R("=(O,I)", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tpush $Rr\n\tpop hl\n\tld de,$Ro\n\tadd hl,de\n\tld ($L),hl\n",
		R_HL),

	/* terminator */
	{NULL, NULL, 0, 0, 0, 0}
};

/* Patterns that should not be reduced */
char *preserve[] = {
	"V", "L", "I", "N", "S", "O", "H", "E", "A", "K", "B", "C", NULL
};
