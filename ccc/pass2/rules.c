/*
 * rules.c - Code generation pattern rules
 */
#include <stddef.h>
#include "pass2.h"
#include "expr.h"
#include "opcodes.h"
#include "rules.h"

/* Common assembly templates for deduplication */
#define T_SAVE_HL "\tld d,h\n\tld e,l\n"
#define T_ADD_HL_DE "\tadd hl,de\n"
#define T_ADD_HL_HL "\tadd hl,hl\n"
#define T_IDX_S_ST "\tld ($L),l\n\tld ($L+),h\n"
#define T_IDX_S_LD "\tld l,($R)\n\tld h,($R+)\n"
#define T_IX_TEST "\tld a,ixl\n\tor a,ixh\n"
#define T_BC_TEST "\tld a,c\n\tor a,b\n"
#define T_HL_TEST "\tld a,l\n\tor a,h\n"
#define T_BC_HL "\tld l,c\n\tld h,b\n"
#define T_DE_TEST "\tld a,e\n\tor a,d\n"

#define R(pat, rep, l, r, d, f, tpl, dest) \
	{pat, rep, l, r, d, f, tpl, dest}

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
	R("V:bF", REGVAR, P_NONE, P_NONE, P_NONE, RF_C, "\tld a,c\n\tor a\n", F_NZ),
	R("V:bF", REGVAR, P_NONE, P_NONE, P_NONE, RF_B, "\tld a,b\n\tor a\n", F_NZ),

	/* assign constant/A/HL to REGVAR C/B */
	R("=(V,N):b", ASSIGN, P_L, P_R, P_L, RF_C, "\tld c,$R\n", R_A),
	R("=(V,N):b", ASSIGN, P_L, P_R, P_L, RF_B, "\tld b,$R\n", R_A),
	R("=(V,A):b", ASSIGN, P_L, P_R, P_L, RF_C, "\tld c,a\n", R_A),
	R("=(V,A):b", ASSIGN, P_L, P_R, P_L, RF_B, "\tld b,a\n", R_A),
	R("=(V,H):b", ASSIGN, P_L, P_R, P_L, RF_C, "\tld c,l\n", R_HL),
	R("=(V,H):b", ASSIGN, P_L, P_R, P_L, RF_B, "\tld b,l\n", R_HL),

	/* load REGVAR C/B to HL (zero-extended) */
	R("=(H,V):b", ASSIGN, P_L, P_R, P_R, RF_C, "\tld l,c\n\tld h,0\n", R_HL),
	R("=(H,V):b", ASSIGN, P_L, P_R, P_R, RF_B, "\tld l,b\n\tld h,0\n", R_HL),

	/* REGVAR C/B -> INA (value in C/B, byte context) */
	R("V:b", INA, P_NONE, P_NONE, P_NONE, RF_C, "\tld a,c\n", R_A),
	R("V:b", INA, P_NONE, P_NONE, P_NONE, RF_B, "\tld a,b\n", R_A),

	/* INHL/INDE/INA in flag context: test for zero */
	R("H:F", INHL, P_NONE, P_NONE, P_NONE, 0, T_HL_TEST, F_NZ),
	R("E:F", INDE, P_NONE, P_NONE, P_NONE, 0, T_DE_TEST, F_NZ),
	R("A:F", INA, P_NONE, P_NONE, P_NONE, 0, "\tor a\n", F_NZ),

	/* copy IX to HL/BC/DE */
	R("=(H,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tpush ix\n\tpop hl\n", R_HL),
	R("=(B,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tld c,ixl\n\tld b,ixh\n", R_BC),
	R("=(E,V)", ASSIGN, P_L, P_R, P_R, RF_IX, "\tld e,ixl\n\tld d,ixh\n", R_DE),

	/* Address rules: IX+offset -> INDEX */
	R("+(V,N)", INDEX, P_NONE, P_NONE, P_L, RF_IX, NULL, 0),
	R("+(D(V),N)", INDEX, P_NONE, P_NONE, P_LL, RF_IXIY, NULL, 0),
	R("+(I,N)", INDEX, P_NONE, P_NONE, P_L, 0, NULL, 0),

	/* symbol + constant offset folds into the SYMREF */
	R("+(O,N)", SYMREF, P_NONE, P_NONE, P_NONE, 0, NULL, 0),

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
	R("*(H,E)", STAR, P_L, P_R, P_NONE, 0, "\tcall __mul16\n", R_HL),
	R("/(H,E)", DIV, P_L, P_R, P_NONE, 0, "\tcall __div16\n", R_HL),
	R("%(H,E)", MOD, P_L, P_R, P_NONE, 0, "\tcall __mod16\n", R_HL),

	/* store to indexed */
	R("=(I,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),$R\n", 0),
	R("=(I,N):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),$Rl\n\tld ($L+),$Rh\n", 0),
	R("=(I,H):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),l\n", R_HL),
	R("=(I,H):s", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_S_ST, 0),
	R("=(I,I):s", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_S_LD T_IDX_S_ST, R_HL),
	R("=(I,E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),e\n\tld ($L+),d\n", 0),
	R("=(I,B):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),c\n\tld ($L+),b\n", 0),

	/* store to symref */
	R("=(O,A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),a\n", R_A),
	R("=(O,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n\tld ($L),a\n", R_A),
	R("=(O,H):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),hl\n", R_HL),
	/* narrowing store: a word result keeps only its low byte */
	R("=(O,H):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,l\n\tld ($L),a\n", R_A),
	R("=(O,B):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,c\n\tld ($L),a\n", R_A),
	R("=(I,B):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),c\n", 0),
	R("=(O,N):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld hl,$R\n\tld ($L),hl\n", R_HL),

	/* load constant to register variable */
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tld ix,$R\n", R_IX),
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_BC, "\tld bc,$R\n", R_BC),
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_DE, "\tld de,$R\n", R_DE),
	R("=(V,N)", ASSIGN, P_L, P_R, P_L, RF_HL, "\tld hl,$R\n", R_HL),

	/* load constant to register (already converted) */
	R("=(B,N)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld bc,$R\n", R_BC),
	R("=(E,N)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld de,$R\n", R_DE),
	R("=(H,N)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld hl,$R\n", R_HL),

	/* assign to IX register variable */
	R("=(V,H)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tpush hl\n\tpop ix\n", R_IX),
	R("=(V,E)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tpush de\n\tpop ix\n", R_IX),
	R("=(V,B)", ASSIGN, P_L, P_R, P_L, RF_IX, "\tpush bc\n\tpop ix\n", R_IX),

	/* register-to-register moves */
	R("=(B,H)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,l\n\tld b,h\n", R_BC),
	R("=(E,H)", ASSIGN, P_L, P_R, P_NONE, 0, "\tex de,hl\n", R_DE),
	R("=(H,E)", ASSIGN, P_L, P_R, P_NONE, 0, "\tex de,hl\n", R_HL),
	R("=(H,B)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,c\n\tld h,b\n", R_HL),
	R("=(B,E)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,e\n\tld b,d\n", R_BC),
	R("=(E,B)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld e,c\n\tld d,b\n", R_DE),
	R("=(B,B)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_BC),
	R("=(E,E)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_DE),
	R("=(H,H)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_HL),

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
	R("X(A):s", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tld $t,a\n\trla\n\tsbc a,a\n\tld $u,a\n", 0),

	/* zero-extended loads */
	R("=(B,A)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,a\n\tld b,0\n", R_BC),
	R("=(H,A)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,a\n\tld h,0\n", R_HL),
	R("=(E,A)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld e,a\n\tld d,0\n", R_DE),

	/* register base address calculations */
	R("+(B,N)", PLUS, P_L, P_R, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld de,$R\n\tadd hl,de\n", R_HL),
	R("+(B,M)", PLUS, P_L, P_R, P_NONE, 0, "\tld l,c\n\tld h,b\n%(\tinc hl\n)", R_HL),
	R("+(E,N)", PLUS, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tld de,$R\n\tadd hl,de\n", R_HL),
	R("+(E,M)", PLUS, P_L, P_R, P_NONE, 0, "\tex de,hl\n%(\tinc hl\n)", R_HL),

	/* negation */
	R("g(B)", NEG, P_L, P_NONE, P_NONE, 0, "\tld a,0\n\tsub c\n\tld l,a\n\tld a,0\n\tsbc a,b\n\tld h,a\n", R_HL),
	R("g(H)", NEG, P_L, P_NONE, P_NONE, 0, "\txor a\n\tsub l\n\tld l,a\n\tld a,0\n\tsbc a,h\n\tld h,a\n", R_HL),
	R("g(E)", NEG, P_L, P_NONE, P_NONE, 0, "\tld a,0\n\tsub e\n\tld l,a\n\tld a,0\n\tsbc a,d\n\tld h,a\n", R_HL),

	/* pre-inc/dec */
	R("i(B)", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc bc\n\tld l,c\n\tld h,b\n", R_HL),
	R("k(B)", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec bc\n\tld l,c\n\tld h,b\n", R_HL),
	/* unary: operand is the LEFT child (T_IDX_S_LD reads $R) */
	R("i(I):s", PREINC, P_L, P_NONE, P_NONE, 0,
	  "\tld l,($L)\n\tld h,($L+)\n\tinc hl\n" T_IDX_S_ST, R_HL),
	R("k(I):s", PREDEC, P_L, P_NONE, P_NONE, 0,
	  "\tld l,($L)\n\tld h,($L+)\n\tdec hl\n" T_IDX_S_ST, R_HL),
	R("i(I):b", PREINC, P_L, P_NONE, P_NONE, 0, "\tld a,($L)\n\tinc a\n\tld ($L),a\n", R_A),
	R("k(I):b", PREDEC, P_L, P_NONE, P_NONE, 0, "\tld a,($L)\n\tdec a\n\tld ($L),a\n", R_A),

	/* byte stores */
	R("=(I,A)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),a\n", R_A),
	R("=(H,N)", ASSIGN, P_L, P_R, P_NONE, 0, NULL, 0),
	R("=(H,A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,a\n", R_HL),
	R("=(H,V):b", ASSIGN, P_L, P_R, P_R, RF_BC, "\tld (hl),c\n", 0),

	/* loads from register addresses */
	R("D(H):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tld a,(hl)\n", R_A),
	R("D(B):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n", R_A),
	R("D(B):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL),
	R("D(E):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tex de,hl\n\tld a,(hl)\n", R_A),
	R("D(E):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tex de,hl\n\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tex de,hl\n", R_HL),

	/* indirect stores via registers */
	R("=(D(B),A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld (hl),a\n", 0),
	R("=(D(E),A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tld (hl),a\n\tex de,hl\n", 0),
	R("=(D(B),H):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tpush hl\n\tld l,c\n\tld h,b\n\tpop de\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0),
	R("=(D(E),H):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tpush de\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tpop hl\n", 0),

	/* indirect stores via HL */
	R("=(D(H),N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),$R\n", 0),
	R("=(D(H),E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0),
	R("=(D(B),N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld (hl),$R\n", 0),
	R("=(D(B),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n", 0),
	R("=(D(E),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tex de,hl\n\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n", 0),
	R("=(D(H),N):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n", 0),
	R("=(D(H),B):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),c\n\tinc hl\n\tld (hl),b\n", 0),

	/* pointer testing */
	R("D(H):pF", DEREF, P_L, P_NONE, P_NONE, 0, "\tld a,(hl)\n\tor a,(hl)\n", F_NZ),

	/* structured loads to BC/DE/HL */
	R("=(B,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,(hl)\n\tinc hl\n\tld b,(hl)\n", R_BC),
	R("=(B,I)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld c,($R)\n\tld b,($R+)\n", R_BC),
	R("=(H,I)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,($R)\n\tld h,($R+)\n", R_HL),
	R("=(E,I)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld e,($R)\n\tld d,($R+)\n", R_DE),
	R("=(I,O)", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld hl,$R\n" T_IDX_S_ST, R_HL),
	R("=(B,O)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld bc,$R\n", R_BC),
	R("=(H,O)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld hl,$R\n", R_HL),
	R("=(E,O)", ASSIGN, P_L, P_R, P_NONE, 0, "\tld de,$R\n", R_DE),
	R("=(B,D(O)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n\tld c,a\n\tld a,($RL+)\n\tld b,a\n", R_BC),
	R("=(E,D(O)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld de,($RL)\n", R_DE),
	R("=(H,D(O)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld hl,($RL)\n", R_HL),
	R("=(E,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n", R_DE),
	R("=(H,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL),
	R("=(I,D(H)):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,(hl)\n\tld ($L),a\n\tinc hl\n\tld a,(hl)\n\tld ($L+),a\n", 0),

	/* arithmetic/logical on indexed */
	R("o(H,N):b", OREQ, P_L, P_R, P_NONE, 0, "\tld a,(hl)\n\tor $R\n\tld (hl),a\n", R_A),
	R("o(I,K):b", OREQ, P_L, P_R, P_NONE, 0, "\tld a,($L)\n\tor e\n\tld ($L),a\n", R_A),
	R("D(I):bF", DEREF, P_L, P_NONE, P_NONE, 0, "\tld a,($L)\n\tor a\n", F_Z),
	R("D(I):sF", DEREF, P_L, P_NONE, P_NONE, 0, "\tld a,($L)\n\tor a,($L+)\n", F_Z),
	R("D(I):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tld $t,($L)\n\tld $u,($L+)\n", 0),
	R("D(I):b", DEREF, P_L, P_NONE, P_NONE, 0, NULL, 0),
	/*
	 * Of the 8-bit registers only A can load from an absolute address,
	 * but the pairs all can, so ld de,(nn) reaches E without touching
	 * A or HL - either of which may hold the left operand.  It reads
	 * the following byte into D as well; D is dead here, and one byte
	 * of over-read is harmless in a flat memory model.
	 */
	R("D(O):b", DEREF, P_L, P_NONE, P_NONE, RF_TDE, "\tld de,($L)\n", R_E),
	R("D(O):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tld a,($L)\n", R_A),
	R("D(I):b", DEREF, P_L, P_NONE, P_NONE, RF_TDE, "\tld e,($L)\n", R_E),
	/* honour the target: as the right operand of a compare this has to
	 * land in DE, or it overwrites the left operand in HL */
	R("D(O):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tld $T,($L)\n", 0),

	/* 16-bit binary arithmetic */
	R("+(H,E)", PLUS, P_L, P_R, P_NONE, 0, T_ADD_HL_DE, R_HL),
	R("+(H,B)", PLUS, P_L, P_R, P_NONE, 0, "\tadd hl,bc\n", R_HL),
	R("-(H,B)", MINUS, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,bc\n", R_HL),
	R("+(B,E)", PLUS, P_L, P_R, P_NONE, 0, T_BC_HL T_ADD_HL_DE, R_HL),
	R("-(B,E)", MINUS, P_L, P_R, P_NONE, 0, T_BC_HL "\tor a\n\tsbc hl,de\n", R_HL),
	R("-(B,N)", MINUS, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tor a\n\tsbc hl,de\n", R_HL),
	R("<(B,N)", LSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(" T_ADD_HL_HL ")", R_HL),
	R("/(B,N)", DIV, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tcall __div16\n", R_HL),
	R("%(B,N)", MOD, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tcall __mod16\n", R_HL),
	R("*(B,N)", STAR, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tcall __mul16\n", R_HL),
	R("+(H,M)", PLUS, P_L, P_R, P_NONE, 0, "%(\tinc hl\n)", R_HL),
	R("-(H,M)", MINUS, P_L, P_R, P_NONE, 0, "%(\tdec hl\n)", R_HL),
	R("+(A,M)", PLUS, P_L, P_R, P_NONE, 0, "%(\tinc a\n)", R_A),
	R("-(A,M)", MINUS, P_L, P_R, P_NONE, 0, "%(\tdec a\n)", R_A),
	R("+(H,N)", PLUS, P_L, P_R, P_NONE, 0, "\tld de,$R\n" T_ADD_HL_DE, R_HL),
	R("+(A,N)", PLUS, P_L, P_R, P_NONE, 0, NULL, 0),
	R("+(D(I),N):b", PLUS, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tadd a,$R\n", R_A),
	R("-(A,N)", MINUS, P_L, P_R, P_NONE, 0, NULL, 0),
	R("-(D(I),N):b", MINUS, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tsub $R\n", R_A),
	R("-(H,E)", MINUS, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,de\n", R_HL),
	R("-(H,N)", MINUS, P_L, P_R, P_NONE, 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", R_HL),

	/* shifts */
	R("<(H,N)", LSHIFT, P_L, P_R, P_NONE, 0, "%(" T_ADD_HL_HL ")", R_HL),
	R("<(A,N):b", LSHIFT, P_L, P_R, P_NONE, 0, "%(\tsla a\n)", R_A),
	R(">(A,N):b", RSHIFT, P_L, P_R, P_NONE, 0, "%(\tsrl a\n)", R_A),
	R(">(H,M)", RSHIFT, P_L, P_R, P_NONE, 0, "%(\tsrl h\n\trr l\n)", R_HL),
	R(">(B,M)", RSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(\tsrl h\n\trr l\n)", R_HL),

	/* stores/loads with indexed/symref */
	R("=(A,D(I)):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n", R_A),
	R("=(A,D(O)):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n", R_A),
	R("=(A,A)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_A),

	/* bit testing and logic */
	R("&(D(I),P):bF", AND, P_L, P_R, P_NONE, RF_POW2, "\tbit $R,($LL)\n", F_NZ),
	R("&(D(I),N):b", AND, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tand $R\n", R_A),
	R("|(D(I),N):b", OR, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tor $R\n", R_A),
	R("^(D(I),N):b", XOR, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\txor $R\n", R_A),
	/*
	 * Byte arithmetic against a memory operand.  These match on the
	 * parent so that A is known to hold the left operand, which makes
	 * HL free to point at the right one - a rule matching the DEREF
	 * alone cannot know that, and would clobber a word left operand.
	 * The Z80 operates directly on (hl) and (iy+d), so no temporary
	 * register is needed at all.
	 */
	R("+(A,D(O)):b", PLUS, P_L, P_R, P_NONE, 0, "\tld hl,$RL\n\tadd a,(hl)\n", R_A),
	R("-(A,D(O)):b", MINUS, P_L, P_R, P_NONE, 0, "\tld hl,$RL\n\tsub (hl)\n", R_A),
	R("&(A,D(O)):b", AND, P_L, P_R, P_NONE, 0, "\tld hl,$RL\n\tand (hl)\n", R_A),
	R("|(A,D(O)):b", OR, P_L, P_R, P_NONE, 0, "\tld hl,$RL\n\tor (hl)\n", R_A),
	R("^(A,D(O)):b", XOR, P_L, P_R, P_NONE, 0, "\tld hl,$RL\n\txor (hl)\n", R_A),
	R("+(A,D(I)):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,($RL)\n", R_A),
	R("-(A,D(I)):b", MINUS, P_L, P_R, P_NONE, 0, "\tsub ($RL)\n", R_A),
	R("&(A,D(I)):b", AND, P_L, P_R, P_NONE, 0, "\tand ($RL)\n", R_A),
	R("|(A,D(I)):b", OR, P_L, P_R, P_NONE, 0, "\tor ($RL)\n", R_A),
	R("^(A,D(I)):b", XOR, P_L, P_R, P_NONE, 0, "\txor ($RL)\n", R_A),

	/* byte arithmetic with both operands live: left in A, right in E */
	R("+(A,K):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,e\n", R_A),
	R("-(A,K):b", MINUS, P_L, P_R, P_NONE, 0, "\tsub e\n", R_A),
	R("&(A,K):b", AND, P_L, P_R, P_NONE, 0, "\tand e\n", R_A),
	R("|(A,K):b", OR, P_L, P_R, P_NONE, 0, "\tor e\n", R_A),
	R("^(A,K):b", XOR, P_L, P_R, P_NONE, 0, "\txor e\n", R_A),
	R("&(A,N):b", AND, P_L, P_R, P_NONE, 0, "\tand $R\n", R_A),
	R("&(A,K):bF", AND, P_L, P_R, P_NONE, 0, "\tand e\n", F_NZ),
	R("|(A,N):b", OR, P_L, P_R, P_NONE, 0, "\tor $R\n", R_A),
	R("^(A,N):b", XOR, P_L, P_R, P_NONE, 0, "\txor $R\n", R_A),
	/* no 16-bit and/or/xor on the Z80 - do it a byte at a time */
	R("&(H,N)", AND, P_L, P_R, P_NONE, 0,
		"\tld a,l\n\tand $Rl\n\tld l,a\n\tld a,h\n\tand $Rh\n\tld h,a\n", R_HL),
	R("|(H,N)", OR, P_L, P_R, P_NONE, 0,
		"\tld a,l\n\tor $Rl\n\tld l,a\n\tld a,h\n\tor $Rh\n\tld h,a\n", R_HL),
	R("^(H,N)", XOR, P_L, P_R, P_NONE, 0,
		"\tld a,l\n\txor $Rl\n\tld l,a\n\tld a,h\n\txor $Rh\n\tld h,a\n", R_HL),
	R("&(B,N)", AND, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld a,l\n\tand $Rl\n\tld l,a\n\tld a,h\n\tand $Rh\n\tld h,a\n", R_HL),
	R("|(B,N)", OR, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld a,l\n\tor $Rl\n\tld l,a\n\tld a,h\n\tor $Rh\n\tld h,a\n", R_HL),
	R("^(B,N)", XOR, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld a,l\n\txor $Rl\n\tld l,a\n\tld a,h\n\txor $Rh\n\tld h,a\n", R_HL),
	R("&(H,E)", AND, P_L, P_R, P_NONE, 0, "\tld a,l\n\tand e\n\tld l,a\n\tld a,h\n\tand d\n\tld h,a\n", R_HL),
	R("|(H,E)", OR, P_L, P_R, P_NONE, 0, "\tld a,l\n\tor e\n\tld l,a\n\tld a,h\n\tor d\n\tld h,a\n", R_HL),
	R("^(H,E)", XOR, P_L, P_R, P_NONE, 0, "\tld a,l\n\txor e\n\tld l,a\n\tld a,h\n\txor d\n\tld h,a\n", R_HL),

	/*
	 * Signed compare against zero is just the sign bit, and it has to
	 * be: sbc hl,de sets carry on an unsigned borrow, so the generic
	 * form below says "x < 0" is false for every x.  Must precede the
	 * T/Y(H,N) rules - zero is a subset of NUMBER and first match wins.
	 */
	R("T(H,Z)", LT, P_L, P_R, P_NONE, RF_SIGNL, "\tld a,h\n\tor a\n", F_M),
	R("Y(H,Z)", GE, P_L, P_R, P_NONE, RF_SIGNL, "\tld a,h\n\tor a\n", F_P),
	R("T(B,Z)", LT, P_L, P_R, P_NONE, RF_SIGNL, "\tld a,b\n\tor a\n", F_M),
	R("Y(B,Z)", GE, P_L, P_R, P_NONE, RF_SIGNL, "\tld a,b\n\tor a\n", F_P),

	/* comparisons */
	R("Q(H,E)", EQ, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,de\n", F_Z),
	/* LE/GT have no cheap flag of their own: swap the operands so the
	 * borrow from sbc answers the reversed question. */
	R("W(H,E)", LE, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tor a\n\tsbc hl,de\n", F_NC),
	R("G(H,E)", GT, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tor a\n\tsbc hl,de\n", F_C),
	R("U(H,E)", NEQ, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,de\n", F_NZ),
	R("T(H,E)", LT, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,de\n", F_C),
	R("Y(H,E)", GE, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,de\n", F_NC),
	/* BC operands: the Z80 has add/sbc hl,bc, so no shuffle needed */
	R("Q(H,B)", EQ, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,bc\n", F_Z),
	R("U(H,B)", NEQ, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,bc\n", F_NZ),
	R("T(H,B)", LT, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,bc\n", F_C),
	R("Y(H,B)", GE, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,bc\n", F_NC),
	R("Q(B,E)", EQ, P_L, P_R, P_NONE, 0, T_BC_HL "\tor a\n\tsbc hl,de\n", F_Z),
	R("U(B,E)", NEQ, P_L, P_R, P_NONE, 0, T_BC_HL "\tor a\n\tsbc hl,de\n", F_NZ),
	R("T(B,E)", LT, P_L, P_R, P_NONE, 0, T_BC_HL "\tor a\n\tsbc hl,de\n", F_C),
	R("Y(B,E)", GE, P_L, P_R, P_NONE, 0, T_BC_HL "\tor a\n\tsbc hl,de\n", F_NC),
	R("Q(B,N)", EQ, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_Z),
	R("U(B,N)", NEQ, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_NZ),
	R("T(B,N)", LT, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_C),
	R("Y(B,N)", GE, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_NC),

	R("Q(H,N)", EQ, P_L, P_R, P_NONE, 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_Z),
	R("U(H,N)", NEQ, P_L, P_R, P_NONE, 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_NZ),
	R("T(H,N)", LT, P_L, P_R, P_NONE, 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_C),
	R("Y(H,N)", GE, P_L, P_R, P_NONE, 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_NC),

	/* byte comparisons */
	R("Q(A,N):F", EQ, P_L, P_R, P_NONE, 0, "\tcp $R\n", F_Z),
	R("U(A,N):F", NEQ, P_L, P_R, P_NONE, 0, "\tcp $R\n", F_NZ),
	R("T(A,N):F", LT, P_L, P_R, P_NONE, 0, "\tcp $R\n", F_C),
	R("Y(A,N):F", GE, P_L, P_R, P_NONE, 0, "\tcp $R\n", F_NC),
	R("Q(D(I),N):bF", EQ, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tcp $R\n", F_Z),
	R("U(D(I),N):bF", NEQ, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tcp $R\n", F_NZ),
	R("T(D(I),N):bF", LT, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tcp $R\n", F_C),
	R("Y(D(I),N):bF", GE, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tcp $R\n", F_NC),

	/* relational transformations */
	R("G(A,N):F", GE, P_L, P_R, P_NONE, RF_INC1, "\tcp $R\n", F_NC),
	R("W(A,N):F", LT, P_L, P_R, P_NONE, RF_INC1, "\tcp $R\n", F_C),
	R("G(D(I),N):bF", GE, P_L, P_R, P_NONE, RF_INC1, "\tld a,($LL)\n\tcp $R\n", F_NC),
	R("W(D(I),N):bF", LT, P_L, P_R, P_NONE, RF_INC1, "\tld a,($LL)\n\tcp $R\n", F_C),
	R("G(H,N)", GE, P_L, P_R, P_NONE, RF_INC1, "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_NC),
	R("W(H,N)", LT, P_L, P_R, P_NONE, RF_INC1, "\tld de,$R\n\tor a\n\tsbc hl,de\n", F_C),

	/* NEQ -> BANG(EQ) */
	R("U(_,N)", 0, P_NONE, P_NONE, P_NONE, RF_NOTEQ, NULL, 0),
	R("U", 0, P_NONE, P_NONE, P_NONE, RF_NOTEQ, NULL, 0),

	/* terminator */
	{NULL, 0, 0, 0, 0, 0, NULL, 0}
};

/* Patterns that should not be reduced */
char *preserve[] = {
	"V", "L", "I", "N", "S", "O", "H", "E", "A", "K", "B", "C", NULL
};
