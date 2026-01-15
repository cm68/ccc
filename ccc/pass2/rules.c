/*
 * rules.c - Code generation pattern rules
 *
 * Compact pattern language:
 *   Operators: + * - / % & | ^ < > D V L I N P _ 0 =
 *   Pattern:   op(left,right) or op(child) or op
 *   Examples:  L          matches LOCALVAR
 *              +(D(V),N)  matches PLUS(DEREF(REGVAR),NUMBER)
 *              *(_,P)     matches STAR(any,POW2)
 */
#include <stddef.h>
#include "pass2.h"
#include "rules.h"

struct rule rules[] = {
	/* LOCALVAR -> INDEX */
	{"L", "I", "", "", "", 0, NULL, 0},

	/* REGVAR -> IN* (value is in register) */
	{"V", "B", "", "", "", RF_BC, NULL, 0},
	{"V", "E", "", "", "", RF_DE, NULL, 0},
	{"V", "H", "", "", "", RF_HL, NULL, 0},

	/* REGVAR IX in flag context: test for zero */
	{"V:F", "V", "", "", "", RF_IX, "\tld a,ixl\n\tor a,ixh\n", F_NZ},

	/* INBC in flag context: test for zero */
	{"B:F", "B", "", "", "", 0, "\tld a,c\n\tor a,b\n", F_NZ},

	/* REGVAR byte C in flag context */
	{"V:bF", "V", "", "", "", RF_C, "\tld a,c\n\tor a\n", F_NZ},

	/* REGVAR byte B in flag context */
	{"V:bF", "V", "", "", "", RF_B, "\tld a,b\n\tor a\n", F_NZ},

	/* assign constant to REGVAR C */
	{"=(V,N):b", "=", "L", "R", "L", RF_C, "\tld c,$R\n", R_A},

	/* assign constant to REGVAR B */
	{"=(V,N):b", "=", "L", "R", "L", RF_B, "\tld b,$R\n", R_A},

	/* assign A to REGVAR C */
	{"=(V,A):b", "=", "L", "R", "L", RF_C, "\tld c,a\n", R_A},

	/* assign A to REGVAR B */
	{"=(V,A):b", "=", "L", "R", "L", RF_B, "\tld b,a\n", R_A},

	/* assign HL (low byte) to REGVAR C */
	{"=(V,H):b", "=", "L", "R", "L", RF_C, "\tld c,l\n", R_HL},

	/* assign HL (low byte) to REGVAR B */
	{"=(V,H):b", "=", "L", "R", "L", RF_B, "\tld b,l\n", R_HL},

	/* load REGVAR C to HL (zero-extended) */
	{"=(H,V):b", "=", "L", "R", "R", RF_C, "\tld l,c\n\tld h,0\n", R_HL},

	/* load REGVAR B to HL (zero-extended) */
	{"=(H,V):b", "=", "L", "R", "R", RF_B, "\tld l,b\n\tld h,0\n", R_HL},

	/* REGVAR C -> INA (value in C, byte context) */
	{"V:b", "A", "", "", "", RF_C, "\tld a,c\n", R_A},

	/* REGVAR B -> INA (value in B, byte context) */
	{"V:b", "A", "", "", "", RF_B, "\tld a,b\n", R_A},

	/* INHL in flag context: test for zero */
	{"H:F", "H", "", "", "", 0, "\tld a,l\n\tor a,h\n", F_NZ},

	/* INDE in flag context: test for zero */
	{"E:F", "E", "", "", "", 0, "\tld a,e\n\tor a,d\n", F_NZ},

	/* INA in flag context: test for zero */
	{"A:F", "A", "", "", "", 0, "\tor a\n", F_NZ},

	/* copy IX to HL (must use push/pop) */
	{"=(H,V)", "=", "L", "R", "R", RF_IX, "\tpush ix\n\tpop hl\n", R_HL},

	/* copy IX to BC */
	{"=(B,V)", "=", "L", "R", "R", RF_IX, "\tld c,ixl\n\tld b,ixh\n", R_BC},

	/* copy IX to DE */
	{"=(E,V)", "=", "L", "R", "R", RF_IX, "\tld e,ixl\n\tld d,ixh\n", R_DE},

	/* PLUS(REGVAR IX, NUM) -> INDEX (ix+offset addressing) */
	{"+(V,N)", "I", "", "", "L", RF_IX, NULL, 0},

	/* PLUS(DEREF(REGVAR), NUM) -> INDEX [normalized: const on right] */
	{"+(D(V),N)", "I", "", "", "LL", RF_IXIY, NULL, 0},

	/* PLUS(INDEX, NUM) -> INDEX (combine offsets) */
	{"+(I,N)", "I", "", "", "L", 0, NULL, 0},

	/* STAR(any, POW2) -> LSHIFT [normalized: const on right] */
	{"*(_,P)", "<", "L", "R", "", RF_POW2, NULL, 0},

	/* STAR by small constants with few set bits */
	/* hl*3 = hl + hl*2 */
	{"*(H,3)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*5 = hl + hl*4 */
	{"*(H,5)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*6 = (hl*3)*2 */
	{"*(H,6)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n", R_HL},
	/* hl*7 = hl + (hl*3)*2 */
	{"*(H,7)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*9 = hl + hl*8 */
	{"*(H,9)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*10 = (hl*5)*2 */
	{"*(H,x)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n", R_HL},
	/* hl*11 = hl*10 + hl */
	{"*(H,e)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n"
		"\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*12 = (hl*3)*4 */
	{"*(H,w)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},
	/* hl*14 = (hl*7)*2 */
	{"*(H,f)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n"
		"\tadd hl,de\n\tadd hl,hl\n", R_HL},
	/* hl*15 = hl*14 + hl */
	{"*(H,n)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n"
		"\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*20 = (hl*5)*4 */
	{"*(H,y)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n"
		"\tadd hl,hl\n\tadd hl,hl\n", R_HL},
	/* hl*24 = (hl*3)*8 */
	{"*(H,q)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,de\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},
	/* hl*40 = (hl*5)*8 */
	{"*(H,z)", "*", "L", "", "", 0,
		"\tld d,h\n\tld e,l\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n"
		"\tadd hl,hl\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},

	/* general HL*DE: call runtime multiply */
	{"*(H,E)", "*", "L", "R", "", 0, "\tcall __mul16\n", R_HL},

	/* general HL/DE: call runtime divide */
	{"/(H,E)", "/", "L", "R", "", 0, "\tcall __div16\n", R_HL},

	/* general HL%DE: call runtime modulo */
	{"%(H,E)", "%", "L", "R", "", 0, "\tcall __mod16\n", R_HL},

	/* byte store to indexed: ld (ix+d), n */
	{"=(I,N):b", "=", "L", "R", "", 0, "\tld ($L),$R\n", 0},

	/* short store to indexed: ld (ix+d), low; ld (ix+d+1), hi */
	{"=(I,N):s", "=", "L", "R", "", 0, "\tld ($L),$Rl\n\tld ($L+),$Rh\n", 0},

	/* byte store L to indexed */
	{"=(I,H):b", "=", "L", "R", "", 0, "\tld ($L),l\n", R_HL},
	{"=(I,H):B", "=", "L", "R", "", 0, "\tld ($L),l\n", R_HL},

	/* short store HL to indexed */
	{"=(I,H):s", "=", "L", "R", "", 0, "\tld ($L),l\n\tld ($L+),h\n", 0},

	/* short copy INDEX to INDEX: load then store */
	{"=(I,I):s", "=", "L", "R", "", 0,
		"\tld l,($R)\n\tld h,($R+)\n"
		"\tld ($L),l\n\tld ($L+),h\n", R_HL},

	/* short store DE to indexed */
	{"=(I,E):s", "=", "L", "R", "", 0, "\tld ($L),e\n\tld ($L+),d\n", 0},

	/* byte store to symref: ld (sym), a */
	{"=(O,A):b", "=", "L", "R", "", 0, "\tld ($L),a\n", R_A},

	/* byte store constant to symref */
	{"=(O,N):b", "=", "L", "R", "", 0, "\tld a,$R\n\tld ($L),a\n", R_A},

	/* short store HL to symref */
	{"=(O,H):s", "=", "L", "R", "", 0, "\tld ($L),hl\n", R_HL},

	/* short store constant to symref */
	{"=(O,N):s", "=", "L", "R", "", 0, "\tld hl,$R\n\tld ($L),hl\n", R_HL},

	/* short store BC to indexed */
	{"=(I,B):s", "=", "L", "R", "", 0, "\tld ($L),c\n\tld ($L+),b\n", 0},

	/* load constant to register variable */
	{"=(V,N)", "=", "L", "R", "L", RF_IX, "\tld ix,$R\n", R_IX},
	{"=(V,N)", "=", "L", "R", "L", RF_BC, "\tld bc,$R\n", R_BC},
	{"=(V,N)", "=", "L", "R", "L", RF_DE, "\tld de,$R\n", R_DE},
	{"=(V,N)", "=", "L", "R", "L", RF_HL, "\tld hl,$R\n", R_HL},

	/* load constant to register (already converted) */
	{"=(B,N)", "=", "L", "R", "", 0, "\tld bc,$R\n", R_BC},
	{"=(E,N)", "=", "L", "R", "", 0, "\tld de,$R\n", R_DE},
	{"=(H,N)", "=", "L", "R", "", 0, "\tld hl,$R\n", R_HL},

	/* assign to IX register variable */
	{"=(V,H)", "=", "L", "R", "L", RF_IX, "\tpush hl\n\tpop ix\n", R_IX},
	{"=(V,E)", "=", "L", "R", "L", RF_IX, "\tpush de\n\tpop ix\n", R_IX},
	{"=(V,B)", "=", "L", "R", "L", RF_IX, "\tpush bc\n\tpop ix\n", R_IX},

	/* register-to-register moves */
	{"=(B,H)", "=", "L", "R", "", 0, "\tld c,l\n\tld b,h\n", R_BC},
	{"=(E,H)", "=", "L", "R", "", 0, "\tex de,hl\n", R_DE},
	{"=(H,E)", "=", "L", "R", "", 0, "\tex de,hl\n", R_HL},
	{"=(H,B)", "=", "L", "R", "", 0, "\tld l,c\n\tld h,b\n", R_HL},
	{"=(B,E)", "=", "L", "R", "", 0, "\tld c,e\n\tld b,d\n", R_BC},
	{"=(E,B)", "=", "L", "R", "", 0, "\tld e,c\n\tld d,b\n", R_DE},
	{"=(B,B)", "=", "L", "R", "", 0, "", R_BC},  /* nop */
	{"=(E,E)", "=", "L", "R", "", 0, "", R_DE},  /* nop */
	{"=(H,H)", "=", "L", "R", "", 0, "", R_HL},  /* nop */

	/* assign register to CODE - result already in place */
	{"=(C,H)", "=", "L", "R", "", 0, "", R_HL},  /* nop */
	{"=(C,E)", "=", "L", "R", "", 0, "", R_DE},  /* nop */
	{"=(C,B)", "=", "L", "R", "", 0, "", R_BC},  /* nop */
	{"=(C,A)", "=", "L", "R", "", 0, "", R_A},   /* nop */

	/* assign A (zero-extended) to BC: ld c,a; ld b,0 */
	{"=(B,A)", "=", "L", "R", "", 0, "\tld c,a\n\tld b,0\n", R_BC},

	/* assign A (zero-extended) to HL: ld l,a; ld h,0 */
	{"=(H,A)", "=", "L", "R", "", 0, "\tld l,a\n\tld h,0\n", R_HL},

	/* assign A (zero-extended) to DE: ld e,a; ld d,0 */
	{"=(E,A)", "=", "L", "R", "", 0, "\tld e,a\n\tld d,0\n", R_DE},

	/* BC + constant -> HL (for struct member access via BC pointer) */
	{"+(B,N)", "+", "L", "R", "", 0,
		"\tld l,c\n\tld h,b\n\tld de,$R\n\tadd hl,de\n", R_HL},

	/* BC + small constant -> HL (more efficient for 1-4) */
	{"+(B,M)", "+", "L", "R", "", 0, "\tld l,c\n\tld h,b\n%(\tinc hl\n)", R_HL},

	/* DE + constant -> HL */
	{"+(E,N)", "+", "L", "R", "", 0,
		"\tex de,hl\n\tld de,$R\n\tadd hl,de\n", R_HL},

	/* DE + small constant -> HL (more efficient for 1-4) */
	{"+(E,M)", "+", "L", "R", "", 0, "\tex de,hl\n%(\tinc hl\n)", R_HL},

	/* NEG BC: negate BC register (result in HL) */
	{"g(B)", "g", "L", "", "", 0,
		"\tld a,0\n\tsub c\n\tld l,a\n"
		"\tld a,0\n\tsbc a,b\n\tld h,a\n", R_HL},

	/* NEG HL: negate HL register */
	{"g(H)", "g", "L", "", "", 0,
		"\txor a\n\tsub l\n\tld l,a\n"
		"\tld a,0\n\tsbc a,h\n\tld h,a\n", R_HL},

	/* NEG DE: negate DE register (result in HL) */
	{"g(E)", "g", "L", "", "", 0,
		"\tld a,0\n\tsub e\n\tld l,a\n"
		"\tld a,0\n\tsbc a,d\n\tld h,a\n", R_HL},

	/* PREINC BC: ++bc (result in HL) */
	{"i(B)", "i", "L", "", "", 0, "\tinc bc\n\tld l,c\n\tld h,b\n", R_HL},

	/* PREDEC BC: --bc (result in HL) */
	{"k(B)", "k", "L", "", "", 0, "\tdec bc\n\tld l,c\n\tld h,b\n", R_HL},

	/* PREINC indexed short: ++(ix+d) */
	{"i(I):s", "i", "L", "", "", 0,
		"\tld l,($L)\n\tld h,($L+)\n\tinc hl\n"
		"\tld ($L),l\n\tld ($L+),h\n", R_HL},

	/* PREDEC indexed short: --(ix+d) */
	{"k(I):s", "k", "L", "", "", 0,
		"\tld l,($L)\n\tld h,($L+)\n\tdec hl\n"
		"\tld ($L),l\n\tld ($L+),h\n", R_HL},

	/* PREINC indexed byte: ++(ix+d) - result in A */
	{"i(I):b", "i", "L", "", "", 0, "\tld a,($L)\n\tinc a\n\tld ($L),a\n", R_A},

	/* PREDEC indexed byte: --(ix+d) - result in A */
	{"k(I):b", "k", "L", "", "", 0, "\tld a,($L)\n\tdec a\n\tld ($L),a\n", R_A},

	/* byte store to indexed: ld (ix+d), a */
	{"=(I,A)", "=", "L", "R", "", 0, "\tld ($L),a\n", R_A},

	/* byte store to (hl): ld (hl), n */
	{"=(H,N)", "=", "L", "R", "", 0, NULL, 0},

	/* byte assign A to HL: ld l,a (for byte returns) */
	{"=(H,A):b", "=", "L", "R", "", 0, "\tld l,a\n", R_HL},

	/* byte store REGVAR B to (HL) - store low byte of BC */
	{"=(H,V):b", "=", "L", "R", "R", RF_BC, "\tld (hl),c\n", 0},

	/* byte load from (hl): ld a, (hl) */
	{"D(H):b", "D", "L", "", "", 0, "\tld a,(hl)\n", R_A},

	/* byte load from (bc): move to hl, then load */
	{"D(B):b", "D", "L", "", "", 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n", R_A},

	/* short load from (bc): move to hl, load */
	{"D(B):s", "D", "L", "", "", 0,
		"\tld l,c\n\tld h,b\n\tld a,(hl)\n"
		"\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL},

	/* byte load from (de): move to hl, then load */
	{"D(E):b", "D", "L", "", "", 0, "\tex de,hl\n\tld a,(hl)\n", R_A},

	/* short load from (de): move to hl, load */
	{"D(E):s", "D", "L", "", "", 0,
		"\tex de,hl\n\tld e,(hl)\n"
		"\tinc hl\n\tld d,(hl)\n\tex de,hl\n", R_HL},

	/* byte store A to *bc (indirect through BC) */
	{"=(D(B),A):b", "=", "L", "R", "", 0,
		"\tld l,c\n\tld h,b\n\tld (hl),a\n", 0},

	/* byte store A to *de (indirect through DE) */
	{"=(D(E),A):b", "=", "L", "R", "", 0,
		"\tex de,hl\n\tld (hl),a\n\tex de,hl\n", 0},

	/* short store HL to *bc (indirect through BC) */
	{"=(D(B),H):s", "=", "L", "R", "", 0,
		"\tpush hl\n\tld l,c\n\tld h,b\n\tpop de\n"
		"\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0},

	/* short store HL to *de (indirect through DE) */
	{"=(D(E),H):s", "=", "L", "R", "", 0,
		"\tex de,hl\n\tpush de\n"
		"\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tpop hl\n", 0},

	/* byte store constant to *hl */
	{"=(D(H),N):b", "=", "L", "R", "", 0, "\tld (hl),$R\n", 0},

	/* short store DE to *hl */
	{"=(D(H),E):s", "=", "L", "R", "", 0,
		"\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0},

	/* byte store constant to *bc */
	{"=(D(B),N):b", "=", "L", "R", "", 0,
		"\tld l,c\n\tld h,b\n\tld (hl),$R\n", 0},

	/* short store constant to *hl */
	{"=(D(H),N):s", "=", "L", "R", "", 0,
		"\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n", 0},

	/* short store BC to *hl */
	{"=(D(H),B):s", "=", "L", "R", "", 0,
		"\tld (hl),c\n\tinc hl\n\tld (hl),b\n", 0},

	/* pointer deref for flags (test if pointer is null) */
	{"D(H):pF", "D", "L", "", "", 0, "\tld a,(hl)\n\tor a,(hl)\n", F_NZ},

	/* short load from (hl) to BC: ld c,(hl); inc hl; ld b,(hl) */
	{"=(B,D(H)):s", "=", "L", "R", "", 0,
		"\tld c,(hl)\n\tinc hl\n\tld b,(hl)\n", R_BC},

	/* load indexed address into BC: copy IX+offset to BC */
	{"=(B,I)", "=", "L", "R", "", 0, "\tld c,($R)\n\tld b,($R+)\n", R_BC},

	/* load SYMREF address into BC */
	{"=(B,O)", "=", "L", "R", "", 0, "\tld bc,$R\n", R_BC},

	/* short load from symref into BC: must go via A */
	{"=(B,D(O)):s", "=", "L", "R", "", 0,
		"\tld a,($RL)\n\tld c,a\n\tld a,($RL+)\n\tld b,a\n", R_BC},

	/* short load from symref into DE */
	{"=(E,D(O)):s", "=", "L", "R", "", 0, "\tld de,($RL)\n", R_DE},

	/* short load from symref into HL */
	{"=(H,D(O)):s", "=", "L", "R", "", 0, "\tld hl,($RL)\n", R_HL},

	/* short load from (hl) to DE: ld e,(hl); inc hl; ld d,(hl) */
	{"=(E,D(H)):s", "=", "L", "R", "", 0,
		"\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n", R_DE},

	/* short load from (hl) to HL: need temp */
	{"=(H,D(H)):s", "=", "L", "R", "", 0,
		"\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL},

	/* short store DEREF(HL) to indexed */
	{"=(I,D(H)):s", "=", "L", "R", "", 0,
		"\tld a,(hl)\n\tld ($L),a\n"
		"\tinc hl\n\tld a,(hl)\n\tld ($L+),a\n", 0},

	/* byte or-equals on (hl): ld a,(hl); or N; ld (hl),a */
	{"o(H,N):b", "o", "L", "R", "", 0,
		"\tld a,(hl)\n\tor $R\n\tld (hl),a\n", R_A},

	/* byte or-equals on indexed with E: ld a,(ix+d); or e; ld (ix+d),a */
	{"o(I,K):b", "o", "L", "R", "", 0,
		"\tld a,($L)\n\tor e\n\tld ($L),a\n", R_A},

	/* byte deref indexed for flags: ld a,(ix+d); or a -> Z */
	{"D(I):bF", "D", "L", "", "", 0, "\tld a,($L)\n\tor a\n", F_Z},

	/* short deref indexed for flags: or low,hi -> Z */
	{"D(I):sF", "D", "L", "", "", 0, "\tld a,($L)\n\tor a,($L+)\n", F_Z},

	/* short load from indexed for value: ld t,(ix+d); ld u,(ix+d+1) */
	{"D(I):s", "D", "L", "", "", 0, "\tld $t,($L)\n\tld $u,($L+)\n", 0},

	/* byte load from indexed for value: ld a, (ix+d) */
	{"D(I):b", "D", "L", "", "", 0, NULL, 0},

	/* byte load from symref: ld a, (sym) */
	{"D(O):b", "D", "L", "", "", 0, "\tld a,($L)\n", R_A},

	/* short load from symref: ld hl, (sym) */
	{"D(O):s", "D", "L", "", "", 0, "\tld hl,($L)\n", R_HL},

	/* 16-bit add: add hl, de */
	{"+(H,E)", "+", "L", "R", "", 0, "\tadd hl,de\n", R_HL},

	/* small increment/decrement: use inc/dec instructions */
	{"+(H,M)", "+", "L", "R", "", 0, "%(\tinc hl\n)", R_HL},
	{"-(H,M)", "-", "L", "R", "", 0, "%(\tdec hl\n)", R_HL},
	{"+(A,M)", "+", "L", "R", "", 0, "%(\tinc a\n)", R_A},
	{"-(A,M)", "-", "L", "R", "", 0, "%(\tdec a\n)", R_A},

	/* add constant to HL using DE */
	{"+(H,N)", "+", "L", "R", "", 0, "\tld de,$R\n\tadd hl,de\n", R_HL},

	/* byte add immediate: add a, n */
	{"+(A,N)", "+", "L", "R", "", 0, NULL, 0},

	/* byte add indexed + constant: ld a,(ix+d); add a,n */
	{"+(D(I),N):b", "+", "L", "R", "", 0, "\tld a,($LL)\n\tadd a,$R\n", R_A},

	/* byte sub immediate: sub n */
	{"-(A,N)", "-", "L", "R", "", 0, NULL, 0},

	/* byte sub indexed - constant: ld a,(ix+d); sub n */
	{"-(D(I),N):b", "-", "L", "R", "", 0, "\tld a,($LL)\n\tsub $R\n", R_A},

	/* 16-bit subtract: HL - DE */
	{"-(H,E)", "-", "L", "R", "", 0, "\tor a\n\tsbc hl,de\n", R_HL},

	/* 16-bit subtract constant: HL - N */
	{"-(H,N)", "-", "L", "R", "", 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", R_HL},

	/* 16-bit left shift by constant: use add hl,hl */
	{"<(H,N)", "<", "L", "R", "", 0, "%(\tadd hl,hl\n)", R_HL},

	/* byte left shift: sla a N times */
	{"<(A,N):b", "<", "L", "R", "", 0, "%(\tsla a\n)", R_A},

	/* byte right shift (logical): srl a N times */
	{">(A,N):b", ">", "L", "R", "", 0, "%(\tsrl a\n)", R_A},

	/* 16-bit right shift by small constant (1-4): srl h; rr l repeated */
	{">(H,M)", ">", "L", "R", "", 0, "%(\tsrl h\n\trr l\n)", R_HL},

	/* assign indexed byte to A */
	{"=(A,D(I)):b", "=", "L", "R", "", 0, "\tld a,($RL)\n", R_A},

	/* assign symref byte to A */
	{"=(A,D(O)):b", "=", "L", "R", "", 0, "\tld a,($RL)\n", R_A},

	/* assign A to A: nop */
	{"=(A,A)", "=", "L", "R", "", 0, "", R_A},

	/* byte bit test indexed: bit n,(ix+d) - Z=0 if bit set */
	{"&(D(I),P):bF", "&", "L", "R", "", RF_POW2, "\tbit $R,($LL)\n", F_NZ},

	/* byte AND indexed: ld a,(ix+d); and n */
	{"&(D(I),N):b", "&", "L", "R", "", 0, "\tld a,($LL)\n\tand $R\n", R_A},

	/* byte OR indexed: ld a,(ix+d); or n */
	{"|(D(I),N):b", "|", "L", "R", "", 0, "\tld a,($LL)\n\tor $R\n", R_A},

	/* byte XOR indexed: ld a,(ix+d); xor n */
	{"^(D(I),N):b", "^", "L", "R", "", 0, "\tld a,($LL)\n\txor $R\n", R_A},

	/* byte AND A with constant */
	{"&(A,N):b", "&", "L", "R", "", 0, "\tand $R\n", R_A},

	/* byte AND A with E for flag testing */
	{"&(A,K):bF", "&", "L", "R", "", 0, "\tand e\n", F_NZ},

	/* byte OR A with constant */
	{"|(A,N):b", "|", "L", "R", "", 0, "\tor $R\n", R_A},

	/* byte XOR A with constant */
	{"^(A,N):b", "^", "L", "R", "", 0, "\txor $R\n", R_A},

	/* 16-bit AND: HL & DE */
	{"&(H,E)", "&", "L", "R", "", 0,
		"\tld a,l\n\tand e\n\tld l,a\n"
		"\tld a,h\n\tand d\n\tld h,a\n", R_HL},

	/* 16-bit OR: HL | DE */
	{"|(H,E)", "|", "L", "R", "", 0,
		"\tld a,l\n\tor e\n\tld l,a\n"
		"\tld a,h\n\tor d\n\tld h,a\n", R_HL},

	/* 16-bit XOR: HL ^ DE */
	{"^(H,E)", "^", "L", "R", "", 0,
		"\tld a,l\n\txor e\n\tld l,a\n"
		"\tld a,h\n\txor d\n\tld h,a\n", R_HL},

	/* 16-bit AND with constant */
	{"&(H,N)", "&", "L", "R", "", 0,
		"\tld a,l\n\tand $Rl\n\tld l,a\n"
		"\tld a,h\n\tand $Rh\n\tld h,a\n", R_HL},

	/* 16-bit OR with constant */
	{"|(H,N)", "|", "L", "R", "", 0,
		"\tld a,l\n\tor $Rl\n\tld l,a\n"
		"\tld a,h\n\tor $Rh\n\tld h,a\n", R_HL},

	/* 16-bit XOR with constant */
	{"^(H,N)", "^", "L", "R", "", 0,
		"\tld a,l\n\txor $Rl\n\tld l,a\n"
		"\tld a,h\n\txor $Rh\n\tld h,a\n", R_HL},

	/* bitwise NOT on A */
	{"~(A):b", "~", "L", "", "", 0, "\tcpl\n", R_A},

	/* compare equal: cp n (Z flag) - value already in A */
	{"Q(A,N):F", "Q", "L", "R", "", 0, "\tcp $R\n", F_Z},

	/* compare equal: A with indexed byte (Z flag) - cp (ix+d) */
	{"Q(A,D(I)):bF", "Q", "L", "R", "", 0, "\tcp ($RL)\n", F_Z},

	/* compare equal: A with E (Z flag) - both in regs */
	{"Q(A,K):bF", "Q", "L", "R", "", 0, "\tcp e\n", F_Z},

	/* compare equal: ld a,(sym); cp n (Z flag) */
	{"Q(D(O),N):F", "Q", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_Z},

	/* compare equal byte indexed: ld a,(ix+d); cp n (Z flag) */
	{"Q(D(I),N):bF", "Q", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_Z},

	/* compare less than: cp n (C flag) - value already in A */
	{"T(A,N):F", "T", "L", "R", "", 0, "\tcp $R\n", F_C},

	/* compare less than: A with indexed byte (C flag) - cp (ix+d) */
	{"T(A,D(I)):bF", "T", "L", "R", "", 0, "\tcp ($RL)\n", F_C},

	/* compare less than: A with E (C flag) - both in regs */
	{"T(A,K):bF", "T", "L", "R", "", 0, "\tcp e\n", F_C},

	/* compare 0 < A: just test A for nonzero */
	{"T(Z,A):F", "T", "L", "R", "", 0, "\tor a\n", F_NZ},

	/* compare less than: ld a,(sym); cp n (C flag) */
	{"T(D(O),N):F", "T", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_C},

	/* compare less than byte indexed: ld a,(ix+d); cp n (C flag) */
	{"T(D(I),N):bF", "T", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_C},

	/* compare equal HL with constant: sub and test for zero */
	{"Q(H,N):F", "Q", "L", "R", "", 0,
		"\tld a,l\n\tsub $Rl\n\tld a,h\n\tsbc a,$Rh\n\tor a\n", F_Z},

	/* compare equal BC with constant */
	{"Q(B,N):F", "Q", "L", "R", "", 0,
		"\tld a,c\n\tsub $Rl\n\tld a,b\n\tsbc a,$Rh\n\tor a\n", F_Z},

	/* compare less than HL < constant: unsigned 16-bit compare */
	{"T(H,N):F", "T", "L", "R", "", 0,
		"\tld a,l\n\tsub $Rl\n\tld a,h\n\tsbc a,$Rh\n", F_C},

	/* compare less than BC < constant */
	{"T(B,N):F", "T", "L", "R", "", 0,
		"\tld a,c\n\tsub $Rl\n\tld a,b\n\tsbc a,$Rh\n", F_C},

	/* compare GE HL >= constant: unsigned 16-bit compare */
	{"Y(H,N):F", "Y", "L", "R", "", 0,
		"\tld a,l\n\tsub $Rl\n\tld a,h\n\tsbc a,$Rh\n", F_NC},

	/* compare GE BC >= constant */
	{"Y(B,N):F", "Y", "L", "R", "", 0,
		"\tld a,c\n\tsub $Rl\n\tld a,b\n\tsbc a,$Rh\n", F_NC},

	/* NEQ -> BANG(EQ): normalize for conditional jumps */
	{"U(_,_)", "!", "L", "R", "", RF_NOTEQ, NULL, 0},

	/* GE: cp n, jp nc (cheap - direct flag) - value already in A */
	{"Y(A,N):F", "Y", "L", "R", "", 0, "\tcp $R\n", F_NC},

	/* GE: A with indexed byte (NC flag) - cp (ix+d) */
	{"Y(A,D(I)):bF", "Y", "L", "R", "", 0, "\tcp ($RL)\n", F_NC},

	/* GE: A with E (NC flag) - both in regs */
	{"Y(A,K):bF", "Y", "L", "R", "", 0, "\tcp e\n", F_NC},

	/* GE: ld a,(sym); cp n (NC flag) */
	{"Y(D(O),N):F", "Y", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_NC},

	/* GE byte indexed: ld a,(ix+d); cp n (NC flag) */
	{"Y(D(I),N):bF", "Y", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_NC},

	/* GT(a,n) -> GE(a,n+1): a > n iff a >= n+1 */
	{"G(_,N)", "Y", "L", "R", "", RF_INC1, NULL, 0},

	/* LE(a,n) -> LT(a,n+1): a <= n iff a < n+1 */
	{"W(_,N)", "T", "L", "R", "", RF_INC1, NULL, 0},

	/* short pre-increment through (hl): load, inc, store, return new */
	{"i(H):s", "i", "L", "", "", 0,
		"\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tinc de\n"
		"\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tex de,hl\n", R_HL},

	/* short post-increment through (hl): load, store inc'd, return old */
	{"j(H):s", "j", "L", "", "", 0,
		"\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tpush de\n"
		"\tinc de\n\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tpop hl\n", R_HL},

	/* short pre-decrement through (hl) */
	{"k(H):s", "k", "L", "", "", 0,
		"\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tdec de\n"
		"\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tex de,hl\n", R_HL},

	/* short post-decrement through (hl) */
	{"m(H):s", "m", "L", "", "", 0,
		"\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tpush de\n"
		"\tdec de\n\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tpop hl\n", R_HL},

	/* short post-increment symref */
	{"j(O):s", "j", "L", "", "", 0,
		"\tld hl,($L)\n\tinc hl\n\tld ($L),hl\n\tdec hl\n", R_HL},

	/* short pre-increment symref */
	{"i(O):s", "i", "L", "", "", 0,
		"\tld hl,($L)\n\tinc hl\n\tld ($L),hl\n", R_HL},

	/* short pre-decrement symref */
	{"k(O):s", "k", "L", "", "", 0,
		"\tld hl,($L)\n\tdec hl\n\tld ($L),hl\n", R_HL},

	/* short post-decrement symref */
	{"m(O):s", "m", "L", "", "", 0,
		"\tld hl,($L)\n\tdec hl\n\tld ($L),hl\n\tinc hl\n", R_HL},

	/* byte pre-increment through (hl) */
	{"i(H):b", "i", "L", "", "", 0, "\tinc (hl)\n\tld a,(hl)\n", R_A},

	/* byte post-increment through (hl) */
	{"j(H):b", "j", "L", "", "", 0, "\tld a,(hl)\n\tinc (hl)\n", R_A},

	/* byte pre-increment indexed: inc then load new value */
	{"i(I):b", "i", "L", "", "", 0, "\tinc ($L)\n\tld a,($L)\n", R_A},

	/* byte post-increment indexed: load old value then inc */
	{"j(I):b", "j", "L", "", "", 0, "\tld a,($L)\n\tinc ($L)\n", R_A},

	/* byte pre-decrement indexed: dec then load new value */
	{"k(I):b", "k", "L", "", "", 0, "\tdec ($L)\n\tld a,($L)\n", R_A},

	/* byte post-decrement indexed: load old value then dec */
	{"m(I):b", "m", "L", "", "", 0, "\tld a,($L)\n\tdec ($L)\n", R_A},

	/* byte pre-increment symref: inc then load new value */
	{"i(O):b", "i", "L", "", "", 0,
		"\tld hl,$L\n\tinc (hl)\n\tld a,(hl)\n", R_A},

	/* byte post-increment symref: load old value then inc */
	{"j(O):b", "j", "L", "", "", 0,
		"\tld hl,$L\n\tld a,(hl)\n\tinc (hl)\n", R_A},

	/* byte pre-decrement symref: dec then load new value */
	{"k(O):b", "k", "L", "", "", 0,
		"\tld hl,$L\n\tdec (hl)\n\tld a,(hl)\n", R_A},

	/* byte post-decrement symref: load old value then dec */
	{"m(O):b", "m", "L", "", "", 0,
		"\tld hl,$L\n\tld a,(hl)\n\tdec (hl)\n", R_A},

	/* SYM + NUMBER -> SYMREF (linker-resolvable) */
	{"+(S,N)", "O", "", "", "", 0, NULL, 0},

	/* SYMREF + NUMBER -> SYMREF with combined offset */
	{"+(O,N)", "O", "", "", "", 0, NULL, 0},

	/* bare SYM -> SYMREF with offset 0 */
	{"S", "O", "", "", "", 0, NULL, 0},

	/* NUMBER in value context: load into register */
	{"N:bV", "C", "", "", "", 0, NULL, R_A},
	{"N:sV", "C", "", "", "", 0, NULL, R_HL},
	{"N:lV", "C", "", "", "", 0, NULL, R_HL},
	{"N:LV", "C", "", "", "", 0, NULL, R_HL},
	{"N:fV", "C", "", "", "", 0, NULL, R_HL},

	/* NUMBER without context: still load into register */
	{"N:b", "C", "", "", "", 0, NULL, R_A},
	{"N:s", "C", "", "", "", 0, NULL, R_HL},
	{"N:p", "C", "", "", "", 0, NULL, R_HL},
	{"N:l", "C", "", "", "", 0, NULL, R_HL},
	{"N:L", "C", "", "", "", 0, NULL, R_HL},
	{"N:f", "C", "", "", "", 0, NULL, R_HL},

	/* ARGNODE: push register pairs */
	{"a(H)", "a", "L", "", "", 0, "\tpush hl\n", 0},
	{"a(E)", "a", "L", "", "", 0, "\tpush de\n", 0},
	{"a(B)", "a", "L", "", "", 0, "\tpush bc\n", 0},

	/* ARGNODE: push constant */
	{"a(N)", "a", "L", "", "", 0, "\tld hl,$L\n\tpush hl\n", 0},

	/* ARGNODE: push symbol address */
	{"a(O)", "a", "L", "", "", 0, "\tld hl,$L\n\tpush hl\n", 0},

	/* ARGNODE: push register variable */
	{"a(V)", "a", "L", "", "L", RF_BC, "\tpush bc\n", 0},
	{"a(V)", "a", "L", "", "L", RF_DE, "\tpush de\n", 0},
	{"a(V)", "a", "L", "", "L", RF_HL, "\tpush hl\n", 0},
	{"a(V)", "a", "L", "", "L", RF_IX, "\tpush ix\n", 0},

	/* ARGNODE: push byte A (extend to 16-bit, push) */
	{"a(A)", "a", "L", "", "", 0, "\tld l,a\n\tld h,0\n\tpush hl\n", 0},

	/* ARGNODE: push byte index value (extend to 16-bit) */
	{"a(D(I)):b", "a", "L", "", "", 0, "\tld l,($LL)\n\tld h,0\n\tpush hl\n", 0},

	/* ARGNODE: push short index value */
	{"a(D(I)):s", "a", "L", "", "", 0,
		"\tld l,($LL)\n\tld h,($LL+)\n\tpush hl\n", 0},

	/* ARGNODE: push symref deref short */
	{"a(D(O)):s", "a", "L", "", "", 0, "\tld hl,($LL)\n\tpush hl\n", 0},

	/* ARGNODE: push symref deref byte */
	{"a(D(O)):b", "a", "L", "", "", 0,
		"\tld a,($LL)\n\tld l,a\n\tld h,0\n\tpush hl\n", 0},

	/* Store HL to indexed (pointer width) */
	{"=(I,H):p", "=", "L", "R", "", 0, "\tld ($L),l\n\tld ($L+),h\n", 0},

	/* Store HL to symref (pointer width) */
	{"=(O,H):p", "=", "L", "R", "", 0, "\tld ($L),hl\n", R_HL},

	/* Load symref pointer to HL */
	{"D(O):p", "D", "L", "", "", 0, "\tld hl,($L)\n", R_HL},

	/* Load indexed pointer to HL */
	{"D(I):p", "D", "L", "", "", 0, "\tld l,($L)\n\tld h,($L+)\n", R_HL},

	/* assign constant to A */
	{"=(A,N):b", "=", "L", "R", "", 0, "\tld a,$R\n", R_A},

	/* short assign BC to symref */
	{"=(O,B):s", "=", "L", "R", "", 0,
		"\tld a,c\n\tld ($L),a\n\tld a,b\n\tld ($L+),a\n", 0},

	/*
	 * Long (32-bit) operations
	 * HLDE convention: DE=low word, HL=high word
	 */

	/* Long load from indexed: 4 bytes from (ix+d) to HLDE */
	{"D(I):l", "D", "L", "", "", 0,
		"\tld e,($L)\n\tld d,($L+)\n\tld l,($L++)\n\tld h,($L+++)\n", R_HL},
	{"D(I):L", "D", "L", "", "", 0,
		"\tld e,($L)\n\tld d,($L+)\n\tld l,($L++)\n\tld h,($L+++)\n", R_HL},

	/* Long store HLDE to indexed: 4 bytes to (ix+d) */
	{"=(I,H):l", "=", "L", "R", "", 0,
		"\tld ($L),e\n\tld ($L+),d\n\tld ($L++),l\n\tld ($L+++),h\n", 0},
	{"=(I,H):L", "=", "L", "R", "", 0,
		"\tld ($L),e\n\tld ($L+),d\n\tld ($L++),l\n\tld ($L+++),h\n", 0},

	/* Long load from symref: call helper */
	{"D(O):l", "D", "L", "", "", 0,
		"\tld hl,$L\n\tcall lld\n", R_HL},
	{"D(O):L", "D", "L", "", "", 0,
		"\tld hl,$L\n\tcall lld\n", R_HL},

	/* Long store HLDE to symref: call helper */
	{"=(O,H):l", "=", "L", "R", "", 0,
		"\tpush hl\n\tld hl,$L\n\tcall lstde\n\tpop hl\n", 0},
	{"=(O,H):L", "=", "L", "R", "", 0,
		"\tpush hl\n\tld hl,$L\n\tcall lstde\n\tpop hl\n", 0},

	/* Long ARGNODE: push 4 bytes (DE then HL) */
	{"a(H):l", "a", "L", "", "", 0, "\tpush de\n\tpush hl\n", 0},
	{"a(H):L", "a", "L", "", "", 0, "\tpush de\n\tpush hl\n", 0},

	/* Long pre-increment: ++(*ptr) via helper */
	{"i(H):l", "i", "L", "", "", 0, "\tcall lainc\n", R_HL},
	{"i(H):L", "i", "L", "", "", 0, "\tcall llinc\n", R_HL},

	/* Long pre-decrement: --(*ptr) via helper */
	{"k(H):l", "k", "L", "", "", 0, "\tcall ladec\n", R_HL},
	{"k(H):L", "k", "L", "", "", 0, "\tcall lldec\n", R_HL},

	/* Long indexed for flags: test HLDE for zero */
	{"D(I):lF", "D", "L", "", "", 0,
		"\tld a,($L)\n\tor ($L+)\n\tor ($L++)\n\tor ($L+++)\n", F_NZ},
	{"D(I):LF", "D", "L", "", "", 0,
		"\tld a,($L)\n\tor ($L+)\n\tor ($L++)\n\tor ($L+++)\n", F_NZ},

	/* INHL long in flag context: test HLDE for zero */
	{"H:lF", "H", "", "", "", 0,
		"\tld a,e\n\tor d\n\tor l\n\tor h\n", F_NZ},
	{"H:LF", "H", "", "", "", 0,
		"\tld a,e\n\tor d\n\tor l\n\tor h\n", F_NZ},

	/*
	 * Float (32-bit) operations
	 * Same HLDE convention as longs (DE=low, HL=high)
	 */

	/* Float load from indexed: 4 bytes from (ix+d) to HLDE */
	{"D(I):f", "D", "L", "", "", 0,
		"\tld e,($L)\n\tld d,($L+)\n\tld l,($L++)\n\tld h,($L+++)\n", R_HL},

	/* Float store HLDE to indexed: 4 bytes to (ix+d) */
	{"=(I,H):f", "=", "L", "R", "", 0,
		"\tld ($L),e\n\tld ($L+),d\n\tld ($L++),l\n\tld ($L+++),h\n", 0},

	/* Float load from symref: call helper */
	{"D(O):f", "D", "L", "", "", 0,
		"\tld hl,$L\n\tcall lld\n", R_HL},

	/* Float store HLDE to symref: call helper */
	{"=(O,H):f", "=", "L", "R", "", 0,
		"\tpush hl\n\tld hl,$L\n\tcall lstde\n\tpop hl\n", 0},

	/* Float ARGNODE: push 4 bytes (DE then HL) */
	{"a(H):f", "a", "L", "", "", 0, "\tpush de\n\tpush hl\n", 0},

	/* Float pre-increment: ++(*ptr) via helper */
	{"i(H):f", "i", "L", "", "", 0, "\tcall lfinc\n", R_HL},

	/* Float pre-decrement: --(*ptr) via helper */
	{"k(H):f", "k", "L", "", "", 0, "\tcall lfdec\n", R_HL},

	/* Float indexed for flags: test for non-zero (not IEEE compliant but simple) */
	{"D(I):fF", "D", "L", "", "", 0,
		"\tld a,($L)\n\tor ($L+)\n\tor ($L++)\n\tor ($L+++)\n", F_NZ},

	/* INHL float in flag context: test HLDE for zero */
	{"H:fF", "H", "", "", "", 0,
		"\tld a,e\n\tor d\n\tor l\n\tor h\n", F_NZ},

	{NULL, NULL, NULL, NULL, NULL, 0, NULL, 0}
};

/*
 * Preserve patterns - subtrees matching these are not reduced.
 * Uses same pattern syntax as rules. Checked during labeling.
 */
char *preserve[] = {
	"D(I)",		/* DEREF(INDEX) - can use cp (ix+d) */
	"D(O)",		/* DEREF(SYMREF) - can use ld hl,sym; cp (hl) */
	"N",		/* NUMBER - can use cp n, or +(V,N)->I */
	"I",		/* INDEX - preserve for direct addressing */
	"+(V,N)",	/* ADD(REGVAR,NUMBER) - can become INDEX */
	NULL
};

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
