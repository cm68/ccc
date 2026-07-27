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
#define T_LD_IHL "\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n"
#define T_SUB_DE "\tor a\n\tsbc hl,de\n"
#define T_SUB_BC "\tor a\n\tsbc hl,bc\n"
/*
 * Turn the result of a signed subtraction into a sign flag: the answer
 * is sign exclusive-or overflow, so flip the top bit of the high byte
 * when P/V says the subtraction overflowed, then let or a set S from
 * it.  M means less than, P means greater or equal.
 */
#define T_SXORV "\tld a,h\n\tjp po,$$+5\n\txor 80h\n\tor a\n"
/* address on the stack, value in HL -> address in HL, value in DE */
#define T_SWAP_ADDR "\tpop de\n\tex de,hl\n"
/* store DE through HL, then bring the value back to HL */
#define T_ST_IHL "\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tex de,hl\n"
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
	/*
	 * Array element with a variable subscript: the base is a frame
	 * slot and the scaled index is already in HL, so form the address
	 * rather than let (ix+d) do it.  A constant subscript never gets
	 * here - +(I,N) above folds it straight into the offset.
	 */
	R("+(I,H)", PLUS, P_L, P_R, P_NONE, 0,
		"\tpush $Lr\n\tpop de\n\tadd hl,de\n\tld de,$Lo\n\tadd hl,de\n", R_HL),
	/* the same with the subscript already in DE, which is where the
	 * reorder leaves it when the index is the costlier side */
	R("+(I,E)", PLUS, P_L, P_R, P_NONE, 0,
		"\tpush $Lr\n\tpop hl\n\tadd hl,de\n\tld de,$Lo\n\tadd hl,de\n", R_HL),

	/* symbol + constant offset folds into the SYMREF */
	R("+(O,N)", SYMREF, P_NONE, P_NONE, P_NONE, 0, NULL, 0),
	/*
	 * The same for a global array, where the base is a link-time
	 * constant and the scaled subscript is in a register - one add,
	 * with the base loaded into whichever half is free.  A constant
	 * subscript never reaches here: +(O,N) above folds it away.
	 */
	R("+(O,H)", PLUS, P_L, P_R, P_NONE, 0, "\tld de,$L\n\tadd hl,de\n", R_HL),
	R("+(O,E)", PLUS, P_L, P_R, P_NONE, 0, "\tld hl,$L\n\tadd hl,de\n", R_HL),

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
	R("*(H,E)", STAR, P_L, P_R, P_NONE, 0, "\tcall amul\n", R_HL),
	R("/(H,E)", DIV, P_L, P_R, P_NONE, RF_SIGNL, "\tcall adiv\n", R_HL),
	R("/(H,E)", DIV, P_L, P_R, P_NONE, 0, "\tcall ldiv\n", R_HL),
	R("%(H,E)", MOD, P_L, P_R, P_NONE, RF_SIGNL, "\tcall amod\n", R_HL),
	R("%(H,E)", MOD, P_L, P_R, P_NONE, 0, "\tcall lmod\n", R_HL),

	/*
	 * Store to a frame slot.  A constant goes straight into the slot
	 * without touching a register, which is the right thing for a
	 * statement and the wrong thing for "i = k = 5": the assignment
	 * has a value, and there has to be one somewhere for the outer
	 * assignment to copy.  The :V forms pay for a register because
	 * something is going to read it.
	 */
	R("=(I,N):bV", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n\tld ($L),a\n", R_A),
	R("=(I,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),$R\n", 0),
	R("=(I,N):sV", ASSIGN, P_L, P_R, P_NONE, 0, "\tld hl,$R\n" T_IDX_S_ST, R_HL),
	R("=(I,N):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),$Rl\n\tld ($L+),$Rh\n", 0),
	R("=(I,H):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),l\n", R_HL),
	/*
	 * These stored from a register and left the value in it, so they
	 * name it.  Claiming whatever register the node was aimed at
	 * would hand the parent one that was never written.
	 */
	R("=(I,H):s", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_S_ST, R_HL),
	R("=(I,I):s", ASSIGN, P_L, P_R, P_NONE, 0, T_IDX_S_LD T_IDX_S_ST, R_HL),
	R("=(I,E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),e\n\tld ($L+),d\n", R_DE),
	R("=(I,B):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),c\n\tld ($L+),b\n", R_BC),

	/* store to symref */
	R("=(O,A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),a\n", R_A),
	R("=(O,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n\tld ($L),a\n", R_A),
	R("=(O,H):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),hl\n", R_HL),
	/* narrowing store: a word result keeps only its low byte */
	R("=(O,H):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,l\n\tld ($L),a\n", R_A),
	R("=(O,B):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,c\n\tld ($L),a\n", R_A),
	R("=(I,B):bV", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,c\n\tld ($L),a\n", R_A),
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
	R("X(A):s", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tld $t,a\n\trla\n\tsbc a,a\n\tld $u,a\n", 0),

	/*
	 * 32-bit values live in HL:DE, high word in HL.
	 *
	 * Sign-extending into that means the value becomes the low word
	 * and the high word becomes all sign bits - rla puts bit 15 (or
	 * bit 7 for a byte) into carry and sbc a,a spreads it.
	 */
	R("X(H):l", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tex de,hl\n\tld a,d\n\trla\n\tsbc a,a\n\tld h,a\n\tld l,a\n", R_HL),
	R("X(B):l", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tld e,c\n\tld d,b\n\tld a,b\n\trla\n\tsbc a,a\n"
		"\tld h,a\n\tld l,a\n", R_HL),
	R("X(A):l", SEXT, P_L, P_NONE, P_NONE, 0,
		"\tld e,a\n\trla\n\tsbc a,a\n\tld d,a\n\tld h,a\n\tld l,a\n", R_HL),
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
		"\tld hl,$L\n\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n"
		"\tinc hl\n\tld (hl),$R2\n\tinc hl\n\tld (hl),$R3\n", 0),
	/* a long already in HL:DE is the return value as it stands */
	R("=(H,C):l", ASSIGN, P_L, P_R, P_NONE, 0, "", R_HL),

	/* test a long in memory for zero */
	R("D(O):lF", DEREF, P_L, P_NONE, P_NONE, 0,
		"\tld hl,$L\n\tld a,(hl)\n\tinc hl\n\tor (hl)\n\tinc hl\n"
		"\tor (hl)\n\tinc hl\n\tor (hl)\n", F_NZ),

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
	/* inc/dec a word global in place */
	R("i(O):s", PREINC, P_L, P_NONE, P_NONE, 0,
		"\tld hl,($L)\n\tinc hl\n\tld ($L),hl\n", R_HL),
	R("k(O):s", PREDEC, P_L, P_NONE, P_NONE, 0,
		"\tld hl,($L)\n\tdec hl\n\tld ($L),hl\n", R_HL),
	R("j(O):s", POSTINC, P_L, P_NONE, P_NONE, 0,
		"\tld hl,($L)\n\tinc hl\n\tld ($L),hl\n\tdec hl\n", R_HL),
	R("m(O):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
		"\tld hl,($L)\n\tdec hl\n\tld ($L),hl\n\tinc hl\n", R_HL),

	/*
	 * inc/dec through an address in HL.  Reading the word costs the
	 * pointer, so it goes on the stack first and comes back to do the
	 * store.  Postfix then undoes the update to get its old value,
	 * the same trick the memory forms use.
	 */
	R("i(H):s", PREINC, P_L, P_NONE, P_NONE, 0,
		"\tpush hl\n" T_LD_IHL "\tinc hl\n" T_SWAP_ADDR T_ST_IHL, R_HL),
	R("k(H):s", PREDEC, P_L, P_NONE, P_NONE, 0,
		"\tpush hl\n" T_LD_IHL "\tdec hl\n" T_SWAP_ADDR T_ST_IHL, R_HL),
	R("j(H):s", POSTINC, P_L, P_NONE, P_NONE, 0,
		"\tpush hl\n" T_LD_IHL "\tinc hl\n" T_SWAP_ADDR T_ST_IHL
		"\tdec hl\n", R_HL),
	R("m(H):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
		"\tpush hl\n" T_LD_IHL "\tdec hl\n" T_SWAP_ADDR T_ST_IHL
		"\tinc hl\n", R_HL),

	/* postfix yields the old value, so read before updating */
	R("j(B)", POSTINC, P_L, P_NONE, P_NONE, 0, "\tld l,c\n\tld h,b\n\tinc bc\n", R_HL),
	R("m(B)", POSTDEC, P_L, P_NONE, P_NONE, 0, "\tld l,c\n\tld h,b\n\tdec bc\n", R_HL),
	/*
	 * Postfix on a word in memory.  The old value is wanted as the
	 * result and the new one in store, and rather than hold both, the
	 * update is undone afterwards - one byte, against a push/pop pair
	 * or a shuffle through DE.
	 */
	R("j(I):s", POSTINC, P_L, P_NONE, P_NONE, 0,
	  "\tld $t,($L)\n\tld $u,($L+)\n\tinc $T\n"
	  "\tld ($L),$t\n\tld ($L+),$u\n\tdec $T\n", 0),
	R("m(I):s", POSTDEC, P_L, P_NONE, P_NONE, 0,
	  "\tld $t,($L)\n\tld $u,($L+)\n\tdec $T\n"
	  "\tld ($L),$t\n\tld ($L+),$u\n\tinc $T\n", 0),
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
	/*
	 * Load a word through HL.  Where the result is wanted in DE it
	 * can go straight there - three bytes instead of four, no
	 * exchange, and A is left alone.  Only when it has to land back
	 * in HL does A get used as the carrier, because then the pointer
	 * and the result are the same register.
	 */
	R("D(H):s", DEREF, P_L, P_NONE, P_NONE, RF_TDE,
		"\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n", R_DE),
	R("D(H):s", DEREF, P_L, P_NONE, P_NONE, 0, T_LD_IHL, R_HL),
	R("D(B):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n", R_A),
	R("D(B):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL),
	R("D(E):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tex de,hl\n\tld a,(hl)\n", R_A),
	R("D(E):s", DEREF, P_L, P_NONE, P_NONE, 0, "\tex de,hl\n\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tex de,hl\n", R_HL),

	/* indirect stores via registers */
	R("=(D(B),A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld l,c\n\tld h,b\n\tld (hl),a\n", 0),
	R("=(D(E),A):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tld (hl),a\n\tex de,hl\n", 0),
	R("=(D(B),H):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tpush hl\n\tld l,c\n\tld h,b\n\tpop de\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0),
	R("=(D(E),H):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tex de,hl\n\tpush de\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tpop hl\n", 0),

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

	/*
	 * Store through a pointer that itself lives in memory - a pointer
	 * parameter, say.  Load it first; the HL form has to shuffle
	 * through DE because HL is holding the value.
	 */
	R("=(D(I),N):s", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld l,($LL)\n\tld h,($LL+)\n\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n", 0),
	R("=(D(I),H):s", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld e,($LL)\n\tld d,($LL+)\n\tex de,hl\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0),
	R("=(D(I),N):b", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld l,($LL)\n\tld h,($LL+)\n\tld (hl),$R\n", 0),
	R("=(D(I),A):b", ASSIGN, P_L, P_R, P_NONE, 0,
		"\tld l,($LL)\n\tld h,($LL+)\n\tld (hl),a\n", 0),

	/* indirect stores via HL */
	R("=(D(H),N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),$R\n", 0),
	R("=(D(H),E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0),
	R("=(D(H),E):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld (hl),e\n", 0),
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
	/* the plain load: a byte local reaching A, as D(O):b does for a
	 * global.  Without it a byte local could only be read by the rules
	 * that match its parent too, so SEXT of one had nothing to widen */
	R("D(I):b", DEREF, P_L, P_NONE, P_NONE, 0, "\tld a,($L)\n", R_A),
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
	R("-(H,B)", MINUS, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,bc\n", R_HL),
	R("+(B,E)", PLUS, P_L, P_R, P_NONE, 0, T_BC_HL T_ADD_HL_DE, R_HL),
	R("-(B,E)", MINUS, P_L, P_R, P_NONE, 0, T_BC_HL "\tor a\n\tsbc hl,de\n", R_HL),
	R("-(B,N)", MINUS, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tor a\n\tsbc hl,de\n", R_HL),
	R("<(B,N)", LSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(" T_ADD_HL_HL ")", R_HL),
	R("/(B,N)", DIV, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL "\tld de,$R\n\tcall adiv\n", R_HL),
	R("/(B,N)", DIV, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tcall ldiv\n", R_HL),
	R("%(B,N)", MOD, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL "\tld de,$R\n\tcall amod\n", R_HL),
	R("%(B,N)", MOD, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tcall lmod\n", R_HL),
	R("*(B,N)", STAR, P_L, P_R, P_NONE, 0,
		T_BC_HL "\tld de,$R\n\tcall amul\n", R_HL),
	R("+(H,M)", PLUS, P_L, P_R, P_NONE, 0, "%(\tinc hl\n)", R_HL),
	R("-(H,M)", MINUS, P_L, P_R, P_NONE, 0, "%(\tdec hl\n)", R_HL),
	R("+(A,M)", PLUS, P_L, P_R, P_NONE, 0, "%(\tinc a\n)", R_A),
	R("-(A,M)", MINUS, P_L, P_R, P_NONE, 0, "%(\tdec a\n)", R_A),
	R("+(H,N)", PLUS, P_L, P_R, P_NONE, 0, "\tld de,$R\n" T_ADD_HL_DE, R_HL),
	/*
	 * A byte in A against a constant, once it is too big for the inc
	 * and dec runs above.  Only at byte width: at word width A holds
	 * the low half and the carry would have nowhere to go.
	 */
	R("+(A,N):b", PLUS, P_L, P_R, P_NONE, 0, "\tadd a,$R\n", R_A),
	R("+(D(I),N):b", PLUS, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tadd a,$R\n", R_A),
	R("-(A,N):b", MINUS, P_L, P_R, P_NONE, 0, "\tsub $R\n", R_A),
	R("-(D(I),N):b", MINUS, P_L, P_R, P_NONE, 0, "\tld a,($LL)\n\tsub $R\n", R_A),
	R("-(H,E)", MINUS, P_L, P_R, P_NONE, 0, "\tor a\n\tsbc hl,de\n", R_HL),
	R("-(H,N)", MINUS, P_L, P_R, P_NONE, 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", R_HL),

	/* shifts */
	R("<(H,N)", LSHIFT, P_L, P_R, P_NONE, 0, "%(" T_ADD_HL_HL ")", R_HL),
	R("<(A,N):b", LSHIFT, P_L, P_R, P_NONE, 0, "%(\tsla a\n)", R_A),
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
		"\tld l,h\n\tld a,h\n\trla\n\tsbc a,a\n\tld h,a\n", R_HL),
	R(">(H,8)", RSHIFT, P_L, P_R, P_NONE, 0, "\tld l,h\n\tld h,0\n", R_HL),
	R("<(H,8)", LSHIFT, P_L, P_R, P_NONE, 0, "\tld h,l\n\tld l,0\n", R_HL),
	R(">(H,M)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL, "%(\tsra h\n\trr l\n)", R_HL),
	R(">(H,M)", RSHIFT, P_L, P_R, P_NONE, 0, "%(\tsrl h\n\trr l\n)", R_HL),
	R(">(B,M)", RSHIFT, P_L, P_R, P_NONE, RF_SIGNL,
		T_BC_HL "%(\tsra h\n\trr l\n)", R_HL),
	R(">(B,M)", RSHIFT, P_L, P_R, P_NONE, 0, T_BC_HL "%(\tsrl h\n\trr l\n)", R_HL),

	/* stores/loads with indexed/symref */
	R("=(A,D(I)):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n", R_A),
	R("=(A,D(O)):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,($RL)\n", R_A),
	R("=(A,A)", ASSIGN, P_L, P_R, P_NONE, 0, "", R_A),
	R("=(A,N):b", ASSIGN, P_L, P_R, P_NONE, 0, "\tld a,$R\n", R_A),
	/* a byte in A stepped in place */
	R("i(A):b", PREINC, P_L, P_NONE, P_NONE, 0, "\tinc a\n", R_A),
	R("k(A):b", PREDEC, P_L, P_NONE, P_NONE, 0, "\tdec a\n", R_A),
	/* storing a word already in DE to a global, so a nested
	 * assignment can be used for its value */
	R("=(O,E):s", ASSIGN, P_L, P_R, P_NONE, 0, "\tld ($L),de\n", R_DE),

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
		"\tld a,h\n\tor a\n\tjp m,$$+7\n\tld a,h\n\tor l\n"
		"\tjr $$+3\n\txor a\n", F_NZ),
	R("W(H,Z)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld a,h\n\tor a\n\tjp m,$$+7\n\tld a,h\n\tor l\n"
		"\tjr $$+3\n\txor a\n", F_Z),
	R("G(B,Z)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld a,b\n\tor a\n\tjp m,$$+7\n\tld a,b\n\tor c\n"
		"\tjr $$+3\n\txor a\n", F_NZ),
	R("W(B,Z)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld a,b\n\tor a\n\tjp m,$$+7\n\tld a,b\n\tor c\n"
		"\tjr $$+3\n\txor a\n", F_Z),

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
		"\tex de,hl\n" T_SUB_DE T_SXORV, F_P),
	R("G(H,E)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		"\tex de,hl\n" T_SUB_DE T_SXORV, F_M),
	R("T(H,B)", LT, P_L, P_R, P_NONE, RF_SIGNL, T_SUB_BC T_SXORV, F_M),
	R("Y(H,B)", GE, P_L, P_R, P_NONE, RF_SIGNL, T_SUB_BC T_SXORV, F_P),
	R("W(H,B)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld e,c\n\tld d,b\n\tex de,hl\n" T_SUB_DE T_SXORV, F_P),
	R("G(H,B)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld e,c\n\tld d,b\n\tex de,hl\n" T_SUB_DE T_SXORV, F_M),
	R("T(H,N)", LT, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld de,$R\n" T_SUB_DE T_SXORV, F_M),
	R("Y(H,N)", GE, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld de,$R\n" T_SUB_DE T_SXORV, F_P),
	R("W(H,N)", LE, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld de,$R\n\tex de,hl\n" T_SUB_DE T_SXORV, F_P),
	R("G(H,N)", GT, P_L, P_R, P_NONE, RF_SIGNL,
		"\tld de,$R\n\tex de,hl\n" T_SUB_DE T_SXORV, F_M),

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
	/*
	 * LE and GT answer the reversed question, and there is no
	 * ex bc,hl - so copy BC into DE and swap that instead.
	 */
	R("W(H,B)", LE, P_L, P_R, P_NONE, 0,
		"\tld e,c\n\tld d,b\n\tex de,hl\n\tor a\n\tsbc hl,de\n", F_NC),
	R("G(H,B)", GT, P_L, P_R, P_NONE, 0,
		"\tld e,c\n\tld d,b\n\tex de,hl\n\tor a\n\tsbc hl,de\n", F_C),
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
	/* byte comparison against another byte, in E */
	R("Q(A,K):F", EQ, P_L, P_R, P_NONE, 0, "\tcp e\n", F_Z),
	R("U(A,K):F", NEQ, P_L, P_R, P_NONE, 0, "\tcp e\n", F_NZ),
	R("T(A,K):F", LT, P_L, P_R, P_NONE, 0, "\tcp e\n", F_C),
	R("Y(A,K):F", GE, P_L, P_R, P_NONE, 0, "\tcp e\n", F_NC),
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
