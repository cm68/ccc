/*
 * templates.c - Template table and emission functions
 *
 * Table-driven assembly emission to reduce code size.
 * Templates are referenced by 1-byte IDs.
 */

#include "cc2.h"
#include "templates.h"

/* Template strings indexed by T_* constants */
char *tmpl[] = {
	/* Load A */
	"ld a,0",		/* T_LDA0 */
	"ld a,1",		/* T_LDA1 */
	"ld a,%d",		/* T_LDAD */
	"ld a,e",		/* T_LDAE */
	"ld a,l",		/* T_LDAL */
	"ld a,h",		/* T_LDAH */
	"ld a,b",		/* T_LDAB */
	"ld a,c",		/* T_LDAC */
	"ld a,(hl)",		/* T_LDAHL */
	"ld a,%s",		/* T_LDAS */
	"ld a,(%s)",		/* T_LDAIS */
	"ld a,(%s%+d)",		/* T_LDASO */
	"ld a,(%s+%d)",		/* T_LDASO2 */

	/* Load HL */
	"ld hl,0",		/* T_LDHL0 */
	"ld hl,%d",		/* T_LDHLD */
	"ld hl,%s",		/* T_LDHLS */
	"ld hl,%s%+d",		/* T_LDHLSD */
	"ld hl,%s+%d",		/* T_LDHLSD2 */
	"ld hl,(%s)",		/* T_LDHLIS */
	"ld hl,(%s+%d)",	/* T_LDHLISO */
	"ld hl,(lR)",		/* T_LDHLLR */
	"ld hl,(lR+2)",		/* T_LDHLLR2 */

	/* Load 8-bit parts */
	"ld l,(%s%+d)",		/* T_LDLSO */
	"ld h,(%s%+d)",		/* T_LDHSO */
	"ld h,0",		/* T_LDH0 */
	"ld l,a",		/* T_LDLA */
	"ld h,a",		/* T_LDHA */
	"ld l,c",		/* T_LDLC */
	"ld h,b",		/* T_LDHB */
	"ld e,(hl)",		/* T_LDEHL */
	"ld d,(hl)",		/* T_LDDHL */
	"ld h,(hl)",		/* T_LDHHL */

	/* Load other regs */
	"ld de,%d",		/* T_LDDED */
	"ld de,-%d",		/* T_LDDEMD */
	"ld bc,%d",		/* T_LDBCD */
	"ld bc,%s",		/* T_LDBCS */
	"ld b,%d",		/* T_LDBD */
	"ld c,%d",		/* T_LDCD */
	"ld e,a",		/* T_LDEA */
	"ld e,b",		/* T_LDEB */
	"ld b,a",		/* T_LDBA */
	"ld b,e",		/* T_LDBE */
	"ld c,e",		/* T_LDCE */

	/* Store */
	"ld (hl),a",		/* T_STHLA */
	"ld (hl),%d",		/* T_STHLD */
	"ld (hl),e",		/* T_STHLE */
	"ld (hl),d",		/* T_STHLDH */
	"ld (de),a",		/* T_STDEA */
	"ld (%s),a",		/* T_STSA */
	"ld (%s),hl",		/* T_STSHL */
	"ld (%s%+d),a",		/* T_STSOA */
	"ld (%s%+d),%d",	/* T_STSOD */
	"ld (%s%+d),l",		/* T_STSOL */
	"ld (%s%+d),h",		/* T_STSOH */
	"ld %s,a",		/* T_STLLA */
	"ld %s,l",		/* T_STLLL */
	"ld (lL),hl",		/* T_STLLHL */
	"ld (lL+2),hl",		/* T_STLL2HL */
	"ld (lR),hl",		/* T_STLRHL */
	"ld (lR+2),hl",		/* T_STLR2HL */

	/* Arithmetic */
	"add hl,de",		/* T_ADDHLDE */
	"add hl,bc",		/* T_ADDHLBC */
	"add hl,hl",		/* T_ADDHLHL */
	"add a,a",		/* T_ADDAA */
	"add a,e",		/* T_ADDAE */
	"add a,%d",		/* T_ADDAD */
	"or a\nsbc hl,de",	/* T_SUBHLDE */
	"or a\nsbc hl,bc",	/* T_SUBHLBC */
	"sub b",		/* T_SUBB */
	"sub h",		/* T_SUBH */
	"sub l",		/* T_SUBL */
	"sub %d",		/* T_SUBD */
	"inc hl",		/* T_INCHL */
	"inc de",		/* T_INCDE */
	"inc a",		/* T_INCA */
	"inc l",		/* T_INCL */
	"dec hl",		/* T_DECHL */
	"neg",			/* T_NEG */

	/* Bitwise */
	"and %d",		/* T_ANDD */
	"or %d",		/* T_ORD */
	"or a",			/* T_ORA */
	"or c",			/* T_ORC */
	"or l",			/* T_ORL */
	"xor a",		/* T_XORA */
	"xor %d",		/* T_XORD */
	"cpl",			/* T_CPL */
	"sbc a,a",		/* T_SBCAA */

	/* Shifts */
	"sra h",		/* T_SRAH */
	"sra a",		/* T_SRAA */
	"srl h",		/* T_SRLH */
	"srl a",		/* T_SRLA */
	"rr l",			/* T_RRL */
	"rlca",			/* T_RLCA */

	/* Compare */
	"cp %d",		/* T_CPD */
	"cp b",			/* T_CPB */
	"cp (hl)",		/* T_CPHL */
	"cp %s",		/* T_CPS */
	"cp (%s%+d)",		/* T_CPSO */

	/* Bit test */
	"bit 7,b",		/* T_BIT7B */
	"bit 7,(hl)",		/* T_BIT7HL */
	"bit %d,(ix%+d)",	/* T_BITIXO */

	/* Stack */
	"push hl",		/* T_PUSHHL */
	"push de",		/* T_PUSHDE */
	"push af",		/* T_PUSHAF */
	"push ix",		/* T_PUSHIX */
	"push %s",		/* T_PUSHS */
	"pop hl",		/* T_POPHL */
	"pop de",		/* T_POPDE */
	"pop bc",		/* T_POPBC */
	"pop ix",		/* T_POPIX */

	/* Exchange */
	"ex de,hl",		/* T_EXDE */
	"exx",			/* T_EXX */

	/* Call runtime */
	"call %s",		/* T_CALLS */
	"call callhl",		/* T_CALLHL */
	"call imul",		/* T_IMUL */
	"call imula",		/* T_IMULA */
	"call imulb",		/* T_IMULB */
	"call idiv",		/* T_IDIV */
	"call idivb",		/* T_IDIVB */
	"call imod",		/* T_IMOD */
	"call imodb",		/* T_IMODB */
	"call ashr",		/* T_ASHR */
	"call ladd",		/* T_LADD */
	"call lsub",		/* T_LSUB */
	"call land",		/* T_LAND */
	"call lor",		/* T_LOR */
	"call lxor",		/* T_LXOR */
	"call lcom",		/* T_LCOM */
	"call lneg",		/* T_LNEG */
	"call lshl",		/* T_LSHL */
	"call lshr",		/* T_LSHR */
	"call lashr",		/* T_LASHR */
	"call lcmp",		/* T_LCMP */
	"call lldHL",		/* T_LLDHL */
	"call lldHLR",		/* T_LLDHLR */
	"call lstHL",		/* T_LSTHL */
	"call lstHLR",		/* T_LSTHLR */

	/* Block ops */
	"ldir",			/* T_LDIR */

	/* Parameterized byte ops */
	"%s a",			/* T_BOPA */
	"%s b",			/* T_BOPB */
	"%s c",			/* T_BOPC */
	"%s d",			/* T_BOPD */
	"%s e",			/* T_BOPE */
	"%s (hl)",		/* T_BOPHL */
	"%s %d",		/* T_BOPIMM */
	"%s %s",		/* T_BOPS */
	"%s (%s%+d)",		/* T_BOPSO */
	"%s hl",		/* T_WOPA */
	"%s bc",		/* T_WOPBC */
};
