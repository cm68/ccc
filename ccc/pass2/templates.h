/*
 * templates.h - Template IDs for table-driven code emission
 *
 * Templates are assembly instruction patterns referenced by 1-byte IDs.
 * Format specifiers: %d=int, %s=string, %o=signed offset
 */
#ifndef TEMPLATES_H
#define TEMPLATES_H

/* Load templates - result in A */
#define T_LDA0      0   /* ld a,0 */
#define T_LDA1      1   /* ld a,1 */
#define T_LDAD      2   /* ld a,%d */
#define T_LDAE      3   /* ld a,e */
#define T_LDAL      4   /* ld a,l */
#define T_LDAH      5   /* ld a,h */
#define T_LDAB      6   /* ld a,b */
#define T_LDAC      7   /* ld a,c */
#define T_LDAHL     8   /* ld a,(hl) */
#define T_LDAS      9   /* ld a,%s */
#define T_LDAIS     10  /* ld a,(%s) */
#define T_LDASO     11  /* ld a,(%s%o) */
#define T_LDASO2    12  /* ld a,(%s+%d) */

/* Load templates - result in HL */
#define T_LDHL0     13  /* ld hl,0 */
#define T_LDHLD     14  /* ld hl,%d */
#define T_LDHLS     15  /* ld hl,%s */
#define T_LDHLSD    16  /* ld hl,%s%d */
#define T_LDHLSD2   17  /* ld hl,%s+%d */
#define T_LDHLIS    18  /* ld hl,(%s) */
#define T_LDHLISO   19  /* ld hl,(%s+%d) */
#define T_LDHLLR    20  /* ld hl,(lR) */
#define T_LDHLLR2   21  /* ld hl,(lR+2) */

/* Load templates - 8-bit parts */
#define T_LDLSO     22  /* ld l,(%s%o) */
#define T_LDHSO     23  /* ld h,(%s%o) */
#define T_LDH0      24  /* ld h,0 */
#define T_LDLA      25  /* ld l,a */
#define T_LDHA      26  /* ld h,a */
#define T_LDLC      27  /* ld l,c */
#define T_LDHB      28  /* ld h,b */
#define T_LDEHL     29  /* ld e,(hl) */
#define T_LDDHL     30  /* ld d,(hl) */
#define T_LDHHL     31  /* ld h,(hl) */

/* Load templates - other regs */
#define T_LDDED     32  /* ld de,%d */
#define T_LDDEMD    33  /* ld de,-%d */
#define T_LDBCD     34  /* ld bc,%d */
#define T_LDBCS     35  /* ld bc,%s */
#define T_LDBD      36  /* ld b,%d */
#define T_LDCD      37  /* ld c,%d */
#define T_LDEA      38  /* ld e,a */
#define T_LDEB      39  /* ld e,b */
#define T_LDBA      40  /* ld b,a */
#define T_LDBE      41  /* ld b,e */
#define T_LDCE      42  /* ld c,e */

/* Store templates */
#define T_STHLA     43  /* ld (hl),a */
#define T_STHLD     44  /* ld (hl),%d */
#define T_STHLE     45  /* ld (hl),e */
#define T_STHLDH    46  /* ld (hl),d */
#define T_STDEA     47  /* ld (de),a */
#define T_STSA      48  /* ld (%s),a */
#define T_STSHL     49  /* ld (%s),hl */
#define T_STSOA     50  /* ld (%s%o),a */
#define T_STSOD     51  /* ld (%s%o),%d */
#define T_STSOL     52  /* ld (%s%o),l */
#define T_STSOH     53  /* ld (%s%o),h */
#define T_STLLA     54  /* ld %s,a */
#define T_STLLL     55  /* ld %s,l */
#define T_STLLHL    56  /* ld (lL),hl */
#define T_STLL2HL   57  /* ld (lL+2),hl */
#define T_STLRHL    58  /* ld (lR),hl */
#define T_STLR2HL   59  /* ld (lR+2),hl */

/* Arithmetic */
#define T_ADDHLDE   60  /* add hl,de */
#define T_ADDHLBC   61  /* add hl,bc */
#define T_ADDHLHL   62  /* add hl,hl */
#define T_ADDAA     63  /* add a,a */
#define T_ADDAE     64  /* add a,e */
#define T_ADDAD     65  /* add a,%d */
#define T_SUBHLDE   66  /* sbc hl,de (with or a first) */
#define T_SUBHLBC   67  /* sbc hl,bc (with or a first) */
#define T_SUBB      68  /* sub b */
#define T_SUBH      69  /* sub h */
#define T_SUBL      70  /* sub l */
#define T_SUBD      71  /* sub %d */
#define T_INCHL     72  /* inc hl */
#define T_INCDE     73  /* inc de */
#define T_INCA      74  /* inc a */
#define T_INCL      75  /* inc l */
#define T_DECHL     76  /* dec hl */
#define T_NEG       77  /* neg */

/* Bitwise */
#define T_ANDD      78  /* and %d */
#define T_ORD       79  /* or %d */
#define T_ORA       80  /* or a */
#define T_ORC       81  /* or c */
#define T_ORL       82  /* or l */
#define T_XORA      83  /* xor a */
#define T_XORD      84  /* xor %d */
#define T_CPL       85  /* cpl */
#define T_SBCAA     86  /* sbc a,a */

/* Shifts */
#define T_SRAH      87  /* sra h */
#define T_SRAA      88  /* sra a */
#define T_SRLH      89  /* srl h */
#define T_SRLA      90  /* srl a */
#define T_RRL       91  /* rr l */
#define T_RLCA      92  /* rlca */

/* Compare */
#define T_CPD       93  /* cp %d */
#define T_CPB       94  /* cp b */
#define T_CPHL      95  /* cp (hl) */
#define T_CPS       96  /* cp %s */
#define T_CPSO      97  /* cp (%s%o) */

/* Bit test */
#define T_BIT7B     98  /* bit 7,b */
#define T_BIT7HL    99  /* bit 7,(hl) */
#define T_BITIXO    100 /* bit %d,(ix%o) */

/* Stack */
#define T_PUSHHL    101 /* push hl */
#define T_PUSHDE    102 /* push de */
#define T_PUSHAF    103 /* push af */
#define T_PUSHIX    104 /* push ix */
#define T_PUSHS     105 /* push %s */
#define T_POPHL     106 /* pop hl */
#define T_POPDE     107 /* pop de */
#define T_POPBC     108 /* pop bc */
#define T_POPIX     109 /* pop ix */

/* Register exchange */
#define T_EXDE      110 /* ex de,hl */
#define T_EXX       111 /* exx */

/* Call runtime */
#define T_CALLS     112 /* call %s */
#define T_CALLHL    113 /* call callhl */
#define T_IMUL      114 /* call imul */
#define T_IMULA     115 /* call imula */
#define T_IMULB     116 /* call imulb */
#define T_IDIV      117 /* call idiv */
#define T_IDIVB     118 /* call idivb */
#define T_IMOD      119 /* call imod */
#define T_IMODB     120 /* call imodb */
#define T_ASHR      121 /* call ashr */
#define T_LADD      122 /* call ladd */
#define T_LSUB      123 /* call lsub */
#define T_LAND      124 /* call land */
#define T_LOR       125 /* call lor */
#define T_LXOR      126 /* call lxor */
#define T_LCOM      127 /* call lcom */
#define T_LNEG      128 /* call lneg */
#define T_LSHL      129 /* call lshl */
#define T_LSHR      130 /* call lshr */
#define T_LASHR     131 /* call lashr */
#define T_LCMP      132 /* call lcmp */
#define T_LLDHL     133 /* call lldHL */
#define T_LLDHLR    134 /* call lldHLR */
#define T_LSTHL     135 /* call lstHL */
#define T_LSTHLR    136 /* call lstHLR */

/* Block operations */
#define T_LDIR      137 /* ldir */

/* Parameterized byte ops: %s is op name (add, sub, and, or, xor) */
#define T_BOPA      138 /* %s a */
#define T_BOPB      139 /* %s b */
#define T_BOPC      140 /* %s c */
#define T_BOPD      141 /* %s d */
#define T_BOPE      142 /* %s e */
#define T_BOPHL     143 /* %s (hl) */
#define T_BOPIMM    144 /* %s %d */
#define T_BOPS      145 /* %s %s */
#define T_BOPSO     146 /* %s (%s%o) */
#define T_WOPA      147 /* %s hl (inc, dec) */
#define T_WOPBC     148 /* %s bc */

/* Max template ID */
#define T_MAX       149

/* Template array and emit macro */
extern char *tmpl[];
#define emitT(t) emit(tmpl[t])

#endif /* TEMPLATES_H */
