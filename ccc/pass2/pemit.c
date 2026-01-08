/*
 * pemit.c - Pattern-driven code emission
 *
 * Uses pattern structure to drive code generation.
 * Each emitter receives the pattern and emits code using pattern operands.
 */
#include <stdio.h>
#include "cc2.h"
#include "templates.h"
#include "../cpp/lexeme.h"

/*
 * Pattern-driven emitters.
 * Each takes a pattern and expression, emits code.
 */

/* Emit constant: #<size><idx> */
void pemitConst(struct pattern *p, struct expr *e)
{
	long val = patGetConst(p, p->str + 2);
	if (e->size == 1)
		emit("ld a,%d", (int)(val & 0xff));
	else if (e->size == 2)
		emit("ld hl,%d", (int)(val & 0xffff));
	else if (e->size == 4)
		emitLImmR(val);
}

/* Emit global symbol: $<size><idx> */
void pemitSym(struct pattern *p, struct expr *e)
{
	char *sym = patGetSym(p, p->str + 2);
	char *rn;
	char off;

	if (e->aux == R_IY || e->aux == R_IX) {
		/* Indexed address: compute IX/IY + offset */
		off = e->offset;
		rn = (e->aux == R_IX) ? "ix" : "iy";
		comment("$%s %s%+d", sym, rn, off);
		emit("push %s", rn);
		emitT(T_POPHL);
		if (off != 0) {
			if (off >= -4 && off <= 4) {
				while (off > 0) { emit("inc hl"); off--; }
				while (off < 0) { emit("dec hl"); off++; }
			} else {
				emit("ld de,%d", off);
				emitT(T_ADDHLDE);
			}
		}
	} else {
		comment("$%s", sym);
		emit("ld hl,%s", sym);
	}
}

/* Emit local variable: V<size><idx> */
void pemitLocal(struct pattern *p, struct expr *e)
{
	int ofs = patGetLocal(p, p->str + 2);
	char *rn = (e->aux == R_IX) ? "ix" : "iy";
	comment("V%c %s%+d", e->type, rn, ofs);
	if (e->size == 1)
		emit("ld a,(%s%o)", rn, ofs);
	else if (e->size == 2) {
		emit("ld l,(%s%o)", rn, ofs);
		emit("ld h,(%s%o)", rn, ofs + 1);
	} else if (e->size == 4) {
		emit("push %s", rn);
		emitT(T_POPHL);
		if (ofs != 0) {
			emit("ld de,%d", ofs);
			emitT(T_ADDHLDE);
		}
		emit("call lldHLR");
	}
}

/* Emit register variable: R<size><idx> */
void pemitReg(struct pattern *p, struct expr *e)
{
	int reg = patGetReg(p, p->str + 2);
	comment("R%c reg=%d", e->type, reg);
	if (reg == R_B)
		emit("ld a,b");
	else if (reg == R_C)
		emit("ld a,c");
	else if (reg == R_BC) {
		emitT(T_LDHB);
		emitT(T_LDLC);
	} else if (reg == R_IX) {
		emit("push ix");
		emitT(T_POPHL);
	}
}

/* Binary operator helpers */
static void peBinPre(struct expr *e)
{
	emitExpr(e->left);
	if (e->size == 2)
		emitT(T_EXDE);
	else if (e->size == 1)
		emitT(T_LDEA);
	emitExpr(e->right);
}

/* Emit PLUS */
void pemitPlus(struct pattern *p, struct expr *e)
{
	comment("+%c [", e->type);
	indent += 2;
	if (e->special == SP_SYMOFS) {
		/* ld hl,sym+ofs */
		comment("SYM %s + %d", e->sym, e->offset);
		if (e->offset > 0)
			emit("ld hl,%s+%d", e->sym, e->offset);
		else if (e->offset < 0)
			emit("ld hl,%s%d", e->sym, e->offset);
		else
			emit("ld hl,%s", e->sym);
	} else if (e->special == SP_ADDBC) {
		/* ld hl,const; add hl,bc */
		comment("REGVAR bc + %d", e->offset);
		emit("ld hl,%d", e->offset);
		emit("add hl,bc");
	} else {
		peBinPre(e);
		if (e->size == 2)
			emitT(T_ADDHLDE);
		else if (e->size == 1)
			emitT(T_ADDAE);
		else if (e->size == 4)
			emit("call ladd");
	}
	indent -= 2;
	comment("]");
}

/* Emit MINUS */
void pemitMinus(struct pattern *p, struct expr *e)
{
	comment("-%c [", e->type);
	indent += 2;
	emitExpr(e->left);
	if (e->size == 2) {
		emitT(T_EXDE);
		emitExpr(e->right);
		emitT(T_EXDE);
		emitT(T_ORA);
		emit("sbc hl,de");
	} else if (e->size == 1) {
		if (e->right->op == AST_CONST) {
			emit("sub %d", e->right->v.c & 0xff);
		} else {
			emitT(T_LDEA);
			emitExpr(e->right);
			emitT(T_LDBA);
			emitT(T_LDAE);
			emitT(T_SUBB);
		}
	} else if (e->size == 4) {
		emitExpr(e->right);
		emit("call lsub");
	}
	indent -= 2;
	comment("]");
}

/* Emit STAR (multiply) */
void pemitStar(struct pattern *p, struct expr *e)
{
	comment("*%c [", e->type);
	indent += 2;
	if (e->special == SP_MUL2) {
		/* Power of 2: use shifts */
		emitExpr(e->left);
		while (e->incr-- > 0) {
			if (e->size == 2)
				emit("add hl,hl");
			else if (e->size == 1)
				emit("add a,a");
		}
	} else {
		peBinPre(e);
		if (e->size == 1)
			emit("call imulb");
		else if (e->size == 2)
			emit("call imul");
	}
	indent -= 2;
	comment("]");
}

/* Emit DIV */
void pemitDiv(struct pattern *p, struct expr *e)
{
	comment("/%c [", e->type);
	indent += 2;
	peBinPre(e);
	if (e->size == 1)
		emit("call idivb");
	else if (e->size == 2)
		emit("call idiv");
	indent -= 2;
	comment("]");
}

/* Emit MOD */
void pemitMod(struct pattern *p, struct expr *e)
{
	comment("%%%c [", e->type);
	indent += 2;
	peBinPre(e);
	if (e->size == 1)
		emit("call imodb");
	else if (e->size == 2)
		emit("call imod");
	indent -= 2;
	comment("]");
}

/* Emit bitwise AND */
void pemitAnd(struct pattern *p, struct expr *e)
{
	comment("&%c [", e->type);
	indent += 2;
	if (e->special == SP_BITTEST) {
		emit("bit %d,(ix%o)", e->incr, e->offset);
	} else {
		peBinPre(e);
		if (e->size == 1)
			emitBOp(AND);
		else if (e->size == 2)
			emitWBit(AND);
		else if (e->size == 4)
			emit("call land");
	}
	indent -= 2;
	comment("]");
}

/* Emit bitwise OR */
void pemitOr(struct pattern *p, struct expr *e)
{
	comment("|%c [", e->type);
	indent += 2;
	peBinPre(e);
	if (e->size == 1)
		emitBOp(OR);
	else if (e->size == 2)
		emitWBit(OR);
	else if (e->size == 4)
		emit("call lor");
	indent -= 2;
	comment("]");
}

/* Emit bitwise XOR */
void pemitXor(struct pattern *p, struct expr *e)
{
	comment("^%c [", e->type);
	indent += 2;
	peBinPre(e);
	if (e->size == 1)
		emitBOp(XOR);
	else if (e->size == 2)
		emitWBit(XOR);
	else if (e->size == 4)
		emit("call lxor");
	indent -= 2;
	comment("]");
}

/* Emit left shift */
void pemitLshift(struct pattern *p, struct expr *e)
{
	comment("<<%c [", e->type);
	indent += 2;
	emitExpr(e->left);
	if (e->right->op == AST_CONST) {
		unsigned char cnt = e->right->v.c & 0xf;
		if (e->size == 2) {
			while (cnt--)
				emit("add hl,hl");
		} else if (e->size == 1) {
			while (cnt--)
				emit("add a,a");
		} else if (e->size == 4) {
			emit("ld a,%d", cnt);
			emit("call lshl");
		}
	} else {
		emitExpr(e->right);
		if (e->size == 2)
			emit("call shl16");
		else if (e->size == 1)
			emit("call shl8");
		else if (e->size == 4)
			emit("call lshl");
	}
	indent -= 2;
	comment("]");
}

/* Emit right shift */
void pemitRshift(struct pattern *p, struct expr *e)
{
	comment(">>%c [", e->type);
	indent += 2;
	emitExpr(e->left);
	if (e->right->op == AST_CONST) {
		unsigned char cnt = e->right->v.c & 0xf;
		if (e->size == 2) {
			while (cnt--) {
				emit("srl h");
				emit("rr l");
			}
		} else if (e->size == 1) {
			while (cnt--)
				emit("srl a");
		} else if (e->size == 4) {
			emit("ld a,%d", cnt);
			emit("call lashr");
		}
	} else {
		emitExpr(e->right);
		if (e->size == 2)
			emit("call ashr");
		else if (e->size == 1)
			emit("call ashr8");
		else if (e->size == 4)
			emit("call lashr");
	}
	indent -= 2;
	comment("]");
}

/* Emit DEREF (memory load) */
void pemitDeref(struct pattern *p, struct expr *e)
{
	comment("M%c [", e->type);
	indent += 2;
	if (e->special == SP_MSYM) {
		/* DEREF[SYM] -> ld hl,(sym) or ld a,(sym) */
		comment("SYM %s", e->sym);
		if (e->size == 1)
			emit("ld a,(%s)", e->sym);
		else
			emit("ld hl,(%s)", e->sym);
	} else if (e->special == SP_SYMOFD) {
		/* DEREF[PLUS SYM AST_CONST] -> ld hl,(sym+ofs) */
		comment("PLUS SYM %s + %d", e->sym, e->offset);
		if (e->size == 1)
			emit("ld a,(%s+%d)", e->sym, e->offset);
		else
			emit("ld hl,(%s+%d)", e->sym, e->offset);
	} else if (e->left->op == REGVAR && e->left->aux == R_IX) {
		comment("REGVAR ix");
		emit("push ix");
		emitT(T_POPHL);
	} else if (e->left->op == REGVAR && e->left->aux == R_BC) {
		comment("REGVAR bc");
		emitT(T_LDHB);
		emitT(T_LDLC);
	} else {
		/* indirect: emit address, then deref */
		emitExpr(e->left);
		if (e->size == 1) {
			emitT(T_LDAHL);
		} else if (e->size == 2) {
			emitT(T_LDAHL);
			emitT(T_INCHL);
			emit("ld h,(hl)");
			emitT(T_LDLA);
		} else if (e->size == 4) {
			emitLLoad(R_HL);
		}
	}
	indent -= 2;
	comment("]");
}

/* Helper: store value to IY/IX-indexed local */
static void peStoreLocal(struct expr *e, char *rn, char ofs)
{
	if (e->special == SP_STCONST && e->right->op == AST_CONST) {
		long val = e->right->v.l;
		if (e->size == 1) {
			emit("ld (%s%o),%d", rn, ofs, (int)(val & 0xff));
		} else if (e->size == 2) {
			if (val == 0) {
				emitT(T_XORA);
				emit("ld (%s%o),a", rn, ofs);
				emit("ld (%s%o),a", rn, ofs + 1);
			} else {
				emit("ld (%s%o),%d", rn, ofs, (int)(val & 0xff));
				emit("ld (%s%o),%d", rn, ofs + 1, (int)((val >> 8) & 0xff));
			}
		} else if (e->size == 4) {
			emit("ld (%s%o),%d", rn, ofs, (int)(val & 0xff));
			emit("ld (%s%o),%d", rn, ofs + 1, (int)((val >> 8) & 0xff));
			emit("ld (%s%o),%d", rn, ofs + 2, (int)((val >> 16) & 0xff));
			emit("ld (%s%o),%d", rn, ofs + 3, (int)((val >> 24) & 0xff));
		}
	} else {
		emitExpr(e->right);
		if (e->size == 1) {
			emit("ld (%s%o),a", rn, ofs);
		} else if (e->size == 2) {
			emit("ld (%s%o),l", rn, ofs);
			emit("ld (%s%o),h", rn, ofs + 1);
		} else if (e->size == 4) {
			emit("push %s", rn);
			emitT(T_POPHL);
			if (ofs != 0) {
				emit("ld de,%d", (int)(char)ofs);
				emitT(T_ADDHLDE);
			}
			emitLStoreR();
		}
	}
}

/* Helper: store constant through HL */
static void peStoreConst(struct expr *e)
{
	long val = e->right->v.l;
	comment("STCONST %ld", val);
	if (e->left->op == DEREF) {
		emitExpr(e->left->left);
		if (e->left->size == 2 && e->left->left->op == SYM) {
			emitT(T_LDAHL);
			emitT(T_INCHL);
			emit("ld h,(hl)");
			emitT(T_LDLA);
		}
	} else if (e->left->op == SYM) {
		emit("ld hl,%s", e->left->sym);
	} else {
		emitExpr(e->left);
	}
	if (e->size == 1) {
		emit("ld (hl),%d", (int)(val & 0xff));
	} else if (e->size == 2) {
		emit("ld (hl),%d", (int)(val & 0xff));
		emitT(T_INCHL);
		emit("ld (hl),%d", (int)((val >> 8) & 0xff));
	} else if (e->size == 4) {
		emit("ld (hl),%d", (int)(val & 0xff));
		emitT(T_INCHL);
		emit("ld (hl),%d", (int)((val >> 8) & 0xff));
		emitT(T_INCHL);
		emit("ld (hl),%d", (int)((val >> 16) & 0xff));
		emitT(T_INCHL);
		emit("ld (hl),%d", (int)((val >> 24) & 0xff));
	}
}

/* Helper: store through computed address */
static void peStoreAddr(struct expr *e)
{
	emitExpr(e->left);
	emitT(T_PUSHHL);
	emitExpr(e->right);
	emitT(T_POPDE);
	if (e->size == 1) {
		emit("ld (de),a");
	} else if (e->size == 2) {
		emitT(T_EXDE);
		emitT(T_STHLE);
		emitT(T_INCHL);
		emitT(T_STHLDH);
	} else if (e->size == 4) {
		emitT(T_EXDE);
		emitLStoreR();
	}
}

/* Emit ASSIGN */
void pemitAssign(struct pattern *p, struct expr *e)
{
	char *rn;
	char ofs;

	comment("=%c%s [", e->type, e->unused ? " U" : "");
	indent += 2;

	if (e->left->op == REGVAR) {
		int reg = e->left->aux;
		if (reg == R_IX) {
			comment("Rp ix");
			emitExpr(e->right);
			emitT(T_PUSHHL);
			emit("pop ix");
		} else if (reg == R_BC) {
			comment("RS bc");
			if (e->right->op == AST_CONST)
				emit("ld bc,%d", e->right->v.s & 0xffff);
			else if (e->right->op == SYM)
				emit("ld bc,%s", e->right->sym);
			else {
				emitExpr(e->right);
				emit("ld b,h");
				emit("ld c,l");
			}
		} else if (reg == R_B) {
			comment("Rb b");
			if (e->special == SP_STCONST && e->right->op == AST_CONST)
				emit("ld b,%d", e->right->v.c & 0xff);
			else {
				emitExpr(e->right);
				emit("ld b,a");
			}
		} else if (reg == R_C) {
			comment("Rb c");
			if (e->special == SP_STCONST && e->right->op == AST_CONST)
				emit("ld c,%d", e->right->v.c & 0xff);
			else {
				emitExpr(e->right);
				emit("ld c,a");
			}
		}
	} else if (e->left->op == SYM && e->left->sym) {
		comment("$%s", e->left->sym);
		emitExpr(e->right);
		if (e->size == 1)
			emit("ld (%s),a", e->left->sym);
		else if (e->size == 2)
			emit("ld (%s),hl", e->left->sym);
		else if (e->size == 4) {
			emit("ld hl,%s", e->left->sym);
			emitLStoreR();
		}
	} else if (e->left->op == LOCALVAR) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		comment("V%c %s%+d", e->left->type, rn, ofs);
		peStoreLocal(e, rn, ofs);
	} else if (e->special == SP_STCONST) {
		peStoreConst(e);
	} else if (e->left->op == POSTINC || e->left->op == POSTDEC) {
		emitExpr(e->left);
		emitT(T_EXDE);
		emitExpr(e->right);
		if (e->size == 1) {
			emit("ld (de),a");
		} else if (e->size == 2) {
			emitT(T_EXDE);
			emitT(T_STHLE);
			emitT(T_INCHL);
			emitT(T_STHLDH);
		} else if (e->size == 4) {
			emitT(T_EXDE);
			emitLStoreR();
		}
	} else if (e->left->op == PLUS || e->left->op == DEREF) {
		peStoreAddr(e);
	} else {
		emitExpr(e->left);
		emitExpr(e->right);
		emit("; = unhandled");
	}
	indent -= 2;
	comment("]");
}

/* Emit compound arith: +=, -=, |=, &=, ^=, %= */
void pemitCmpArith(struct pattern *p, struct expr *e)
{
	char ofs;
	char *rn;
	unsigned char r;

	comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
	indent += 2;

	if (e->left->op == REGVAR && e->size == 1) {
		rn = (e->left->aux == R_B) ? "b" : "c";
		comment("Rb %s", rn);
		emitExpr(e->right);
		if (e->right->op == AST_CONST) {
			unsigned char val = e->right->v.c;
			emit("ld a,%s", rn);
			switch (e->op) {
			case PLUSEQ: emit("add a,%d", val); break;
			case SUBEQ: emit("sub %d", val); break;
			case OREQ: emit("or %d", val); break;
			case ANDEQ: emit("and %d", val); break;
			case XOREQ: emit("xor %d", val); break;
			}
		} else {
			emitT(T_LDEA);
			emit("ld a,%s", rn);
			emitBOp(e->op);
		}
		emit("ld %s,a", rn);
	} else if (e->left->op == REGVAR && e->size == 2) {
		r = e->left->aux;
		comment("RS %s", regnames[r]);
		emitExpr(e->right);
		emitT(T_EXDE);
		if (r == R_BC) {
			emitT(T_LDHB);
			emitT(T_LDLC);
		} else if (r == R_IX) {
			emit("push ix");
			emit("pop hl");
		}
		switch (e->op) {
		case PLUSEQ: emit("add hl,de"); break;
		case SUBEQ: emit("or a"); emit("sbc hl,de"); break;
		case OREQ: case ANDEQ: case XOREQ: emitWBit(e->op); break;
		}
		if (r == R_BC) {
			emit("ld b,h");
			emit("ld c,l");
		} else if (r == R_IX) {
			emit("push hl");
			emit("pop ix");
		}
	} else if (e->left->op == LOCALVAR && e->size == 1) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		comment("Vb %s%+d", rn, ofs);
		emit("ld a,(%s%o)", rn, ofs);
		if (e->right->op == AST_CONST) {
			unsigned char val = e->right->v.c;
			switch (e->op) {
			case PLUSEQ: emit("add a,%d", val); break;
			case SUBEQ: emit("sub %d", val); break;
			case OREQ: emit("or %d", val); break;
			case ANDEQ: emit("and %d", val); break;
			case XOREQ: emit("xor %d", val); break;
			}
		} else {
			emitExpr(e->right);
			emit("ld a,(%s%o)", rn, ofs);
			emitBOp(e->op);
		}
		emit("ld (%s%o),a", rn, ofs);
	} else if (e->size == 1 && e->right->op == AST_CONST) {
		unsigned char val = e->right->v.c;
		emitExpr(e->left);
		emitT(T_LDAHL);
		switch (e->op) {
		case PLUSEQ: emit("add a,%d", val); break;
		case SUBEQ: emit("sub %d", val); break;
		case OREQ: emit("or %d", val); break;
		case ANDEQ: emit("and %d", val); break;
		case XOREQ: emit("xor %d", val); break;
		}
		emitT(T_STHLA);
	} else if (e->size == 1) {
		emitExpr(e->right);
		if (e->right->size == 2)
			emit("ld a,l");
		emitT(T_LDEA);
		emitExpr(e->left);
		emitT(T_LDAHL);
		emitBOp(e->op);
		emitT(T_STHLA);
	} else if (e->left->op == LOCALVAR && e->size == 2) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		comment("VS %s%+d", rn, ofs);
		emitExpr(e->right);
		emitT(T_EXDE);
		emit("ld l,(%s%o)", rn, ofs);
		emit("ld h,(%s%o)", rn, ofs + 1);
		switch (e->op) {
		case PLUSEQ: emit("add hl,de"); break;
		case SUBEQ: emit("or a"); emit("sbc hl,de"); break;
		case OREQ: case ANDEQ: case XOREQ: emitWBit(e->op); break;
		}
		emit("ld (%s%o),l", rn, ofs);
		emit("ld (%s%o),h", rn, ofs + 1);
	} else if (e->left->op == LOCALVAR && e->size == 4) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		comment("Vl %s%+d", rn, ofs);
		emitExpr(e->right);
		emit("push %s", rn);
		emit("pop hl");
		if (ofs != 0) {
			emit("ld de,%d", (int)(char)ofs);
			emitT(T_ADDHLDE);
		}
		emit("call lldHL");
		switch (e->op) {
		case PLUSEQ: emit("call ladd"); break;
		case SUBEQ: emit("call lsub"); break;
		case OREQ: emit("call lor"); break;
		case ANDEQ: emit("call land"); break;
		case XOREQ: emit("call lxor"); break;
		}
		emit("push %s", rn);
		emit("pop hl");
		if (ofs != 0) {
			emit("ld de,%d", (int)(char)ofs);
			emitT(T_ADDHLDE);
		}
		emit("call lstHLR");
	} else if (e->size == 2 && e->right->op == AST_CONST) {
		unsigned val = e->right->v.s & 0xffff;
		emitExpr(e->left);
		emit("ld e,(hl)");
		emit("inc hl");
		emit("ld d,(hl)");
		emit("dec hl");
		emitT(T_EXDE);
		switch (e->op) {
		case PLUSEQ: case SUBEQ: emitWSubBC(e->op, val); break;
		case OREQ: case ANDEQ: case XOREQ: emitWBitImm(e->op, val); break;
		}
		emitT(T_EXDE);
		emit("ld (hl),e");
		emit("inc hl");
		emit("ld (hl),d");
	} else if (e->size == 2) {
		emitExpr(e->right);
		emit("push hl");
		emitExpr(e->left);
		emit("ld e,(hl)");
		emit("inc hl");
		emit("ld d,(hl)");
		emit("dec hl");
		emit("push hl");
		emitT(T_EXDE);
		emit("pop de");
		emit("pop bc");
		switch (e->op) {
		case PLUSEQ: emit("add hl,bc"); break;
		case SUBEQ: emit("or a"); emit("sbc hl,bc"); break;
		case OREQ: case ANDEQ: case XOREQ: emitWBitBC(e->op); break;
		}
		emitT(T_EXDE);
		emit("ld (hl),e");
		emit("inc hl");
		emit("ld (hl),d");
	}
	indent -= 2;
	comment("]");
}

/* Emit compound shift: <<=, >>= */
void pemitCmpShift(struct pattern *p, struct expr *e)
{
	char ofs;
	char *rn;
	unsigned char cnt;

	comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
	indent += 2;

	if (e->left->op == REGVAR && e->size == 2 && e->right->op == AST_CONST) {
		cnt = e->right->v.c & 0xf;
		comment("RS bc");
		emitT(T_LDHB);
		emitT(T_LDLC);
		if (e->op == LSHIFTEQ) {
			while (cnt--)
				emit("add hl,hl");
		} else {
			while (cnt--) {
				emit("srl h");
				emit("rr l");
			}
		}
		emit("ld b,h");
		emit("ld c,l");
	} else if (e->left->op == LOCALVAR && e->size == 2 && e->right->op == AST_CONST) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		cnt = e->right->v.c & 0xf;
		comment("VS %s%+d", rn, ofs);
		emit("ld l,(%s%o)", rn, ofs);
		emit("ld h,(%s%o)", rn, ofs + 1);
		if (e->op == LSHIFTEQ) {
			while (cnt--)
				emit("add hl,hl");
		} else {
			while (cnt--) {
				emit("srl h");
				emit("rr l");
			}
		}
		emit("ld (%s%o),l", rn, ofs);
		emit("ld (%s%o),h", rn, ofs + 1);
	} else if (e->left->op == LOCALVAR && e->size == 4 && e->right->op == AST_CONST) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		cnt = e->right->v.c & 0x1f;
		comment("Vl %s%+d", rn, ofs);
		emit("push %s", rn);
		emit("pop hl");
		if (ofs != 0) {
			emit("ld de,%d", (int)(char)ofs);
			emitT(T_ADDHLDE);
		}
		emit("call lldHLR");
		emit("ld a,%d", cnt);
		if (e->op == LSHIFTEQ)
			emit("call lshl");
		else
			emit("call lashr");
		emit("push %s", rn);
		emit("pop hl");
		if (ofs != 0) {
			emit("ld de,%d", (int)(char)ofs);
			emitT(T_ADDHLDE);
		}
		emit("call lstHLR");
	}
	indent -= 2;
	comment("]");
}

/* Emit compound mul/div: *=, /= */
void pemitCmpMulDiv(struct pattern *p, struct expr *e)
{
	char ofs;
	char *rn;
	unsigned char r;

	comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
	indent += 2;

	if (e->left->op == REGVAR && e->left->size == 1) {
		rn = (e->left->aux == R_B) ? "b" : "c";
		comment("Rb %s", rn);
		emitExpr(e->right);
		emitT(T_LDEA);
		emit("ld a,%s", rn);
		if (e->op == MULTEQ)
			emit("call imulb");
		else
			emit("call idivb");
		emit("ld %s,l", rn);
	} else if (e->left->op == REGVAR && e->left->size == 2) {
		r = e->left->aux;
		comment("RS %s", regnames[r]);
		emitExpr(e->right);
		emitT(T_EXDE);
		if (r == R_BC) {
			emitT(T_LDHB);
			emitT(T_LDLC);
		} else if (r == R_IX) {
			emit("push ix");
			emit("pop hl");
		}
		if (e->op == MULTEQ)
			emit("call imul");
		else
			emit("call idiv");
		if (r == R_BC) {
			emit("ld b,h");
			emit("ld c,l");
		} else if (r == R_IX) {
			emit("push hl");
			emit("pop ix");
		}
	} else if (e->left->op == LOCALVAR && e->size == 1) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		comment("Vb %s%+d", rn, ofs);
		emitExpr(e->right);
		emitT(T_LDEA);
		emit("ld a,(%s%o)", rn, ofs);
		if (e->op == MULTEQ)
			emit("call imulb");
		else
			emit("call idivb");
		emit("ld a,l");
		emit("ld (%s%o),a", rn, ofs);
	} else if (e->left->op == LOCALVAR && e->size == 2) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		comment("VS %s%+d", rn, ofs);
		emitExpr(e->right);
		emitT(T_EXDE);
		emit("ld l,(%s%o)", rn, ofs);
		emit("ld h,(%s%o)", rn, ofs + 1);
		if (e->op == MULTEQ)
			emit("call imul");
		else
			emit("call idiv");
		emit("ld (%s%o),l", rn, ofs);
		emit("ld (%s%o),h", rn, ofs + 1);
	}
	indent -= 2;
	comment("]");
}

/* Emit pre-increment/decrement */
void pemitPreIncDec(struct pattern *p, struct expr *e)
{
	char *ins;
	char ofs;
	char *rn;
	unsigned char i;

	if (e->special == SP_INCR || e->special == SP_DECR) {
		ins = (e->special == SP_INCR) ? "inc" : "dec";
		if (e->dest == R_IYO) {
			rn = (e->left->aux == R_IX) ? "ix" : "iy";
			for (i = 0; i < e->incr; i++)
				emit("%s (%s%o)", ins, rn, e->offset);
		} else {
			for (i = 0; i < e->incr; i++)
				emit("%s %s", ins, regnames[e->dest]);
		}
	} else if (e->special == SP_INCGLOB) {
		ins = e->aux2 ? "inc" : "dec";
		emit("ld hl,(%s)", e->sym);
		for (i = 0; i < e->incr; i++)
			emit("%s hl", ins);
		emit("ld (%s),hl", e->sym);
	} else {
		comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
		indent += 2;
		emitExpr(e->left);
		comment("incr=%d", e->aux2);
		if (e->unused && e->size == 1 && e->aux2 <= 4) {
			ins = (e->op == PREINC) ? "inc" : "dec";
			for (i = 0; i < e->aux2; i++)
				emit("%s (hl)", ins);
		} else if (e->left->op == LOCALVAR && e->size == 2 && e->aux2 <= 4) {
			ofs = e->left->offset;
			rn = (e->left->aux == R_IX) ? "ix" : "iy";
			ins = (e->op == PREINC) ? "inc" : "dec";
			for (i = 0; i < e->aux2; i++)
				emit("%s hl", ins);
			emit("ld (%s%o),l", rn, ofs);
			emit("ld (%s%o),h", rn, ofs + 1);
		} else if (e->left->op == REGVAR && e->left->aux == R_BC && e->size == 2) {
			comment("Rs bc incr=%d", e->aux2);
			if (e->aux2 <= 4) {
				ins = (e->op == PREINC) ? "inc" : "dec";
				for (i = 0; i < e->aux2; i++)
					emit("%s bc", ins);
			} else {
				emit("ld hl,%d", e->aux2);
				if (e->op == PREINC)
					emit("add hl,bc");
				else {
					emit("ex de,hl");
					emit("ld h,b");
					emit("ld l,c");
					emit("or a");
					emit("sbc hl,de");
				}
				emit("ld b,h");
				emit("ld c,l");
			}
			emit("ld h,b");
			emit("ld l,c");
		} else if (e->size == 2 && e->aux2 <= 4) {
			ins = (e->op == PREINC) ? "inc" : "dec";
			emit("ld e,(hl)");
			emit("inc hl");
			emit("ld d,(hl)");
			emit("dec hl");
			emit("push hl");
			emit("ex de,hl");
			for (i = 0; i < e->aux2; i++)
				emit("%s hl", ins);
			emit("pop de");
			emit("ex de,hl");
			emit("ld (hl),e");
			emit("inc hl");
			emit("ld (hl),d");
			emit("ex de,hl");
		} else if (e->size == 2) {
			emit("ld e,(hl)");
			emit("inc hl");
			emit("ld d,(hl)");
			emit("dec hl");
			emit("push hl");
			emit("ex de,hl");
			if (e->op == PREINC)
				emit("ld de,%d", e->aux2);
			else
				emit("ld de,-%d", e->aux2);
			emit("add hl,de");
			emit("pop de");
			emit("ex de,hl");
			emit("ld (hl),e");
			emit("inc hl");
			emit("ld (hl),d");
			emit("ex de,hl");
		}
		indent -= 2;
		comment("]");
	}
}

/* Emit post-increment/decrement */
void pemitPostInc(struct pattern *p, struct expr *e)
{
	char *ins;
	char ofs;
	char *rn;
	unsigned char i, r;

	comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
	indent += 2;

	if (e->unused && e->left->op == REGVAR && e->aux2 <= 4) {
		ins = (e->op == POSTINC) ? "inc" : "dec";
		r = e->left->aux;
		comment("Rp %s incr=%d", regnames[r], e->aux2);
		for (i = 0; i < e->aux2; i++)
			emit("%s %s", ins, regnames[r]);
	} else if (!e->unused && e->left->op == REGVAR && e->aux2 <= 4) {
		ins = (e->op == POSTINC) ? "inc" : "dec";
		r = e->left->aux;
		comment("Rp %s incr=%d", regnames[r], e->aux2);
		if (r == R_BC) {
			emit("ld h,b");
			emit("ld l,c");
		} else if (r == R_IX) {
			emit("push ix");
			emit("pop hl");
		}
		for (i = 0; i < e->aux2; i++)
			emit("%s %s", ins, regnames[r]);
	} else if (e->left->op == LOCALVAR && e->size == 2 && e->aux2 <= 4) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		ins = (e->op == POSTINC) ? "inc" : "dec";
		emitExpr(e->left);
		comment("incr=%d", e->aux2);
		if (!e->unused)
			emit("push hl");
		for (i = 0; i < e->aux2; i++)
			emit("%s hl", ins);
		emit("ld (%s%o),l", rn, ofs);
		emit("ld (%s%o),h", rn, ofs + 1);
		if (!e->unused)
			emit("pop hl");
	} else if (e->left->op == LOCALVAR && e->size == 1 && e->aux2 <= 4) {
		ofs = e->left->offset;
		rn = (e->left->aux == R_IX) ? "ix" : "iy";
		ins = (e->op == POSTINC) ? "inc" : "dec";
		emit("ld a,(%s%o)", rn, ofs);
		comment("incr=%d", e->aux2);
		if (!e->unused)
			emit("ld e,a");
		for (i = 0; i < e->aux2; i++)
			emit("%s a", ins);
		emit("ld (%s%o),a", rn, ofs);
		if (!e->unused)
			emit("ld a,e");
	} else if (e->size == 2 && e->aux2 <= 4) {
		ins = (e->op == POSTINC) ? "inc" : "dec";
		emitExpr(e->left);
		comment("incr=%d", e->aux2);
		emit("ld e,(hl)");
		emit("inc hl");
		emit("ld d,(hl)");
		emit("dec hl");
		if (!e->unused)
			emit("push de");
		emit("ex de,hl");
		for (i = 0; i < e->aux2; i++)
			emit("%s hl", ins);
		emit("ex de,hl");
		emit("ld (hl),e");
		emit("inc hl");
		emit("ld (hl),d");
		if (!e->unused)
			emit("pop hl");
	} else if (e->size == 2) {
		emitExpr(e->left);
		comment("incr=%d", e->aux2);
		emit("ld e,(hl)");
		emit("inc hl");
		emit("ld d,(hl)");
		emit("dec hl");
		if (!e->unused)
			emit("push de");
		emit("ex de,hl");
		if (e->op == POSTINC)
			emit("ld de,%d", e->aux2);
		else
			emit("ld de,-%d", e->aux2);
		emit("add hl,de");
		emit("ex de,hl");
		emit("ld (hl),e");
		emit("inc hl");
		emit("ld (hl),d");
		if (!e->unused)
			emit("pop hl");
	} else if (e->size == 1 && e->aux2 <= 4) {
		ins = (e->op == POSTINC) ? "inc" : "dec";
		emitExpr(e->left);
		comment("incr=%d", e->aux2);
		emit("ld a,(hl)");
		if (!e->unused)
			emit("ld e,a");
		for (i = 0; i < e->aux2; i++)
			emit("%s a", ins);
		emit("ld (hl),a");
		if (!e->unused)
			emit("ld a,e");
	} else if (e->size == 1) {
		emitExpr(e->left);
		comment("incr=%d", e->aux2);
		emit("ld a,(hl)");
		if (!e->unused)
			emit("ld e,a");
		if (e->op == POSTINC)
			emit("add a,%d", e->aux2);
		else
			emit("sub %d", e->aux2);
		emit("ld (hl),a");
		if (!e->unused)
			emit("ld a,e");
	}
	indent -= 2;
	comment("]");
}

/* Helper: emit comparison result to A (0 or 1) */
static void peCmpResult(unsigned char op)
{
	switch (op) {
	case EQ:
		emitT(T_LDA0);
		emit("jr nz,$+3");
		emitT(T_INCA);
		break;
	case NEQ:
		emitT(T_LDA0);
		emit("jr z,$+3");
		emitT(T_INCA);
		break;
	case LT:
		emitT(T_LDA0);
		emit("jr nc,$+3");
		emitT(T_INCA);
		break;
	}
}

/* Emit comparison: LT, EQ, NEQ */
void pemitCompare(struct pattern *p, struct expr *e)
{
	unsigned char ctype;
	int subCmpZero;
	int lbl;
	char *rn;
	struct expr *simple, *complex;

	comment("%c%c%s [", e->op, e->type, e->cond ? " C" : "");
	indent += 2;

	if (e->special == SP_CMPHL) {
		complex = e->left;
		simple = e->right;
		comment("CMPHL");
		emitExpr(complex->left);
		emit("ld a,(hl)");
		if (simple->op == AST_CONST) {
			emit("cp %d", simple->v.c & 0xff);
		} else if (simple->op == REGVAR) {
			emit("cp %s", regnames[simple->aux]);
		} else if (simple->op == LOCALVAR) {
			rn = (simple->aux == R_IX) ? "ix" : "iy";
			emit("cp (%s%o)", rn, simple->offset);
		} else if (simple->op == DEREF && simple->left->op == SYM) {
			emit("ld hl,%s", simple->left->sym);
			emitT(T_CPHL);
		} else if (simple->op == DEREF && simple->left->op == LOCALVAR) {
			rn = (simple->left->aux == R_IX) ? "ix" : "iy";
			emit("cp (%s%o)", rn, simple->left->offset);
		}
		if (e->cond)
			emitCondJmp(e->op, e->aux2);
		else
			peCmpResult(e->op);
	} else if (e->special == SP_CMPV) {
		rn = (e->aux == R_IX) ? "ix" : "iy";
		comment("CMPV %s%+d #%d", rn, e->offset, e->incr);
		emit("ld a,%d", e->incr);
		emit("cp (%s%o)", rn, e->offset);
		if (e->cond)
			emitCondJmp(e->op, e->aux2);
		else
			peCmpResult(e->op);
	} else if (e->special == SP_CMPR) {
		rn = (e->aux == R_B) ? "b" : "c";
		comment("CMPR %s #%d", rn, e->incr);
		emit("ld a,%d", e->incr);
		emit("cp %s", rn);
		if (e->cond)
			emitCondJmp(e->op, e->aux2);
		else
			peCmpResult(e->op);
	} else if (e->special == SP_SIGN) {
		comment("SIGN $%s ofs=%d", e->sym ? e->sym : "?", e->offset);
		if (e->offset > 0)
			emit("ld hl,%s+%d", e->sym, e->offset);
		else
			emit("ld hl,%s", e->sym);
		emitT(T_BIT7HL);
		if (!e->cond) {
			lbl = labelCnt++;
			emitT(T_LDA0);
			emit("jp nz,sg%d_%d", lbl, fnIndex);
			emitT(T_INCA);
			emit("sg%d_%d:", lbl, fnIndex);
		}
	} else if (e->special == SP_SIGNREG) {
		comment("SIGNREG bc");
		emitT(T_BIT7B);
		if (!e->cond) {
			lbl = labelCnt++;
			emitT(T_LDA0);
			emit("jp nz,sg%d_%d", lbl, fnIndex);
			emitT(T_INCA);
			emit("sg%d_%d:", lbl, fnIndex);
		}
	} else if (e->special == SP_CMPEQ) {
		comment("CMPEQ %d", e->incr);
		emitExpr(e->left);
		if (e->incr == 1)
			emit("dec hl");
		else if (e->incr == -1 || e->incr == 0xffff)
			emit("inc hl");
		if (!e->cond) {
			lbl = labelCnt++;
			emit("ld a,h");
			emit("or l");
			if (e->op == EQ) {
				emitT(T_LDA1);
				emit("jr z,eq%d_%d", lbl, fnIndex);
				emit("xor a");
			} else {
				emitT(T_LDA0);
				emit("jr z,eq%d_%d", lbl, fnIndex);
				emitT(T_INCA);
			}
			emit("eq%d_%d:", lbl, fnIndex);
		}
	} else {
		ctype = e->left->type;
		subCmpZero = (e->left->op == MINUS && e->right->op == AST_CONST &&
			      e->right->v.s == 0);
		if (ISLONG(ctype)) {
			emitExpr(e->left);
			emitExpr(e->right);
			emit("call lcmp");
		} else if (subCmpZero) {
			emitExpr(e->left);
			if (ISBYTE(ctype) && e->left->size == 2)
				emitT(T_LDAL);
			if (e->cond) {
				emitCondJmp(e->op, e->aux2);
				indent -= 2;
				comment("]");
				return;
			}
			peCmpResult(e->op);
		} else {
			emitExpr(e->left);
			if (ISBYTE(ctype)) {
				if (e->left->size == 2)
					emitT(T_LDAL);
				if (e->right->op == AST_CONST) {
					emit("cp %d", e->right->v.c & 0xff);
				} else {
					emit("ld e,a");
					emitExpr(e->right);
					emit("ld b,a");
					emit("ld a,e");
					emitT(T_CPB);
				}
			} else if (ISWORD(ctype)) {
				emitT(T_EXDE);
				emitExpr(e->right);
				emitT(T_EXDE);
				emit("or a");
				emit("sbc hl,de");
			}
		}
		if (e->cond) {
			emitCondJmp(e->op, e->aux2);
			indent -= 2;
			comment("]");
			return;
		}
		if (e->special != SP_SIGN && e->special != SP_SIGNREG)
			peCmpResult(e->op);
	}
	indent -= 2;
	comment("]");
}

/*
 * Try pattern-driven emission.
 * Returns 1 if handled, 0 if fallback needed.
 */
int pemitExpr(struct expr *e)
{
	struct pattern p;
	unsigned char op;

	if (!e) return 1;

	patFromExpr(&p, e);
	if (p.len == 0) return 0;

	op = p.str[0];
	switch (op) {
	case PAT_CONST:
		pemitConst(&p, e);
		return 1;
	case PAT_SYM:
		pemitSym(&p, e);
		return 1;
	case PAT_LOCAL:
		pemitLocal(&p, e);
		return 1;
	case PAT_REG:
		pemitReg(&p, e);
		return 1;
	case PLUS:
		pemitPlus(&p, e);
		return 1;
	case MINUS:
		pemitMinus(&p, e);
		return 1;
	case STAR:
		pemitStar(&p, e);
		return 1;
	case DIV:
		pemitDiv(&p, e);
		return 1;
	case MOD:
		pemitMod(&p, e);
		return 1;
	case AND:
		pemitAnd(&p, e);
		return 1;
	case OR:
		pemitOr(&p, e);
		return 1;
	case XOR:
		pemitXor(&p, e);
		return 1;
	case LSHIFT:
		pemitLshift(&p, e);
		return 1;
	case RSHIFT:
		pemitRshift(&p, e);
		return 1;
	case DEREF:
		pemitDeref(&p, e);
		return 1;
	case ASSIGN:
		pemitAssign(&p, e);
		return 1;
	case LT:
	case EQ:
	case NEQ:
		pemitCompare(&p, e);
		return 1;
	case PREINC:
	case PREDEC:
		pemitPreIncDec(&p, e);
		return 1;
	case POSTINC:
	case POSTDEC:
		pemitPostInc(&p, e);
		return 1;
	case PLUSEQ:
	case SUBEQ:
	case OREQ:
	case ANDEQ:
	case XOREQ:
	case MODEQ:
		pemitCmpArith(&p, e);
		return 1;
	case LSHIFTEQ:
	case RSHIFTEQ:
		pemitCmpShift(&p, e);
		return 1;
	case MULTEQ:
	case DIVEQ:
		pemitCmpMulDiv(&p, e);
		return 1;
	}
	return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
