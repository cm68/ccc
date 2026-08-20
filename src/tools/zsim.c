/*
 * zsim — Z280 simulator (unprivileged instructions only)
 *
 * Runs a raw Z280 memory image.  Implements the Z80-compatible base plus
 * the Z280 extensions the ccc compiler emits: SR (sp+d) and X (ix+d/iy+d)
 * addressing, LDA, LDW (word load/store through a register), CPW/ADDW/SUBW,
 * MULT/DIV, and PUSH immediate/memory.  System calls are the micronix
 * convention - rst 08h with the syscall number in the following byte - and
 * a small set is served by the host (exit, read, write, ...).
 *
 * Flags follow the Z280 manual: 8-bit and 16-bit arithmetic set P/V to
 * overflow; logic sets it to parity; add hl,rr sets only H/N/C.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char  u8;
typedef signed char    i8;
typedef unsigned short u16;
typedef short          i16;
typedef unsigned int   u32;
typedef int            i32;

/* ------------------------------------------------------------------ */
/* registers                                                           */

static u16 pc, sp, ix, iy;
static u8  a, f, b, c, d, e, h, l;
static u8  a2, f2, b2, c2, d2, e2, h2, l2;   /* alternate (exx / ex af,af') */
static u8  i_, r;                            /* interrupt vector, refresh   */

/* flag bits */
#define FS   0x80
#define FZ   0x40
#define FH   0x10
#define FPV  0x04
#define FN   0x02
#define FC   0x01

/* memory */
static u8 mem[65536];

/* ------------------------------------------------------------------ */
/* register pair access                                                */

#define getBC()  ((u16)((b << 8) | c))
#define getDE()  ((u16)((d << 8) | e))
#define getHL()  ((u16)((h << 8) | l))
#define getAF()  ((u16)((a << 8) | f))
static void setBC(u16 v) { b = (u8)(v >> 8); c = (u8)v; }
static void setDE(u16 v) { d = (u8)(v >> 8); e = (u8)v; }
static void setHL(u16 v) { h = (u8)(v >> 8); l = (u8)v; }
static void setAF(u16 v) { a = (u8)(v >> 8); f = (u8)v; }

static u8 rd8(u16 addr) { return mem[addr]; }
static u16 rd16(u16 addr) { return (u16)(mem[addr] | (mem[(addr + 1) & 0xffff] << 8)); }
static void wr16(u16 addr, u16 v) { mem[addr] = (u8)v; mem[(addr + 1) & 0xffff] = (u8)(v >> 8); }

static u8 fetch8(void) { return mem[pc++]; }
static u16 fetch16(void) { u16 v = rd16(pc); pc += 2; return v; }

/* forward decls: the DD/FD index-prefix helpers, used by addhl16 below */
static u8  idx_mode;
static u16 idxbase(void);
static u16 cur_hl(void);
static void set_cur_hl(u16 v);

/* ------------------------------------------------------------------ */
/* flags                                                               */

static u8 parity_tab[256];
static void init_parity(void)
{
	int i, j, n;
	for (i = 0; i < 256; i++) {
		n = 0;
		for (j = 0; j < 8; j++)
			if (i & (1 << j))
				n++;
		parity_tab[i] = (u8)(n & 1);   /* 1 = odd parity (P/V clear) */
	}
}

static void set_sz(u8 v)
{
	f = (u8)((f & ~(FS | FZ)) | (v & FS) | (v ? 0 : FZ));
}

static void set_szp(u8 v)
{
	f = (u8)((f & ~(FS | FZ | FPV)) | (v & FS) | (v ? 0 : FZ)
		| (parity_tab[v] ? 0 : FPV));
}

/* addition: half-carry and overflow */
static u8 hc_add(u8 x, u8 y, u8 ci) { return ((x & 0x0f) + (y & 0x0f) + ci) > 0x0f; }
static u8 ov_add(u8 x, u8 y, u8 res) { return ((res ^ x) & (res ^ y) & 0x80) != 0; }
static u8 hc_sub(u8 x, u8 y, u8 ci) { return ((x & 0x0f) - (y & 0x0f) - ci) < 0; }
static u8 ov_sub(u8 x, u8 y, u8 res) { return ((x ^ y) & (x ^ res) & 0x80) != 0; }

static void add8(u8 v)
{
	u8 x = a, res = (u8)(a + v);
	f = 0;
	f |= (res & FS) | (res ? 0 : FZ);
	if (hc_add(x, v, 0)) f |= FH;
	if (ov_add(x, v, res)) f |= FPV;
	if (res & FC) f |= FC;
	a = res;
}

static void adc8(u8 v)
{
	u8 x = a, ci = (u8)(f & FC), res = (u8)(a + v + ci);
	f = 0;
	f |= (res & FS) | (res ? 0 : FZ);
	if (hc_add(x, v, ci)) f |= FH;
	if (ov_add(x, v, res)) f |= FPV;
	if ((u16)x + v + ci > 0xff) f |= FC;
	a = res;
}

static void sub8(u8 v)
{
	u8 x = a, res = (u8)(a - v);
	f = FN;
	f |= (res & FS) | (res ? 0 : FZ);
	if (hc_sub(x, v, 0)) f |= FH;
	if (ov_sub(x, v, res)) f |= FPV;
	if (x < v) f |= FC;
	a = res;
}

static void sbc8(u8 v)
{
	u8 x = a, ci = (u8)(f & FC), res = (u8)(a - v - ci);
	f = FN;
	f |= (res & FS) | (res ? 0 : FZ);
	if (hc_sub(x, v, ci)) f |= FH;
	if (ov_sub(x, v, res)) f |= FPV;
	if ((u16)x < (u16)v + ci) f |= FC;
	a = res;
}

static void and8(u8 v) { a &= v; set_szp(a); f = (u8)((f & ~(FN | FH | FC)) | FH); }
static void xor8(u8 v) { a ^= v; set_szp(a); f = (u8)(f & ~(FN | FH | FC)); }
static void or8(u8 v)  { a |= v; set_szp(a); f = (u8)(f & ~(FN | FH | FC)); }
static void cp8(u8 v)  { sub8(v); a = (u8)(a + v); }   /* restore A, keep flags */

static void inc8(u8 *rp)
{
	u8 x = *rp, res = (u8)(x + 1);
	f = (u8)((f & ~(FS | FZ | FH | FPV | FN)) | (res & FS) | (res ? 0 : FZ)
		| (((x & 0x0f) == 0x0f) ? FH : 0) | (ov_add(x, 1, res) ? FPV : 0));
	*rp = res;
}

static void dec8(u8 *rp)
{
	u8 x = *rp, res = (u8)(x - 1);
	f = (u8)((f & ~(FS | FZ | FH | FPV)) | FN | (res & FS) | (res ? 0 : FZ)
		| (((x & 0x0f) == 0) ? FH : 0) | (ov_sub(x, 1, res) ? FPV : 0));
	*rp = res;
}

/* add hl|ix|iy, rr : sets H, N=0, C; S/Z untouched */
static void addhl16(u16 v)
{
	u16 x = cur_hl();
	u32 r = (u32)x + v;
	f = (u8)((f & ~(FH | FN | FC)) | (((x & 0x0fff) + (v & 0x0fff)) > 0x0fff ? FH : 0)
		| (r > 0xffff ? FC : 0));
	set_cur_hl((u16)r);
}

static void adchl16(u16 v)
{
	u8 ci = (u8)(f & FC);
	u32 r = (u32)getHL() + v + ci;
	f = 0;
	if (((getHL() & 0x0fff) + (v & 0x0fff) + ci) > 0x0fff) f |= FH;
	if (ov_add((u8)(getHL() >> 8), (u8)(v >> 8), (u8)(r >> 8))) f |= FPV;
	if (r > 0xffff) f |= FC;
	if ((u16)r & 0x8000) f |= FS;
	if (!(u16)r) f |= FZ;
	setHL((u16)r);
}

static void sbchl16(u16 v)
{
	u8 ci = (u8)(f & FC);
	u32 r = (u32)getHL() - v - ci;
	f = FN;
	if (((getHL() & 0x0fff) - (v & 0x0fff) - ci) < 0) f |= FH;
	if (ov_sub((u8)(getHL() >> 8), (u8)(v >> 8), (u8)(r >> 8))) f |= FPV;
	if (r > 0xffff) f |= FC;
	if ((u16)r & 0x8000) f |= FS;
	if (!(u16)r) f |= FZ;
	setHL((u16)r);
}

/* ------------------------------------------------------------------ */
/* stack                                                               */

static void push(u16 v) { sp -= 2; wr16(sp, v); }
static u16 pop(void) { u16 v = rd16(sp); sp += 2; return v; }

/* ------------------------------------------------------------------ */
/* conditional flag test                                               */

static int cond(u8 cc)
{
	switch (cc) {
	case 0: return !(f & FZ);         /* nz */
	case 1: return (f & FZ) != 0;     /* z  */
	case 2: return !(f & FC);         /* nc */
	case 3: return (f & FC) != 0;     /* c  */
	case 4: return !(f & FPV);        /* po */
	case 5: return (f & FPV) != 0;    /* pe */
	case 6: return !(f & FS);         /* p  */
	case 7: return (f & FS) != 0;     /* m  */
	}
	return 0;
}

/* ------------------------------------------------------------------ */
/* syscalls (rst 08h)                                                  */

#define SYS_EXIT  1
#define SYS_READ  3
#define SYS_WRITE 4

static int running = 1;

static void syscall(void)
{
	u8 code = rd8(pc);          /* syscall number after the rst */
	u16 addr = rd16(pc + 1);    /* pointer to the .db/.dw descriptor */
	pc += 3;
	/* the descriptor holds "rst 08h ; db <real number>" */
	if (code == 0 && addr) {
		/* byte 0 is 0xcf (rst 8), byte 1 is the real number */
		code = mem[addr + 1];
	}

	switch (code) {
	case SYS_EXIT:
		running = 0;
		break;
	case SYS_WRITE: {
		/* fd in hl, buffer in de, count in bc (convention varies) */
		u16 fd = getHL();
		u16 buf = getDE();
		u16 n = getBC();
		(void)fd;
		if (buf && n && buf + n <= 0x10000)
			fwrite(mem + buf, 1, n, stdout);
		setHL(n);
		break;
	}
	case SYS_READ: {
		u16 buf = getDE();
		u16 n = getBC();
		if (buf && n && buf + n <= 0x10000) {
			size_t got = fread(mem + buf, 1, n, stdin);
			setHL((u16)got);
		} else
			setHL(0);
		break;
	}
	default:
		/* unknown syscall: signal by setting carry */
		f |= FC;
		setHL(0xffff);
		break;
	}
}

/* ------------------------------------------------------------------ */
/* index registers: DD/FD prefix handling                              */

/*
 * The DD/FD prefix makes the following instruction use IX/IY in place of
 * HL (and H/L become IXH/IXL etc for the byte forms).  We implement this
 * with a flag and a displacement, checked by the memory/HL accessors.
 */
static u8 idx_mode;    /* 0 = none, 1 = ix, 2 = iy */
static i16 idx_disp;

static u16 idxbase(void) { return idx_mode == 1 ? ix : iy; }
static u8 *idxlo(void) { return idx_mode == 1 ? (u8 *)&ix : (u8 *)&iy; }
static u8 *idxhi(void) { return idx_mode == 1 ? (u8 *)((u8 *)&ix + 1) : (u8 *)((u8 *)&iy + 1); }

/* a register operand, honoring the index prefix */
static u8 *reg8(u8 n)
{
	switch (n) {
	case 0: return &b;
	case 1: return &c;
	case 2: return &d;
	case 3: return &e;
	case 4: return idx_mode ? idxhi() : &h;
	case 5: return idx_mode ? idxlo() : &l;
	case 6: return 0;   /* (hl) */
	case 7: return &a;
	}
	return 0;
}

/* the (hl) / (ix+d) / (iy+d) address */
static u16 indaddr(void)
{
	if (idx_mode)
		return (u16)(idxbase() + idx_disp);
	return getHL();
}

/* the "current HL": HL, or IX/IY under a DD/FD prefix */
static u16 cur_hl(void) { return idx_mode ? idxbase() : getHL(); }
static void set_cur_hl(u16 v) { if (idx_mode) { if (idx_mode == 1) ix = v; else iy = v; } else setHL(v); }

/* ------------------------------------------------------------------ */
/* instruction execution                                               */

static void neg8(void);
static void daa(void);
static void exec_cb(void);   /* CB prefix */
static void exec_ed(void);   /* ED prefix */
static void exec_base(u8 op);

/* ------------------------------------------------------------------ */
/* DD/FD index-prefix dispatch                                         */
/*                                                                     */
/* The Z280 uses DD/FD both as the IX/IY index prefix and as a prefix  */
/* for extension opcodes (push nn = FD F5 nn, cpw hl,ix = DD ED E7).   */
/* Under the index prefix the operand codes are reinterpreted:         */
/*   0=SR (sp+dd16)  1..3=BX (hl+ix/hl+iy/ix+iy)  4/5=IXH/IXL (RX)     */
/*   6=SX (ix+d8)    7=DA (addr16)                                     */
/* except ld h/l,(ix+d) and ld (ix+d),h/l keep real H/L.               */

/* full-flag 16-bit compare/add/subtract of HL with a word operand */
static void cpw16(u16 v)
{
	u32 r = (u32)getHL() - v;
	f = FN;
	if (((getHL() & 0x0fff) - (v & 0x0fff)) < 0) f |= FH;
	if (ov_sub((u8)(getHL() >> 8), (u8)(v >> 8), (u8)(r >> 8))) f |= FPV;
	if (r > 0xffff) f |= FC;
	if ((u16)r & 0x8000) f |= FS;
	if (!(u16)r) f |= FZ;
}

static void addw16(u16 v)
{
	u16 x = getHL();
	u32 r = (u32)x + v;
	f = 0;
	if (((x & 0x0fff) + (v & 0x0fff)) > 0x0fff) f |= FH;
	if (ov_add((u8)(x >> 8), (u8)(v >> 8), (u8)(r >> 8))) f |= FPV;
	if (r > 0xffff) f |= FC;
	if ((u16)r & 0x8000) f |= FS;
	if (!(u16)r) f |= FZ;
	setHL((u16)r);
}

static void subw16(u16 v)
{
	u16 x = getHL();
	u32 r = (u32)x - v;
	f = FN;
	if (((x & 0x0fff) - (v & 0x0fff)) < 0) f |= FH;
	if (ov_sub((u8)(x >> 8), (u8)(v >> 8), (u8)(r >> 8))) f |= FPV;
	if (r > 0xffff) f |= FC;
	if ((u16)r & 0x8000) f |= FS;
	if (!(u16)r) f |= FZ;
	setHL((u16)r);
}

/* byte source operand (code 0-7) under DD/FD, addressing mode resolved */
static u8 idx_op8(u8 c)
{
	switch (c) {
	case 0: { i16 dd = (i16)fetch16(); return mem[(u16)(sp + dd)]; }  /* SR */
	case 1: return mem[(u16)(getHL() + ix)];   /* BX hl+ix */
	case 2: return mem[(u16)(getHL() + iy)];   /* BX hl+iy */
	case 3: return mem[(u16)(ix + iy)];        /* BX ix+iy */
	case 4: return *idxhi();                    /* RX ixh/iyh */
	case 5: return *idxlo();                    /* RX ixl/iyl */
	case 6: { i8 d = (i8)fetch8(); return mem[(u16)(idxbase() + d)]; } /* SX */
	case 7: { u16 n = fetch16(); return mem[n]; }   /* DA */
	}
	return 0;
}

/* ED under DD/FD: the Z280 word extensions naming IX/IY */
static void exec_ed_idx(void)
{
	u8 op = fetch8();
	switch (op) {
	case 0x26: {                            /* ld hl,(ix+d8)/(iy+d8) */
		i8 d = (i8)fetch8();
		setHL(rd16((u16)(idxbase() + d)));
		return;
	}
	case 0xe6: addw16(idxbase()); return;    /* addw hl,ix/iy */
	case 0xe7: cpw16(idxbase()); return;     /* cpw hl,ix/iy */
	case 0xee: subw16(idxbase()); return;    /* subw hl,ix/iy */
	case 0xf7:                               /* FD=IM nn, DD=RA (pc+disp) */
		if (idx_mode == 2) cpw16(fetch16());
		else { i16 d = (i16)fetch16(); cpw16(rd16((u16)(pc + d))); }
		return;
	default:
		fprintf(stderr, "zsim: unimplemented DD/FD ED 0x%02x at pc=%04x\n", op, pc - 2);
		running = 0;
		return;
	}
}

static void exec_idx(void)
{
	u8 op = fetch8();

	if (op == 0xcb) {                    /* DD/FD CB d8 op */
		idx_disp = (i16)(i8)fetch8();
		exec_cb();
		return;
	}
	if (op == 0xed) {                    /* DD/FD ED ... */
		exec_ed_idx();
		return;
	}

	/* 16-bit IX/IY register ops (no displacement byte) */
	switch (op) {
	case 0x09: addhl16(getBC()); return;
	case 0x19: addhl16(getDE()); return;
	case 0x21: set_cur_hl(fetch16()); return;
	case 0x22: { u16 n = fetch16(); wr16(n, cur_hl()); return; }
	case 0x23: set_cur_hl((u16)(cur_hl() + 1)); return;
	case 0x29: addhl16(cur_hl()); return;
	case 0x2a: { u16 n = fetch16(); set_cur_hl(rd16(n)); return; }
	case 0x2b: set_cur_hl((u16)(cur_hl() - 1)); return;
	case 0x39: addhl16(sp); return;
	case 0xe1: set_cur_hl(pop()); return;
	case 0xe3: { u16 v = rd16(sp); wr16(sp, cur_hl()); set_cur_hl(v); return; }
	case 0xe5: push(cur_hl()); return;
	case 0xe9: pc = cur_hl(); return;
	case 0xeb: { u16 t = getDE(); setDE(cur_hl()); set_cur_hl(t); return; }
	case 0xf9: sp = cur_hl(); return;
	}

	/* Z280 extension: FD F5 nn16 = push nn (IM); DD F5 d16 = push (pc+d) */
	if (op == 0xf5) {
		if (idx_mode == 2) push(fetch16());
		else { i16 d = (i16)fetch16(); push(rd16((u16)(pc + d))); }
		return;
	}

	/* SR byte ops: DD 04/05 dd16 = inc/dec (sp+dd); DD 06 dd16 n = ld (sp+dd),n */
	if (op == 0x04 || op == 0x05) {
		i16 dd = (i16)fetch16();
		u16 ad = (u16)(sp + dd);
		u8 v = mem[ad];
		if (op == 0x04) inc8(&v); else dec8(&v);
		mem[ad] = v;
		return;
	}
	if (op == 0x06) {
		i16 dd = (i16)fetch16();
		mem[(u16)(sp + dd)] = fetch8();
		return;
	}

	/* ld a,src (0x78-0x7f): source is SR/BX/RX/SX/DA */
	if (op >= 0x78 && op <= 0x7f) {
		a = idx_op8(op & 7);
		return;
	}

	/* ALU a,src (0x80-0xbf) */
	if (op >= 0x80 && op <= 0xbf) {
		u8 v = idx_op8(op & 7);
		switch (op & 0xf8) {
		case 0x80: add8(v); break;
		case 0x88: adc8(v); break;
		case 0x90: sub8(v); break;
		case 0x98: sbc8(v); break;
		case 0xa0: and8(v); break;
		case 0xa8: xor8(v); break;
		case 0xb0: or8(v); break;
		case 0xb8: cp8(v); break;
		}
		return;
	}

	/* general LD block (0x40-0x6f, 0x70-0x77): only code 6 is memory (SX) */
	if ((op >= 0x40 && op <= 0x6f) || (op >= 0x70 && op <= 0x77)) {
		u8 src = op & 7, dst = (op >> 3) & 7;
		if (src == 6) {
			i8 d = (i8)fetch8();
			u8 v = mem[(u16)(idxbase() + d)];
			if (dst == 4) { h = v; return; }   /* ld h,(ix+d): real H */
			if (dst == 5) { l = v; return; }   /* ld l,(ix+d): real L */
			*reg8(dst) = v;
			return;
		}
		if (dst == 6) {
			i8 d = (i8)fetch8();
			u8 v = (src == 4) ? h : (src == 5) ? l : *reg8(src);
			mem[(u16)(idxbase() + d)] = v;
			return;
		}
		*reg8(dst) = *reg8(src);
		return;
	}

	/* SX byte ops: DD 34/35 d8 = inc/dec (ix+d); DD 36 d8 n = ld (ix+d),n */
	if (op == 0x34 || op == 0x35) {
		i8 d = (i8)fetch8();
		u16 ad = (u16)(idxbase() + d);
		u8 v = mem[ad];
		if (op == 0x34) inc8(&v); else dec8(&v);
		mem[ad] = v;
		return;
	}
	if (op == 0x36) {
		i8 d = (i8)fetch8();
		mem[(u16)(idxbase() + d)] = fetch8();
		return;
	}

	fprintf(stderr, "zsim: unimplemented DD/FD 0x%02x at pc=%04x\n", op, pc - 1);
	running = 0;
}

static void step(void)
{
	u8 op = fetch8();
	switch (op) {
	case 0xcb: exec_cb(); return;
	case 0xdd: idx_mode = 1; exec_idx(); idx_mode = 0; return;
	case 0xfd: idx_mode = 2; exec_idx(); idx_mode = 0; return;
	case 0xed: exec_ed(); return;
	default: exec_base(op); return;
	}
}

/* CB prefix: rotates, shifts, bit tests */
static void exec_cb(void)
{
	u8 op = fetch8();
	u8 *rp = reg8(op & 7);
	u8 v;

	if (rp) {
		v = *rp;
	} else {
		v = mem[indaddr()];
	}

	switch (op >> 3) {
	case 0: /* rlc */ { u8 c_ = v >> 7; v = (u8)((v << 1) | c_); f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 1: /* rrc */ { u8 c_ = v & 1; v = (u8)((v >> 1) | (c_ << 7)); f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 2: /* rl */ { u8 c_ = v >> 7; v = (u8)((v << 1) | (f & FC)); f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 3: /* rr */ { u8 c_ = v & 1; v = (u8)((v >> 1) | ((f & FC) << 7)); f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 4: /* sla */ { u8 c_ = v >> 7; v <<= 1; f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 5: /* sra */ { u8 c_ = v & 1; v = (u8)((v >> 1) | (v & 0x80)); f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 6: /* sll (undoc) - treat as sla */ { u8 c_ = v >> 7; v = (u8)((v << 1) | 1); f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	case 7: /* srl */ { u8 c_ = v & 1; v >>= 1; f = (u8)((f & ~(FS|FZ|FH|FPV|FN|FC)) | (v & FS) | (v?0:FZ) | (parity_tab[v]?0:FPV) | (c_?FC:0)); break; }
	default: { /* bit n,r / res / set: op >= 0x40 */
		u8 bit = (op >> 3) & 7;
		u8 opkind = (op >> 6) & 3;   /* 1 = bit, 2 = res, 3 = set */
		if (opkind == 1) {
			f = (u8)((f & ~(FZ | FN | FS | FPV)) | FH | ((v & (1 << bit)) ? 0 : FZ));
			/* S and P/V are undefined on bit; leave them as-is for simplicity */
		} else if (opkind == 2) {
			v &= (u8)~(1 << bit);
		} else {
			v |= (u8)(1 << bit);
		}
		break;
	}
	}

	if (rp)
		*rp = v;
	else
		mem[indaddr()] = v;
}

/* ED prefix: extended, plus Z280 word ops */
static void exec_ed(void)
{
	u8 op = fetch8();

	/* word store through HL: ld (hl),bc/de/hl/sp */
	if (op == 0x0e || op == 0x1e || op == 0x2e || op == 0x3e) {
		u16 v = (op == 0x0e) ? getBC() : (op == 0x1e) ? getDE()
		      : (op == 0x2e) ? getHL() : sp;
		wr16(getHL(), v);
		return;
	}
	/* word load through HL / indexed: ld hl,(hl) = 0x26, ld hl,(hl+dd) = 0x3c */
	if (op == 0x26) {
		if (idx_mode) {          /* DD/FD ED 26 d8 : ld hl,(ix+d8)/(iy+d8) */
			i8 dd = (i8)fetch8();
			setHL(rd16((u16)(idxbase() + dd)));
		} else
			setHL(rd16(getHL()));
		return;
	}
	if (op == 0x3c) {
		i16 dd = (i16)fetch16();
		setHL(rd16((u16)(getHL() + dd)));
		return;
	}
	/* ld hl|ix|iy,(ix+dd) = 0x2c / (iy+dd) = 0x34 */
	if (op == 0x2c || op == 0x34) {
		i16 dd = (i16)fetch16();
		setHL(rd16((u16)((op == 0x2c ? ix : iy) + dd)));
		return;
	}
	/* ld hl,(sp+dd) = 0x04 */
	if (op == 0x04) {
		i16 dd = (i16)fetch16();
		setHL(rd16((u16)(sp + dd)));
		return;
	}
	/* ld (sp+dd),hl = 0x05 */
	if (op == 0x05) {
		i16 dd = (i16)fetch16();
		wr16((u16)(sp + dd), getHL());
		return;
	}
	/* ld (sp+dd),a = 0x03 ; lda hl,(sp+dd) = 0x02 */
	if (op == 0x03) {
		i16 dd = (i16)fetch16();
		mem[(u16)(sp + dd)] = a;
		return;
	}
	if (op == 0x02) {   /* lda hl,(sp+dd) */
		i16 dd = (i16)fetch16();
		setHL((u16)(sp + dd));
		return;
	}
	/* lda hl,(ix+dd) = 0x2a / (iy+dd) = 0x32 */
	if (op == 0x2a || op == 0x32) {
		i16 dd = (i16)fetch16();
		setHL((u16)((op == 0x2a ? ix : iy) + dd));
		return;
	}

	switch (op) {
	case 0x40: b = a; return;                    /* in b,(c) - skip: NOP-ish */
	case 0x41: return;                           /* out (c),b - ignored */
	case 0x42: sbchl16(getBC()); return;         /* sbc hl,bc */
	case 0x52: sbchl16(getDE()); return;          /* sbc hl,de */
	case 0x62: sbchl16(getHL()); return;          /* sbc hl,hl */
	case 0x72: sbchl16(sp); return;               /* sbc hl,sp */
	case 0x4a: adchl16(getBC()); return;          /* adc hl,bc */
	case 0x5a: adchl16(getDE()); return;          /* adc hl,de */
	case 0x6a: adchl16(getHL()); return;          /* adc hl,hl */
	case 0x7a: adchl16(sp); return;               /* adc hl,sp */

	/* Z280 cpw hl,rr : compare, sets flags, no store */
	case 0xc6: addw16(getBC()); return;   /* addw hl,bc */
	case 0xd6: addw16(getDE()); return;   /* addw hl,de */
	case 0xe6: addw16(getHL()); return;   /* addw hl,hl */
	case 0xf6: addw16(sp); return;        /* addw hl,sp */
	case 0xc7: cpw16(getBC()); return;    /* cpw hl,bc */
	case 0xd7: cpw16(getDE()); return;    /* cpw hl,de */
	case 0xe7: cpw16(getHL()); return;    /* cpw hl,hl */
	case 0xf7: cpw16(sp); return;         /* cpw hl,sp */
	case 0xce: subw16(getBC()); return;   /* subw hl,bc */
	case 0xde: subw16(getDE()); return;   /* subw hl,de */
	case 0xee: subw16(getHL()); return;   /* subw hl,hl */
	case 0xfe: subw16(sp); return;        /* subw hl,sp */

	case 0x44: neg8(); return;
	case 0x45: case 0x55: case 0x5d: case 0x6d: case 0x7d: return;  /* retn/reti: ret */
	case 0x47: i_ = a; return;                     /* ld i,a */
	case 0x4f: r = a; return;                      /* ld r,a */
	case 0x57: a = i_; set_sz(a); f = (u8)(f & ~(FH | FN)); return;  /* ld a,i */
	case 0x5f: a = r; set_sz(a); f = (u8)(f & ~(FH | FN)); return;   /* ld a,r */
	case 0x4b: case 0x5b: case 0x6b: case 0x7b: {  /* ld bc/de/hl/sp,(nn) */
		u16 nn = fetch16();
		u16 v = rd16(nn);
		if (op == 0x4b) setBC(v); else if (op == 0x5b) setDE(v);
		else if (op == 0x6b) setHL(v); else sp = v;
		return;
	}
	case 0x43: case 0x53: case 0x63: case 0x73: {  /* ld (nn),bc/de/hl/sp */
		u16 nn = fetch16();
		u16 v = (op == 0x43) ? getBC() : (op == 0x53) ? getDE()
		      : (op == 0x63) ? getHL() : sp;
		wr16(nn, v);
		return;
	}
	default:
		fprintf(stderr, "zsim: unimplemented ED 0x%02x at pc=%04x\n", op, pc - 2);
		running = 0;
		return;
	}
}

static void neg8(void)
{
	u8 x = a;
	a = (u8)(0 - a);
	f = FN;
	f |= (a & FS) | (a ? 0 : FZ);
	if ((x & 0x0f) != 0) f |= FH;
	if (ov_sub(0, x, a)) f |= FPV;
	if (x != 0) f |= FC;
}

/* ------------------------------------------------------------------ */
/* base opcodes                                                        */

static void exec_base(u8 op)
{
	/* fast path: LD r,r' (0x40..0x7f, except 0x76 = halt) */
	if (op >= 0x40 && op <= 0x7f && op != 0x76) {
		u8 src = op & 7, dst = (op >> 3) & 7;
		u8 *rp = reg8(src);
		u8 *dp = reg8(dst);
		u8 v;
		if (rp) v = *rp; else v = mem[indaddr()];
		if (dp) *dp = v; else mem[indaddr()] = v;
		return;
	}

	switch (op) {
	case 0x00: return;                                  /* nop */
	case 0x01: setBC(fetch16()); return;
	case 0x02: mem[getBC()] = a; return;
	case 0x03: setBC((u16)(getBC() + 1)); return;
	case 0x04: inc8(&b); return;
	case 0x05: dec8(&b); return;
	case 0x06: b = fetch8(); return;
	case 0x07: { u8 c_ = a >> 7; a = (u8)((a << 1) | c_); f = (u8)((f & ~(FH|FN|FC)) | (c_?FC:0)); return; }
	case 0x08: { u8 t = a; a = a2; a2 = t; t = f; f = f2; f2 = t; return; }  /* ex af,af' */
	case 0x09: addhl16(getBC()); return;
	case 0x0a: a = mem[getBC()]; return;
	case 0x0b: setBC((u16)(getBC() - 1)); return;
	case 0x0c: inc8(&c); return;
	case 0x0d: dec8(&c); return;
	case 0x0e: c = fetch8(); return;
	case 0x0f: { u8 c_ = a & 1; a = (u8)((a >> 1) | (c_ << 7)); f = (u8)((f & ~(FH|FN|FC)) | (c_?FC:0)); return; }
	case 0x10: { i8 dd = (i8)fetch8(); if (--b) pc = (u16)(pc + dd); return; }   /* djnz */
	case 0x11: setDE(fetch16()); return;
	case 0x12: mem[getDE()] = a; return;
	case 0x13: setDE((u16)(getDE() + 1)); return;
	case 0x14: inc8(&d); return;
	case 0x15: dec8(&d); return;
	case 0x16: d = fetch8(); return;
	case 0x17: { u8 c_ = a >> 7; a = (u8)((a << 1) | (f & FC)); f = (u8)((f & ~(FH|FN|FC)) | (c_?FC:0)); return; }
	case 0x18: { i8 dd = (i8)fetch8(); pc = (u16)(pc + dd); return; }
	case 0x19: addhl16(getDE()); return;
	case 0x1a: a = mem[getDE()]; return;
	case 0x1b: setDE((u16)(getDE() - 1)); return;
	case 0x1c: inc8(&e); return;
	case 0x1d: dec8(&e); return;
	case 0x1e: e = fetch8(); return;
	case 0x1f: { u8 c_ = a & 1; a = (u8)((a >> 1) | ((f & FC) << 7)); f = (u8)((f & ~(FH|FN|FC)) | (c_?FC:0)); return; }

	case 0x20: case 0x28: case 0x30: case 0x38: {  /* jr cc,d */
		i8 dd = (i8)fetch8();
		if (cond((op - 0x20) >> 3)) pc = (u16)(pc + dd);
		return;
	}
	case 0x21: set_cur_hl(fetch16()); return;
	case 0x22: { u16 nn = fetch16(); wr16(nn, cur_hl()); return; }
	case 0x23: set_cur_hl((u16)(cur_hl() + 1)); return;
	case 0x24: inc8(reg8(4)); return;
	case 0x25: dec8(reg8(4)); return;
	case 0x26: *reg8(4) = fetch8(); return;
	case 0x27: daa(); return;
	case 0x29: addhl16(cur_hl()); return;
	case 0x2a: { u16 nn = fetch16(); set_cur_hl(rd16(nn)); return; }
	case 0x2b: set_cur_hl((u16)(cur_hl() - 1)); return;
	case 0x2c: inc8(reg8(5)); return;
	case 0x2d: dec8(reg8(5)); return;
	case 0x2e: *reg8(5) = fetch8(); return;
	case 0x2f: a = (u8)~a; f = (u8)((f & ~(FN | FH)) | FN | FH); return;
	case 0x31: sp = fetch16(); return;
	case 0x32: { u16 nn = fetch16(); mem[nn] = a; return; }
	case 0x33: sp = (u16)(sp + 1); return;
	case 0x34: { u16 ad = indaddr(); u8 v = mem[ad]; inc8(&v); mem[ad] = v; return; }
	case 0x35: { u16 ad = indaddr(); u8 v = mem[ad]; dec8(&v); mem[ad] = v; return; }
	case 0x36: { u16 ad = indaddr(); mem[ad] = fetch8(); return; }
	case 0x37: f = (u8)((f & ~(FH | FN)) | FC); return;   /* scf */
	case 0x39: addhl16(sp); return;
	case 0x3a: { u16 nn = fetch16(); a = mem[nn]; return; }
	case 0x3b: sp = (u16)(sp - 1); return;
	case 0x3c: inc8(&a); return;
	case 0x3d: dec8(&a); return;
	case 0x3e: a = fetch8(); return;
	case 0x3f: { u8 c_ = f & FC; f = (u8)((f & ~(FH | FC | FN)) | (c_ ? 0 : FC)); return; }  /* ccf */

	/* ALU a,r */
	case 0x80: case 0x81: case 0x82: case 0x83: case 0x84: case 0x85: case 0x86: case 0x87:
	case 0x88: case 0x89: case 0x8a: case 0x8b: case 0x8c: case 0x8d: case 0x8e: case 0x8f:
	case 0x90: case 0x91: case 0x92: case 0x93: case 0x94: case 0x95: case 0x96: case 0x97:
	case 0x98: case 0x99: case 0x9a: case 0x9b: case 0x9c: case 0x9d: case 0x9e: case 0x9f:
	case 0xa0: case 0xa1: case 0xa2: case 0xa3: case 0xa4: case 0xa5: case 0xa6: case 0xa7:
	case 0xa8: case 0xa9: case 0xaa: case 0xab: case 0xac: case 0xad: case 0xae: case 0xaf:
	case 0xb0: case 0xb1: case 0xb2: case 0xb3: case 0xb4: case 0xb5: case 0xb6: case 0xb7:
	case 0xb8: case 0xb9: case 0xba: case 0xbb: case 0xbc: case 0xbd: case 0xbe: case 0xbf: {
		u8 *rp = reg8(op & 7);
		u8 v = rp ? *rp : mem[indaddr()];
		switch (op & 0xf8) {
		case 0x80: add8(v); break;
		case 0x88: adc8(v); break;
		case 0x90: sub8(v); break;
		case 0x98: sbc8(v); break;
		case 0xa0: and8(v); break;
		case 0xa8: xor8(v); break;
		case 0xb0: or8(v); break;
		case 0xb8: cp8(v); break;
		}
		return;
	}

	case 0xc0: case 0xc8: case 0xd0: case 0xd8: case 0xe0: case 0xe8: case 0xf0: case 0xf8: {
		if (cond((op - 0xc0) >> 3)) pc = pop();
		return;
	}
	case 0xc1: setBC(pop()); return;
	case 0xc2: case 0xca: case 0xd2: case 0xda: case 0xe2: case 0xea: case 0xf2: case 0xfa: {
		u16 nn = fetch16();
		if (cond((op - 0xc2) >> 3)) pc = nn;
		return;
	}
	case 0xc3: pc = fetch16(); return;
	case 0xc4: case 0xcc: case 0xd4: case 0xdc: case 0xe4: case 0xec: case 0xf4: case 0xfc: {
		u16 nn = fetch16();
		if (cond((op - 0xc4) >> 3)) { push(pc); pc = nn; }
		return;
	}
	case 0xc5: push(getBC()); return;
	case 0xc6: add8(fetch8()); return;
	case 0xc7: syscall(); return;   /* rst 08h (also rst 0, but 0x00 is nop) */
	case 0xc9: pc = pop(); return;
	case 0xcd: { u16 nn = fetch16(); push(pc); pc = nn; return; }
	case 0xce: adc8(fetch8()); return;
	case 0xcf: syscall(); return;   /* rst 08h - the micronix syscall */
	case 0xd1: setDE(pop()); return;
	case 0xd5: push(getDE()); return;
	case 0xd6: sub8(fetch8()); return;
	case 0xd9: { u8 t; t=b;b=b2;b2=t; t=c;c=c2;c2=t; t=d;d=d2;d2=t; t=e;e=e2;e2=t; t=h;h=h2;h2=t; t=l;l=l2;l2=t; return; }  /* exx */
	case 0xde: sbc8(fetch8()); return;
	case 0xe1: set_cur_hl(pop()); return;
	case 0xe3: { u16 v = rd16(sp); wr16(sp, cur_hl()); set_cur_hl(v); return; }  /* ex (sp),hl */
	case 0xe5: push(cur_hl()); return;
	case 0xe6: and8(fetch8()); return;
	case 0xe9: pc = cur_hl(); return;
	case 0xeb: { u16 t = getDE(); setDE(cur_hl()); set_cur_hl(t); return; }  /* ex de,hl */
	case 0xee: xor8(fetch8()); return;
	case 0xf1: setAF(pop()); return;
	case 0xf5: push(getAF()); return;
	case 0xf6: or8(fetch8()); return;
	case 0xf9: sp = cur_hl(); return;
	case 0xfb: return;   /* ei */
	case 0xf3: return;   /* di */
	case 0xfe: cp8(fetch8()); return;
	case 0x76: running = 0; return;  /* halt */
	default:
		fprintf(stderr, "zsim: unimplemented opcode 0x%02x at pc=%04x\n", op, pc - 1);
		running = 0;
		return;
	}
}

/* decimal adjust accumulator */
static void daa(void)
{
	u8 x = a, adj = 0;
	if ((f & FH) || ((x & 0x0f) > 9)) adj |= 0x06;
	if ((f & FC) || (x > 0x99)) adj |= 0x60;
	if (f & FN) {
		a = (u8)(x - adj);
		f = (u8)((f & ~(FH | FPV | FC)) | (((x & 0x0f) < (adj & 0x0f)) ? FH : 0)
			| (x >= adj ? 0 : FC));
	} else {
		a = (u8)(x + adj);
		f = (u8)((f & ~(FH | FPV | FC)) | (((x & 0x0f) + (adj & 0x0f)) > 0x0f ? FH : 0)
			| (((u16)x + adj) > 0xff ? FC : 0));
	}
	set_sz(a);
}

/* ------------------------------------------------------------------ */
/* loading and entry                                                   */

static void load_image(const char *path, u16 load_addr)
{
	FILE *fp = fopen(path, "rb");
	long n;
	if (!fp) { fprintf(stderr, "zsim: cannot open %s\n", path); exit(1); }
	fseek(fp, 0, SEEK_END);
	n = ftell(fp);
	fseek(fp, 0, SEEK_SET);
	if (load_addr + n > 0x10000) { fprintf(stderr, "zsim: image too large\n"); exit(1); }
	if (fread(mem + load_addr, 1, (size_t)n, fp) != (size_t)n)
		{ fprintf(stderr, "zsim: short read\n"); exit(1); }
	fclose(fp);
}

int main(int argc, char **argv)
{
	u16 entry = 0x0100;
	u16 load = 0x0100;
	u16 stack = 0xfffe;

	if (argc < 2) {
		fprintf(stderr, "usage: %s <image> [entry] [load] [stack]\n", argv[0]);
		return 1;
	}
	if (argc > 2) entry = (u16)strtol(argv[2], 0, 0);
	if (argc > 3) load = (u16)strtol(argv[3], 0, 0);
	if (argc > 4) stack = (u16)strtol(argv[4], 0, 0);

	init_parity();
	load_image(argv[1], load);

	pc = entry;
	sp = stack;

	{
		long count = 0;
		while (running && count < 1000000) {
			if (getenv("ZTRACE")) fprintf(stderr, "pc=%04x op=%02x\n", pc, mem[pc]);
			step();
			count++;
		}
		if (count >= 1000000) fprintf(stderr, "zsim: instruction limit hit\n");
	}

	fprintf(stderr, "zsim: stopped at pc=%04x, a=%02x bc=%04x de=%04x hl=%04x\n",
		pc, a, getBC(), getDE(), getHL());
	return (int)(getHL() & 0xff);
}
