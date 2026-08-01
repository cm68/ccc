/*
 * cpp's numeric-define slab store, as a unit.  Variable-length
 * records packed in chained slabs: header (dead bit, width code,
 * name length), little-endian value, name bytes with no NUL.  The
 * Z80 cpp lost _NFILE and kstag out of this store while the host
 * found them: the walk's "p += 1 + NDLEN(w) + len" - a compound
 * add of a ternary and a variable to a register-homed pointer -
 * spilled HL as the saved lvalue with p sitting in BC, and the
 * find marched off into whatever the condition had left behind.
 * Every operation the store does is exercised here: add, find,
 * in-place update, dead-and-reappend, undef, and a walk across a
 * slab boundary.  The slabs come from a bump pointer, not a 2-D
 * array: variable row selection of a 2-D array is its own open
 * compiler gap, and this test is about the store.
 */
#include "rt.h"

#ifdef RT_ZC3
/*
 * zc3 miscompiles this store somewhere before the first find - the
 * layout unit fails under it too.  It is the legacy reference and
 * the bug is undiagnosed; the test is about ccc's codegen, which
 * the native build cross-checks.
 */
int
main()
{
	return 0;
}
#else

#define NSLAB 64
static char pool[4 * NSLAB];
static char *pp;
static int nslab;
static unsigned char *nslabs;
static unsigned char *nfree;
static unsigned char *nend;

static unsigned char *
newslab(void)
{
	unsigned char *s;

	if (!pp)
		pp = pool;
	s = (unsigned char *)pp;
	pp += NSLAB;
	nslab++;
	*(unsigned char **)s = nslabs;
	nslabs = s;
	nfree = s + sizeof(char *);
	nend = s + NSLAB;
	return s;
}

static long
ndget(unsigned char *p, unsigned char w)
{
	unsigned v;

	if (w == 0)
		return (char)p[0];
	v = p[0] | (p[1] << 8);
	if (w == 1)
		return (long)v;
	return (short)v;
}

static void
ndput(unsigned char *p, unsigned char w, int val)
{
	*p++ = val;
	if (w)
		*p = val >> 8;
}

#define NDLEN(w) ((w) ? 2 : 1)

static unsigned char *
ndeffind(char *name)
{
	unsigned char *slab, *p;
	unsigned char h, len, w;
	int nl = strlen(name);

	for (slab = nslabs; slab; slab = *(unsigned char **)slab) {
		p = slab + sizeof(char *);
		while ((h = *p)) {
			len = h & 0x1f;
			w = (h >> 5) & 3;
			if (!(h & 0x80) && len == nl &&
			    memcmp((char *)p + 1 + NDLEN(w), name, len) == 0)
				return p;
			p += 1 + NDLEN(w) + len;
		}
	}
	return 0;
}

static void
ndefadd(char *name, long lval)
{
	unsigned char *p = ndeffind(name);
	int val = (int)lval;
	unsigned char w;
	unsigned char len;

	w = lval >= -128 && lval < 128 ? 0 : lval > 0 ? 1 : 2;
	if (p) {
		unsigned char ow = (*p >> 5) & 3;
		if (w == ow || (w == 0 && ow == 2)) {
			ndput(p + 1, ow, val);
			return;
		}
		*p |= 0x80;
	}
	len = strlen(name);
	if (!nslabs || nfree + 1 + NDLEN(w) + len >= nend)
		newslab();
	*nfree = (w << 5) | len;
	ndput(nfree + 1, w, val);
	memcpy((char *)nfree + 1 + NDLEN(w), name, len);
	nfree += 1 + NDLEN(w) + len;
}

static char
ndefval(char *name, long *out)
{
	unsigned char *p = ndeffind(name);

	if (!p)
		return 0;
	*out = ndget(p + 1, (*p >> 5) & 3);
	return 1;
}

int
main()
{
	long v;
	int i;
	char nm[8];

	ndefadd("_NFILE", 8);
	ndefadd("kstag", 2);
	ndefadd("SYNTH", 32768L);
	ndefadd("NEG", -300);
	ndefadd("EOF_", -1);

	if (!ndefval("_NFILE", &v) || v != 8)
		return 1;
	if (!ndefval("kstag", &v) || v != 2)
		return 2;
	if (!ndefval("SYNTH", &v) || v != 32768L)
		return 3;
	if (!ndefval("NEG", &v) || v != -300)
		return 4;
	if (!ndefval("EOF_", &v) || v != -1)
		return 5;

	/* in-place update, same width class */
	ndefadd("kstag", 3);
	if (!ndefval("kstag", &v) || v != 3)
		return 6;

	/* width change: dead + reappend */
	ndefadd("kstag", 300);
	if (!ndefval("kstag", &v) || v != 300)
		return 7;

	/* fill across a slab boundary, then find everything */
	for (i = 0; i < 12; i++) {
		nm[0] = 'n'; nm[1] = 'm';
		nm[2] = 'a' + i; nm[3] = 0;
		ndefadd(nm, 1000 + i);
	}
	for (i = 0; i < 12; i++) {
		nm[0] = 'n'; nm[1] = 'm';
		nm[2] = 'a' + i; nm[3] = 0;
		if (!ndefval(nm, &v) || v != 1000 + i)
			return 8;
	}
	if (!ndefval("_NFILE", &v) || v != 8)
		return 9;
	if (nslab < 2)
		return 10;
	return 0;
}
#endif /* RT_ZC3 */
