/*
 * astio.c - AST file I/O
 */
#include "pass2.h"
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef DEBUG
#include "debug.h"
#endif

int infd;
int outfd;
static int pushback = -1;

/*
 * The .n sidecar, from cpp -j: identifiers crossed the front of the
 * compiler as 2-byte ids and their spellings live here - a 2-byte
 * count, a table of 2-byte offsets, then the names in id order,
 * NUL-terminated.  Ids are 1-based; 0 is reserved.  Two seeks fetch
 * any name, so nothing gets loaded.  c1 is where ids become symbols
 * again: expansion happens as names are READ, so everything
 * downstream - codegen, the peephole, our own diagnostics - sees
 * real spellings and never knows the ids existed.
 */
static int nidfd = -1;

/*
 * Sidecar seeks bypass lseek's _fdpos tracking on the Z80: this fd
 * is only ever read at seeked positions, nothing asks where it is,
 * and lseek would drag 600 bytes of position machinery into c1.
 */
#ifdef CCC
extern int seekraw();
#define NSEEK(fd, off) seekraw(fd, (int)(off), 0)
#else
#define NSEEK(fd, off) lseek(fd, (long)(off), 0)
#endif

void
nidopen(char *f1)
{
	char nf[256];
	int n = strlen(f1);

	strcpy(nf, f1);
	if (n > 2 && nf[n - 2] == '.')	/* base.1 -> base.n */
		nf[n - 1] = 'n';
	nidfd = open(nf, O_RDONLY);
}

/*
 * Fetch a spelling.  The read overshoots into the following names;
 * the first NUL marks the end.
 */
static void
nidname(unsigned short id, char *buf, int size)
{
	unsigned char two[2];
	int n, i;

	NSEEK(nidfd, 2 + 2 * (id - 1));
	read(nidfd, (char *)two, 2);
	NSEEK(nidfd, two[0] | (two[1] << 8));
	n = read(nidfd, buf, size - 1);
	for (i = 0; i < n; i++)
		if (!buf[i])
			return;
	buf[i] = 0;
}

/*
 * Replace @id - optionally behind the global underscore - with its
 * spelling, in place.  Anything else passes through untouched:
 * synthetics (S%d, L%d, str%d) and plain-mode names never contain
 * '@'.
 */
static void
nidxp(char *buf, int size)
{
	char *p = buf;
	char *q;
	unsigned short id;

	if (*p == '_')
		p++;
	if (*p != '@' || nidfd < 0)
		return;
	id = 0;
	for (q = p + 1; *q >= '0' && *q <= '9'; q++)
		id = id * 10 + (*q - '0');
	nidname(id, p, size - (p - buf));
}

void
out(char *s)
{
	write(outfd, s, strlen(s));
}

void
outc(char c)
{
	write(outfd, &c, 1);
}

/*
 * Negating before converting cannot represent the most negative int,
 * which stayed negative and turned every digit into whatever character
 * sits that far below '0'.  Build the digits from the negative side
 * instead, where every value is representable.
 */
void
outd(int n)
{
	char buf[12];
	register char *p = buf + 11;
	int neg = n < 0;
	if (!neg) n = -n;
	*p = 0;
	do { *--p = '0' - n % 10; n /= 10; } while (n);
	if (neg) *--p = '-';
	out(p);
}

/*
 * Copy pass1's assembly output (globals, string literals, file-scope
 * asm) through to our output, then select .text for the code we are
 * about to generate.  The .2 stream starts in .text and emits its own
 * segment directives as it goes, so it needs no preamble.
 */
/*
 * The id-expanding copy: @id becomes its spelling except inside
 * string data, where an @ followed by digits is just text.  A
 * backslash escapes the next character within quotes.  Output is
 * batched - a write per character is a syscall per character, and
 * under the simulator those are the whole bill.
 */
static char cxbuf[128];
static int cxn;

static void
cxput(char c)
{
	cxbuf[cxn++] = c;
	if (cxn == sizeof(cxbuf)) {
		write(outfd, cxbuf, cxn);
		cxn = 0;
	}
}

static void
copyxp(void)
{
	char buf[64];
	char nam[20];
	char *s;
	int n, i;
	char c;
	char inq = 0, esc = 0;
	char at = 0;		/* digits collected after '@', +1 */
	unsigned short id = 0;

	while ((n = read(in2fd, buf, sizeof(buf))) > 0) {
		for (i = 0; i < n; i++) {
			c = buf[i];
			if (at) {
				if (c >= '0' && c <= '9') {
					id = id * 10 + (c - '0');
					at = 2;
					continue;
				}
				if (at > 1) {	/* a bare '@' is just text */
					nidname(id, nam, sizeof(nam));
					for (s = nam; *s; s++)
						cxput(*s);
				} else {
					cxput('@');
				}
				at = 0;
			}
			if (inq) {
				if (esc)
					esc = 0;
				else if (c == '\\')
					esc = 1;
				else if (c == '"')
					inq = 0;
			} else if (c == '"') {
				inq = 1;
			} else if (c == '@') {
				at = 1;
				id = 0;
				continue;
			}
			cxput(c);
		}
	}
	if (at > 1) {
		nidname(id, nam, sizeof(nam));
		for (s = nam; *s; s++)
			cxput(*s);
	} else if (at) {
		cxput('@');
	}
	if (cxn)
		write(outfd, cxbuf, cxn);
}

void
copyinit(void)
{
	char buf[64];
	int n;

	if (nidfd < 0) {
		while ((n = read(in2fd, buf, sizeof(buf))) > 0)
			write(outfd, buf, n);
	} else {
		copyxp();
	}
	out("\t.text\n");
}

unsigned char
read1(void)
{
	unsigned char c;
	if (pushback >= 0) {
		c = pushback;
		pushback = -1;
	} else {
		c = read(infd, &c, 1) == 1 ? c : E_O_F;
	}
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "read1: %d (0x%02x) '%c'\n", c, c,
			(c >= 32 && c < 127) ? c : '.');
#endif
	return c;
}

void
unread1(unsigned char c)
{
	pushback = c;
}

unsigned short
read2(void)
{
	unsigned char buf[2];
	unsigned short v;
	read(infd, buf, 2);
	v = buf[0] | (buf[1] << 8);
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "read2: %u (0x%04x)\n", v, v);
#endif
	return v;
}

/*
 * The four bytes land in val4 and the function returns nothing.  A
 * long return travels in HL:DE, and the one caller stored it into a
 * local just to push it again; longs are expensive here and rare in
 * the source, so the value goes to memory once and stays there.
 *
 * No shifting either: the file is little-endian by definition -
 * write4 on the other side puts the low byte first - and so is the
 * machine, so the bytes are read to where they belong.
 */
unsigned long val4;

void
read4(void)
{
	read(infd, (char *)&val4, 4);
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "read4: %lu (0x%08lx)\n", val4, val4);
#endif
}

/*
 * Read a counted string.  The size is not optional: the length comes
 * off the file as a byte, so it can say up to 255, and a name that
 * exactly filled its buffer put the terminator one past the end.  What
 * followed became part of the name - a 16-character label came out with
 * a stray byte in the middle of it and the assembler stopped on a
 * symbol nobody wrote.
 */
void
readS(char *buf, int size)
{
	unsigned char len = read1();
	int keep = len < size - 1 ? len : size - 1;
	int over = len - keep;
	char waste;

	read(infd, buf, keep);
	buf[keep] = 0;
	while (over-- > 0)		/* the rest still has to come off */
		read(infd, &waste, 1);
	nidxp(buf, size);
#ifdef DEBUG
	if (VERBOSE(V_IO))
		fprintf(stderr, "readS: \"%s\" (len=%d)\n", buf, len);
#endif
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
