/*
 * peep.c - peephole optimizer: peep [-v] in.s out.s
 *
 * A window of lines slides through the file.  Rules match at its head;
 * when one fires the window is reconsidered from the top so that a
 * rewrite can expose another, and when none matches the head line is
 * written out and the window shifts by one.
 *
 * It is a window and not the whole file because this has to run on the
 * Z80 eventually, and the largest thing the compiler compiles produces
 * a .s of over two hundred kilobytes.  Nothing here needs more than a
 * dozen lines of context: the longest pattern is a run of eight inc sp
 * and the pop that follows it.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "peep.h"

struct line win[WINDOW];
int nwin = 0;
int verbose = 0;

static FILE *in;
static FILE *out;
static int ateof = 0;

/*
 * Where the output stands, counted here rather than asked of stdio.
 * The frameless patch needs positions; ftell on a buffered write
 * stream is exactly the piece of the target's stdio this work found
 * wanting - it padded a flush out with a block of zeros - and peep is
 * the only writer, so the count is both cheap and beyond argument.
 */
static long outcnt;

/*
 * Squeeze a line down to what the rules match against: no comment, no
 * leading or trailing blanks, and runs of blanks and tabs reduced to
 * one space.  "\tld\thl,0" and "    ld hl,0" are the same instruction
 * and pass2 emits both spellings.
 */
void
normalise(char *src, char *dst)
{
	int sp = 0, n = 0;

	while (*src == ' ' || *src == '\t')
		src++;
	while (*src && *src != ';' && *src != '\n' && n < KLEN - 2) {
		if (*src == ' ' || *src == '\t') {
			sp = 1;
			src++;
			continue;
		}
		if (sp && n)
			dst[n++] = ' ';
		sp = 0;
		dst[n++] = *src++;
	}
	dst[n] = '\0';
}

/*
 * A label is a name at the start of the line ending in a colon; a
 * directive starts with a dot.  Both stop the rules: a label because
 * anything may jump to it, a directive because it is not an
 * instruction and its operands are not registers.
 */
int
classify(char *key)
{
	int i;

	if (!key[0])
		return L_BLANK;
	if (key[0] == '.')
		return L_DIRECT;
	for (i = 0; key[i]; i++) {
		if (key[i] == ':')
			return L_LABEL;
		if (key[i] == ' ')
			break;
	}
	return L_INSN;
}

/*
 * Storage for a slot.  A slot owns its two strings and nothing else
 * points at them, so putting a line in one frees what was there and
 * the window shuffles move the pointers rather than the bytes.
 */
static void
freeline(int i)
{
	if (win[i].text)
		free(win[i].text);
	if (win[i].key)
		free(win[i].key);
	win[i].text = 0;
	win[i].key = 0;
}

static char *
sdup(char *s)
{
	char *p = malloc(strlen(s) + 1);

	if (!p) {
		fprintf(stderr, "peep: out of memory\n");
		exit(1);
	}
	return strcpy(p, s);
}

/*
 * Put text in slot i, deriving the key and the kind.  The slot's
 * previous contents go.  Text is taken as given - the caller either
 * read it or built it - and is not bounded; the key is, by
 * normalise().
 */
static void
putline(int i, char *s)
{
	char kbuf[KLEN];

	freeline(i);
	normalise(s, kbuf);
	win[i].text = sdup(s);
	win[i].key = sdup(kbuf);
	win[i].kind = classify(win[i].key);
}

/*
 * Read one whole line, however long, into a buffer that grows to fit.
 *
 * fgets with a fixed buffer would split the long ones, and a split is
 * poison twice over: the pool's skip loop orphans the tail of a
 * merged literal, and the tail of a split comment no longer begins
 * with a semicolon, so the rules would read it as an instruction and
 * could rewrite it.  c1's expression dumps run past a kilobyte, so
 * this is not hypothetical - the 1024-byte buffer this replaces was
 * already too small for five lines of the compiler's own output.
 */
static char *
readtext(void)
{
	char chunk[128];
	char *p = 0, *q;
	int len = 0, n;

	for (;;) {
		if (!fgets(chunk, sizeof(chunk), in))
			break;
		n = strlen(chunk);
		q = p ? realloc(p, len + n + 1) : malloc(n + 1);
		if (!q) {
			fprintf(stderr, "peep: out of memory\n");
			exit(1);
		}
		p = q;
		strcpy(p + len, chunk);
		len += n;
		if (n && chunk[n - 1] == '\n')
			break;
	}
	return p;			/* 0 at end of file with nothing read */
}

/* read one line into window slot i; returns 0 at end of file.
 *
 * Blank and comment-only lines never enter the window: they go
 * straight to the output here.  The window therefore holds sixteen
 * SIGNIFICANT lines, and every rule sees the same sixteen whether the
 * compiler was built with the DEBUG commentary or without it - which
 * is what keeps the host build and the native build making identical
 * decisions, a thing the selfhost gate checks by the byte.  The cost
 * is that a comment can come out up to a window ahead of the code it
 * sat beside, in DEBUG output only. */
int
readline(int i)
{
	char *s;
	char kbuf[KLEN];

	for (;;) {
		if (ateof)
			return 0;
		s = readtext();
		if (!s) {
			ateof = 1;
			return 0;
		}
		normalise(s, kbuf);
		if (!kbuf[0]) {
			fputs(s, out);
			outcnt += strlen(s);
			free(s);
			continue;
		}
		freeline(i);
		win[i].text = s;		/* readtext already allocated it */
		win[i].key = sdup(kbuf);
		win[i].kind = classify(win[i].key);
		return 1;
	}
}

/* top up the window */
void
fill(void)
{
	while (nwin < WINDOW && readline(nwin))
		nwin++;
}

/* drop n lines starting at i */
void
delline(int i, int n)
{
	int j;

	for (j = i; j < i + n && j < nwin; j++)
		freeline(j);
	for (j = i; j + n < nwin; j++) {
		win[j].text = win[j + n].text;
		win[j].key = win[j + n].key;
		win[j].kind = win[j + n].kind;
	}
	nwin -= n;
	/* the slots that fell off the end no longer own anything */
	for (j = nwin; j < nwin + n && j < WINDOW; j++) {
		win[j].text = 0;
		win[j].key = 0;
	}
}

/*
 * Put s at slot i, pushing the rest down.  Every rule shrinks its
 * match before it inserts, so the window cannot actually overflow -
 * and if one ever did, letting the last line fall off the end would
 * delete an instruction from the program, which is not a thing to do
 * quietly.
 */
void
insline(int i, char *s)
{
	int j;

	if (nwin >= WINDOW) {
		fprintf(stderr, "peep: window overflow - a rule grew its match\n");
		exit(1);
	}
	for (j = nwin; j > i; j--) {
		win[j].text = win[j - 1].text;
		win[j].key = win[j - 1].key;
		win[j].kind = win[j - 1].kind;
	}
	win[i].text = 0;			/* the slot's old strings moved up */
	win[i].key = 0;
	putline(i, s);
	nwin++;
}


/*
 * The frame that stops being necessary.
 *
 * A function whose first argument arrives in HL may need no frame at
 * all: once r_hlarg has deleted the reloads, "f(x) { return x + 1; }"
 * is call fenterw / inc hl / jp fexitw, and the frame work is pure
 * ceremony - nothing in the body reaches through IY.  The projection
 * counted 102 one-parameter functions in that shape; their
 * no-parameter cousins whose only frame use is the BC save are the
 * same case wearing fentb, and the tree's frame-free functions run
 * past a hundred significant lines, so no window holds one.
 *
 * No window has to.  The entry line is c1 saying everything it knows
 * - fenterw means no saves and no scalar area, ".dw 0" after
 * fentbw/fentxw/fentbxw means saves but no scalars - and the rest is
 * a fact about the text between entry and exit, which streams through
 * here one line at a time anyway.  So the entry is written normally
 * and its file position remembered; every line on the way out feeds a
 * dirt bit; and a clean exit writes pops and a ret in place of the
 * exit helper, then seeks back and overwrites the entry with pushes
 * padded to exactly the bytes the call and its word occupied - the
 * assembler does not care about trailing blanks.  One held line of
 * state, no buffering that scales with anything, and the same
 * decision on the host and on the target because it is made from the
 * normalised keys, which the DEBUG commentary never reaches.
 *
 * The dirt test is deliberately broad: "iy" catches (iy+d), qldiy and
 * the frame pointer itself; "sp" catches the rest-of-frame
 * allocation, ex (sp),hl, and an asm() body doing anything stackish.
 * At worst a symbol with sp in its name costs a candidate, never
 * correctness.  Register saves become plain pushes and pops - the
 * body is SP-balanced, the same guarantee the no-frame functions
 * pass2 already emits rely on - and the q entries are left alone: a
 * long first argument spills two words and earns its frame.
 */
extern long n_noframe;		/* the counter lives with the others */
extern long saved;

/* does s contain the letter pair a,b anywhere - the target libc has
 * no strstr, and two letters are all the dirt test needs */
static int
has2(char *s, char a, char b)
{
	for (; s[0] && s[1]; s++)
		if (s[0] == a && s[1] == b)
			return 1;
	return 0;
}

/*
 * The DEBUG commentary makes the patching two-part.  Comments bypass
 * the window at read time while significant lines drain behind them,
 * so in the OUTPUT a comment from sixteen lines further on can sit
 * between the entry call and its .dw even though c1 wrote them with
 * one outf.  Each of the two entry lines is therefore patched as its
 * own region, at the position recorded when that line was written:
 * the pushes go over the call, a blank line goes over the word, and
 * whatever landed between them is not touched.  The bx entries would
 * need two pushes in the space of one call line, which do not fit;
 * they were worth two bytes and keep their frames.
 */
static struct nf {
	char *entry;		/* the entry call's key */
	char entl;		/* its line count: 1, or 2 with ".dw 0" */
	char *exit;		/* the exit's key */
	char extl;		/* its line count: 1, or 2 with a .dw */
	char *exitdw;		/* the exit .dw key that must match */
	char *push;		/* what replaces the entry call, or 0 */
	char *pop;		/* what replaces the exit */
	char nsaved;		/* text bytes the rewrite buys */
} nftab[] = {
	{ "call fenterw", 1, "jp fexitw", 1, 0,
	  0, "\tret\n", 5 },
	{ "call fenter", 1, "jp fexit", 1, 0,
	  0, "\tret\n", 5 },
	{ "call fentbw", 2, "call fexbw", 2, ".dw -2",
	  "\tpush\tbc\n", "\tpop\tbc\n\tret\n", 7 },
	{ "call fentb", 2, "call fexb", 2, ".dw -2",
	  "\tpush\tbc\n", "\tpop\tbc\n\tret\n", 7 },
	{ "call fentxw", 2, "call fexxw", 2, ".dw -2",
	  "\tpush\tix\n", "\tpop\tix\n\tret\n", 5 },
	{ "call fentx", 2, "call fexx", 2, ".dw -2",
	  "\tpush\tix\n", "\tpop\tix\n\tret\n", 5 },
	{ 0, 0, 0, 0, 0, 0, 0, 0 }
};

#define NFPAT	64		/* a line longer than this is left alone */

static void
outs(char *s)
{
	fputs(s, out);
	outcnt += strlen(s);
}

static struct nf *nfent;	/* the armed entry's table row */
static long nfpos1, nfpos2;	/* where its two lines begin */
static int nflen1, nflen2;	/* and their lengths as written */
static int nfdirty;
static int nfarm;		/* 0 idle, 1 waiting for ".dw 0", 2 armed */
static char *nfheld;		/* an exit call held for its .dw */

/* overwrite the region at pos/len, in place and to the byte, with
 * body (or nothing) padded out with blanks ahead of the newline */
static void
nfover(long pos, int len, char *body)
{
	char buf[NFPAT + 2];
	int n, p;

	n = body ? strlen(body) : 0;
	p = n ? n - 1 : 0;		/* the newline moves to the end */
	if (n)
		memcpy(buf, body, p);
	while (p < len - 1)
		buf[p++] = ' ';
	buf[p++] = '\n';
	buf[p] = '\0';

	/*
	 * Through the file descriptor, not the stream: flush what is
	 * buffered, drop the patch in with lseek and write, and put the
	 * offset back where the count says the end is.  The bytes
	 * overwritten are bytes already flushed or just flushed, and
	 * stdio never learns the offset moved.
	 */
	fflush(out);
	lseek(fileno(out), pos, 0);
	write(fileno(out), buf, p);
	lseek(fileno(out), outcnt, 0);
}

static void
nfpatch(void)
{
	nfover(nfpos1, nflen1, nfent->push);
	if (nfent->entl == 2)
		nfover(nfpos2, nflen2, 0);
	n_noframe++;
	saved += nfent->nsaved;
	nfarm = 0;
}

/* every significant line leaves through here */
static void
nfemit(char *text, char *key)
{
	struct nf *t;

	if (nfheld) {
		if (strcmp(key, nfent->exitdw) == 0) {
			outs(nfent->pop);
			free(nfheld);
			nfheld = 0;
			nfpatch();
			return;		/* the exit and its word are gone */
		}
		outs(nfheld);		/* not our exit after all */
		free(nfheld);
		nfheld = 0;
		nfarm = 0;
		outs(text);
		return;
	}

	if (nfarm == 1) {
		if (strcmp(key, ".dw 0") == 0 &&
		    strlen(text) <= NFPAT) {
			nfpos2 = outcnt;
			nflen2 = strlen(text);
			nfarm = 2;
		} else
			nfarm = 0;	/* a real scalar area */
		outs(text);
		return;
	}

	for (t = nftab; t->entry; t++)
		if (strcmp(key, t->entry) == 0)
			break;
	if (t->entry) {
		nfent = t;
		nfpos1 = outcnt;
		nflen1 = strlen(text);
		nfdirty = 0;
		nfarm = 0;
		if (nflen1 <= NFPAT &&
		    (!t->push || (int)strlen(t->push) <= nflen1))
			nfarm = t->entl == 2 ? 1 : 2;
		outs(text);
		return;
	}

	if (nfarm == 2) {
		if (!nfdirty && strcmp(key, nfent->exit) == 0) {
			if (nfent->extl == 2) {
				nfheld = sdup(text);
				return;		/* the .dw decides */
			}
			outs(nfent->pop);
			nfpatch();
			return;
		}
		if (has2(key, 'i', 'y') || has2(key, 's', 'p') ||
		    strncmp(key, "call fe", 7) == 0 ||
		    strncmp(key, "jp fexit", 8) == 0)
			nfdirty = 1;
	}
	outs(text);
}

void
usage(void)
{
	fprintf(stderr, "usage: peep [-v] input.s output.s\n");
	exit(1);
}

int
main(int argc, char **argv)
{
	int i;
	char *inf = 0, *outf = 0;

	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-v") == 0)
			verbose = 1;
		else if (!inf)
			inf = argv[i];
		else if (!outf)
			outf = argv[i];
		else
			usage();
	}
	if (!inf || !outf)
		usage();

	in = fopen(inf, "r");
	if (!in) {
		fprintf(stderr, "peep: cannot open %s\n", inf);
		exit(1);
	}
	out = fopen(outf, "w");
	if (!out) {
		fprintf(stderr, "peep: cannot create %s\n", outf);
		exit(1);
	}
#ifdef CCC
	/*
	 * The target's stdio has CP/M text mode in it: a \r slipped in
	 * ahead of every \n on the way out, \r eaten and ctrl-Z taken
	 * for the end on the way in.  This is a byte-for-byte rewrite
	 * of an LF-only file - what comes out must be what went in,
	 * less what the rules removed - so both streams run binary.
	 * The host's stdio has no such mode and no such flag.
	 */
	/*
	 * _IOBINARY is gone: nothing ever tested it, and its bit is
	 * _IORW now - setting it here would tell fseek these streams
	 * were read-write when they are not.
	 */
#endif

	/*
	 * Before the window starts: find the literal blocks that spell
	 * the same bytes and elect survivors.  The window then drops the
	 * losers and rewrites every reference on the way out.
	 */
	poolscan(in);

	fill();
	while (nwin > 0) {
		if (poolskip(win[0].key)) {
			/* a merged literal: the label and its data go */
			delline(0, 1);
			fill();
			while (nwin > 0 && pooldata(win[0].text)) {
				delline(0, 1);
				fill();
			}
			continue;
		}
		if (!applyrules()) {
			/*
			 * Sized to the line in hand: poolmap can only grow it
			 * by the digits a remapped strN gains, and it stops
			 * short of the end it is given.
			 */
			int sz = strlen(win[0].text) + 64;
			char *mapped = malloc(sz);

			if (!mapped) {
				fprintf(stderr, "peep: out of memory\n");
				exit(1);
			}
			poolmap(win[0].text, mapped, sz);
			nfemit(mapped, win[0].key);
			free(mapped);
			delline(0, 1);
		}
		fill();
	}

	fclose(in);
	if (fclose(out)) {
		fprintf(stderr, "peep: write failed on %s\n", outf);
		exit(1);
	}
	if (verbose)
		report();
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
