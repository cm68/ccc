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
#include "peep.h"

struct line win[WINDOW];
int nwin = 0;
int verbose = 0;

static FILE *in;
static FILE *out;
static int ateof = 0;

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
dup(char *s)
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
	win[i].text = dup(s);
	win[i].key = dup(kbuf);
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

/* read one line into window slot i; returns 0 at end of file */
int
readline(int i)
{
	char *s;

	if (ateof)
		return 0;
	s = readtext();
	if (!s) {
		ateof = 1;
		return 0;
	}
	freeline(i);
	win[i].text = s;			/* readtext already allocated it */
	{
		char kbuf[KLEN];

		normalise(s, kbuf);
		win[i].key = dup(kbuf);
	}
	win[i].kind = classify(win[i].key);
	return 1;
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
	in->_flag |= _IOBINARY;
	out->_flag |= _IOBINARY;
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
			fputs(mapped, out);
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
