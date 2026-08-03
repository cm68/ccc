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
	while (*src && *src != ';' && *src != '\n' && n < LLEN - 2) {
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

/* read one line into window slot i; returns 0 at end of file */
int
readline(int i)
{
	char buf[LLEN];

	if (ateof)
		return 0;
	if (!fgets(buf, sizeof(buf), in)) {
		ateof = 1;
		return 0;
	}
	strncpy(win[i].text, buf, LLEN - 1);
	win[i].text[LLEN - 1] = '\0';
	normalise(buf, win[i].key);
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

	for (j = i; j + n < nwin; j++) {
		strcpy(win[j].text, win[j + n].text);
		strcpy(win[j].key, win[j + n].key);
		win[j].kind = win[j + n].kind;
	}
	nwin -= n;
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
		strcpy(win[j].text, win[j - 1].text);
		strcpy(win[j].key, win[j - 1].key);
		win[j].kind = win[j - 1].kind;
	}
	strncpy(win[i].text, s, LLEN - 1);
	win[i].text[LLEN - 1] = '\0';
	normalise(s, win[i].key);
	win[i].kind = classify(win[i].key);
	nwin++;
}

void
setline(int i, char *s)
{
	strncpy(win[i].text, s, LLEN - 1);
	win[i].text[LLEN - 1] = '\0';
	normalise(s, win[i].key);
	win[i].kind = classify(win[i].key);
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
			char mapped[LLEN + 32];

			poolmap(win[0].text, mapped, sizeof(mapped));
			fputs(mapped, out);
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
