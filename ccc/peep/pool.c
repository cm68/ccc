/*
 * pool.c - merge identical string literals
 *
 * c1 gives every string literal its own strN label and never looks
 * back, so a file that spells the same template twice carries it
 * twice: rules.c was carrying 270 duplicates, twenty-three hundred
 * bytes of them.  These labels are the compiler's own literal pool -
 * never written, file-local, and C says identical literals may share
 * storage - so merging them is not an optimization of the program,
 * just of the spelling.
 *
 * Two passes.  The scan reads the whole file collecting each strN
 * block's position and a hash of its data lines; a hash match is
 * confirmed by re-reading both spans, so a collision can slow the
 * scan but never merge two different strings.  The main pass then
 * drops the duplicate blocks and rewrites references - outside
 * quotes only, because the corpus itself contains "str%d".
 *
 * The tables are sized for the worst file in the tree with room to
 * spare, and small enough that the Z80 build can afford them: six
 * bytes a string plus the remap words.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "peep.h"

#define POOLMAX	3072		/* rules.c holds ~1300 today */

static long spos[POOLMAX];		/* file offset of the block's data */
static unsigned short shash[POOLMAX];	/* quick signature */
static short slen[POOLMAX];		/* data lines in the block */
static short smap[POOLMAX];		/* survivor this id resolves to */
static int nstr;			/* highest id seen + 1 */

static int poolon;			/* tables valid */

long poolsaved;
long poolmerged;

/* the id when the line is exactly "strN:", else -1 */
static int
strlabel(char *line)
{
	int id = 0;
	char *p = line;

	if (p[0] != 's' || p[1] != 't' || p[2] != 'r')
		return -1;
	p += 3;
	if (*p < '0' || *p > '9')
		return -1;
	while (*p >= '0' && *p <= '9')
		id = id * 10 + (*p++ - '0');
	if (p[0] != ':' || (p[1] != '\n' && p[1] != '\0'))
		return -1;
	return id;
}

/* a data line belonging to a literal block: whitespace then .db */
static int
isdata(char *line)
{
	while (*line == ' ' || *line == '\t')
		line++;
	return line[0] == '.' && line[1] == 'd' && line[2] == 'b';
}

/*
 * Are two blocks byte-identical?  Re-read both spans and compare the
 * raw text.  The hash brought us here; this is what makes a merge a
 * fact rather than a probability.
 */
static int
sameblock(FILE *f, int a, int b)
{
	char la[LLEN], lb[LLEN];
	long save = ftell(f);
	long pa = spos[a], pb = spos[b];
	int i, same = 1;

	if (slen[a] != slen[b])
		return 0;
	for (i = 0; i < slen[a]; i++) {
		fseek(f, pa, 0);
		if (!fgets(la, sizeof(la), f)) { same = 0; break; }
		pa = ftell(f);
		fseek(f, pb, 0);
		if (!fgets(lb, sizeof(lb), f)) { same = 0; break; }
		pb = ftell(f);
		if (strcmp(la, lb) != 0) { same = 0; break; }
	}
	fseek(f, save, 0);
	return same;
}

/*
 * Scan the file, fill the tables, decide the survivors.  Called once
 * before the window starts; leaves the stream rewound.
 */
void
poolscan(FILE *f)
{
	char line[LLEN];
	int id, i, j, n;
	long at;
	unsigned short h;
	char *p;

	nstr = 0;
	poolon = 0;
	for (i = 0; i < POOLMAX; i++)
		smap[i] = -1;

	id = -1;
	while ((at = ftell(f)), fgets(line, sizeof(line), f)) {
		i = strlabel(line);
		if (i >= 0) {
			if (i >= POOLMAX)
				return;		/* too rich for the table: no pooling */
			id = i;
			if (id >= nstr)
				nstr = id + 1;
			spos[id] = -1;
			slen[id] = 0;
			shash[id] = 0;
			continue;
		}
		if (id >= 0 && isdata(line)) {
			if (spos[id] < 0)
				spos[id] = at;
			h = shash[id];
			for (p = line; *p; p++)
				h = (h << 3) + (h >> 13) + (unsigned char)*p;
			shash[id] = h;
			slen[id]++;
		} else {
			id = -1;
		}
	}

	/* survivors: first block with each content keeps its label */
	n = 0;
	for (i = 0; i < nstr; i++) {
		if (slen[i] == 0)
			continue;
		smap[i] = i;
		for (j = 0; j < i; j++) {
			if (smap[j] != j || slen[j] == 0)
				continue;
			if (shash[i] == shash[j] && sameblock(f, i, j)) {
				smap[i] = j;
				poolmerged++;
				n++;
				break;
			}
		}
	}
	rewind(f);
	if (n > 0)
		poolon = 1;
}

/* is this line the label of a block that merged away? */
int
poolskip(char *line)
{
	int id;

	if (!poolon)
		return 0;
	id = strlabel(line);
	if (id < 0 || id >= nstr)
		return 0;
	return smap[id] >= 0 && smap[id] != id;
}

/* while skipping: the block's own data lines go too */
int
pooldata(char *line)
{
	return isdata(line);
}

/*
 * Rewrite strN references in an output line to their survivors.
 * Quoted spans pass through untouched - the corpus itself contains
 * "str%d" - and only a whole token strN is a reference: str5x is
 * somebody's identifier.
 */
void
poolmap(char *line, char *out, int outsz)
{
	char *d = out, *end = out + outsz - 16;
	char *p = line;
	int q = 0, id;
	char *tail;

	if (!poolon) {
		strncpy(out, line, outsz - 1);
		out[outsz - 1] = '\0';
		return;
	}
	while (*p && d < end) {
		if (*p == '"')
			q = !q;
		if (!q && p[0] == 's' && p[1] == 't' && p[2] == 'r' &&
		    p[3] >= '0' && p[3] <= '9' &&
		    (p == line || !(p[-1] == '_' ||
		     (p[-1] >= 'a' && p[-1] <= 'z') ||
		     (p[-1] >= 'A' && p[-1] <= 'Z') ||
		     (p[-1] >= '0' && p[-1] <= '9')))) {
			id = 0;
			tail = p + 3;
			while (*tail >= '0' && *tail <= '9')
				id = id * 10 + (*tail++ - '0');
			if (id < nstr && smap[id] >= 0 && smap[id] != id)
				id = smap[id];
			d += sprintf(d, "str%d", id);
			p = tail;
			continue;
		}
		*d++ = *p++;
	}
	*d = '\0';
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
