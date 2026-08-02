/*
 * peep.h - peephole optimizer for ccc
 *
 * Reads a .s file and writes a .s file.  Nothing else in the compiler
 * knows it exists; the driver runs it between c1 and asz when -O is
 * given, and the output is the same assembly with cheaper encodings
 * substituted for sequences that pass2's rules table cannot see past.
 *
 * pass2 emits one rule at a time and cannot know what the next rule
 * will do, so a value it materialises for a push is materialised again
 * for the next one, and stack cleanup after a call is counted out one
 * byte at a time.  Those are the patterns here.
 */

#define WINDOW	16			/* lines held for matching */
/*
 * Long enough for the longest line c1 emits, which is a .db carrying
 * a whole rule template - 274 bytes in today's rules.s.  A line
 * fgets splits in two is poison twice over: the pool's skip loop
 * orphans the tail of a merged literal, and a rule could match
 * across the seam.
 */
#define LLEN	1024			/* longest line handled - see asz linebuf */

/*
 * A window line.  The original text is kept so that lines nothing
 * matched come out byte for byte as they went in - the assembler does
 * not care, but a diff of two .s files is how this gets debugged, and
 * gratuitous reformatting would bury the real changes.  The key is the
 * same line with tabs and runs of blanks squeezed to one space and the
 * comment removed, which is what the rules match against.
 */
struct line {
	char text[LLEN];		/* as read, less the newline */
	char key[LLEN];			/* normalised for matching */
	unsigned char kind;		/* L_* below */
};

#define L_INSN		0		/* an instruction */
#define L_LABEL		1		/* a label definition */
#define L_DIRECT	2		/* .db, .text, and the like */
#define L_BLANK		3		/* blank or comment-only */

/*
 * Registers, as a bitmask so that one call can ask about a pair.  A
 * and the flags are separate because most of what is worth proving
 * here is "is A still wanted", and the answer usually turns on an
 * instruction that writes A without reading it.
 */
#define R_A		0x01
#define R_B		0x02
#define R_C		0x04
#define R_D		0x08
#define R_E		0x10
#define R_H		0x20
#define R_L		0x40
#define R_IX	0x80
#define R_IY	0x100
#define R_F		0x200		/* the flags */

#define R_BC	(R_B | R_C)
#define R_DE	(R_D | R_E)
#define R_HL	(R_H | R_L)

/* what a call may destroy: everything not callee saved */
#define R_CALL	(R_A | R_BC | R_DE | R_HL | R_F)

/* peep.c */
extern struct line win[WINDOW];
extern int nwin;					/* lines currently in the window */
extern int verbose;
extern void delline(int i, int n);
extern void setline(int i, char *s);
extern void insline(int i, char *s);

/* regs.c */
extern int reads(char *insn);
extern int writes(char *insn);
extern int isdead(int reg, int from);
extern int isbranch(char *insn);
extern int memok(char *p);

/* rules.c */
extern int applyrules(void);
extern void report(void);

/* pool.c */
#include <stdio.h>
extern void poolscan(FILE *f);
extern int poolskip(char *line);
extern int pooldata(char *line);
extern void poolmap(char *line, char *out, int outsz);
extern long poolmerged;

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
