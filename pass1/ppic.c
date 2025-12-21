/*
 * ppic - Pretty Print Intermediate Code
 *
 * Reads .1 and .2 files from c0 and produces human-readable output.
 *
 * Usage: ppic base
 *   Reads base.1 and base.2, writes base.pp
 *
 * Format of .1/.2 files:
 *   B (opcode): byte followed by 0xfe marker
 *   N (number): 16-bit little-endian
 *   S (symbol): '_' prefix + null-terminated string
 *   F (float):  null-terminated string
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Opcodes from c0.h */
char *opnames[] = {
	"EOFC",		/* 0 */
	"SEMI",		/* 1 */
	"LBRACE",	/* 2 */
	"RBRACE",	/* 3 */
	"LBRACK",	/* 4 */
	"RBRACK",	/* 5 */
	"LPARN",	/* 6 */
	"RPARN",	/* 7 */
	"COLON",	/* 8 */
	"COMMA",	/* 9 */
	"FSEL",		/* 10 */
	"CAST",		/* 11 */
	"ETYPE",	/* 12 */
	"ITOP",		/* 13 */
	"PTOI",		/* 14 */
	"LTOP",		/* 15 */
	0, 0, 0,	/* 16-18 */
	"KEYW",		/* 19 */
	"NAME",		/* 20 */
	"CON",		/* 21 */
	"STRING",	/* 22 */
	"FCON",		/* 23 */
	"SFCON",	/* 24 */
	"LCON",		/* 25 */
	"SLCON",	/* 26 */
	0, 0,		/* 27-28 */
	"NULLOP",	/* 29 */
	"INCBEF",	/* 30 */
	"DECBEF",	/* 31 */
	"INCAFT",	/* 32 */
	"DECAFT",	/* 33 */
	"EXCLA",	/* 34 */
	"AMPER",	/* 35 */
	"STAR",		/* 36 */
	"NEG",		/* 37 */
	"COMPL",	/* 38 */
	"DOT",		/* 39 */
	"PLUS",		/* 40 */
	"MINUS",	/* 41 */
	"TIMES",	/* 42 */
	"DIVIDE",	/* 43 */
	"MOD",		/* 44 */
	"RSHIFT",	/* 45 */
	"LSHIFT",	/* 46 */
	"AND",		/* 47 */
	"OR",		/* 48 */
	"EXOR",		/* 49 */
	"ARROW",	/* 50 */
	"ITOF",		/* 51 */
	"FTOI",		/* 52 */
	"LOGAND",	/* 53 */
	"LOGOR",	/* 54 */
	0,		/* 55 */
	"FTOL",		/* 56 */
	"LTOF",		/* 57 */
	"ITOL",		/* 58 */
	"LTOI",		/* 59 */
	"EQUAL",	/* 60 */
	"NEQUAL",	/* 61 */
	"LESSEQ",	/* 62 */
	"LESS",		/* 63 */
	"GREATEQ",	/* 64 */
	"GREAT",	/* 65 */
	"LESSEQP",	/* 66 */
	"LESSP",	/* 67 */
	"GREATQP",	/* 68 */
	"GREATP",	/* 69 */
	"ASPLUS",	/* 70 */
	"ASMINUS",	/* 71 */
	"ASTIMES",	/* 72 */
	"ASDIV",	/* 73 */
	"ASMOD",	/* 74 */
	"ASRSH",	/* 75 */
	"ASLSH",	/* 76 */
	"ASSAND",	/* 77 */
	"ASOR",		/* 78 */
	"ASXOR",	/* 79 */
	"ASSIGN",	/* 80 */
};

#define MAXOP 80

/* Special opcodes 90+ */
char *getop(int op)
{
	static char buf[32];

	if (op <= MAXOP && opnames[op])
		return opnames[op];

	switch (op) {
	case 90: return "QUEST";
	case 91: return "SIZEOF";
	case 93: return "MAX";
	case 94: return "MAXP";
	case 95: return "MIN";
	case 96: return "MINP";
	case 97: return "SEQNC";
	case 100: return "CALL";
	case 101: return "MCALL";
	case 102: return "JUMP";
	case 103: return "CBRANCH";
	case 104: return "INIT";
	case 105: return "SETREG";
	case 109: return "ITOC";
	case 110: return "RFORCE";
	case 111: return "BRANCH";
	case 112: return "LABEL";
	case 113: return "NLABEL";
	case 114: return "RLABEL";
	case 115: return "STRASG";
	case 200: return "BDATA";
	case 201: return "WDATA";
	case 202: return "PROG";
	case 203: return "DATA";
	case 204: return "BSS";
	case 205: return "CSPACE";
	case 206: return "SSPACE";
	case 207: return "SYMDEF";
	case 208: return "SAVE";
	case 209: return "RETRN";
	case 210: return "EVEN";
	case 212: return "PROFIL";
	case 213: return "SWIT";
	case 214: return "EXPR";
	case 215: return "SNAME";
	case 216: return "RNAME";
	case 217: return "ANAME";
	case 218: return "XNULLOP";
	case 219: return "SETSTK";
	case 220: return "SINIT";
	case 223: return "ASSEM";
	default:
		sprintf(buf, "OP%d", op);
		return buf;
	}
}

/* Storage classes */
char *getclass(int c)
{
	switch (c) {
	case 9: return "TYPEDEF";
	case 10: return "MOS";
	case 11: return "AUTO";
	case 12: return "EXTERN";
	case 13: return "STATIC";
	case 14: return "REG";
	case 15: return "STRTAG";
	case 16: return "ARG";
	case 17: return "ARG1";
	case 18: return "AREG";
	case 20: return "DEFXTRN";
	case 21: return "MOU";
	case 22: return "ENUMTAG";
	case 24: return "ENUMCON";
	default: return NULL;
	}
}

/* Type names */
char *gettypebase(int t)
{
	switch (t & 017) {
	case 0: return "int";
	case 1: return "char";
	case 2: return "float";
	case 3: return "double";
	case 4: return "struct";
	case 6: return "long";
	case 7: return "unsigned";
	case 8: return "uchar";
	case 9: return "ulong";
	case 10: return "void";
	default: return "?";
	}
}

void printtype(FILE *out, int t)
{
	int xt;
	/* Print base type */
	fprintf(out, "%s", gettypebase(t));
	/* Print modifiers */
	t >>= 4;
	while (t) {
		xt = t & 3;
		switch (xt) {
		case 1: fprintf(out, "*"); break;
		case 2: fprintf(out, "()"); break;
		case 3: fprintf(out, "[]"); break;
		}
		t >>= 2;
	}
}

int getbyte(FILE *f)
{
	int c = fgetc(f);
	return (c == EOF) ? -1 : c;
}

int getword(FILE *f)
{
	int lo = getbyte(f);
	int hi = getbyte(f);
	if (lo < 0 || hi < 0) return -1;
	return lo | (hi << 8);
}

/* Read null-terminated string */
int getstr(FILE *f, char *buf, int max)
{
	int c, i = 0;
	while ((c = getbyte(f)) > 0 && i < max - 1)
		buf[i++] = c;
	buf[i] = 0;
	return i;
}

void ppfile(FILE *in, FILE *out)
{
	int c, op, n, n2, n3, type, class;
	char buf[256];

	while ((c = getbyte(in)) >= 0) {
		op = c;
		/* Consume 0xfe marker after opcode */
		if (getbyte(in) != 0xfe) {
			fprintf(out, "ERROR: expected 0xfe after opcode %d\n", op);
			return;
		}
		fprintf(out, "%-10s", getop(op));

		switch (op) {
		case 20: /* NAME */
			class = getword(in);
			type = getword(in);
			fprintf(out, " ");
			if (getclass(class))
				fprintf(out, "%s ", getclass(class));
			else
				fprintf(out, "class=%d ", class);
			printtype(out, type);
			/* Check if followed by string or number */
			c = getbyte(in);
			if (c == '_') {
				getstr(in, buf, sizeof(buf));
				fprintf(out, " _%s", buf);
			} else if (c >= 0) {
				/* offset - put back and read as word */
				n = c | (getbyte(in) << 8);
				fprintf(out, " offset=%d", (short)n);
			}
			break;

		case 21: /* CON */
			type = getword(in);
			n = getword(in);
			printtype(out, type);
			fprintf(out, " %d", (short)n);
			break;

		case 23: /* FCON */
			type = getword(in);
			getstr(in, buf, sizeof(buf));
			printtype(out, type);
			fprintf(out, " %s", buf);
			break;

		case 25: /* LCON */
		case 26: /* SLCON */
			type = getword(in);
			n = getword(in);
			n2 = getword(in);
			printtype(out, type);
			fprintf(out, " %ld", ((long)n2 << 16) | (n & 0xffff));
			break;

		case 103: /* CBRANCH */
			n = getword(in);
			n2 = getword(in);
			n3 = getword(in);
			fprintf(out, " L%d cond=%d line=%d", n, n2, n3);
			break;

		case 105: /* SETREG */
		case 111: /* BRANCH */
		case 112: /* LABEL */
			n = getword(in);
			fprintf(out, " %d", n);
			break;

		case 113: /* NLABEL */
		case 114: /* RLABEL */
			getstr(in, buf, sizeof(buf));
			fprintf(out, " %s", buf);
			break;

		case 115: /* STRASG */
			type = getword(in);
			n = getword(in);
			fprintf(out, " size=%d", n);
			break;

		case 200: /* BDATA */
			/* BDATA has no immediate argument, but is followed by
			 * pairs of words: (flag, value) where flag=1 means
			 * data byte, flag=0 means end of data sequence */
			fprintf(out, " \"");
			while ((n = getword(in)) == 1) {
				n2 = getword(in);
				if (n2 >= 32 && n2 < 127 && n2 != '"' && n2 != '\\')
					fprintf(out, "%c", n2);
				else
					fprintf(out, "\\x%02x", n2);
			}
			fprintf(out, "\"");
			/* n==0 means end of data, we already consumed it */
			break;

		case 205: /* CSPACE */
			getstr(in, buf, sizeof(buf));
			n = getword(in);
			fprintf(out, " %s %d", buf, n);
			break;

		case 206: /* SSPACE */
			n = getword(in);
			fprintf(out, " %d", n);
			break;

		case 207: /* SYMDEF */
			getstr(in, buf, sizeof(buf));
			fprintf(out, " %s", buf);
			break;

		case 215: /* SNAME */
		case 216: /* RNAME */
		case 217: /* ANAME */
			getstr(in, buf, sizeof(buf));
			n = getword(in);
			fprintf(out, " %s offset=%d", buf, n);
			break;

		case 214: /* EXPR */
			n = getword(in);
			fprintf(out, " line=%d", n);
			break;

		case 219: /* SETSTK */
			n = getword(in);
			fprintf(out, " %d", (short)n);
			break;

		case 220: /* SINIT */
			n = getword(in);
			fprintf(out, " %d", n);
			break;

		/* Operators with just type */
		case 9: /* COMMA */
		case 29: case 30: case 31: case 32: case 33:
		case 34: case 35: case 36: case 37: case 38:
		case 40: case 41: case 42: case 43: case 44:
		case 45: case 46: case 47: case 48: case 49:
		case 51: case 52: case 56: case 57: case 58: case 59:
		case 60: case 61: case 62: case 63: case 64: case 65:
		case 66: case 67: case 68: case 69:
		case 70: case 71: case 72: case 73: case 74:
		case 75: case 76: case 77: case 78: case 79: case 80:
		case 90: case 97: case 100: case 109: case 110:
			type = getword(in);
			fprintf(out, " ");
			printtype(out, type);
			break;

		/* No arguments */
		case 202: case 203: case 204: case 208: case 209:
		case 210: case 218:
			break;

		default:
			/* Try to read a type if it looks like an operator */
			if (op >= 10 && op < 120) {
				type = getword(in);
				fprintf(out, " ");
				printtype(out, type);
			}
			break;
		}

		fprintf(out, "\n");
	}
}

int
main(int argc, char **argv)
{
	char f1[256], f2[256], fpp[256];
	FILE *in1, *in2, *out;

	if (argc != 2) {
		fprintf(stderr, "Usage: ppic base\n");
		fprintf(stderr, "  Reads base.1 and base.2, writes base.pp\n");
		return 1;
	}

	sprintf(f1, "%s.1", argv[1]);
	sprintf(f2, "%s.2", argv[1]);
	sprintf(fpp, "%s.pp", argv[1]);

	if ((in1 = fopen(f1, "rb")) == NULL) {
		perror(f1);
		return 1;
	}

	if ((out = fopen(fpp, "w")) == NULL) {
		perror(fpp);
		return 1;
	}

	fprintf(out, "=== %s ===\n", f1);
	ppfile(in1, out);
	fclose(in1);

	if ((in2 = fopen(f2, "rb")) != NULL) {
		fprintf(out, "\n=== %s ===\n", f2);
		ppfile(in2, out);
		fclose(in2);
	}

	fclose(out);
	printf("Wrote %s\n", fpp);
	return 0;
}
