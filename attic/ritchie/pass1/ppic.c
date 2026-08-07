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
 *   F (float):  null-terminated string (up to 1000 chars)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Opcodes from c0.h
 */
char *opnames[] = {
	"EOFC",						/* 0 */
	"SEMI",						/* 1 */
	"LBRACE",					/* 2 */
	"RBRACE",					/* 3 */
	"LBRACK",					/* 4 */
	"RBRACK",					/* 5 */
	"LPARN",					/* 6 */
	"RPARN",					/* 7 */
	"COLON",					/* 8 */
	"COMMA",					/* 9 */
	"FSEL",						/* 10 */
	"CAST",						/* 11 */
	"ETYPE",					/* 12 */
	"ITOP",						/* 13 */
	"PTOI",						/* 14 */
	"LTOP",						/* 15 */
	0, 0, 0,					/* 16-18 */
	"KEYW",						/* 19 */
	"NAME",						/* 20 */
	"CON",						/* 21 */
	"STRING",					/* 22 */
	"FCON",						/* 23 */
	"SFCON",					/* 24 */
	"LCON",						/* 25 */
	"SLCON",					/* 26 */
	0, 0,						/* 27-28 */
	"NULLOP",					/* 29 */
	"INCBEF",					/* 30 */
	"DECBEF",					/* 31 */
	"INCAFT",					/* 32 */
	"DECAFT",					/* 33 */
	"EXCLA",					/* 34 */
	"AMPER",					/* 35 */
	"STAR",						/* 36 */
	"NEG",						/* 37 */
	"COMPL",					/* 38 */
	"DOT",						/* 39 */
	"PLUS",						/* 40 */
	"MINUS",					/* 41 */
	"TIMES",					/* 42 */
	"DIVIDE",					/* 43 */
	"MOD",						/* 44 */
	"RSHIFT",					/* 45 */
	"LSHIFT",					/* 46 */
	"AND",						/* 47 */
	"OR",						/* 48 */
	"EXOR",						/* 49 */
	"ARROW",					/* 50 */
	"ITOF",						/* 51 */
	"FTOI",						/* 52 */
	"LOGAND",					/* 53 */
	"LOGOR",					/* 54 */
	0,							/* 55 */
	"FTOL",						/* 56 */
	"LTOF",						/* 57 */
	"ITOL",						/* 58 */
	"LTOI",						/* 59 */
	"EQUAL",					/* 60 */
	"NEQUAL",					/* 61 */
	"LESSEQ",					/* 62 */
	"LESS",						/* 63 */
	"GREATEQ",					/* 64 */
	"GREAT",					/* 65 */
	"LESSEQP",					/* 66 */
	"LESSP",					/* 67 */
	"GREATQP",					/* 68 */
	"GREATP",					/* 69 */
	"ASPLUS",					/* 70 */
	"ASMINUS",					/* 71 */
	"ASTIMES",					/* 72 */
	"ASDIV",					/* 73 */
	"ASMOD",					/* 74 */
	"ASRSH",					/* 75 */
	"ASLSH",					/* 76 */
	"ASSAND",					/* 77 */
	"ASOR",						/* 78 */
	"ASXOR",					/* 79 */
	"ASSIGN",					/* 80 */
};

#define MAXOP 80

/*
 * Special opcodes 90+
 */
char *
getop(int op)
{
	static char buf[32];

	if (op <= MAXOP && opnames[op])
		return opnames[op];

	switch (op) {
	case 90:
		return "QUEST";
	case 91:
		return "SIZEOF";
	case 97:
		return "SEQNC";
	case 100:
		return "CALL";
	case 101:
		return "MCALL";
	case 102:
		return "JUMP";
	case 103:
		return "CBRANCH";
	case 104:
		return "INIT";
	case 105:
		return "SETREG";
	case 109:
		return "ITOC";
	case 110:
		return "RFORCE";
	case 111:
		return "BRANCH";
	case 112:
		return "LABEL";
	case 113:
		return "NLABEL";
	case 114:
		return "RLABEL";
	case 115:
		return "STRASG";
	case 116:
		return "LINENO";
	case 117:
		return "NEWLINE";
	case 200:
		return "BDATA";
	case 201:
		return "WDATA";
	case 202:
		return "PROG";
	case 203:
		return "DATA";
	case 204:
		return "BSS";
	case 205:
		return "CSPACE";
	case 206:
		return "SSPACE";
	case 207:
		return "SYMDEF";
	case 208:
		return "SAVE";
	case 209:
		return "RETRN";
	case 210:
		return "EVEN";
	case 212:
		return "PROFIL";
	case 213:
		return "SWIT";
	case 214:
		return "EXPR";
	case 215:
		return "SNAME";
	case 216:
		return "RNAME";
	case 217:
		return "ANAME";
	case 218:
		return "XNULLOP";
	case 219:
		return "SETSTK";
	case 220:
		return "SINIT";
	case 223:
		return "ASSEM";
	default:
		sprintf(buf, "OP%d", op);
		return buf;
	}
}

/*
 * Storage classes
 */
char *
getclass(int c)
{
	switch (c) {
	case 9:
		return "TYPEDEF";
	case 10:
		return "MOS";
	case 11:
		return "AUTO";
	case 12:
		return "EXTERN";
	case 13:
		return "STATIC";
	case 14:
		return "REG";
	case 15:
		return "STRTAG";
	case 16:
		return "ARG";
	case 17:
		return "ARG1";
	case 18:
		return "AREG";
	case 20:
		return "DEFXTRN";
	case 21:
		return "MOU";
	case 22:
		return "ENUMTAG";
	case 24:
		return "ENUMCON";
	default:
		return NULL;
	}
}

/*
 * Type names
 */
char *
gettypebase(int t)
{
	switch (t & 017) {
	case 0:
		return "int";
	case 1:
		return "char";
	case 2:
		return "float";
	case 3:
		return "double";
	case 4:
		return "struct";
	case 6:
		return "long";
	case 7:
		return "unsigned";
	case 8:
		return "uchar";
	case 9:
		return "ulong";
	case 10:
		return "void";
	default:
		return "?";
	}
}

void
printtype(FILE * out, int t)
{
	int xt;

	/*
	 * Print base type
	 */
	fprintf(out, "%s", gettypebase(t));
	/*
	 * Print modifiers
	 */
	t >>= 4;
	while (t) {
		xt = t & 3;
		switch (xt) {
		case 1:
			fprintf(out, "*");
			break;
		case 2:
			fprintf(out, "()");
			break;
		case 3:
			fprintf(out, "[]");
			break;
		}
		t >>= 2;
	}
}

int
getbyte(FILE * f)
{
	int c = fgetc(f);

	return (c == EOF) ? -1 : c;
}

int
getword(FILE * f)
{
	int lo = getbyte(f);
	int hi = getbyte(f);

	if (lo < 0 || hi < 0)
		return -1;
	return lo | (hi << 8);
}

/*
 * Read null-terminated string
 */
int
getstr(FILE * f, char *buf, int max)
{
	int c, i = 0;

	while ((c = getbyte(f)) > 0 && i < max - 1)
		buf[i++] = c;
	buf[i] = 0;
	return i;
}

void
ppfile(FILE * in, FILE * out)
{
	int c, op, n, n2, n3, type, class;
	char buf[1024];

	while ((c = getbyte(in)) >= 0) {
		op = c;
		/*
		 * Consume 0xfe marker after opcode
		 */
		if (getbyte(in) != 0xfe) {
			fprintf(out, "ERROR: expected 0xfe after opcode %d\n", op);
			return;
		}
		fprintf(out, "%-10s", getop(op));

		switch (op) {
		/*
		 * NAME: class(N) type(N) then either symbol(S) or offset(N)
		 */
		case 20:				/* NAME */
			class = getword(in);
			type = getword(in);
			fprintf(out, " ");
			if (getclass(class))
				fprintf(out, "%s ", getclass(class));
			else
				fprintf(out, "class=%d ", class);
			printtype(out, type);
			/*
			 * Check if followed by '_' (symbol) or number
			 */
			c = getbyte(in);
			if (c == '_') {
				getstr(in, buf, sizeof(buf));
				fprintf(out, " _%s", buf);
			} else if (c >= 0) {
				/* offset - put back and read as word */
				n = c | (getbyte(in) << 8);
				fprintf(out, " offset=%d", (short) n);
			}
			break;

		/*
		 * CON: type(N) value(N)
		 */
		case 21:				/* CON */
			type = getword(in);
			n = getword(in);
			printtype(out, type);
			fprintf(out, " %d", (short) n);
			break;

		/*
		 * STRING: type(N) label(N)
		 */
		case 22:				/* STRING */
			type = getword(in);
			n = getword(in);
			printtype(out, type);
			fprintf(out, " L%d", n);
			break;

		/*
		 * FCON: type(N) string(F)
		 */
		case 23:				/* FCON */
		case 24:				/* SFCON */
			type = getword(in);
			getstr(in, buf, sizeof(buf));
			printtype(out, type);
			fprintf(out, " %s", buf);
			break;

		/*
		 * LCON/SLCON: type(N) lo(N) hi(N)
		 */
		case 25:				/* LCON */
		case 26:				/* SLCON */
			type = getword(in);
			n = getword(in);
			n2 = getword(in);
			printtype(out, type);
			fprintf(out, " %ld", ((long) n2 << 16) | (n & 0xffff));
			break;

		/*
		 * CBRANCH: label(N) cond(N) line(N)
		 */
		case 103:				/* CBRANCH */
			n = getword(in);
			n2 = getword(in);
			n3 = getword(in);
			fprintf(out, " L%d cond=%d line=%d", n, n2, n3);
			break;

		/*
		 * SETREG: number(N) - count of available registers
		 */
		case 105:				/* SETREG */
			n = getword(in);
			fprintf(out, " %d (", n);
			/* Z80 regs: 0=hl, 1=de, 2=bc, 3=ix, 4=iy */
			if (n >= 1) fprintf(out, "hl");
			if (n >= 2) fprintf(out, ",de");
			if (n >= 3) fprintf(out, ",bc");
			if (n >= 4) fprintf(out, ",ix");
			if (n >= 5) fprintf(out, ",iy");
			fprintf(out, ")");
			break;

		/*
		 * BRANCH/LABEL: number(N)
		 */

		case 111:				/* BRANCH */
			n = getword(in);
			fprintf(out, " L%d", n);
			break;

		case 112:				/* LABEL */
			n = getword(in);
			fprintf(out, " L%d", n);
			break;

		/*
		 * NLABEL/RLABEL: string(S)
		 */
		case 113:				/* NLABEL */
		case 114:				/* RLABEL */
			getstr(in, buf, sizeof(buf));
			fprintf(out, " %s", buf);
			break;

		/*
		 * STRASG: type(N) size(N)
		 */
		case 115:				/* STRASG */
			type = getword(in);
			n = getword(in);
			printtype(out, type);
			fprintf(out, " size=%d", n);
			break;

		/*
		 * LINENO: line(N) filename(S)
		 */
		case 116:				/* LINENO */
			n = getword(in);
			getstr(in, buf, sizeof(buf));
			fprintf(out, " %d \"%s\"", n, buf);
			break;

		/*
		 * NEWLINE: (no args, just increments line)
		 */
		case 117:				/* NEWLINE */
			break;

		/*
		 * BDATA: pairs of (flag, value) until flag=0
		 */
		case 200:				/* BDATA */
			fprintf(out, " \"");
			while ((n = getword(in)) == 1) {
				n2 = getword(in);
				if (n2 >= 32 && n2 < 127 && n2 != '"' && n2 != '\\')
					fprintf(out, "%c", n2);
				else
					fprintf(out, "\\x%02x", n2 & 0xff);
			}
			fprintf(out, "\"");
			/* n==0 means end of data */
			break;

		/*
		 * CSPACE: string(S) size(N)
		 */
		case 205:				/* CSPACE */
			getstr(in, buf, sizeof(buf));
			n = getword(in);
			fprintf(out, " %s size=%d", buf, n);
			break;

		/*
		 * SSPACE: size(N)
		 */
		case 206:				/* SSPACE */
			n = getword(in);
			fprintf(out, " %d", n);
			break;

		/*
		 * SYMDEF: string(S)
		 */
		case 207:				/* SYMDEF */
			getstr(in, buf, sizeof(buf));
			fprintf(out, " %s", buf);
			break;

		/*
		 * SWIT: deflab(N) line(N) then pairs of (label, value) until label=0
		 */
		case 213:				/* SWIT */
			n = getword(in);
			n2 = getword(in);
			fprintf(out, " default=L%d line=%d cases:", n, n2);
			while ((n = getword(in)) != 0) {
				n2 = getword(in);
				fprintf(out, " L%d=%d", n, (short)n2);
			}
			break;

		/*
		 * EXPR: line(N)
		 */
		case 214:				/* EXPR */
			n = getword(in);
			fprintf(out, " line=%d", n);
			break;

		/*
		 * SNAME/RNAME/ANAME: string(S) offset(N)
		 */
		case 215:				/* SNAME */
		case 216:				/* RNAME */
		case 217:				/* ANAME */
			getstr(in, buf, sizeof(buf));
			n = getword(in);
			fprintf(out, " %s offset=%d", buf, (short)n);
			break;

		/*
		 * SETSTK: offset(N)
		 */
		case 219:				/* SETSTK */
			n = getword(in);
			fprintf(out, " %d", (short) n);
			break;

		/*
		 * SINIT: value(N)
		 */
		case 220:				/* SINIT */
			n = getword(in);
			fprintf(out, " %d", n);
			break;

		/*
		 * ASSEM: string(F)
		 */
		case 223:				/* ASSEM */
			getstr(in, buf, sizeof(buf));
			fprintf(out, " \"%s\"", buf);
			break;

		/*
		 * No arguments: PROG, DATA, BSS, SAVE, RETRN, EVEN, XNULLOP
		 */
		case 202:				/* PROG */
		case 203:				/* DATA */
		case 204:				/* BSS */
		case 208:				/* SAVE */
		case 209:				/* RETRN */
		case 210:				/* EVEN */
		case 218:				/* XNULLOP */
			break;

		/*
		 * Operators with just type(N)
		 */
		case 9:					/* COMMA */
		case 10:				/* FSEL */
		case 11:				/* CAST */
		case 12:				/* ETYPE */
		case 13:				/* ITOP */
		case 14:				/* PTOI */
		case 15:				/* LTOP */
		case 29:				/* NULLOP */
		case 30:				/* INCBEF */
		case 31:				/* DECBEF */
		case 32:				/* INCAFT */
		case 33:				/* DECAFT */
		case 34:				/* EXCLA */
		case 35:				/* AMPER */
		case 36:				/* STAR */
		case 37:				/* NEG */
		case 38:				/* COMPL */
		case 40:				/* PLUS */
		case 41:				/* MINUS */
		case 42:				/* TIMES */
		case 43:				/* DIVIDE */
		case 44:				/* MOD */
		case 45:				/* RSHIFT */
		case 46:				/* LSHIFT */
		case 47:				/* AND */
		case 48:				/* OR */
		case 49:				/* EXOR */
		case 51:				/* ITOF */
		case 52:				/* FTOI */
		case 53:				/* LOGAND */
		case 54:				/* LOGOR */
		case 56:				/* FTOL */
		case 57:				/* LTOF */
		case 58:				/* ITOL */
		case 59:				/* LTOI */
		case 60:				/* EQUAL */
		case 61:				/* NEQUAL */
		case 62:				/* LESSEQ */
		case 63:				/* LESS */
		case 64:				/* GREATEQ */
		case 65:				/* GREAT */
		case 66:				/* LESSEQP */
		case 67:				/* LESSP */
		case 68:				/* GREATQP */
		case 69:				/* GREATP */
		case 70:				/* ASPLUS */
		case 71:				/* ASMINUS */
		case 72:				/* ASTIMES */
		case 73:				/* ASDIV */
		case 74:				/* ASMOD */
		case 75:				/* ASRSH */
		case 76:				/* ASLSH */
		case 77:				/* ASSAND */
		case 78:				/* ASOR */
		case 79:				/* ASXOR */
		case 80:				/* ASSIGN */
		case 90:				/* QUEST */
		case 97:				/* SEQNC */
		case 100:				/* CALL */
		case 104:				/* INIT */
		case 109:				/* ITOC */
		case 110:				/* RFORCE */
			type = getword(in);
			fprintf(out, " ");
			printtype(out, type);
			break;

		default:
			/*
			 * Unknown opcode - try to read a type if it looks like an operator
			 */
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

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
