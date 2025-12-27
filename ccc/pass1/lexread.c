/*
 * lexread.c - Lexeme stream reader for cc1
 *
 * Reads preprocessed binary token stream from .x files produced by cpp.
 *
 * Binary format:
 *   Simple tokens: single byte (see CPP_* defines below)
 *   KEYW (19):     19 + 1-byte keyword index
 *   SYM (20):      20 + 1-byte length + name bytes
 *   NUMBER (21):   21 + 4-byte little-endian value
 *   STRING (22):   22 + 2-byte LE length + bytes
 *   FNUMBER (23):  23 + 4-byte IEEE754 bits
 *   LNUMBER (25):  25 + 4-byte little-endian value
 *   LABEL (112):   112 + 1-byte length + name bytes
 *   LINENO (116):  116 + 2-byte LE line + 1-byte len + filename
 *   NEWLINE (117): 117 (line increment by 1)
 *   ASMSTR (118):  118 + 2-byte LE length + bytes
 */
#include "cc1.h"
#include <fcntl.h>
#include <unistd.h>

/* CPP binary token values */
#define CPP_EOF     0
#define CPP_SEMI    1
#define CPP_BEGIN   2
#define CPP_END     3
#define CPP_LBRACK  4
#define CPP_RBRACK  5
#define CPP_LPAR    6
#define CPP_RPAR    7
#define CPP_COLON   8
#define CPP_COMMA   9
#define CPP_KEYW    19
#define CPP_SYM     20
#define CPP_NUMBER  21
#define CPP_STRING  22
#define CPP_FNUMBER 23
#define CPP_LNUMBER 25
#define CPP_INCR    30
#define CPP_DECR    31
#define CPP_BANG    34
#define CPP_AMPER   35
#define CPP_STAR    36
#define CPP_TWIDDLE 38
#define CPP_DOT     39
#define CPP_PLUS    40
#define CPP_MINUS   41
#define CPP_TIMES   42
#define CPP_DIV     43
#define CPP_MOD     44
#define CPP_RSHIFT  45
#define CPP_LSHIFT  46
#define CPP_AND     47
#define CPP_OR      48
#define CPP_XOR     49
#define CPP_ARROW   50
#define CPP_LAND    53
#define CPP_LOR     54
#define CPP_EQ      60
#define CPP_NEQ     61
#define CPP_LE      62
#define CPP_LT      63
#define CPP_GE      64
#define CPP_GT      65
#define CPP_PLUSEQ  70
#define CPP_SUBEQ   71
#define CPP_MULTEQ  72
#define CPP_DIVEQ   73
#define CPP_MODEQ   74
#define CPP_RSHIFTEQ 75
#define CPP_LSHIFTEQ 76
#define CPP_ANDEQ   77
#define CPP_OREQ    78
#define CPP_XOREQ   79
#define CPP_ASSIGN  80
#define CPP_QUES    90
#define CPP_SIZEOF  91
#define CPP_ELLIPSIS 92
#define CPP_LABEL   112
#define CPP_LINENO  116
#define CPP_NEWLINE 117
#define CPP_ASMSTR  118

/* Token lookahead - visible to parser */
struct token cur, next;

/* String buffer for literals */
char strbuf[STRBUFSIZE];

/* Globals for error reporting */
char *filename;
int lineno = 0;
static char filenameBuf[256];

/* Input buffer */
static int lexFd = -1;
static unsigned char lexBuf[512];
static int lexPos = 0;
static int lexValid = 0;

/*
 * Keyword index to pass1 token mapping
 * Index from cpp's KEYW + index byte
 */
static unsigned char kwmap[] = {
	INT,      /* 0: int */
	CHAR,     /* 1: char */
	FLOAT,    /* 2: float */
	DOUBLE,   /* 3: double */
	STRUCT,   /* 4: struct */
	0,        /* 5: (unused) */
	LONG,     /* 6: long */
	UNSIGNED, /* 7: unsigned */
	UNION,    /* 8: union */
	TYPEDEF,  /* 9: typedef */
	VOID,     /* 10: void */
	AUTO,     /* 11: auto */
	EXTERN,   /* 12: extern */
	STATIC,   /* 13: static */
	REGISTER, /* 14: register */
	0, 0, 0, 0, 0, /* 15-19: unused */
	GOTO,     /* 20: goto */
	RETURN,   /* 21: return */
	IF,       /* 22: if */
	WHILE,    /* 23: while */
	ELSE,     /* 24: else */
	SWITCH,   /* 25: switch */
	CASE,     /* 26: case */
	BREAK,    /* 27: break */
	CONTINUE, /* 28: continue */
	DO,       /* 29: do */
	DEFAULT,  /* 30: default */
	FOR,      /* 31: for */
	ENUM,     /* 32: enum */
	ASM,      /* 33: asm */
};
#define NKEYWORDS (sizeof(kwmap)/sizeof(kwmap[0]))

/*
 * Read a byte from the lexeme stream
 */
static int
readByte(void)
{
	if (lexPos >= lexValid) {
		lexValid = read(lexFd, lexBuf, sizeof(lexBuf));
		lexPos = 0;
		if (lexValid <= 0)
			return -1;
	}
	return lexBuf[lexPos++];
}

/*
 * Read N bytes into buffer
 */
static int
readBytes(char *buf, int n)
{
	int i;
	for (i = 0; i < n; i++) {
		int c = readByte();
		if (c < 0) return -1;
		buf[i] = c;
	}
	return n;
}

/*
 * Read 2-byte little-endian value
 */
static int
readLE2(void)
{
	int lo = readByte();
	int hi = readByte();
	if (lo < 0 || hi < 0) return -1;
	return lo | (hi << 8);
}

/*
 * Read 4-byte little-endian value
 */
static unsigned long
readLE4(void)
{
	unsigned long val = 0;
	int i;
	for (i = 0; i < 4; i++) {
		int c = readByte();
		if (c < 0) return 0;
		val |= ((unsigned long)c) << (i * 8);
	}
	return val;
}

/*
 * Free token resources
 */
static void
freeToken(struct token *t)
{
	if (t->type == SYM || t->type == LABEL) {
		if (t->v.name) {
			free(t->v.name);
			t->v.name = NULL;
		}
	} else if (t->type == ASM) {
		if (t->v.str) {
			free(t->v.str);
			t->v.str = NULL;
		}
	}
}

/*
 * Read next token into 'next'
 */
static void
readNextToken(void)
{
	int c;
	int len;
	char *s;
	union { unsigned long l; float f; } u;

again:
	c = readByte();
	if (c < 0) {
		next.type = E_O_F;
		return;
	}

	next.v.name = NULL;

	switch (c) {
	/* Line tracking - transparent to parser */
	case CPP_LINENO:
		lineno = readLE2();
		len = readByte();
		if (len > 0 && len < sizeof(filenameBuf)) {
			readBytes(filenameBuf, len);
			filenameBuf[len] = '\0';
			filename = filenameBuf;
		}
		goto again;

	case CPP_NEWLINE:
		lineno++;
		goto again;

	/* Simple tokens - map to pass1 values */
	case CPP_EOF:    next.type = E_O_F; break;
	case CPP_SEMI:   next.type = SEMI; break;
	case CPP_BEGIN:  next.type = BEGIN; break;
	case CPP_END:    next.type = END; break;
	case CPP_LBRACK: next.type = LBRACK; break;
	case CPP_RBRACK: next.type = RBRACK; break;
	case CPP_LPAR:   next.type = LPAR; break;
	case CPP_RPAR:   next.type = RPAR; break;
	case CPP_COLON:  next.type = COLON; break;
	case CPP_COMMA:  next.type = COMMA; break;

	/* Operators */
	case CPP_INCR:   next.type = INCR; break;
	case CPP_DECR:   next.type = DECR; break;
	case CPP_BANG:   next.type = BANG; break;
	case CPP_AMPER:  next.type = AND; break;
	case CPP_STAR:   next.type = STAR; break;
	case CPP_TWIDDLE: next.type = TWIDDLE; break;
	case CPP_DOT:    next.type = DOT; break;
	case CPP_PLUS:   next.type = PLUS; break;
	case CPP_MINUS:  next.type = MINUS; break;
	case CPP_TIMES:  next.type = STAR; break;
	case CPP_DIV:    next.type = DIV; break;
	case CPP_MOD:    next.type = MOD; break;
	case CPP_RSHIFT: next.type = RSHIFT; break;
	case CPP_LSHIFT: next.type = LSHIFT; break;
	case CPP_AND:    next.type = AND; break;
	case CPP_OR:     next.type = OR; break;
	case CPP_XOR:    next.type = XOR; break;
	case CPP_ARROW:  next.type = ARROW; break;
	case CPP_LAND:   next.type = LAND; break;
	case CPP_LOR:    next.type = LOR; break;

	/* Comparison */
	case CPP_EQ:     next.type = EQ; break;
	case CPP_NEQ:    next.type = NEQ; break;
	case CPP_LE:     next.type = LE; break;
	case CPP_LT:     next.type = LT; break;
	case CPP_GE:     next.type = GE; break;
	case CPP_GT:     next.type = GT; break;

	/* Assignment operators */
	case CPP_ASSIGN: next.type = ASSIGN; break;
	case CPP_PLUSEQ: next.type = PLUSEQ; break;
	case CPP_SUBEQ:  next.type = SUBEQ; break;
	case CPP_MULTEQ: next.type = MULTEQ; break;
	case CPP_DIVEQ:  next.type = DIVEQ; break;
	case CPP_MODEQ:  next.type = MODEQ; break;
	case CPP_RSHIFTEQ: next.type = RSHIFTEQ; break;
	case CPP_LSHIFTEQ: next.type = LSHIFTEQ; break;
	case CPP_ANDEQ:  next.type = ANDEQ; break;
	case CPP_OREQ:   next.type = OREQ; break;
	case CPP_XOREQ:  next.type = XOREQ; break;

	/* Special */
	case CPP_QUES:   next.type = QUES; break;
	case CPP_SIZEOF: next.type = SIZEOF; break;
	case CPP_ELLIPSIS: next.type = ELLIPSIS; break;

	/* Keywords */
	case CPP_KEYW:
		c = readByte();
		if (c >= 0 && c < NKEYWORDS && kwmap[c])
			next.type = kwmap[c];
		else
			next.type = E_O_F;
		break;

	/* Symbol */
	case CPP_SYM:
		len = readByte();
		if (len < 0 || len >= STRBUFSIZE) {
			next.type = E_O_F;
			return;
		}
		s = malloc(len + 1);
		readBytes(s, len);
		s[len] = '\0';
		next.type = SYM;
		next.v.name = s;
		break;

	/* Numbers */
	case CPP_NUMBER:
	case CPP_LNUMBER:
		next.type = NUMBER;
		next.v.numeric = (long)readLE4();
		break;

	case CPP_FNUMBER:
		next.type = FNUMBER;
		u.l = readLE4();
		next.v.fval = u.f;
		break;

	/* String */
	case CPP_STRING:
		len = readLE2();
		if (len < 0 || len >= STRBUFSIZE - 1) {
			next.type = E_O_F;
			return;
		}
		/* Counted string format: first byte is length */
		strbuf[0] = len;
		readBytes(strbuf + 1, len);
		next.type = STRING;
		next.v.str = strbuf;
		break;

	/* Label */
	case CPP_LABEL:
		len = readByte();
		if (len < 0 || len >= STRBUFSIZE) {
			next.type = E_O_F;
			return;
		}
		s = malloc(len + 1);
		readBytes(s, len);
		s[len] = '\0';
		next.type = LABEL;
		next.v.name = s;
		break;

	/* Inline assembly */
	case CPP_ASMSTR:
		len = readLE2();
		if (len < 0) {
			next.type = E_O_F;
			return;
		}
		s = malloc(len + 1);
		readBytes(s, len);
		s[len] = '\0';
		next.type = ASM;
		next.v.str = s;
		break;

	default:
		/* Unknown token */
		next.type = E_O_F;
		break;
	}
}

/*
 * Open lexeme file and prime the token stream
 */
void
lexOpen(char *fn)
{
	lexFd = open(fn, O_RDONLY);
	if (lexFd < 0) {
		fdprintf(2, "cannot open lexeme file: %s\n", fn);
		exit(1);
	}

	filename = "(unknown)";
	lineno = 0;

	/* Prime the token stream - need two reads */
	readNextToken();     /* Fill next */
	/* Shift to cur, fill next */
	cur = next;
	readNextToken();
}

/*
 * Close lexeme file
 */
void
lexClose(void)
{
	if (lexFd >= 0) {
		close(lexFd);
		lexFd = -1;
	}
}

/*
 * Rewind lexeme file to start for phase 2
 */
void
lexRewind(void)
{
	/* Free any allocated token memory */
	freeToken(&cur);
	freeToken(&next);

	/* Seek to start of file */
	lseek(lexFd, 0, SEEK_SET);

	/* Reset buffer state */
	lexPos = 0;
	lexValid = 0;

	/* Reset line tracking */
	filename = "(unknown)";
	lineno = 0;

	/* Re-prime the token stream */
	readNextToken();
	cur = next;
	readNextToken();
}

/*
 * Get next token - shifts next into cur, reads new next
 */
void
gettoken(void)
{
	/* Free old cur if it had allocated memory */
	freeToken(&cur);

	/* Shift next to cur */
	cur = next;

	/* Read new next */
	readNextToken();
}

/*
 * Check if current token matches and consume it if so
 */
char
match(token_t t)
{
	if (cur.type == t) {
		gettoken();
		return 1;
	}
	return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
