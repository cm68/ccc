/*
 * lexread.c - Lexeme stream reader for cc1
 *
 * Reads preprocessed binary token stream from .x files produced by cpp.
 * Token values from cpp match pass1's token.h directly (no translation).
 *
 * Binary format:
 *   Simple tokens: single byte (values match token.h)
 *   Keywords:      single byte (128-160, match token.h directly)
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

/* CPP special token values (not simple pass-through) */
#define CPP_LNUMBER 25
#define CPP_AMPER   35      /* CPP uses AMPER, pass1 uses AND */
#define CPP_TIMES   42      /* CPP uses TIMES, pass1 uses STAR */
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

/* Keyword tokens (128-160) now pass through directly from cpp */

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
 *
 * Most token values pass through directly since cpp and pass1 now
 * use the same values. Only special tokens need handling.
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

	/* CPP uses AMPER (35) for &, pass1 uses AND (47) */
	case CPP_AMPER:
		next.type = AND;
		break;

	/* CPP uses TIMES (42) for *, pass1 uses STAR (36) */
	case CPP_TIMES:
		next.type = STAR;
		break;

	/* Symbol - has length + bytes */
	case SYM:
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

	/* Numbers - have 4-byte value */
	case NUMBER:
	case CPP_LNUMBER:
		next.type = NUMBER;
		next.v.numeric = (long)readLE4();
		break;

	/* Float number - 4-byte IEEE754 */
	case FNUMBER:
		next.type = FNUMBER;
		u.l = readLE4();
		next.v.fval = u.f;
		break;

	/* String - 2-byte length + bytes */
	case STRING:
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

	/* Label - 1-byte length + bytes */
	case LABEL:
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

	/* Inline assembly - 2-byte length + bytes */
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
		/* All other tokens pass through directly */
		next.type = c;
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
