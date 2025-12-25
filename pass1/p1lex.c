/*
 *
 * The lex.c file is part of the restored P1.COM program
 * from the Hi-Tech CP/M Z80 C v3.09
 *
 * Not a commercial goal of this laborious work is to popularize among
 * potential fans of 8-bit computers the old HI-TECH Z80 C compiler V3.09
 * (HI-TECH Software) and extend its life, outside of the CP/M environment
 * for full operation in windows 32/64 and Unix-like operating systems
 *
 * The HI-TECH Z80 C cross compiler V3.09 is provided free of charge for any use,
 * private or commercial, strictly as-is. No warranty or product support
 * is offered or implied including merchantability, fitness for a particular
 * purpose, or non-infringement. In no event will HI-TECH Software or its
 * corporate affiliates be liable for any direct or indirect damages.
 *
 * You may use this software for whatever you like, providing you acknowledge
 * that the copyright to this software remains with HI-TECH Software and its
 * corporate affiliates.
 *
 * All copyrights to the algorithms used, binary code, trademarks, etc.
 * belong to the legal owner - Microchip Technology Inc. and its subsidiaries.
 * Commercial use and distribution of recreated source codes without permission
 * from the copyright holderis strictly prohibited.
 *
 *
 * See the readme.md file for additional commentary
 *
 * Mark Ogden
 * 09-Jul-2022
 */
#include "p1.h"

char *keywords[] = {
	"asm", "auto", "break", "case", "char", "continue", "default",
	"do", "double", "else", "@@@@@", "enum", "extern", "float",
	"for", "goto", "if", "int", "long", "register", "return",
	"short", "sizeof", "static", "struct", "switch", "typedef", "union",
	"unsigned", "void", "while"
};

char lastEmitSrc[64];			/* 9d60 */
bool sInfoEmitted;				/* 9da0 */
int16_t inCnt;					/* 9da1 */
char lastEmitFunc[40];			/* 9da3 */
YYTYPE yylval;					/* 9dcb */
char nameBuf[32];				/* 9dcf */
uint8_t ungetTok;				/* 9def */

int16_t strChCnt;				/* 9df0 */
bool lInfoEmitted;				/* 9df2 */
int16_t startTokCnt;			/* 9df3 */
int16_t ungetCh;				/* 9df5 */

uint8_t parseNumber(int16_t ch);
uint8_t parseName(int8_t ch);
void parseAsm(void);
void parseString(int16_t ch);
int16_t getCh(void);
void prErrMsg(void);
int16_t skipWs(void);
int8_t escCh(int16_t ch);

sym_t *sigptr;

/*
 * 56: 2671 PMO +++
 * location of two basic blocks swapped, code equivalent
 *
 * Main lexer function. Returns the next token from input. Handles:
 * - Identifiers and keywords (via parseName)
 * - Numeric constants (via parseNumber)
 * - String and character literals
 * - All C operators including compound assignment
 * - Preprocessor directives (# line numbers, #asm blocks)
 * Uses single-character lookahead (ungetCh) and token pushback (ungetTok).
 */
uint8_t
yylex(void)
{
	int16_t ch;
	uint8_t tok;
	char buf[50];
	register char *s;

	if (ungetTok) {
		tok = ungetTok;
		ungetTok = 0;
		if (tok == T_ID && lexMember)
			yylval.ySym = lookupOrAddSym(nameBuf);
		return tok;
	}
	for (;;) {
		ch = skipWs();
		startTokCnt = inCnt;
		if (Isalpha(ch))
			return parseName((int8_t) ch);
		if (Isdigit(ch))
			return parseNumber(ch);
		switch (ch) {
		case EOF:
			return T_EOF;
		case '#':
			do {
				ch = getCh();
			} while (Isspace(ch) && ch != '\n');
			if (Isdigit(ch) && parseNumber(ch) == T_ICONST) {
				lineNo = (int16_t) (yylval.yNum - 1);
				do {
					ch = getCh();
				} while (Isspace(ch) && ch != '\n');
				if (ch == '"') {
					for (s = buf; (ch = getCh()) != '"' && ch != '\n';)
						*s++ = (char) ch;
					*s = '\0';
					if (buf[0])
						strcpy(srcFile, buf);
					else if (srcFileArg)
						strcpy(srcFile, srcFileArg);
					else
						*srcFile = '\0';
					if (crfFp)
						fprintf(crfFp, "~%s\n", srcFile);
				}
				break;
			} else {
				s = buf;
				do {
					*s++ = (char) ch;
					ch = getCh();
				} while (ch != '\n' && !Isspace(ch));
				*s = '\0';
				while (ch != '\n')
					ch = getCh();
				if (strcmp(buf, "asm") == 0) {
					parseAsm();
					break;
				} else
					fatalErr("illegal '#' directive");
			}
			/*
			 * FALLTHRU 
			 */
		case '"':
			parseString('"');
			return T_SCONST;
		case '\'':
			ch = getCh();
			yylval.yNum = (ch == '\\') ? escCh(getCh()) : ch;
			ch = getCh();
			if (ch == '\n')
				expectErr("closing quote");
			else if (ch != '\'')
				prError("char const too long");

			while (ch != '\n' && ch != '\'')
				ch = getCh();
			return T_ICONST;
		case ';':
			return T_SEMI;
		case ':':
			return T_COLON;
		case '+':
			ch = getCh();
			if (ch == '+')
				return T_PREINC;
			ungetCh = ch;
			ch = skipWs();
			if (ch == '=')
				return P1_EQPLUS;
			ungetCh = ch;
			return T_PLUS;
		case '-':
			ch = getCh();
			if (ch == '-')
				return T_PREDEC;
			if (ch == '>')
				return T_POINTER;

			ungetCh = ch;
			ch = skipWs();
			if (ch == '=')
				return P1_EQMINUS;
			ungetCh = ch;
			return T_MINUS;
		case '*':
			ch = skipWs();
			if (ch == '=')
				return P1_EQMUL;
			ungetCh = ch;
			return T_MUL;		/* deref or multiply */
		case '/':
			ch = skipWs();
			if (ch == '=')
				return P1_EQDIV;
			ungetCh = ch;
			return T_DIV;
		case '%':
			ch = skipWs();
			if (ch == '=')
				return P1_EQMOD;
			ungetCh = ch;
			return T_MOD;
		case '&':
			ch = getCh();
			if (ch == '&')
				return T_LAND;
			ungetCh = ch;
			ch = skipWs();
			if (ch == '=')
				return P1_EQAND;
			ungetCh = ch;
			return T_BAND;
		case '|':
			ch = getCh();
			if (ch == '|')
				return T_LOR;
			ungetCh = ch;
			ch = skipWs();
			if (ch == '=')
				return P1_EQOR;
			ungetCh = ch;
			return T_BOR;
		case '^':
			ch = skipWs();
			if (ch == '=')
				return P1_EQXOR;
			ungetCh = ch;
			return T_XOR;
		case '<':
			ch = getCh();
			if (ch == '<') {
				ch = skipWs();
				if (ch == '=')
					return P1_EQSHL;
				else {
					ungetCh = ch;
					return T_SHL;
				}
			} else if (ch == '=')
				return T_LE;
			ungetCh = ch;
			return T_LT;
		case '>':
			ch = getCh();
			if (ch == '>') {
				ch = skipWs();
				if (ch == '=')
					return P1_EQSHR;
				else {
					ungetCh = ch;
					return T_SHR;
				}
			} else if (ch == '=')
				return T_GE;
			ungetCh = ch;
			return T_GT;
		case '=':
			ch = getCh();
			if (ch == '=')
				return T_EQEQ;
			ungetCh = ch;
			return T_EQ;
		case '!':
			ch = getCh();
			if (ch == '=')
				return T_NE;
			ungetCh = ch;
			return T_LNOT;
		case '~':
			return T_BNOT;
		case '(':
			return T_LPAREN;
		case ')':
			return T_RPAREN;
		case '[':
			return T_LBRACK;
		case ']':
			return T_RBRACK;
		case '{':
			return T_LBRACE;
		case '}':
			return T_RBRACE;
		case '.':
			ch = getCh();
			if (Isdigit(ch)) {
				ungetCh = ch;
				return parseNumber('.');
			}
			if (ch == '.') {
				ch = getCh();
				if (ch != '.')
					prError("'.' expected after '..'");
				return T_3DOT;
			}
			ungetCh = ch;
			return T_DOT;

		case '?':
			return T_QUEST;
		case ',':
			return T_COMMA;
		default:
			prError("illegal character 0%o", ch);
			break;
		}
	}
}

/*
 * 57: 2CC3 PMO +++
 * two blocks change from ex de,hl ld de,xxx
 * to ld hl,xxx ex de,hl (xxx are to locations on stack)
 * optimiser also uses byte compare for digit >= base
 *
 * Parses numeric literals. Handles decimal, octal (0-prefix), and
 * hexadecimal (0x prefix) integers, plus floating point numbers.
 * Returns T_ICONST for int, T_LCONST for long (L suffix), or
 * T_FCONST for floating point. Sets yylval.yNum or yylval.yStr.
 */
uint8_t
parseNumber(int16_t ch)
{
	long lval;
	uint8_t base;
	char buf[50];
	uint8_t digit;
	register char *s = buf;

	while (Isdigit(ch)) {
		*s++ = (char) ch;
		ch = getCh();
	}
	if (ch == '.' || ch == 'e' || ch == 'E') {
		if (ch == '.')
			do {
				*s++ = (char) ch;
				ch = getCh();
			} while (Isdigit(ch));
		if (ch == 'e' || ch == 'E') {
			*s++ = 'e';
			ch = getCh();
			if (ch == '+' || ch == '-') {
				*s++ = (char) ch;
				ch = getCh();
			}
			if (Isdigit(ch))
				do {
					*s++ = (char) ch;
					ch = getCh();
				} while (Isdigit(ch));
			else
				prError("exponent expected");
		}
		ungetCh = ch;
		*s++ = 0;
		if (*buf == '.')
			s++;
		yylval.yStr = xalloc(s - buf);
		if (*buf == '.')
			strcat(strcpy(yylval.yStr, "0"), buf);
		else
			strcpy(yylval.yStr, buf);
		return T_FCONST;
	}
	base = 10;

#ifdef BUGGY
	if (ch == 'x' || (ch == 'X' && *buf == '0')) {
#else
	/*
	 * original code would allow invalid numbers such as 99x123 and
	 * 0999X123 both as 0x123 
	 */
	if ((ch == 'x' || ch == 'X') && s == buf + 1 && *buf == '0') {
#endif
		base = 16;
		s = buf;
		while (Isxdigit(ch = getCh()))
			*s++ = (char) ch;
		if (s == buf)
			prError("hex digit expected");
	} else if (*buf == '0')
		base = 8;
	lval = 0L;
	*s = 0;
	s = buf;
	while (*s) {
		if (*s >= 'A')
			digit = (*(uint8_t *) s++ | 0x20) - 'a' + 10;
		else
			digit = *(uint8_t *) s++ - '0';
		if (digit >= base) {
			prError("digit out of range");
			break;
		}
		lval = lval * base + digit;
	}
	yylval.yNum = lval;
	if (ch == 'l' || ch == 'L')
		return T_LCONST;
	ungetCh = ch;
	return T_ICONST;
}

/*
 * 58: 2F75 PMO +++
 * uses ld hl,xxx ex de,hl compared to ex de,hl ld de,xxx
 * equivalent code
 *
 * Parses identifiers and keywords. Collects alphanumeric chars into
 * nameBuf (max 31 chars), then performs binary search on keyword table.
 * Returns keyword token or S_CLASS/S_TYPE for storage/type specifiers.
 * For non-keywords, looks up or creates symbol and returns T_ID.
 */
uint8_t
parseName(int8_t ch)
{
	int16_t len;
	uint8_t mid;
	uint8_t lo;
	uint8_t hi;
	int16_t cmp;
	register char *s = nameBuf;

	len = 0;
	do {
		if (len != sizeof(nameBuf) - 1) {
			*s++ = ch;
			len++;
		}
		ch = (int8_t) getCh();
	} while (Isalnum(ch));
	ungetCh = ch;
	*s = 0;
	lo = T_ASM;
	hi = T_WHILE;
	do {
		mid = (lo + hi) / 2;
		cmp = (int16_t) strcmp(nameBuf, keywords[mid - T_ASM]);
		if (cmp <= 0)
			hi = mid - 1;
		if (cmp >= 0)
			lo = mid + 1;
	} while (hi >= lo);
	if (hi < lo - 1) {
		switch (mid) {
		case T_AUTO:
		case T_EXTERN:
		case T_REGISTER:
		case T_STATIC:
		case T_TYPEDEF:
			yylval.yVal = mid;
			return S_CLASS;
		case T_CHAR:
		case T_DOUBLE:
		case T_ENUM:
		case T_FLOAT:
		case T_INT:
		case T_LONG:
		case T_SHORT:
		case T_STRUCT:
		case T_UNION:
		case T_UNSIGNED:
		case T_VOID:
			yylval.yVal = mid;
			return S_TYPE;
		case _T_SIZEOF:
			return T_SIZEOF;
		}
		return mid;
	}
	yylval.ySym = lookupOrAddSym(nameBuf);
	return T_ID;
}

/*
 * 59: 308B PMO +++
 *
 * Parses inline assembly block (#asm ... #endasm). Reads lines until
 * #endasm and outputs them as assembly comments (;; prefix).
 * Fatal error if EOF reached before #endasm.
 */
void
parseAsm(void)
{
	int16_t ch;
	char buf[512];
	register char *s;

	for (;;) {
		s = buf;
		while ((ch = getCh()) != '\n' && ch != EOF)
			*s++ = (char) ch;
		*s = 0;
		if (ch == EOF)
			fatalErr("EOF in #asm");
		if (strncmp(buf, "#endasm", 7) == 0)
			return;
		printf(";; %s\n", buf);
	}
}

/*
 * 60: 310B PMO +++
 * one basic block relocated. Code equivalent
 *
 * Parses string literals. Handles escape sequences, concatenation of
 * adjacent strings, and embedded nulls. Allocates memory for result
 * and sets yylval.yStr. Also sets strChCnt to string length.
 */
void
parseString(int16_t ch)
{
	char *strCopy;
	char *strStart;
	char buf[1024];
	register char *s = buf;

	while (ch == '"') {
		while ((ch = getCh()) != '"') {
			if (ch == '\n') {
				expectErr("closing quote");
				break;
			}
			if (ch == '\\') {
				if ((ch = getCh()) != '\n')
					*s++ = escCh(ch);
			} else
				*s++ = (char) ch;
		}
		ch = skipWs();
	}
	ungetCh = ch;
	*s = 0;
	strChCnt = (int16_t) (s - buf);
	strCopy = strStart = xalloc(strChCnt + 1);
	ch = strChCnt + 1;			/* unwound memcpy. Note strcpy cannot
								 * handle embedded '\0' */
	s = buf;
	while (ch--)
		*strCopy++ = *s++;
	yylval.yStr = strStart;
}

/*
 * 61: 320D PMO +++
 * move of 2 basic blocks code equivalent
 *
 * Gets next character from input. Handles pushback via ungetCh,
 * reads from input buffer, and refills buffer when empty. Also
 * handles line number tracking, source info emission, and CR/LF
 * normalization on non-CPM platforms.
 */
int16_t
getCh(void)
{
	int16_t ch;

#if !defined(CPM) && !defined(_WIN32)
	do {
#endif
		if (ungetCh) {
			ch = ungetCh;
			ungetCh = 0;
		} else if ((ch = inBuf[inCnt++]) == 0) {
			if (s_opt)
				emitSrcInfo();
			sInfoEmitted = false;
			lInfoEmitted = false;

			if (!fgets(inBuf, 512, stdin))
				return EOF;
			ch = inBuf[0];
			inCnt = 1;
			startTokCnt = 0;
			lineNo++;
			if (l_opt)
				prErrMsg();
		}
#if !defined(CPM) && !defined(_WIN32)
		if (ch == 0x1a)
			return EOF;
	} while (ch == '\r');
#endif
	return ch;
}

/*
 * 62: 329A PMO +++
 *
 * Prints source location context for error messages. Outputs file name
 * and function (if changed since last call), then the current source
 * line with line number. Only prints once per line (tracks lInfoEmitted).
 */
void
prErrMsg(void)
{
	register char *iy;

	if (!lInfoEmitted) {
		iy = depth && curFuncNode ? curFuncNode->nVName : "";

		if (!l_opt
			&& (strcmp(srcFile, lastEmitSrc)
				|| strcmp(iy, lastEmitFunc))) {
			fprintf(stderr, "%s:", srcFile);
			if (*iy)
				fprintf(stderr, " %s()\n", iy);
			else
				fputc('\n', stderr);
			strcpy(lastEmitSrc, srcFile);
			strcpy(lastEmitFunc, iy);
		}
		fprintf(stderr, "%6d:\t%s", lineNo, inBuf);
		lInfoEmitted = true;
	}
}

/*
 * 63: 3350 PMO +++
 *
 * Prints error message with caret pointing to error location in source.
 * Calculates column position from startTokCnt, handling tabs.
 * Places message before or after caret depending on available space.
 */
void
prMsgAt(register char *buf)
{
	int16_t i;
	uint16_t col;

	prErrMsg();
	if (!*inBuf)
		fputs(buf, stderr);
	else {
		fputc('\t', stderr);
		for (col = i = 0; i < startTokCnt - 1; i++)
			if (inBuf[i] == '\t')
				col = (col + 8) & 0xfff8;
			else
				col++;
		if (strlen(buf) + 1 < col)
			fprintf(stderr, "%*s ^ ", col - 1, buf);
		else
			fprintf(stderr, "%*c %s", col + 1, '^', buf);
	}
}

/*
 * 64: 3429 PMO +++
 *
 * Emits source line as comment in output (for -s option). Outputs
 * line with file:line prefix if not already emitted for current line.
 * Skips blank lines and preprocessor directives.
 */
void
emitSrcInfo(void)
{
	register char *s;

	if (!sInfoEmitted && inBuf[0]) {
		for (s = inBuf; *s && Isspace(*s); s++);
		if (*s && *s != '#')
			printf(";; ;%s: %d: %s", srcFile, lineNo, inBuf);
	}
	sInfoEmitted = true;
}

/*
 * 65: 347A PMO +++
 * equivalent code
 * uses ex de,hl ld de,xxx, cf. ld hl,xxx ex de,hl
 *
 * Skips whitespace characters and returns the first non-whitespace char.
 */
int16_t
skipWs(void)
{
	int16_t ch;

	while (Isspace(ch = getCh()));
	return ch;
}

/*
 * 66: 3495 PMO +++
 * basic block move and different equivalent code
 * and optimisations
 *
 * Processes escape sequences in strings and character literals.
 * Handles \n, \t, \r, etc., octal escapes (\0-\377), and hex
 * escapes (\xNN). Returns the escaped character value.
 */
int8_t
escCh(int16_t ch)
{
	int16_t val;
	int8_t cnt;

	if (Isdigit(ch)) {
		val = ch - '0';
		ch = getCh();
		if (Isdigit(ch)) {
			val = val * 8 + ch - '0';
			if (Isdigit(ch = getCh()))
				val = val * 8 + ch - '0';
			else
				ungetCh = ch;
		} else
			ungetCh = ch;
		return (int8_t) val;
	}
	switch (ch) {
	case 'n':
		return '\n';
	case 'b':
		return '\b';
	case 'r':
		return '\r';
	case 't':
		return '\t';
	case 'f':
		return '\f';
	case 'a':
		return '\a';
	case 'v':
		return '\v';
	case 'x':
		val = 0;
		cnt = 3;
		do {
			ch = getCh();
			if (!Isxdigit(ch)) {
				ungetCh = ch;
				return (int8_t) val;
			}
			val *= 16;
			if (Isupper(ch))
				ch |= 0x20;
			if (Islower(ch))
				val += ch - 'a' + 10;
			else
				val += ch - '0';
		} while (cnt--);
		return (int8_t) val;
	}
	return (int8_t) ch;
}

/*
 * 67: 35F7 PMO +++
 *
 * Peeks at next non-whitespace character without consuming it.
 * Skips whitespace, then pushes the character back via ungetCh.
 */
int16_t
peekCh(void)
{
	int16_t ch;

	ungetCh = ch = skipWs();
	return ch;
}

/*
 * 68: 3610 PMO +++
 *
 * Skips tokens until a statement delimiter (; or }) is found.
 * Used for error recovery. Pushes the delimiter back via ungetTok.
 * Fatal error if EOF is encountered.
 */
void
skipStmt(uint8_t tok)
{

	while (tok > T_RBRACE)
		tok = yylex();
	if (tok == T_EOF)
		fatalErr("unexpected EOF");
	ungetTok = tok;
}

/*
 * 69: 363F PMO +++
 * uint8_t parameter
 *
 * Expects a specific token. If found, consumes it. Otherwise reports
 * error using msg and skips to next statement delimiter for recovery.
 */
void
expect(uint8_t etok, char *msg)
{
	uint8_t tok;

	if ((tok = yylex()) == etok)
		return;
	expectErr(msg);
	skipStmt(tok);
}

/*
 * 70: 3666 PMO +++
 *
 * Skips tokens until semicolon is found. Pushes the semicolon back
 * via ungetTok so caller can consume it. Used for error recovery.
 */
void
skipToSemi(void)
{
	uint8_t tok;

	do {
		tok = yylex();
	} while (tok != T_SEMI);
	ungetTok = T_SEMI;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab: 
 */
