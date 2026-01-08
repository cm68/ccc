;../../libsrc/include/stdlib.h: 4: typedef	int		ptrdiff_t;
;../../libsrc/include/stdlib.h: 5: typedef	unsigned	size_t;
;../../libsrc/include/stdlib.h: 15: extern int	errno;
;../../libsrc/include/stdlib.h: 20: extern double	atof(char *);
;../../libsrc/include/stdlib.h: 21: extern int	atoi(char *);
;../../libsrc/include/stdlib.h: 22: extern long	atol(char *);
;../../libsrc/include/stdlib.h: 23: extern int	rand(void);
;../../libsrc/include/stdlib.h: 24: extern void	srand(unsigned int);
;../../libsrc/include/stdlib.h: 25: extern void *	calloc(size_t, size_t);
;../../libsrc/include/stdlib.h: 26: extern void	free(void *);
;../../libsrc/include/stdlib.h: 27: extern void *	malloc(size_t);
;../../libsrc/include/stdlib.h: 28: extern void *	realloc(void *, size_t);
;../../libsrc/include/stdlib.h: 29: extern void	abort(void);
;../../libsrc/include/stdlib.h: 30: extern void	exit(int);
;../../libsrc/include/stdlib.h: 31: extern char *	getenv(char *);
;../../libsrc/include/stdlib.h: 32: extern int	system(char *);
;../../libsrc/include/stdlib.h: 33: extern void	qsort(void *, size_t, size_t, int (*)(void *, void *));
;../../libsrc/include/stdlib.h: 34: extern int	abs(int);
;../../libsrc/include/stdlib.h: 35: extern long	labs(long);
;../../libsrc/include/stdio.h: 6: extern	struct	_iobuf {
;../../libsrc/include/stdio.h: 7: 	char *		_ptr;
;../../libsrc/include/stdio.h: 8: 	int		_cnt;
;../../libsrc/include/stdio.h: 9: 	char *		_base;
;../../libsrc/include/stdio.h: 10: 	unsigned char		_flag;
;../../libsrc/include/stdio.h: 11: 	char		_file;
;../../libsrc/include/stdio.h: 12: } _iob[	6];
;../../libsrc/include/stdio.h: 14: extern unsigned char _setup;
;../../libsrc/include/stdio.h: 43: extern 	struct _iobuf *stdin;
;../../libsrc/include/stdio.h: 44: extern 	struct _iobuf *stdout;
;../../libsrc/include/stdio.h: 45: extern 	struct _iobuf *stderr;
;../../libsrc/include/stdio.h: 47: 	struct _iobuf *		fopen();
;../../libsrc/include/stdio.h: 48: 	struct _iobuf *		freopen();
;../../libsrc/include/stdio.h: 49: 	struct _iobuf *		fdopen();
;../../libsrc/include/stdio.h: 50: long		ftell();
;../../libsrc/include/stdio.h: 51: char *		fgets();
;../../libsrc/include/stdio.h: 52: char *		_bufallo();
;../../libsrc/include/stdio.h: 53: int _flsbuf();
;../../libsrc/include/stdio.h: 54: char fgetc();
;../../libsrc/include/stdio.h: 55: char fputc();
;../../libsrc/include/stdio.h: 56: int printf(char *, ...);
;../../libsrc/include/stdio.h: 57: int fprintf(	struct _iobuf *, char *, ...);
;../../libsrc/include/stdio.h: 58: int sprintf(char *, char *, ...);
;../../libsrc/include/string.h: 14: extern int	errno;
;../../libsrc/include/string.h: 16: extern void *	memcpy(void *, void *, size_t);
;../../libsrc/include/string.h: 17: extern void *	memmove(void *, void *, size_t);
;../../libsrc/include/string.h: 18: extern char *	strcpy(char *, char *);
;../../libsrc/include/string.h: 19: extern char *	strncpy(char *, char *, size_t);
;../../libsrc/include/string.h: 20: extern char *	strcat(char *, char *);
;../../libsrc/include/string.h: 21: extern char *	strncat(char *, char *, size_t);
;../../libsrc/include/string.h: 22: extern int	memcmp(void *, void *, size_t);
;../../libsrc/include/string.h: 23: extern int	strcmp(char *, char *);
;../../libsrc/include/string.h: 24: extern int	strncmp(char *, char *, size_t);
;../../libsrc/include/string.h: 25: extern size_t	strcoll(char *, size_t, char *);
;../../libsrc/include/string.h: 26: extern void *	memchr(void *, int, size_t);
;../../libsrc/include/string.h: 27: extern size_t	strcspn(char *, char *);
;../../libsrc/include/string.h: 28: extern char *	strpbrk(char *, char *);
;../../libsrc/include/string.h: 29: extern size_t	strspn(char *, char *);
;../../libsrc/include/string.h: 30: extern char *	strstr(char *, char *);
;../../libsrc/include/string.h: 31: extern char *	strtok(char *, char *);
;../../libsrc/include/string.h: 32: extern void *	memset(void *, int, size_t);
;../../libsrc/include/string.h: 33: extern char *	strerror(int);
;../../libsrc/include/string.h: 34: extern size_t	strlen(char *);
;../../libsrc/include/string.h: 35: extern char *	strchr(char *, int);
;../../libsrc/include/string.h: 36: extern char *	strrchr(char *, int);
;../../libsrc/include/string.h: 37: extern char *	strdup(char *);
;./cpp.h: 16: typedef unsigned char token_t;
;./cpp.h: 33: typedef char *cstring;
;./cpp.h: 34: typedef unsigned char byte;
;./cpp.h: 35: typedef unsigned short word;
;./cpp.h: 36: typedef unsigned long dword;
;./cpp.h: 55: typedef int error_t;
;./cpp.h: 74: struct token {
;./cpp.h: 75:     token_t type;
;./cpp.h: 76:     int lineno;
;./cpp.h: 77:     char *filename;
;./cpp.h: 78:     union {
;./cpp.h: 79:         long numeric;
;./cpp.h: 80:         float fval;
;./cpp.h: 81:         char *name;
;./cpp.h: 82:         cstring str;
;./cpp.h: 83:     } v;
;./cpp.h: 84: };
;./cpp.h: 90: struct textbuf {
;./cpp.h: 91:     char fd;
;./cpp.h: 92:     char *name;
;./cpp.h: 93:     char *storage;
;./cpp.h: 94:     short offset;
;./cpp.h: 95:     short valid;
;./cpp.h: 96:     short lineno;
;./cpp.h: 97:     long file_size;
;./cpp.h: 98:     char saved_column;
;./cpp.h: 99:     char direction;
;./cpp.h: 100:     struct textbuf *prev;
;./cpp.h: 101: };
;./cpp.h: 106: struct macro {
;./cpp.h: 107:     unsigned char parmcount;
;./cpp.h: 108:     char *name;
;./cpp.h: 109:     char **parms;
;./cpp.h: 110:     char *mactext;
;./cpp.h: 111:     struct macro *next;
;./cpp.h: 112: };
;./cpp.h: 117: struct cond {
;./cpp.h: 118:     unsigned char flags;
;./cpp.h: 122:     struct cond *next;
;./cpp.h: 123: };
;./cpp.h: 126: extern char lexFd;
;./cpp.h: 127: extern char ppFd;
;./cpp.h: 128: extern char *curFile;
;./cpp.h: 129: extern int lineNo;
;./cpp.h: 130: extern char noLineMarkers;
;./cpp.h: 132: extern unsigned char curchar;
;./cpp.h: 133: extern unsigned char nextchar;
;./cpp.h: 134: extern int lineno;
;./cpp.h: 135: extern char *filename;
;./cpp.h: 136: extern char column;
;./cpp.h: 137: extern char *sysIncPath;
;./cpp.h: 138: extern struct textbuf *tbtop;
;./cpp.h: 140: extern struct token cur, next;
;./cpp.h: 141: extern char strbuf[];
;./cpp.h: 142: extern struct macro *macros;
;./cpp.h: 143: extern char *macbuffer;
;./cpp.h: 144: extern struct cond *cond;
;./cpp.h: 147: extern void pushfile(char *name);
;./cpp.h: 148: extern void insertmacro(char *name, char *macbuf);
;./cpp.h: 149: extern void insertfile(char *name, int sysdirs);
;./cpp.h: 150: extern void advance();
;./cpp.h: 151: extern void ioinit();
;./cpp.h: 152: extern void addInclude(char *name);
;./cpp.h: 155: extern struct textbuf *obtop;
;./cpp.h: 156: extern void outbufPush(void);
;./cpp.h: 157: extern void outbufPop(void);
;./cpp.h: 158: extern void outbufReplay(void);
;./cpp.h: 159: extern void outbufWrite(void *data, int len);
;./cpp.h: 162: extern void gettoken();
;./cpp.h: 163: extern void skipws();
;./cpp.h: 164: extern void skipws1();
;./cpp.h: 165: extern char match(token_t t);
;./cpp.h: 166: extern char issym();
;./cpp.h: 169: extern unsigned char cppkw[];
;./cpp.h: 170: extern unsigned char ckw[];
;./cpp.h: 171: extern unsigned char kwlook(unsigned char *str, unsigned char *table);
;./cpp.h: 174: extern void macdefine(char *s);
;./cpp.h: 175: extern void macundefine(char *s);
;./cpp.h: 176: extern void addDefine(char *s);
;./cpp.h: 179: extern void emitFileStart(char *file);
;./cpp.h: 180: extern void emitToken(unsigned char tok);
;./cpp.h: 181: extern void emitKeyword(unsigned char kwval);
;./cpp.h: 182: extern void emitSym(char *name);
;./cpp.h: 183: extern void emitNumber(long val);
;./cpp.h: 184: extern void emitFNumber(float val);
;./cpp.h: 185: extern void emitString(char *str, int len);
;./cpp.h: 186: extern void emitLabel(char *name);
;./cpp.h: 187: extern void emitLine(int line, char *file);
;./cpp.h: 188: extern void emitPP(char *text, int len);
;./cpp.h: 189: extern void emitPPStr(char *text);
;./cpp.h: 190: extern void emitCurToken(void);
;./cpp.h: 193: extern void error(char *msg);
;./cpp.h: 194: extern void fatal(char *msg);
;./cpp.h: 195: extern void gripe(error_t err);
;./cpp.h: 198: extern unsigned char tflags;
;./cpp.h: 199: extern cstring nextstr;
;./cpp.h: 200: extern unsigned long readcppconst(void);
;./cpp.h: 201: extern char cpppseudofunc(void);
;./cpp.h: 204: extern struct macro *maclookup(char *name);
;./cpp.h: 205: extern char macexpand(char *name);
;./cpp.h: 208: extern unsigned char lookupc(char *table, unsigned char c);
;./cpp.h: 211: extern int fdprintf(int fd, char *fmt, ...);
;./cpp.h: 212: extern long parseConst(token_t stop);
;./cpp.h: 214: extern char *strdup(char *s);
;./cpp.h: 218: extern void knrInit(void);
;./cpp.h: 219: extern void knrAddTypedef(char *name);
;./cpp.h: 220: extern void knrFilter(unsigned char type, long num, float fnum,
;./cpp.h: 221:                       char *str, int slen);
;./cpp.h: 222: extern void knrFilterToken(unsigned char type);
;./cpp.h: 223: extern void knrFiltKw(unsigned char kw);
;./cpp.h: 224: extern void knrFilterSym(char *name);
;./cpp.h: 225: extern void knrFiltNum(long val);
;./cpp.h: 226: extern void knrFiltFNum(float val);
;./cpp.h: 227: extern void knrFiltStr(char *str, int len);
;lex.c: 10: static unsigned char incomment = 0;
psect	data
_incomment:
defb	0
;lex.c: 13: static char *pendingAsm = ((void *)0);
_pendingAsm:
defw	0
;lex.c: 15: static char pendingSemi = 0;
_pendingSemi:
defb	0
;lex.c: 18: struct token cur, next;
;lex.c: 24: char strbuf[256      ];
;lex.c: 31: static char bigbuf[1024];
;lex.c: 32: static int bigbuflen;
;lex.c: 34: unsigned long readcppconst();
;lex.c: 35: char cpppseudofunc();
;lex.c: 40: struct cond *cond;
;lex.c: 42: unsigned char tflags;
;lex.c: 64: char
;lex.c: 65: match(token_t t)
;lex.c: 66: {
psect	text
global	_match
_match:
global	ncsv, cret, indir
call	ncsv
defw	f114
;lex.c: 67:     if (cur.type == t) {
global	_cur
ld	a,(_cur)
ld	e,a
ld	a,(ix+6)
cp	e
jp	nz,l8
;lex.c: 68:         gettoken();
global	_gettoken
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 69:         return 1;
ld	l,.low.1
jp	l7
;lex.c: 70:     }
;lex.c: 71:     return 0;
l8:
ld	l,.low.0
jp	l7
;lex.c: 72: }
l7:
jp	cret
f114	equ	0
;lex.c: 95: char
;lex.c: 96: charmatch(unsigned char c)
;lex.c: 97: {
global	_charmatch
_charmatch:
call	ncsv
defw	f115
;lex.c: 98:     if (curchar == c) {
global	_curchar
ld	a,(_curchar)
ld	e,a
ld	a,(ix+6)
cp	e
jp	nz,l10
;lex.c: 99:         advance();
global	_advance
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 100:         return 1;
ld	l,.low.1
jp	l9
;lex.c: 101:     } else {
jp	l11
l10:
;lex.c: 102:         return 0;
ld	l,.low.0
jp	l9
;lex.c: 103:     }
l11:
;lex.c: 104: }
l9:
jp	cret
f115	equ	0
;lex.c: 122: void
;lex.c: 123: skipws()
;lex.c: 124: {
global	_skipws
_skipws:
call	ncsv
defw	f117
;lex.c: 125:     while ((curchar == ' ') || (curchar == '\n')) {
jp	l13
l14:
;lex.c: 126:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 127:     }
l13:
ld	a,(_curchar)
cp	.low.32
jp	z,20f
jp	21f
21:
ld	a,(_curchar)
cp	.low.10
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l14
11:
l15:
;lex.c: 128: }
l12:
jp	cret
f117	equ	0
;lex.c: 146: void
;lex.c: 147: skipws1()
;lex.c: 148: {
global	_skipws1
_skipws1:
call	ncsv
defw	f118
;lex.c: 149:     while (curchar == ' ' || curchar == '\t') {
jp	l17
l18:
;lex.c: 150:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 151:     }
l17:
ld	a,(_curchar)
cp	.low.32
jp	z,20f
jp	21f
21:
ld	a,(_curchar)
cp	.low.9
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l18
11:
l19:
;lex.c: 152: }
l16:
jp	cret
f118	equ	0
;lex.c: 168: void
;lex.c: 169: skiptoeol()
;lex.c: 170: {
global	_skiptoeol
_skiptoeol:
call	ncsv
defw	f119
;lex.c: 171:     while (curchar && (curchar != '\n')) {
jp	l21
l22:
;lex.c: 172:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 173:     }
l21:
ld	a,(_curchar)
or	a
jp	lnz,20f
jp	21f
20:
ld	a,(_curchar)
cp	.low.10
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l22
11:
l23:
;lex.c: 174: }
l20:
jp	cret
f119	equ	0
;lex.c: 208: static
;lex.c: 209: int
;lex.c: 210: getint(unsigned char base)
;lex.c: 211: {
_getint:
call	ncsv
defw	f120
;lex.c: 212:     int i = 0;
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
;lex.c: 213:     unsigned char c;
;lex.c: 214:     int len = 0;
ld	(ix+-5),.low.0
ld	(ix+1+-5),.high.0
;lex.c: 216:     while (1) {
jp	l25
l26:
;lex.c: 217:         c = curchar;
ld	a,(_curchar)
ld	(ix+-3),a
;lex.c: 218:         if (c < '0') break;
ld	de,48
ld	l,(ix+-3)
ld	h,0
global	wrelop
call	wrelop
jp	lge,l28
jp	l27
;lex.c: 219:         if (c > '9') {
l28:
ld	e,(ix+-3)
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,l29
;lex.c: 220:             c |= 0x20;
set	5&7,(ix+-3)
;lex.c: 221:             if (c >= 'a' && c <= 'f') {
ld	de,97
ld	l,(ix+-3)
ld	h,0
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:
ld	e,(ix+-3)
ld	d,0
ld	hl,102
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l30
11:
;lex.c: 222:                 c = 10 + c - 'a';
ld	a,(ix+-3)
add	a,.low.-87
ld	(ix+-3),a
;lex.c: 223:             } else {
jp	l31
l30:
;lex.c: 224:                 break;
jp	l27
;lex.c: 225:             }
l31:
;lex.c: 226:         } else {
jp	l32
l29:
;lex.c: 227:             c -= '0';
ld	a,(ix+-3)
add	a,.low.-48
ld	(ix+-3),a
;lex.c: 228:         }
l32:
;lex.c: 229:         if ((c+1) > base) {
ld	e,(ix+-3)
ld	d,0
inc	de
ld	l,(ix+6)
ld	h,0
global	wrelop
call	wrelop
jp	lge,l33
;lex.c: 230:             break;
jp	l27
;lex.c: 231:         }
;lex.c: 232:         i *= base;
l33:
ld	e,(ix+6)
ld	d,0
push	ix
pop	hl
dec	hl
dec hl
global	asamul
call	asamul
;lex.c: 233:         i += c;
ld	e,(ix+-3)
ld	d,0
ld	l,(ix+-2)
ld	h,(ix+1+-2)
add	hl,de
ld	(ix+-2),l
ld	(ix+1+-2),h
;lex.c: 234:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 235:         len++;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
;lex.c: 236:     }
l25:
ld	a,.low.1
or	.low.0
jp	nz,l26
l27:
;lex.c: 238:     if ((len == 0) && ((base == 2) || (base == 16))) {
ld	a,(ix+-5)
or	(ix+1+-5)
jp	nz,20f
jp	21f
21:
ld	a,(ix+6)
cp	.low.2
jp	nz,30f
jp	31f
30:
ld	a,(ix+6)
cp	.low.16
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l34
11:
;lex.c: 239:         gripe(    1       );
global	_gripe
ld	hl,1
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 240:     }
;lex.c: 241:     return i;
l34:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l24
;lex.c: 242: }
l24:
jp	cret
f120	equ	-5
;lex.c: 283: static char escval[] = {
psect	data
_escval:
;lex.c: 284:     0, '\b', 0, 0, '\x1b', '\f', 0, 0, 0, 0, 0, 0, 0,
defb	0
defb	8
defb	0
defb	0
defb	27
defb	12
defb	0
defb	0
defb	0
defb	0
defb	0
defb	0
defb	0
;lex.c: 285:     '\n', 0, 0, 0, '\r', 0, '\t', 0, '\v', 0, 0, 0, 0
defb	10
defb	0
defb	0
defb	0
defb	13
defb	0
defb	9
defb	0
defb	11
defb	0
defb	0
defb	0
;lex.c: 286: };
defb	0
;lex.c: 288: static unsigned char termin;
;lex.c: 290: static unsigned char
;lex.c: 291: getlit()
;lex.c: 292: {
psect	text
_getlit:
call	ncsv
defw	f123
;lex.c: 293:     unsigned char c;
;lex.c: 294: top:
l36:
;lex.c: 295:     if (curchar != '\\') {
ld	a,(_curchar)
cp	.low.92
jp	z,l37
;lex.c: 296:         if ((curchar < 0x20) || (curchar > 0x7e)) {
ld	de,32
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,126
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l38
11:
;lex.c: 297:             gripe(    2       );
ld	hl,2
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 298:             curchar = ' ';
ld	a,.low.32
ld	(_curchar),a
;lex.c: 299:         }
;lex.c: 300:         c = curchar;
l38:
ld	a,(_curchar)
ld	(ix+-1),a
;lex.c: 301:     } else {
jp	l39
l37:
;lex.c: 302:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 308:         if (curchar == '\n') {
ld	a,(_curchar)
cp	.low.10
jp	nz,l40
;lex.c: 309:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 310:             if (curchar == termin) return 0xff;
ld	a,(_termin)
ld	e,a
ld	a,(_curchar)
cp	e
jp	nz,l41
ld	l,.low.-1
jp	l35
;lex.c: 311:             goto top;
l41:
jp	l36
;lex.c: 312:         }
;lex.c: 313:         if (curchar >= '0' && curchar <= '7') return getint(8);
l40:
ld	de,48
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,55
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l42
11:
ld	l,.low.8
push	hl
call	_getint
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	l,l
jp	l35
;lex.c: 314:         if ((curchar | 0x20) == 'x') { advance(); return getint(16); }
l42:
ld	a,(_curchar)
or	.low.32
cp	.low.120
jp	nz,l43
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
ld	l,.low.16
push	hl
call	_getint
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	l,l
jp	l35
;lex.c: 315:         if (curchar == 'B') { advance(); return getint(2); }
l43:
ld	a,(_curchar)
cp	.low.66
jp	nz,l44
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
ld	l,.low.2
push	hl
call	_getint
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	l,l
jp	l35
;lex.c: 316:         if (curchar == 'D') { advance(); return getint(10); }
l44:
ld	a,(_curchar)
cp	.low.68
jp	nz,l45
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
ld	l,.low.10
push	hl
call	_getint
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	l,l
jp	l35
;lex.c: 317:         if (curchar >= 'a' && curchar <= 'z' && escval[curchar - 'a'])
l45:
ld	de,97
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	llt,30f
jp	31f
31:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,122
global	wrelop
call	wrelop
jp	llt,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,_escval+-97
add	hl,de
ld	a,(hl)
or	a
jp	az,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l46
11:
;lex.c: 318:             c = escval[curchar - 'a'];
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,_escval+-97
add	hl,de
ld	l,(hl)
ld	(ix+-1),l
;lex.c: 319:         else
jp	l47
l46:
;lex.c: 320:             c = curchar;
ld	a,(_curchar)
ld	(ix+-1),a
l47:
;lex.c: 321:     }
l39:
;lex.c: 322:     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 323:     return c;
ld	l,(ix+-1)
jp	l35
;lex.c: 324: }
l35:
jp	cret
f123	equ	-1
;lex.c: 363: char
;lex.c: 364: isnumber()
;lex.c: 365: {
global	_isnumber
_isnumber:
call	ncsv
defw	f124
;lex.c: 366:     unsigned char base;
;lex.c: 367:     char *p;
;lex.c: 369:     if (charmatch('\'')) {
ld	l,.low.39
push	hl
call	_charmatch
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l49
;lex.c: 370:         termin = '\'';
ld	a,.low.39
ld	(_termin),a
;lex.c: 371:         next.v.numeric = getlit();
global	_next
call	_getlit
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	hl,0
ld	d,l
ld	e,e
ld	(_next+5),de
ld	(_next+2+5),hl
;lex.c: 373:         if (next.v.numeric == 0xff || curchar != '\'') {
ld	de,255
ld	hl,0
push	hl
push	de
ld	de,(_next+5)
ld	hl,(_next+2+5)
global	arelop
call	arelop
jp	anz,20f
jp	21f
20:
ld	a,(_curchar)
cp	.low.39
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l50
11:
;lex.c: 374:             gripe(    3       );
ld	hl,3
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 375:         }
;lex.c: 376:         advance();
l50:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 377:         return 1;
ld	l,.low.1
jp	l48
;lex.c: 378:     }
;lex.c: 381:     if (curchar == '.' && nextchar >= '0' && nextchar <= '9') {
l49:
global	_nextchar
ld	a,(_curchar)
cp	.low.46
jp	nz,30f
jp	31f
31:
ld	de,48
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	llt,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l51
11:
;lex.c: 382:         p = strbuf;
global	_strbuf
ld	hl,_strbuf
ld	(ix+-3),l
ld	(ix+1+-3),h
;lex.c: 383:         *p++ = '0';
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),48
;lex.c: 384:         *p++ = '.';
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),46
;lex.c: 385:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 386:         while (curchar >= '0' && curchar <= '9') {
jp	l52
l53:
;lex.c: 387:             *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 388:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 389:         }
l52:
ld	de,48
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l53
11:
l54:
;lex.c: 390:         if ((curchar | 0x20) == 'e') {
ld	a,(_curchar)
or	.low.32
cp	.low.101
jp	nz,l55
;lex.c: 391:             *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 392:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 393:             if (curchar == '+' || curchar == '-') {
ld	a,(_curchar)
cp	.low.43
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
cp	.low.45
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l56
11:
;lex.c: 394:                 *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 395:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 396:             }
;lex.c: 397:             while (curchar >= '0' && curchar <= '9') {
l56:
jp	l57
l58:
;lex.c: 398:                 *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 399:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 400:             }
l57:
ld	de,48
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l58
11:
l59:
;lex.c: 401:         }
;lex.c: 402:         *p = '\0';
l55:
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	(hl),0
;lex.c: 403:         next.v.fval = (float)atof(strbuf);
global	_atof
ld	hl,_strbuf
push	hl
call	_atof
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	(_next+5),de
ld	(_next+2+5),hl
;lex.c: 404:         if ((curchar | 0x20) == 'f' || (curchar | 0x20) == 'l') {
ld	a,(_curchar)
or	.low.32
cp	.low.102
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
or	.low.32
cp	.low.108
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l60
11:
;lex.c: 405:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 406:         }
;lex.c: 407:         return 2;
l60:
ld	l,.low.2
jp	l48
;lex.c: 408:     }
;lex.c: 410:     if ((curchar < '0') || (curchar > '9')) {
l51:
ld	de,48
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l61
11:
;lex.c: 411:         return 0;
ld	l,.low.0
jp	l48
;lex.c: 412:     }
;lex.c: 414:     base = 10;
l61:
ld	(ix+-1),10
;lex.c: 415:     if (charmatch('0')) {
ld	l,.low.48
push	hl
call	_charmatch
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l62
;lex.c: 416:         if ((curchar | 0x20) == 'x') {
ld	a,(_curchar)
or	.low.32
cp	.low.120
jp	nz,l63
;lex.c: 417:             base = 16;
ld	(ix+-1),16
;lex.c: 418:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 419:         } else if ((curchar | 0x20) == 'b') {
jp	l64
l63:
ld	a,(_curchar)
or	.low.32
cp	.low.98
jp	nz,l65
;lex.c: 420:             base = 2;
ld	(ix+-1),2
;lex.c: 421:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 422:         } else if ((curchar | 0x20) == 'd') {
jp	l66
l65:
ld	a,(_curchar)
or	.low.32
cp	.low.100
jp	nz,l67
;lex.c: 423:             base = 10;
ld	(ix+-1),10
;lex.c: 424:         } else {
jp	l68
l67:
;lex.c: 425:             base = 8;
ld	(ix+-1),8
;lex.c: 426:         }
l68:
l66:
l64:
;lex.c: 427:     }
;lex.c: 428:     next.v.numeric = getint(base);
l62:
ld	l,(ix+-1)
push	hl
call	_getint
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(_next+5),de
ld	(_next+2+5),hl
;lex.c: 433:     if (base == 10 && ((curchar == '.' && (
;lex.c: 434:             (nextchar >= '0' && nextchar <= '9') ||
;lex.c: 435:             (nextchar | 0x20) == 'e' ||
;lex.c: 436:             !((nextchar >= 'a' && nextchar <= 'z') ||
;lex.c: 437:               (nextchar >= 'A' && nextchar <= 'Z') ||
;lex.c: 438:               nextchar == '_')))
;lex.c: 439:                        || (curchar | 0x20) == 'e')) {
ld	a,(ix+-1)
cp	.low.10
jp	nz,20f
jp	21f
21:
ld	a,(_curchar)
cp	.low.46
jp	nz,40f
jp	41f
41:
ld	de,48
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	llt,70f
jp	71f
71:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	llt,70f
jp	71f
71:jp	61f
70:jp	60f
60:
ld	a,(_nextchar)
or	.low.32
cp	.low.101
jp	nz,60f
jp	61f
61:jp	51f
60:jp	50f
50:
ld	de,97
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,80f
jp	81f
80:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,122
global	wrelop
call	wrelop
jp	lge,80f
jp	81f
81:jp	71f
80:jp	70f
71:
ld	de,65
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,80f
jp	81f
80:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,90
global	wrelop
call	wrelop
jp	lge,80f
jp	81f
81:jp	71f
80:jp	70f
71:jp	61f
70:jp	60f
61:
ld	a,(_nextchar)
cp	.low.95
jp	z,60f
jp	61f
61:jp	51f
60:jp	50f
51:jp	41f
50:jp	40f
41:jp	31f
40:jp	30f
30:
ld	a,(_curchar)
or	.low.32
cp	.low.101
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l69
11:
;lex.c: 441:         p = strbuf;
ld	hl,_strbuf
ld	(ix+-3),l
ld	(ix+1+-3),h
;lex.c: 442:         sprintf(p, "%ld", next.v.numeric);
global	_sprintf
ld	de,(_next+5)
ld	hl,(_next+2+5)
push	hl
push	de
ld	hl,19f
push	hl
ld	l,(ix+-3)
ld	h,(ix+1+-3)
push	hl
call	_sprintf
ld	hl,2+2+4
add	hl,sp
ld	sp,hl
;lex.c: 443:         p += strlen(p);
global	_strlen
ld	l,(ix+-3)
ld	h,(ix+1+-3)
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	l,(ix+-3)
ld	h,(ix+1+-3)
add	hl,de
ld	(ix+-3),l
ld	(ix+1+-3),h
;lex.c: 444:         if (curchar == '.') {
ld	a,(_curchar)
cp	.low.46
jp	nz,l70
;lex.c: 445:             *p++ = '.';
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),46
;lex.c: 446:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 447:             while (curchar >= '0' && curchar <= '9') {
jp	l71
l72:
;lex.c: 448:                 *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 449:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 450:             }
l71:
ld	de,48
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l72
11:
l73:
;lex.c: 451:         }
;lex.c: 452:         if ((curchar | 0x20) == 'e') {
l70:
ld	a,(_curchar)
or	.low.32
cp	.low.101
jp	nz,l74
;lex.c: 453:             *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 454:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 455:             if (curchar == '+' || curchar == '-') {
ld	a,(_curchar)
cp	.low.43
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
cp	.low.45
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l75
11:
;lex.c: 456:                 *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 457:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 458:             }
;lex.c: 459:             while (curchar >= '0' && curchar <= '9') {
l75:
jp	l76
l77:
;lex.c: 460:                 *p++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
dec	hl
ld	(hl),a
;lex.c: 461:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 462:             }
l76:
ld	de,48
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l77
11:
l78:
;lex.c: 463:         }
;lex.c: 464:         *p = '\0';
l74:
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	(hl),0
;lex.c: 465:         next.v.fval = (float)atof(strbuf);
ld	hl,_strbuf
push	hl
call	_atof
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	(_next+5),de
ld	(_next+2+5),hl
;lex.c: 467:         if ((curchar | 0x20) == 'f' || (curchar | 0x20) == 'l') {
ld	a,(_curchar)
or	.low.32
cp	.low.102
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
or	.low.32
cp	.low.108
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l79
11:
;lex.c: 468:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 469:         }
;lex.c: 470:         return 2;
l79:
ld	l,.low.2
jp	l48
;lex.c: 471:     }
;lex.c: 474:     if ((curchar == 'L') || (curchar == 'l')) {
l69:
ld	a,(_curchar)
cp	.low.76
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
cp	.low.108
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l80
11:
;lex.c: 475:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 476:     }
;lex.c: 478:     return 1;
l80:
ld	l,.low.1
jp	l48
;lex.c: 479: }
l48:
jp	cret
f124	equ	-3
;lex.c: 517: char
;lex.c: 518: issym()
;lex.c: 519: {
global	_issym
_issym:
call	ncsv
defw	f126
;lex.c: 520:     char *s = strbuf;
ld	hl,_strbuf
ld	(ix+-2),l
ld	(ix+1+-2),h
;lex.c: 523:     if (!(((curchar >= 'a') && (curchar <= 'z')) || 
;lex.c: 524:           ((curchar >= 'A') && (curchar <= 'Z')) ||
;lex.c: 525:           (curchar == '_'))) {
ld	de,97
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,40f
jp	41f
40:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,122
global	wrelop
call	wrelop
jp	lge,40f
jp	41f
41:jp	31f
40:jp	30f
31:
ld	de,65
ld	a,(_curchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,40f
jp	41f
40:
ld	a,(_curchar)
ld	e,a
ld	d,0
ld	hl,90
global	wrelop
call	wrelop
jp	lge,40f
jp	41f
41:jp	31f
40:jp	30f
31:jp	21f
30:jp	20f
21:
ld	a,(_curchar)
cp	.low.95
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l82
11:
;lex.c: 526:         return 0;
ld	l,.low.0
jp	l81
;lex.c: 527:     }
;lex.c: 529:     while (1) {
l82:
jp	l83
l84:
;lex.c: 530:         *s++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
dec	hl
ld	(hl),a
;lex.c: 531:         *s = 0;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),0
;lex.c: 533:         if (!((nextchar >= 'A' && nextchar <= 'Z') ||
;lex.c: 534:             (nextchar >= 'a' && nextchar <= 'z') ||
;lex.c: 535:             (nextchar >= '0' && nextchar <= '9') ||
;lex.c: 536:             (nextchar == '_'))) {
ld	de,65
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,50f
jp	51f
50:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,90
global	wrelop
call	wrelop
jp	lge,50f
jp	51f
51:jp	41f
50:jp	40f
41:
ld	de,97
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,50f
jp	51f
50:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,122
global	wrelop
call	wrelop
jp	lge,50f
jp	51f
51:jp	41f
50:jp	40f
41:jp	31f
40:jp	30f
31:
ld	de,48
ld	a,(_nextchar)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,40f
jp	41f
40:
ld	a,(_nextchar)
ld	e,a
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,40f
jp	41f
41:jp	31f
40:jp	30f
31:jp	21f
30:jp	20f
21:
ld	a,(_nextchar)
cp	.low.95
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l86
11:
;lex.c: 537:             break;
jp	l85
;lex.c: 538:         }
;lex.c: 539:         advance();
l86:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 540:     }
l83:
ld	a,.low.1
or	.low.0
jp	nz,l84
l85:
;lex.c: 543:     if ((s - strbuf) > 14) {
ld	de,_strbuf
ld	l,(ix+-2)
ld	h,(ix+1+-2)
or	a
sbc	hl,de
ex	de,hl
ld	hl,14
global	wrelop
call	wrelop
jp	age,l87
;lex.c: 544:         gripe(    4       );
ld	hl,4
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 545:         fdprintf(2, "  Identifier '%s' exceeds 14 character limit\n", strbuf);
global	_fdprintf
ld	hl,_strbuf
push	hl
ld	hl,29f
push	hl
ld	hl,2
push	hl
call	_fdprintf
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;lex.c: 546:     }
;lex.c: 554:     return 1;
l87:
ld	l,.low.1
jp	l81
;lex.c: 555: }
l81:
jp	cret
f126	equ	-2
;lex.c: 611: void
;lex.c: 612: doCpp(unsigned char t)
;lex.c: 613: {
global	_doCpp
_doCpp:
call	ncsv
defw	f127
;lex.c: 614:     char *s;
;lex.c: 615:     unsigned char k;
;lex.c: 616:     struct cond *c;
;lex.c: 617:     unsigned long v;
;lex.c: 620:     switch (t) {
jp	l90
;lex.c: 621:     case 243:
l91:
;lex.c: 622:         v = readcppconst();
global	_readcppconst
call	_readcppconst
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	(ix+-9),e
ld	(ix+1+-9),d
ld	(ix+2+-9),l
ld	(ix+3+-9),h
;lex.c: 623:         c = malloc(sizeof(*c));
global	_malloc
ld	hl,3
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-5),l
ld	(ix+1+-5),h
;lex.c: 624:         c->next = cond;
global	_cond
ld	de,(_cond)
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(hl),e
inc	hl
ld	(hl),d
;lex.c: 625:         cond = c;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
ld	(_cond),hl
;lex.c: 627:         if (c->next && !(c->next->flags &      0x01))
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	a,(hl)
inc	hl
or	(hl)
jp	z,20f
jp	21f
21:
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
bit	0&7,(hl)
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l92
11:
;lex.c: 628:             v = 0;
ld	(ix+-9),.low.0
ld	(ix+1+-9),.high.0
ld	(ix+2+-9),.low.0
ld	(ix+3+-9),.high.0
;lex.c: 629:         cond->flags = (v ? (     0x01| 0x04) : 0);
l92:
ld	a,(ix+-9)
or	(ix+1+-9)
or	(ix+2+-9)
or	(ix+3+-9)
jp	nz,20f
jp	21f
21:
ld	hl,0
jp	22f
20:
ld	hl,5
22:
ld	a,l
ld	hl,(_cond)
ld	(hl),a
;lex.c: 640:         return;
jp	l88
;lex.c: 641:     case 245:
l93:
;lex.c: 642:     case 244:
l94:
;lex.c: 643:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 644:         if (!issym()) {
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	anz,l95
;lex.c: 645:             gripe(    5       );
ld	hl,5
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 646:             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 647:             return;
jp	l88
;lex.c: 648:         }
;lex.c: 649:         advance();
l95:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 650:         v = (maclookup(strbuf) != 0);
global	_maclookup
ld	hl,_strbuf
push	hl
call	_maclookup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
ld	de,1
jp	nz,20f
dec	de
20:
ld	hl,0
ld	(ix+-9),e
ld	(ix+1+-9),d
ld	(ix+2+-9),l
ld	(ix+3+-9),h
;lex.c: 651:         if (t == 245) v = !v;
ld	a,(ix+6)
cp	.low.-11
jp	nz,l96
ld	a,(ix+-9)
or	(ix+1+-9)
or	(ix+2+-9)
or	(ix+3+-9)
ld	de,1
jp	z,20f
dec	de
20:
ld	hl,0
ld	(ix+-9),e
ld	(ix+1+-9),d
ld	(ix+2+-9),l
ld	(ix+3+-9),h
;lex.c: 652:         c = malloc(sizeof(*c));
l96:
ld	hl,3
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-5),l
ld	(ix+1+-5),h
;lex.c: 653:         c->next = cond;
ld	de,(_cond)
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(hl),e
inc	hl
ld	(hl),d
;lex.c: 654:         cond = c;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
ld	(_cond),hl
;lex.c: 656:         if (c->next && !(c->next->flags &      0x01))
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	a,(hl)
inc	hl
or	(hl)
jp	z,20f
jp	21f
21:
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
bit	0&7,(hl)
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l97
11:
;lex.c: 657:             v = 0;
ld	(ix+-9),.low.0
ld	(ix+1+-9),.high.0
ld	(ix+2+-9),.low.0
ld	(ix+3+-9),.high.0
;lex.c: 658:         cond->flags = (v ? (     0x01| 0x04) : 0);
l97:
ld	a,(ix+-9)
or	(ix+1+-9)
or	(ix+2+-9)
or	(ix+3+-9)
jp	nz,20f
jp	21f
21:
ld	hl,0
jp	22f
20:
ld	hl,5
22:
ld	a,l
ld	hl,(_cond)
ld	(hl),a
;lex.c: 659:         skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 660:         return;
jp	l88
;lex.c: 661:     case 246:
l98:
;lex.c: 662:         if (!(tflags &   0x01      )) {
global	_tflags
ld	a,(_tflags)
bit	0&7,a
jp	nz,l99
;lex.c: 663:             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 664:         }
;lex.c: 675:         if (!cond) {
l99:
ld	hl,(_cond)
ld	a,l
or	h
jp	nz,l100
;lex.c: 676:             gripe(    6       );
ld	hl,6
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 677:             return;
jp	l88
;lex.c: 678:         }
;lex.c: 679:         c = cond;
l100:
ld	hl,(_cond)
ld	(ix+-5),l
ld	(ix+1+-5),h
;lex.c: 680:         cond = c->next;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_cond),bc
;lex.c: 681:         free(c);
global	_free
ld	l,(ix+-5)
ld	h,(ix+1+-5)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 692:         return;
jp	l88
;lex.c: 693:     case 248:
l101:
;lex.c: 694:         if (!(tflags &   0x01      )) {
ld	a,(_tflags)
bit	0&7,a
jp	nz,l102
;lex.c: 695:             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 696:         }
;lex.c: 697:         if (!cond) {
l102:
ld	hl,(_cond)
ld	a,l
or	h
jp	nz,l103
;lex.c: 698:             gripe(    6       );
ld	hl,6
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 699:             return;
jp	l88
;lex.c: 700:         }
;lex.c: 701:         if (cond->flags &  0x02) {
l103:
ld	hl,(_cond)
bit	1&7,(hl)
jp	z,l104
;lex.c: 702:             gripe(    7       );
ld	hl,7
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 703:             return;
jp	l88
;lex.c: 704:         }
;lex.c: 705:         cond->flags |=  0x02;
l104:
ld	hl,(_cond)
set	1&7,(hl)
;lex.c: 706:         if (cond->flags &  0x04) {
ld	hl,(_cond)
bit	2&7,(hl)
jp	z,l105
;lex.c: 708:             cond->flags &= ~     0x01;
ld	hl,(_cond)
ld	a,(hl)
and	.low.-2
ld	(hl),a
;lex.c: 709:         } else {
jp	l106
l105:
;lex.c: 711:             cond->flags |= (     0x01 |  0x04);
ld	hl,(_cond)
ld	a,(hl)
or	.low.5
ld	(hl),a
;lex.c: 712:         }
l106:
;lex.c: 713:         return;
jp	l88
;lex.c: 714:     case 247:
l107:
;lex.c: 715:         if (!cond) {
ld	hl,(_cond)
ld	a,l
or	h
jp	nz,l108
;lex.c: 716:             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 717:             gripe(    6       );
ld	hl,6
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 718:             return;
jp	l88
;lex.c: 719:         }
;lex.c: 720:         v = readcppconst();
l108:
call	_readcppconst
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	(ix+-9),e
ld	(ix+1+-9),d
ld	(ix+2+-9),l
ld	(ix+3+-9),h
;lex.c: 721:         if (cond->flags &  0x02) {
ld	hl,(_cond)
bit	1&7,(hl)
jp	z,l109
;lex.c: 722:             gripe(    7       );
ld	hl,7
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 723:             return;
jp	l88
;lex.c: 724:         }
;lex.c: 725:         if (cond->flags &  0x04) {
l109:
ld	hl,(_cond)
bit	2&7,(hl)
jp	z,l110
;lex.c: 726:             cond->flags ^=      0x01;
ld	hl,(_cond)
ld	a,(hl)
xor	.low.1
ld	(hl),a
;lex.c: 727:         } else {
jp	l111
l110:
;lex.c: 728:             cond->flags |= (v ? (     0x01 |  0x04) : 0);
ld	a,(ix+-9)
or	(ix+1+-9)
or	(ix+2+-9)
or	(ix+3+-9)
jp	nz,20f
jp	21f
21:
ld	hl,0
jp	22f
20:
ld	hl,5
22:
ld	e,l
ld	hl,(_cond)
ld	a,(hl)
or	e
ld	(hl),a
;lex.c: 729:         }
l111:
;lex.c: 734:         return;
jp	l88
;lex.c: 735:     case 241:
l112:
;lex.c: 736:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 737:         if (!issym()) {
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	anz,l113
;lex.c: 738:             gripe(    5       );
ld	hl,5
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 739:             return;
jp	l88
;lex.c: 740:         }
;lex.c: 741:         advance();
l113:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 742:         macdefine(strbuf);
global	_macdefine
ld	hl,_strbuf
push	hl
call	_macdefine
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 743:         return;
jp	l88
;lex.c: 744:     case 242:
l114:
;lex.c: 745:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 746:         if (!issym()) {
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	anz,l115
;lex.c: 747:             gripe(    5       );
ld	hl,5
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 748:             return;
jp	l88
;lex.c: 749:         }
;lex.c: 750:         advance();
l115:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 751:         macundefine(strbuf);
global	_macundefine
ld	hl,_strbuf
push	hl
call	_macundefine
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 752:         return;
jp	l88
;lex.c: 753:     case 240:
l116:
;lex.c: 759:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 760:         if (curchar == '<') {
ld	a,(_curchar)
cp	.low.60
jp	nz,l117
;lex.c: 761:             k = '>';
ld	(ix+-3),62
;lex.c: 762:         } else if (curchar == '\"') {
jp	l118
l117:
ld	a,(_curchar)
cp	.low.34
jp	nz,l119
;lex.c: 763:             k = '\"';
ld	(ix+-3),34
;lex.c: 764:         } else {
jp	l120
l119:
;lex.c: 765:             gripe(    8       );
ld	hl,8
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 766:             return;
jp	l88
;lex.c: 767:         }
l120:
l118:
;lex.c: 768:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 769:         s = strbuf;
ld	hl,_strbuf
ld	(ix+-2),l
ld	(ix+1+-2),h
;lex.c: 770:         while ((curchar != '\n') && (curchar != ' ') && (curchar != k)) {
jp	l121
l122:
;lex.c: 771:             *s++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
dec	hl
ld	(hl),a
;lex.c: 772:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 773:         }
l121:
ld	a,(_curchar)
cp	.low.10
jp	nz,30f
jp	31f
30:
ld	a,(_curchar)
cp	.low.32
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(_curchar)
ld	e,a
ld	a,(ix+-3)
cp	e
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l122
11:
l123:
;lex.c: 774:         *s = 0;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),0
;lex.c: 775:         if (curchar != k) {
ld	a,(_curchar)
ld	e,a
ld	a,(ix+-3)
cp	e
jp	z,l124
;lex.c: 776:             gripe(    8       );
ld	hl,8
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 777:         }
;lex.c: 778:         skiptoeol();
l124:
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 788:         insertfile(strbuf, k == '>');
global	_insertfile
ld	a,(ix+-3)
cp	.low.62
ld	hl,1
jp	z,10f
dec	hl
10:
push	hl
ld	hl,_strbuf
push	hl
call	_insertfile
ld	hl,2+2
add	hl,sp
ld	sp,hl
;lex.c: 790:         return;
jp	l88
;lex.c: 791:     }
jp	l89
l90:
ld	a,(ix+6)
add	a,.low.16
ld	l,a
ld	h,0
ld	a,0
cp	h
jp	c,l89
jp	nz,1f
ld	a,8
cp	l
jp	c,l89
1:add	hl,hl
ld	de,S128
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
jp	(hl)
psect	data
S128:
defw	l116
defw	l112
defw	l114
defw	l91
defw	l94
defw	l93
defw	l98
defw	l107
defw	l101
psect	text
l89:
;lex.c: 792: }
l88:
jp	cret
f127	equ	-9
;lex.c: 803: char
;lex.c: 804: isstring()
;lex.c: 805: {
global	_isstring
_isstring:
call	ncsv
defw	f129
;lex.c: 806: 	unsigned char c;
;lex.c: 808:     if (!charmatch('\"')) {
ld	l,.low.34
push	hl
call	_charmatch
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	anz,l126
;lex.c: 809:         return 0;
ld	l,.low.0
jp	l125
;lex.c: 810:     }
;lex.c: 811:     termin = '"';
l126:
ld	a,.low.34
ld	(_termin),a
;lex.c: 812:     while (!charmatch('\"')) {
jp	l127
l128:
;lex.c: 813:         c = getlit();
call	_getlit
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 814:         if (c == 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	nz,l130
;lex.c: 816:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 817:             break;
jp	l129
;lex.c: 818:         }
;lex.c: 819:         if (bigbuflen >= 1024 - 1) {
l130:
ld	de,1023
ld	hl,(_bigbuflen)
global	wrelop
call	wrelop
jp	alt,l131
;lex.c: 820:             error("string too long");
global	_error
ld	hl,39f
push	hl
call	_error
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 822:             while (!charmatch('\"'))
jp	l132
l133:
;lex.c: 823:                 getlit();
call	_getlit
ld	hl,0
add	hl,sp
ld	sp,hl
l132:
ld	l,.low.34
push	hl
call	_charmatch
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l133
l134:
;lex.c: 824:             break;
jp	l129
;lex.c: 825:         }
;lex.c: 826:         bigbuf[bigbuflen++] = c;
l131:
ld	a,(ix+-1)
ld	de,_bigbuf
ld	hl,(_bigbuflen)
inc	hl
ld	(_bigbuflen),hl
dec	hl
add	hl,de
ld	(hl),a
;lex.c: 827:     }
l127:
ld	l,.low.34
push	hl
call	_charmatch
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l128
l129:
;lex.c: 828:     bigbuf[bigbuflen] = 0;
ld	de,(_bigbuflen)
ld	hl,_bigbuf
add	hl,de
ld	(hl),0
;lex.c: 834:     return 1;
ld	l,.low.1
jp	l125
;lex.c: 835: }
l125:
jp	cret
f129	equ	-1
;lex.c: 841: char simpleChars[] = "{},[]();=.+-/*%&|^<>!~?:";
psect	data
global	_simpleChars
_simpleChars:
defb	123
defb	125
defb	44
defb	91
defb	93
defb	40
defb	41
defb	59
defb	61
defb	46
defb	43
defb	45
defb	47
defb	42
defb	37
defb	38
defb	124
defb	94
defb	60
defb	62
defb	33
defb	126
defb	63
defb	58
defb	0
;lex.c: 842: char simpleToks[] = {
global	_simpleToks
_simpleToks:
;lex.c: 843:       2,     3,   9,  4,  5,    6,    7,    1,
defb	2
defb	3
defb	9
defb	4
defb	5
defb	6
defb	7
defb	1
;lex.c: 844:      80,     39,    40,   41,     43,   42,     44,     47,      48,     49,
defb	80
defb	39
defb	40
defb	41
defb	43
defb	42
defb	44
defb	47
defb	48
defb	49
;lex.c: 845:          63,      65,    34, 38,    90,   8, 0
defb	63
defb	65
defb	34
defb	38
defb	90
defb	8
;lex.c: 846: };
defb	0
;lex.c: 851: char dblChars[] = "+-|&=><";
global	_dblChars
_dblChars:
defb	43
defb	45
defb	124
defb	38
defb	61
defb	62
defb	60
defb	0
;lex.c: 852: char dbltok[] = {
global	_dbltok
_dbltok:
;lex.c: 853:        30,    31,     54,    53,      60,  45,  46, 0
defb	30
defb	31
defb	54
defb	53
defb	60
defb	45
defb	46
;lex.c: 854: };
defb	0
;lex.c: 860: char eqChars[] = "+-*/%&|^><!";
global	_eqChars
_eqChars:
defb	43
defb	45
defb	42
defb	47
defb	37
defb	38
defb	124
defb	94
defb	62
defb	60
defb	33
defb	0
;lex.c: 861: char eqtok[] = {
global	_eqtok
_eqtok:
;lex.c: 862:      70,   71,  72,   73,   74,   77,    78,   79,
defb	70
defb	71
defb	72
defb	73
defb	74
defb	77
defb	78
defb	79
;lex.c: 863:          64,      62,     61, 0
defb	64
defb	62
defb	61
;lex.c: 864: };
defb	0
;lex.c: 891: void
;lex.c: 892: freetoken()
;lex.c: 893: {
psect	text
global	_freetoken
_freetoken:
call	ncsv
defw	f136
;lex.c: 900: }
l135:
jp	cret
f136	equ	0
;lex.c: 983: unsigned char lineend = 0;
psect	data
global	_lineend
_lineend:
defb	0
;lex.c: 985: void
;lex.c: 986: gettoken()
;lex.c: 987: {
psect	text
_gettoken:
call	ncsv
defw	f138
;lex.c: 988:     token_t t;
;lex.c: 989:     unsigned char c;
;lex.c: 991:     freetoken();
call	_freetoken
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 993:     memcpy(&cur, &next, sizeof(cur));
global	_memcpy
ld	hl,9
push	hl
ld	hl,_next
push	hl
ld	hl,_cur
push	hl
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;lex.c: 994:     next.v.str = 0;
ld	hl,0
ld	(_next+5),hl
;lex.c: 995:     next.type = 255;
ld	a,.low.-1
ld	(_next),a
;lex.c: 998:     if (pendingSemi) {
ld	a,(_pendingSemi)
or	a
jp	az,l137
;lex.c: 999:         pendingSemi = 0;
ld	a,.low.0
ld	(_pendingSemi),a
;lex.c: 1000:         next.type =    1;
ld	a,.low.1
ld	(_next),a
;lex.c: 1001:         return;
jp	l136
;lex.c: 1002:     }
;lex.c: 1005:     if (pendingAsm) {
l137:
ld	hl,(_pendingAsm)
ld	a,l
or	h
jp	z,l138
;lex.c: 1006:         next.type =  118;
ld	a,.low.118
ld	(_next),a
;lex.c: 1007:         next.v.name = pendingAsm;
ld	hl,(_pendingAsm)
ld	(_next+5),hl
;lex.c: 1008:         pendingAsm = ((void *)0);
ld	hl,0
ld	(_pendingAsm),hl
;lex.c: 1009:         pendingSemi = 1;
ld	a,.low.1
ld	(_pendingSemi),a
;lex.c: 1010:         return;
jp	l136
;lex.c: 1011:     }
;lex.c: 1013:     while (1) {
l138:
jp	l139
l140:
;lex.c: 1014:         if (curchar == 0) {
ld	a,(_curchar)
or	a
jp	lnz,l142
;lex.c: 1015:             next.type =   0;
ld	a,.low.0
ld	(_next),a
;lex.c: 1025:             break;
jp	l141
;lex.c: 1026:         }
;lex.c: 1028:         if (curchar == '/') {
l142:
ld	a,(_curchar)
cp	.low.47
jp	nz,l143
;lex.c: 1029:             if (nextchar == '*') {
ld	a,(_nextchar)
cp	.low.42
jp	nz,l144
;lex.c: 1039:                 incomment = 1;
ld	a,.low.1
ld	(_incomment),a
;lex.c: 1040:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1041:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1042:                 continue;
jp	l139
;lex.c: 1043:             }
;lex.c: 1044:         }
l144:
;lex.c: 1045:         if (!incomment && (curchar == '/') && (nextchar == '/')) {
l143:
ld	a,(_incomment)
or	a
jp	lnz,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.47
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_nextchar)
cp	.low.47
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l145
11:
;lex.c: 1046:             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1047:             continue;
jp	l139
;lex.c: 1048:         }
;lex.c: 1049:         if ((incomment) && (curchar == '*') && (nextchar == '/')) {
l145:
ld	a,(_incomment)
or	a
jp	lz,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.42
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_nextchar)
cp	.low.47
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l146
11:
;lex.c: 1057:             incomment = 0;
ld	a,.low.0
ld	(_incomment),a
;lex.c: 1058:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1059:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1060:             continue;
jp	l139
;lex.c: 1061:         }
;lex.c: 1062:         if (incomment) {
l146:
ld	a,(_incomment)
or	a
jp	lz,l147
;lex.c: 1063:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1064:             continue;
jp	l139
;lex.c: 1065:         }
;lex.c: 1067:         if (charmatch('#')) {
l147:
ld	l,.low.35
push	hl
call	_charmatch
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l148
;lex.c: 1078:             if (column != 1) {
global	_column
ld	a,(_column)
cp	.low.1
jp	z,l149
;lex.c: 1080:                 next.type = '#';
ld	a,.low.35
ld	(_next),a
;lex.c: 1081:                 break;
jp	l141
;lex.c: 1082:             }
;lex.c: 1084:             skipws1();
l149:
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1085:             if (issym()) {
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l150
;lex.c: 1086:                 t = kwlook((unsigned char *)strbuf, cppkw);
global	_kwlook
global	_cppkw
ld	hl,_cppkw
push	hl
ld	hl,_strbuf
push	hl
call	_kwlook
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 1092:                 if (t != 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	z,l151
;lex.c: 1093:                     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1098:                     if (cond && !(cond->flags &      0x01)) {
ld	hl,(_cond)
ld	a,l
or	h
jp	z,20f
jp	21f
21:
ld	hl,(_cond)
bit	0&7,(hl)
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l152
11:
;lex.c: 1099:                         if (t != 243 && t != 244 && t != 245 &&
;lex.c: 1100:                             t != 247 && t != 248 && t != 246) {
ld	a,(ix+-1)
cp	.low.-13
jp	z,60f
jp	61f
61:
ld	a,(ix+-1)
cp	.low.-12
jp	z,60f
jp	61f
61:jp	51f
60:jp	50f
51:
ld	a,(ix+-1)
cp	.low.-11
jp	z,50f
jp	51f
51:jp	41f
50:jp	40f
41:
ld	a,(ix+-1)
cp	.low.-9
jp	z,40f
jp	41f
41:jp	31f
40:jp	30f
31:
ld	a,(ix+-1)
cp	.low.-8
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(ix+-1)
cp	.low.-10
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l153
11:
;lex.c: 1101:                             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1102:                             continue;
jp	l139
;lex.c: 1103:                         }
;lex.c: 1104:                     }
l153:
;lex.c: 1113:                     doCpp(t);
l152:
ld	l,(ix+-1)
push	hl
call	_doCpp
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 1131:                     continue;
jp	l139
;lex.c: 1132:                 }
;lex.c: 1133:                 gripe(    9       );
l151:
ld	hl,9
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 1134:             }
;lex.c: 1135:             if (isnumber()) {
l150:
call	_isnumber
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l154
;lex.c: 1136:                 lineno = next.v.numeric;
global	_lineno
ld	hl,(_next+5)
ld	(_lineno),hl
;lex.c: 1137:                 skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1138:                 continue;
jp	l139
;lex.c: 1139:             }
;lex.c: 1140:         }
l154:
;lex.c: 1141:         if (curchar == '\n') {
l148:
ld	a,(_curchar)
cp	.low.10
jp	nz,l155
;lex.c: 1142:             lineend = 1;
ld	a,.low.1
ld	(_lineend),a
;lex.c: 1143:         }
;lex.c: 1144:         if ((tflags &   0x01      ) && (curchar == '\n')) {
l155:
ld	a,(_tflags)
bit	0&7,a
jp	z,20f
jp	21f
21:
ld	a,(_curchar)
cp	.low.10
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l156
11:
;lex.c: 1145:             next.type =    1;
ld	a,.low.1
ld	(_next),a
;lex.c: 1150:             break;
jp	l141
;lex.c: 1151:         }
;lex.c: 1152:         if (!(tflags &   0x01      ) && cond && !(cond->flags &      0x01) &&
l156:
;lex.c: 1153:             curchar != '#') {
ld	a,(_tflags)
bit	0&7,a
jp	nz,40f
jp	41f
41:
ld	hl,(_cond)
ld	a,l
or	h
jp	z,40f
jp	41f
41:jp	31f
40:jp	30f
31:
ld	hl,(_cond)
bit	0&7,(hl)
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_curchar)
cp	.low.35
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l157
11:
;lex.c: 1162:             skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1163:             if (curchar == '\n') {
ld	a,(_curchar)
cp	.low.10
jp	nz,l158
;lex.c: 1164:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1165:             }
;lex.c: 1166:             continue;
l158:
jp	l139
;lex.c: 1167:         }
;lex.c: 1168:         if ((curchar == ' ') || (curchar == '\t') || (curchar == '\n')) {
l157:
ld	a,(_curchar)
cp	.low.32
jp	nz,30f
jp	31f
30:
ld	a,(_curchar)
cp	.low.9
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(_curchar)
cp	.low.10
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l159
11:
;lex.c: 1169:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1183:             if (cond && curchar != '#' && curchar != 0) {
ld	hl,(_cond)
ld	a,l
or	h
jp	z,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.35
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_curchar)
or	a
jp	lz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l160
11:
;lex.c: 1184:                 if (!(cond->flags &      0x01)) {
ld	hl,(_cond)
bit	0&7,(hl)
jp	nz,l161
;lex.c: 1185:                     skiptoeol();
call	_skiptoeol
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1186:                     if (curchar == '\n') {
ld	a,(_curchar)
cp	.low.10
jp	nz,l162
;lex.c: 1187:                         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1188:                     }
;lex.c: 1189:                 }
l162:
;lex.c: 1190:             }
l161:
;lex.c: 1191:             continue;
l160:
jp	l139
;lex.c: 1192:         }
;lex.c: 1194:         next.lineno = lineno;
l159:
ld	hl,(_lineno)
ld	(_next+1),hl
;lex.c: 1195:         next.filename = filename;
global	_filename
ld	hl,(_filename)
ld	(_next+3),hl
;lex.c: 1196:         if (issym()) {
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l163
;lex.c: 1198:             if (cpppseudofunc()) {
global	_cpppseudofunc
call	_cpppseudofunc
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l164
;lex.c: 1203:                 continue;
jp	l139
;lex.c: 1204:             }
;lex.c: 1205:             if (macexpand(strbuf)) {
l164:
global	_macexpand
ld	hl,_strbuf
push	hl
call	_macexpand
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l165
;lex.c: 1206:                 continue;
jp	l139
;lex.c: 1207:             }
;lex.c: 1208:             advance();
l165:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1209:             t = kwlook((unsigned char *)strbuf, ckw);
global	_ckw
ld	hl,_ckw
push	hl
ld	hl,_strbuf
push	hl
call	_kwlook
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 1210:             if (t != 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	z,l166
;lex.c: 1211:                 next.type = t;
ld	a,(ix+-1)
ld	(_next),a
;lex.c: 1213:                 if (t ==         157) {
ld	a,(ix+-1)
cp	.low.-99
jp	nz,l167
;lex.c: 1215:                     while (curchar == ' ' || curchar == '\t' || curchar == '\n')
jp	l168
l169:
;lex.c: 1216:                         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
l168:
ld	a,(_curchar)
cp	.low.32
jp	z,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.9
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_curchar)
cp	.low.10
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l169
11:
l170:
;lex.c: 1217:                     if (curchar == '{') {
ld	a,(_curchar)
cp	.low.123
jp	nz,l171
;lex.c: 1218:                         int depth = 1;
ld	(ix+-4),.low.1
ld	(ix+1+-4),.high.1
;lex.c: 1219:                         char *p;
;lex.c: 1220:                         bigbuflen = 0;
ld	hl,0
ld	(_bigbuflen),hl
;lex.c: 1221:                         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1223:                         while (depth > 0 && curchar) {
jp	l172
l173:
;lex.c: 1224:                             if (curchar == '{') depth++;
ld	a,(_curchar)
cp	.low.123
jp	nz,l175
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
ld	(ix+-4),l
ld	(ix+1+-4),h
;lex.c: 1225:                             else if (curchar == '}') {
jp	l176
l175:
ld	a,(_curchar)
cp	.low.125
jp	nz,l177
;lex.c: 1226:                                 depth--;
ld	l,(ix+-4)
ld	h,(ix+1+-4)
dec	hl
ld	(ix+-4),l
ld	(ix+1+-4),h
;lex.c: 1227:                                 if (depth == 0) break;
ld	a,(ix+-4)
or	(ix+1+-4)
jp	nz,l178
jp	l174
;lex.c: 1228:                             }
l178:
;lex.c: 1229:                             if (bigbuflen < 1024 - 1)
l177:
l176:
ld	de,1023
ld	hl,(_bigbuflen)
global	wrelop
call	wrelop
jp	age,l179
;lex.c: 1230:                                 bigbuf[bigbuflen++] = curchar;
ld	a,(_curchar)
ld	de,_bigbuf
ld	hl,(_bigbuflen)
inc	hl
ld	(_bigbuflen),hl
dec	hl
add	hl,de
ld	(hl),a
;lex.c: 1231:                             advance();
l179:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1232:                         }
l172:
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,0
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
20:
ld	a,(_curchar)
or	a
jp	lnz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l173
11:
l174:
;lex.c: 1233:                         bigbuf[bigbuflen] = 0;
ld	de,(_bigbuflen)
ld	hl,_bigbuf
add	hl,de
ld	(hl),0
;lex.c: 1234:                         if (curchar == '}') advance();
ld	a,(_curchar)
cp	.low.125
jp	nz,l180
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1236:                         while (bigbuflen > 0 && (bigbuf[bigbuflen-1] == ' ' ||
l180:
jp	l181
l182:
;lex.c: 1237:                                bigbuf[bigbuflen-1] == '\t' ||
;lex.c: 1238:                                bigbuf[bigbuflen-1] == '\n'))
;lex.c: 1239:                             bigbuf[--bigbuflen] = 0;
ld	de,_bigbuf
ld	hl,(_bigbuflen)
dec	hl
ld	(_bigbuflen),hl
add	hl,de
ld	(hl),0
l181:
ld	de,(_bigbuflen)
ld	hl,0
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
20:
ld	de,(_bigbuflen)
ld	hl,_bigbuf+-1
add	hl,de
ld	a,(hl)
cp	.low.32
jp	z,40f
jp	41f
41:
ld	de,(_bigbuflen)
ld	hl,_bigbuf+-1
add	hl,de
ld	a,(hl)
cp	.low.9
jp	z,40f
jp	41f
41:jp	31f
40:jp	30f
31:
ld	de,(_bigbuflen)
ld	hl,_bigbuf+-1
add	hl,de
ld	a,(hl)
cp	.low.10
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l182
11:
l183:
;lex.c: 1241:                         p = bigbuf;
ld	hl,_bigbuf
ld	(ix+-6),l
ld	(ix+1+-6),h
;lex.c: 1242:                         while (*p == ' ' || *p == '\t' || *p == '\n') p++;
jp	l184
l185:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
inc	hl
ld	(ix+-6),l
ld	(ix+1+-6),h
l184:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	a,(hl)
cp	.low.32
jp	z,30f
jp	31f
31:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	a,(hl)
cp	.low.9
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	a,(hl)
cp	.low.10
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l185
11:
l186:
;lex.c: 1243:                         if (p != bigbuf) {
ld	de,_bigbuf
ld	l,(ix+-6)
ld	h,(ix+1+-6)
or	a
sbc	hl,de
jp	z,l187
;lex.c: 1244:                             bigbuflen = strlen(p);
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_bigbuflen),hl
;lex.c: 1245:                             memmove(bigbuf, p, bigbuflen + 1);
global	_memmove
ld	hl,(_bigbuflen)
inc	hl
push	hl
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
ld	hl,_bigbuf
push	hl
call	_memmove
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;lex.c: 1246:                         }
;lex.c: 1248:                         pendingAsm = bigbuf;
l187:
ld	hl,_bigbuf
ld	(_pendingAsm),hl
;lex.c: 1249:                     }
;lex.c: 1250:                 }
l171:
;lex.c: 1251:                 break;
l167:
jp	l141
;lex.c: 1252:             }
;lex.c: 1253:             next.type =     20;
l166:
ld	a,.low.20
ld	(_next),a
;lex.c: 1255:             if (strlen(strbuf) > 16        ) {
ld	hl,_strbuf
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	hl,16
global	wrelop
call	wrelop
jp	lge,l188
;lex.c: 1256:                 gripe(13    );
ld	hl,13
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 1257:                 strbuf[16        ] = '\0';
ld	hl,_strbuf+16
ld	(hl),0
;lex.c: 1258:             }
;lex.c: 1259:             next.v.name = strdup(strbuf);
l188:
global	_strdup
ld	hl,_strbuf
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_next+5),hl
;lex.c: 1260:             break;
jp	l141
;lex.c: 1261:         }
;lex.c: 1262:         {
l163:
;lex.c: 1263:             char numtype = isnumber();
call	_isnumber
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-7),e
;lex.c: 1264:             if (numtype) {
ld	a,(ix+-7)
or	a
jp	az,l189
;lex.c: 1265:                 next.type = (numtype == 2) ? 23 :  21;
ld	a,(ix+-7)
cp	.low.2
jp	z,20f
jp	21f
21:
ld	hl,21
jp	22f
20:
ld	hl,23
22:
ld	a,l
ld	(_next),a
;lex.c: 1266:                 break;
jp	l141
;lex.c: 1267:             }
;lex.c: 1268:         }
l189:
;lex.c: 1270:         bigbuflen = 0;
ld	hl,0
ld	(_bigbuflen),hl
;lex.c: 1271:         if (isstring()) {
call	_isstring
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l190
;lex.c: 1272:             next.type =  22;
ld	a,.low.22
ld	(_next),a
;lex.c: 1275:         concat:
l191:
;lex.c: 1276:             while (curchar == ' ' || curchar == '\t' || curchar == '\n')
jp	l192
l193:
;lex.c: 1277:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
l192:
ld	a,(_curchar)
cp	.low.32
jp	z,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.9
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_curchar)
cp	.low.10
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l193
11:
l194:
;lex.c: 1279:             if (curchar == '/' && nextchar == '*') {
ld	a,(_curchar)
cp	.low.47
jp	nz,20f
jp	21f
21:
ld	a,(_nextchar)
cp	.low.42
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l195
11:
;lex.c: 1280:                 advance(); advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1281:                 while (!(curchar == '*' && nextchar == '/')) {
jp	l196
l197:
;lex.c: 1282:                     if (curchar == 0) break;
ld	a,(_curchar)
or	a
jp	lnz,l199
jp	l198
;lex.c: 1283:                     advance();
l199:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1284:                 }
l196:
ld	a,(_curchar)
cp	.low.42
jp	nz,20f
jp	21f
21:
ld	a,(_nextchar)
cp	.low.47
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l197
11:
l198:
;lex.c: 1285:                 advance(); advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1286:                 goto concat;
jp	l191
;lex.c: 1287:             }
;lex.c: 1289:             if (curchar == '/' && nextchar == '/') {
l195:
ld	a,(_curchar)
cp	.low.47
jp	nz,20f
jp	21f
21:
ld	a,(_nextchar)
cp	.low.47
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l200
11:
;lex.c: 1290:                 while (curchar != '\n' && curchar != 0)
jp	l201
l202:
;lex.c: 1291:                     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
l201:
ld	a,(_curchar)
cp	.low.10
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
or	a
jp	lnz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l202
11:
l203:
;lex.c: 1292:                 goto concat;
jp	l191
;lex.c: 1293:             }
;lex.c: 1295:             while (curchar == '"') {
l200:
jp	l204
l205:
;lex.c: 1296:                 if (!isstring())
call	_isstring
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	anz,l207
;lex.c: 1297:                     break;
jp	l206
;lex.c: 1298:                 goto concat;
l207:
jp	l191
;lex.c: 1299:             }
l204:
ld	a,(_curchar)
cp	.low.34
jp	z,l205
l206:
;lex.c: 1301:             next.v.str = malloc(bigbuflen + 3);
ld	hl,(_bigbuflen)
inc	hl
inc hl
inc hl
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_next+5),hl
;lex.c: 1302:             next.v.str[0] = bigbuflen & 0xff;
ld	a,(_bigbuflen)
ld	hl,(_next+5)
ld	(hl),a
;lex.c: 1303:             next.v.str[1] = (bigbuflen >> 8) & 0xff;
ld	b,.low.8
ld	hl,(_bigbuflen)
global	shar
call	shar
ld	l,l
ld	iy,(_next+5)
ld	(iy+1),l
;lex.c: 1304:             memcpy(next.v.str + 2, bigbuf, bigbuflen + 1);
ld	hl,(_bigbuflen)
inc	hl
push	hl
ld	hl,_bigbuf
push	hl
ld	hl,(_next+5)
inc	hl
inc hl
push	hl
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;lex.c: 1305:             break;
jp	l141
;lex.c: 1306:         }
;lex.c: 1309:         t = lookupc(simpleChars, curchar);
l190:
global	_lookupc
ld	a,(_curchar)
ld	c,a
push	bc
ld	hl,_simpleChars
push	hl
call	_lookupc
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 1310:         if (t == 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	nz,l208
;lex.c: 1311:             gripe(    10      );
ld	hl,10
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 1312:             curchar= ';';
ld	a,.low.59
ld	(_curchar),a
;lex.c: 1313:             t = lookupc(simpleChars, ';');
ld	l,.low.59
push	hl
ld	hl,_simpleChars
push	hl
call	_lookupc
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 1314:         }
;lex.c: 1316:         next.type = simpleToks[t];
l208:
ld	e,(ix+-1)
ld	d,0
ld	hl,_simpleToks
add	hl,de
ld	a,(hl)
ld	(_next),a
;lex.c: 1317:         c = curchar;
ld	a,(_curchar)
ld	(ix+-2),a
;lex.c: 1318:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1321:         if (c == '.' && curchar == '.') {
ld	a,(ix+-2)
cp	.low.46
jp	nz,20f
jp	21f
21:
ld	a,(_curchar)
cp	.low.46
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l209
11:
;lex.c: 1322:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1323:             if (curchar == '.') {
ld	a,(_curchar)
cp	.low.46
jp	nz,l210
;lex.c: 1324:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1325:                 next.type = 92;
ld	a,.low.92
ld	(_next),a
;lex.c: 1326:             }
;lex.c: 1328:         }
l210:
;lex.c: 1331:         if (curchar == c) {
l209:
ld	a,(_curchar)
ld	e,a
ld	a,(ix+-2)
cp	e
jp	nz,l211
;lex.c: 1332:             t = lookupc(dblChars, c);
ld	l,(ix+-2)
push	hl
ld	hl,_dblChars
push	hl
call	_lookupc
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 1333:             if (t != 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	z,l212
;lex.c: 1334:                 next.type = dbltok[t];
ld	e,(ix+-1)
ld	d,0
ld	hl,_dbltok
add	hl,de
ld	a,(hl)
ld	(_next),a
;lex.c: 1335:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1337:                 if (curchar == '=' && (c == '>' || c == '<')) {
ld	a,(_curchar)
cp	.low.61
jp	nz,20f
jp	21f
21:
ld	a,(ix+-2)
cp	.low.62
jp	nz,30f
jp	31f
30:
ld	a,(ix+-2)
cp	.low.60
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l213
11:
;lex.c: 1338:                     next.type = (c == '>') ? 75 : 76;
ld	a,(ix+-2)
cp	.low.62
jp	z,20f
jp	21f
21:
ld	hl,76
jp	22f
20:
ld	hl,75
22:
ld	a,l
ld	(_next),a
;lex.c: 1339:                     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1340:                 }
;lex.c: 1341:             }
l213:
;lex.c: 1342:         }
l212:
;lex.c: 1345:         if (curchar == '=') {
l211:
ld	a,(_curchar)
cp	.low.61
jp	nz,l214
;lex.c: 1346:             t = lookupc(eqChars, c);
ld	l,(ix+-2)
push	hl
ld	hl,_eqChars
push	hl
call	_lookupc
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	(ix+-1),e
;lex.c: 1347:             if (t != 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	z,l215
;lex.c: 1348:                 next.type = eqtok[t];
ld	e,(ix+-1)
ld	d,0
ld	hl,_eqtok
add	hl,de
ld	a,(hl)
ld	(_next),a
;lex.c: 1349:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1350:             }
;lex.c: 1351:         }
l215:
;lex.c: 1352:         if ((c == '-') && (curchar == '>')) {
l214:
ld	a,(ix+-2)
cp	.low.45
jp	nz,20f
jp	21f
21:
ld	a,(_curchar)
cp	.low.62
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l216
11:
;lex.c: 1353:             next.type =   50;
ld	a,.low.50
ld	(_next),a
;lex.c: 1354:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1355:         }
;lex.c: 1356:         break;
l216:
jp	l141
;lex.c: 1357:     }
l139:
ld	a,.low.1
or	.low.0
jp	nz,l140
l141:
;lex.c: 1359:     lineend = 0;
ld	a,.low.0
ld	(_lineend),a
;lex.c: 1366:     return;
jp	l136
;lex.c: 1367: }
l136:
jp	cret
f138	equ	-7
;lex.c: 1414: char
;lex.c: 1415: cpppseudofunc()
;lex.c: 1416: {
_cpppseudofunc:
call	ncsv
defw	f144
;lex.c: 1417:     int r = 0;
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
;lex.c: 1419:     if ((strcmp("defined", strbuf) == 0) && (tflags &  0x02      )) {
global	_strcmp
ld	hl,_strbuf
push	hl
ld	hl,49f
push	hl
call	_strcmp
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	nz,20f
jp	21f
21:
ld	a,(_tflags)
bit	1&7,a
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l218
11:
;lex.c: 1420:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1421:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1422:         if (curchar != '(') {
ld	a,(_curchar)
cp	.low.40
jp	z,l219
;lex.c: 1423:             gripe(    11      );
ld	hl,11
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 1424:             curchar = '0';
ld	a,.low.48
ld	(_curchar),a
;lex.c: 1425:             return 1;
ld	l,.low.1
jp	l217
;lex.c: 1426:         }
;lex.c: 1427:         advance();
l219:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1428:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1429:         if (issym()) {
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l220
;lex.c: 1430:             r = (maclookup(strbuf) != 0);
ld	hl,_strbuf
push	hl
call	_maclookup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
ld	hl,1
jp	nz,10f
dec	hl
10:
ld	(ix+-2),l
ld	(ix+1+-2),h
;lex.c: 1431:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1432:         }
;lex.c: 1433:         skipws1();
l220:
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1434:         if (curchar != ')') {
ld	a,(_curchar)
cp	.low.41
jp	z,l221
;lex.c: 1435:             gripe(    11      );
ld	hl,11
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;lex.c: 1436:             r = 0;
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
;lex.c: 1437:         } else {
jp	l222
l221:
;lex.c: 1438:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1439:         }
l222:
;lex.c: 1440:         curchar = r ? '1' : '0';
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,20f
jp	21f
21:
ld	hl,48
jp	22f
20:
ld	hl,49
22:
ld	a,l
ld	(_curchar),a
;lex.c: 1441:         return 1;
ld	l,.low.1
jp	l217
;lex.c: 1442:     }
;lex.c: 1443:     return 0;
l218:
ld	l,.low.0
jp	l217
;lex.c: 1444: }
l217:
jp	cret
f144	equ	-2
;lex.c: 1500: unsigned long
;lex.c: 1501: readcppconst()
;lex.c: 1502: {
_readcppconst:
call	ncsv
defw	f145
;lex.c: 1503:     unsigned long val;
;lex.c: 1504:     char savedtflags = tflags;
ld	a,(_tflags)
ld	(ix+-5),a
;lex.c: 1505:     struct token saved_cur;
;lex.c: 1507:     memcpy(&saved_cur, &cur, sizeof(cur));
ld	hl,9
push	hl
ld	hl,_cur
push	hl
push	ix
pop	de
ld	hl,-14
add	hl,de
push	hl
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;lex.c: 1509:     tflags =   0x01       |  0x02      ;
ld	a,.low.3
ld	(_tflags),a
;lex.c: 1512:     skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1523:     gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1532:     gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;lex.c: 1542:     val = parseConst(   1);
global	_parseConst
ld	l,.low.1
push	hl
call	_parseConst
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
;lex.c: 1550:     memcpy(&cur, &saved_cur, sizeof(cur));
ld	hl,9
push	hl
push	ix
pop	de
ld	hl,-14
add	hl,de
push	hl
ld	hl,_cur
push	hl
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;lex.c: 1551:     tflags = savedtflags;
ld	a,(ix+-5)
ld	(_tflags),a
;lex.c: 1552:     lineend = 0;
ld	a,.low.0
ld	(_lineend),a
;lex.c: 1563:     return val;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
jp	l223
;lex.c: 1564: }
l223:
jp	cret
f145	equ	-14
psect	data
19:
defb	37,108,100,0
29:
defb	32,32,73,100,101,110,116,105,102,105,101,114,32,39,37,115
defb	39,32,101,120,99,101,101,100,115,32,49,52,32,99,104,97
defb	114,97,99,116,101,114,32,108,105,109,105,116,10,0
39:
defb	115,116,114,105,110,103,32,116,111,111,32,108,111,110,103,0
49:
defb	100,101,102,105,110,101,100,0
psect	bss
_bigbuf:
	defs	1024
_strbuf:
	defs	256
_next:
	defs	9
_cond:
	defs	2
_termin:
	defs	1
_bigbuflen:
	defs	2
_tflags:
	defs	1
_cur:
	defs	9
