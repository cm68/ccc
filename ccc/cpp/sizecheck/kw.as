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
;kw.c: 31: unsigned char cppkw[] = {
psect	data
global	_cppkw
_cppkw:
;kw.c: 32:     'd', 'e', 'f', 'i', 'n', 'e', 0xff, 241,
defb	100
defb	101
defb	102
defb	105
defb	110
defb	101
defb	-1
defb	-15
;kw.c: 33:     'e'|0x80, 17, 'l'|0x80, 9, 's'|0x80, 3, 'e', 0xff, 248,
defb	-27
defb	17
defb	-20
defb	9
defb	-13
defb	3
defb	101
defb	-1
defb	-8
;kw.c: 34:             'i', 'f', 0xff, 247,
defb	105
defb	102
defb	-1
defb	-9
;kw.c: 35:         'n', 'd', 'i', 'f', 0xff, 246,
defb	110
defb	100
defb	105
defb	102
defb	-1
defb	-10
;kw.c: 36:     'i'|0x80, 25, 'f'|0x80, 15, 0xfe, 243,
defb	-23
defb	25
defb	-26
defb	15
defb	-2
defb	-13
;kw.c: 37:             'd'|0x80, 4, 'e', 'f', 0xff, 244,
defb	-28
defb	4
defb	101
defb	102
defb	-1
defb	-12
;kw.c: 38:             'n'|0x80, 5, 'd', 'e', 'f', 0xff, 245,
defb	-18
defb	5
defb	100
defb	101
defb	102
defb	-1
defb	-11
;kw.c: 39:         'n', 'c', 'l', 'u', 'd', 'e', 0xff, 240,
defb	110
defb	99
defb	108
defb	117
defb	100
defb	101
defb	-1
defb	-16
;kw.c: 40:     'u', 'n', 'd', 'e', 'f', 0xff, 242,
defb	117
defb	110
defb	100
defb	101
defb	102
defb	-1
defb	-14
;kw.c: 41:     0
;kw.c: 42: };
defb	0
;kw.c: 202: unsigned char
;kw.c: 203: kwlook(unsigned char *str, unsigned char *table)
;kw.c: 204: {
psect	text
global	_kwlook
_kwlook:
global	ncsv, cret, indir
call	ncsv
defw	f103
;kw.c: 205: 	unsigned char c;
;kw.c: 206: 	unsigned char *s = str;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(ix+-3),l
ld	(ix+1+-3),h
;kw.c: 208: 	while (1) {
jp	l8
l9:
;kw.c: 209: 		c = *table;
ld	l,(ix+8)
ld	h,(ix+1+8)
ld	l,(hl)
ld	(ix+-1),l
;kw.c: 210: 		if (c == 0) {
ld	a,(ix+-1)
or	a
jp	lnz,l11
;kw.c: 211: 			return 0xff;
ld	l,.low.-1
jp	l7
;kw.c: 212: 		}
;kw.c: 213: 		if (c == 0xff || c == 0xfe) {
l11:
ld	a,(ix+-1)
cp	.low.-1
jp	nz,20f
jp	21f
20:
ld	a,(ix+-1)
cp	.low.-2
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l12
11:
;kw.c: 215: 			if (*s == 0) {
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	a,(hl)
or	a
jp	lnz,l13
;kw.c: 216: 				return table[1];
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
ld	l,(hl)
jp	l7
;kw.c: 217: 			}
;kw.c: 219: 			table += 2;
l13:
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
inc hl
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 220: 			if (c == 0xff) {
ld	a,(ix+-1)
cp	.low.-1
jp	nz,l14
;kw.c: 222: 				s = str;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(ix+-3),l
ld	(ix+1+-3),h
;kw.c: 223: 			}
;kw.c: 225: 			continue;
l14:
jp	l8
;kw.c: 226: 		}
;kw.c: 227: 		if (c & 0x80) {
l12:
bit	7&7,(ix+-1)
jp	z,l15
;kw.c: 229: 			if (*s == (c & 0x7f)) {
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	a,(ix+-1)
and	.low.127
cp	(hl)
jp	nz,l16
;kw.c: 230: 				s++;
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
;kw.c: 231: 				table += 2;
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
inc hl
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 232: 			} else {
jp	l17
l16:
;kw.c: 234: 				table += table[1] + 2;
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
ld	e,(hl)
ld	d,0
inc	de
inc de
ld	l,(ix+8)
ld	h,(ix+1+8)
add	hl,de
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 235: 			}
l17:
;kw.c: 236: 			continue;
jp	l8
;kw.c: 237: 		}
;kw.c: 239: 		if (c != *s) {
l15:
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	a,(ix+-1)
cp	(hl)
jp	z,l18
;kw.c: 241: 			while (*table != 0xff && *table != 0xfe && *table != 0) {
jp	l19
l20:
;kw.c: 242: 				if (*table & 0x80) {
ld	l,(ix+8)
ld	h,(ix+1+8)
bit	7&7,(hl)
jp	z,l22
;kw.c: 244: 					table += 2;
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
inc hl
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 245: 				} else {
jp	l23
l22:
;kw.c: 246: 					table++;
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 247: 				}
l23:
;kw.c: 248: 			}
l19:
ld	l,(ix+8)
ld	h,(ix+1+8)
ld	a,(hl)
cp	.low.-1
jp	nz,30f
jp	31f
30:
ld	l,(ix+8)
ld	h,(ix+1+8)
ld	a,(hl)
cp	.low.-2
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	l,(ix+8)
ld	h,(ix+1+8)
ld	a,(hl)
or	a
jp	lnz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l20
11:
l21:
;kw.c: 250: 			if (*table == 0) {
ld	l,(ix+8)
ld	h,(ix+1+8)
ld	a,(hl)
or	a
jp	lnz,l24
;kw.c: 251: 				return 0xff;
ld	l,.low.-1
jp	l7
;kw.c: 252: 			}
;kw.c: 254: 			table += 2;
l24:
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
inc hl
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 255: 			s = str;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(ix+-3),l
ld	(ix+1+-3),h
;kw.c: 256: 			continue;
jp	l8
;kw.c: 257: 		}
;kw.c: 258: 		s++;
l18:
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(ix+-3),l
ld	(ix+1+-3),h
;kw.c: 259: 		table++;
ld	l,(ix+8)
ld	h,(ix+1+8)
inc	hl
ld	(ix+8),l
ld	(ix+1+8),h
;kw.c: 260: 	}
l8:
ld	a,.low.1
or	.low.0
jp	nz,l9
l10:
;kw.c: 261: }
l7:
jp	cret
f103	equ	-3
