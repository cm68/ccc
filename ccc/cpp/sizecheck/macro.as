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
;macro.c: 16: char *macbuffer;
;macro.c: 17: struct macro *macros;
;macro.c: 45: void
;macro.c: 46: addDefine(char *s)
;macro.c: 47: {
psect	text
global	_addDefine
_addDefine:
global	ncsv, cret, indir
call	ncsv
defw	f104
;macro.c: 48:     struct macro *m;
;macro.c: 49:     char *eq;
;macro.c: 50:     unsigned char namelen;
;macro.c: 53:     if (!*s) {
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
or	a
jp	anz,l8
;macro.c: 54:         return;
jp	l7
;macro.c: 55:     }
;macro.c: 57:     m = malloc(sizeof(*m));
l8:
global	_malloc
ld	hl,9
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-2),l
ld	(ix+1+-2),h
;macro.c: 60:     eq = strchr(s, '=');
global	_strchr
ld	hl,61
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_strchr
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-4),l
ld	(ix+1+-4),h
;macro.c: 62:     if (eq) {
ld	a,(ix+-4)
or	(ix+1+-4)
jp	z,l9
;macro.c: 64:         namelen = eq - s;
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
or	a
sbc	hl,de
ld	l,l
ld	(ix+-5),l
;macro.c: 65:         m->name = malloc(namelen + 1);
ld	l,(ix+-5)
ld	h,0
inc	hl
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 66:         memcpy(m->name, s, namelen);
global	_memcpy
ld	l,(ix+-5)
ld	h,0
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;macro.c: 67:         m->name[namelen] = '\0';
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+-5)
ld	h,0
add	hl,bc
ld	(hl),0
;macro.c: 68:         m->mactext = strdup(eq + 1);
global	_strdup
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 69:     } else {
jp	l10
l9:
;macro.c: 71:         m->name = strdup(s);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 72:         m->mactext = strdup("1");
ld	hl,19f
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 73:     }
l10:
;macro.c: 75:     m->parmcount = 0;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),0
;macro.c: 76:     m->parms = 0;
ld	de,0
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 77:     m->next = macros;
global	_macros
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	de,(_macros)
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 78:     macros = m;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_macros),hl
;macro.c: 79: }
l7:
jp	cret
f104	equ	-5
;macro.c: 104: struct macro *
;macro.c: 105: maclookup(char *name)
;macro.c: 106: {
global	_maclookup
_maclookup:
call	ncsv
defw	f105
;macro.c: 107:     struct macro *m;
;macro.c: 110:     for (m = macros; m; m = m->next) {
ld	hl,(_macros)
ld	(ix+-2),l
ld	(ix+1+-2),h
jp	l15
l12:
;macro.c: 111:         if (strcmp(m->name, name) == 0) {
global	_strcmp
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
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
jp	nz,l16
;macro.c: 112:             return m;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l11
;macro.c: 113:         }
;macro.c: 114:     }
l16:
l14:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-2),c
ld	(ix+1+-2),b
l15:
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l12
l13:
;macro.c: 115:     return 0;
ld	hl,0
jp	l11
;macro.c: 116: }
l11:
jp	cret
f105	equ	-2
;macro.c: 142: void
;macro.c: 143: macundefine(char *s)
;macro.c: 144: {
global	_macundefine
_macundefine:
call	ncsv
defw	f106
;macro.c: 145:     unsigned char i;
;macro.c: 146:     struct macro *m, *p;
;macro.c: 148:     m = maclookup(s);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_maclookup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-3),l
ld	(ix+1+-3),h
;macro.c: 149:     if (!m) {
ld	a,(ix+-3)
or	(ix+1+-3)
jp	nz,l18
;macro.c: 150:         return;
jp	l17
;macro.c: 151:     }
;macro.c: 153:     if (m == macros) {
l18:
ld	de,(_macros)
ld	l,(ix+-3)
ld	h,(ix+1+-3)
or	a
sbc	hl,de
jp	nz,l19
;macro.c: 154:         macros = m->next;
ld	e,(ix+-3)
ld	d,(ix+1+-3)
ld	hl,7
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_macros),bc
;macro.c: 155:     } else {
jp	l20
l19:
;macro.c: 156:         for (p = macros; p->next != m; p = p->next) ;
ld	hl,(_macros)
ld	(ix+-5),l
ld	(ix+1+-5),h
jp	l24
l21:
l23:
ld	e,(ix+-5)
ld	d,(ix+1+-5)
ld	hl,7
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-5),c
ld	(ix+1+-5),b
l24:
ld	e,(ix+-5)
ld	d,(ix+1+-5)
ld	hl,7
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	e,(ix+-3)
ld	d,(ix+1+-3)
or	a
sbc	hl,de
jp	nz,l21
l22:
;macro.c: 157:         p->next = m->next;
ld	e,(ix+-3)
ld	d,(ix+1+-3)
ld	hl,7
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-5)
ld	d,(ix+1+-5)
ld	hl,7
add	hl,de
ld	(hl),c
inc	hl
ld	(hl),b
;macro.c: 158:     }
l20:
;macro.c: 159:     for (i = 0; i < m->parmcount; i++) {
ld	(ix+-1),0
jp	l28
l25:
;macro.c: 160:         free(m->parms[i]);
global	_free
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+-1)
ld	h,0
add	hl,hl
add	hl,bc
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 161:     }
l27:
inc	(ix+-1)
l28:
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	e,(hl)
ld	d,0
ld	l,(ix+-1)
ld	h,0
global	wrelop
call	wrelop
jp	llt,l25
l26:
;macro.c: 162:     free(m->parms);
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 163:     free(m->name);
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 164:     free(m->mactext);
ld	e,(ix+-3)
ld	d,(ix+1+-3)
ld	hl,5
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 165:     free(m);
ld	l,(ix+-3)
ld	h,(ix+1+-3)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 166: }
l17:
jp	cret
f106	equ	-5
;macro.c: 209: void
;macro.c: 210: macdefine(char *s)
;macro.c: 211: {
global	_macdefine
_macdefine:
call	ncsv
defw	f107
;macro.c: 212:     unsigned char i;
;macro.c: 213:     struct macro *m = malloc(sizeof(*m));
ld	hl,9
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-3),l
ld	(ix+1+-3),h
;macro.c: 214:     char *parms[10         ];
;macro.c: 217:     if (!macbuffer) {
global	_macbuffer
ld	hl,(_macbuffer)
ld	a,l
or	h
jp	nz,l30
;macro.c: 218:         macbuffer = malloc(1024);
ld	hl,1024
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_macbuffer),hl
;macro.c: 219:     }
;macro.c: 221:     m->name = strdup(s);
l30:
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 222:     m->parmcount = 0;
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	(hl),0
;macro.c: 223:     m->parms = ((void *)0);
ld	de,0
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 230:     if (curchar == '(') {
global	_curchar
ld	a,(_curchar)
cp	.low.40
jp	nz,l31
;macro.c: 231:         advance();
global	_advance
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 232:         while (1) {
jp	l32
l33:
;macro.c: 233:             skipws1();
global	_skipws1
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 234:             if (issym()) {
global	_issym
call	_issym
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
or	a
jp	az,l35
;macro.c: 235:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 236:                 parms[m->parmcount++] = strdup(s);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
push	ix
pop	de
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	a,(hl)
inc	(hl)
ld	l,a
ld	h,0
add	hl,hl
add	hl,de
ld	de,-23
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 237:                 skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 238:                 if (curchar == ',') {
ld	a,(_curchar)
cp	.low.44
jp	nz,l36
;macro.c: 239:                     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 240:                     continue;
jp	l32
;macro.c: 241:                 }
;macro.c: 242:             }
l36:
;macro.c: 243:             if (curchar == ')') {
l35:
ld	a,(_curchar)
cp	.low.41
jp	nz,l37
;macro.c: 244:                 break;
jp	l34
;macro.c: 245:             }
;macro.c: 246:             gripe(    11      );
l37:
global	_gripe
ld	hl,11
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 247:             break;
jp	l34
;macro.c: 248:         }
l32:
ld	a,.low.1
or	.low.0
jp	nz,l33
l34:
;macro.c: 249:         if (m->parmcount) {
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	a,(hl)
or	a
jp	lz,l38
;macro.c: 250:             m->parms = malloc(sizeof(char *) * m->parmcount);
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	l,(hl)
ld	h,0
add	hl,hl
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 251:             for (i = 0; i < m->parmcount; i++) {
ld	(ix+-1),0
jp	l42
l39:
;macro.c: 252:                 m->parms[i] = parms[i];
push	ix
pop	de
ld	l,(ix+-1)
ld	h,0
add	hl,hl
add	hl,de
ld	de,-23
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+-1)
ld	h,0
add	hl,hl
add	hl,bc
pop	bc
ld	(hl),c
inc	hl
ld	(hl),b
;macro.c: 253:             }
l41:
inc	(ix+-1)
l42:
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	e,(hl)
ld	d,0
ld	l,(ix+-1)
ld	h,0
global	wrelop
call	wrelop
jp	llt,l39
l40:
;macro.c: 254:         } else {
jp	l43
l38:
;macro.c: 256:             m->parms = (char **)1;
ld	de,1
ld	l,(ix+-3)
ld	h,(ix+1+-3)
inc	hl
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 257:         }
l43:
;macro.c: 258:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 259:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 260:     } else {
jp	l44
l31:
;macro.c: 262:         skipws1();
call	_skipws1
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 263:     }
l44:
;macro.c: 264:     s = macbuffer;
ld	hl,(_macbuffer)
ld	(ix+6),l
ld	(ix+1+6),h
;macro.c: 267:     while (curchar != '\n') {
jp	l45
l46:
;macro.c: 269:         if (curchar == '/' && nextchar == '/') {
global	_nextchar
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
jp	l48
11:
;macro.c: 271:             while (curchar != '\n') {
jp	l49
l50:
;macro.c: 272:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 273:             }
l49:
ld	a,(_curchar)
cp	.low.10
jp	nz,l50
l51:
;macro.c: 274:             break;
jp	l47
;macro.c: 275:         }
;macro.c: 277:         if (curchar == '/' && nextchar == '*') {
l48:
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
jp	l52
11:
;macro.c: 279:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 280:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 281:             while (1) {
jp	l53
l54:
;macro.c: 282:                 if (curchar == '*' && nextchar == '/') {
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
jp	l56
11:
;macro.c: 283:                     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 284:                     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 285:                     break;
jp	l55
;macro.c: 286:                 }
;macro.c: 287:                 if (curchar == 0) {
l56:
ld	a,(_curchar)
or	a
jp	lnz,l57
;macro.c: 289:                     break;
jp	l55
;macro.c: 290:                 }
;macro.c: 292:                 advance();
l57:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 293:             }
l53:
ld	a,.low.1
or	.low.0
jp	nz,l54
l55:
;macro.c: 295:             if (s > macbuffer && s[-1] != ' ' && s[-1] != '\t') {
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,(_macbuffer)
global	wrelop
call	wrelop
jp	lge,30f
jp	31f
31:
ld	l,(ix+6)
ld	h,(ix+1+6)
dec	hl
ld	a,(hl)
cp	.low.32
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	l,(ix+6)
ld	h,(ix+1+6)
dec	hl
ld	a,(hl)
cp	.low.9
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l58
11:
;macro.c: 296:                 *s++ = ' ';
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
dec	hl
ld	(hl),32
;macro.c: 297:             }
;macro.c: 298:             continue;
l58:
jp	l45
;macro.c: 299:         }
;macro.c: 300:         if ((curchar == '\\') && (nextchar == '\n')) {
l52:
ld	a,(_curchar)
cp	.low.92
jp	nz,20f
jp	21f
21:
ld	a,(_nextchar)
cp	.low.10
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l59
11:
;macro.c: 301:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 302:             curchar = ' ';
ld	a,.low.32
ld	(_curchar),a
;macro.c: 303:         }
;macro.c: 304:         *s++ = curchar;
l59:
ld	a,(_curchar)
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
dec	hl
ld	(hl),a
;macro.c: 305:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 306:     }
l45:
ld	a,(_curchar)
cp	.low.10
jp	nz,l46
l47:
;macro.c: 307:     *s = 0;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(hl),0
;macro.c: 310:     while (s > macbuffer && (s[-1] == ' ' || s[-1] == '\t')) {
jp	l60
l61:
;macro.c: 311:         s--;
ld	l,(ix+6)
ld	h,(ix+1+6)
dec	hl
ld	(ix+6),l
ld	(ix+1+6),h
;macro.c: 312:         *s = 0;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(hl),0
;macro.c: 313:     }
l60:
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,(_macbuffer)
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
20:
ld	l,(ix+6)
ld	h,(ix+1+6)
dec	hl
ld	a,(hl)
cp	.low.32
jp	z,30f
jp	31f
31:
ld	l,(ix+6)
ld	h,(ix+1+6)
dec	hl
ld	a,(hl)
cp	.low.9
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l61
11:
l62:
;macro.c: 315:     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 316:     m->mactext = strdup(macbuffer);
ld	hl,(_macbuffer)
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	e,(ix+-3)
ld	d,(ix+1+-3)
ld	hl,5
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 317:     m->next = macros;
ld	e,(ix+-3)
ld	d,(ix+1+-3)
ld	hl,7
add	hl,de
ld	de,(_macros)
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 318:     macros = m;
ld	l,(ix+-3)
ld	h,(ix+1+-3)
ld	(_macros),hl
;macro.c: 319: }
l29:
jp	cret
f107	equ	-23
;macro.c: 383: char
;macro.c: 384: macexpand(char *s)
;macro.c: 385: {
global	_macexpand
_macexpand:
call	ncsv
defw	f110
;macro.c: 386:     struct macro *m;
;macro.c: 387:     unsigned char plevel;
;macro.c: 388:     char *d;
;macro.c: 389:     unsigned char args;
;macro.c: 390:     char *parms[10         ];
;macro.c: 391:     unsigned char c;
;macro.c: 392:     char *n;
;macro.c: 393:     unsigned char i;
;macro.c: 394:     char stringify = 0;
ld	(ix+-31),0
;macro.c: 396:     if (!macbuffer) {
ld	hl,(_macbuffer)
ld	a,l
or	h
jp	nz,l64
;macro.c: 397:         macbuffer = malloc(1024);
ld	hl,1024
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_macbuffer),hl
;macro.c: 398:     }
;macro.c: 400:     m = maclookup(s);
l64:
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_maclookup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-2),l
ld	(ix+1+-2),h
;macro.c: 401:     if (!m) {
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l65
;macro.c: 402:         return 0;
ld	l,.low.0
jp	l63
;macro.c: 403:     }
;macro.c: 405:     args = 0;
l65:
ld	(ix+-6),0
;macro.c: 406:     d = macbuffer;
ld	hl,(_macbuffer)
ld	(ix+-5),l
ld	(ix+1+-5),h
;macro.c: 407:     plevel = 0;
ld	(ix+-3),0
;macro.c: 413:     if (m->parms != ((void *)0)) {
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	a,(hl)
inc	hl
or	(hl)
jp	z,l66
;macro.c: 415:         while ( ((nextchar) == ' ' || (nextchar) == '\t' || (nextchar) == '\r')) {
jp	l67
l68:
;macro.c: 416:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 417:         }
l67:
ld	a,(_nextchar)
cp	.low.32
jp	z,30f
jp	31f
31:
ld	a,(_nextchar)
cp	.low.9
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	a,(_nextchar)
cp	.low.13
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l68
11:
l69:
;macro.c: 418:         if (nextchar != '(') {
ld	a,(_nextchar)
cp	.low.40
jp	z,l70
;macro.c: 420:             return 0;
ld	l,.low.0
jp	l63
;macro.c: 421:         }
;macro.c: 422:         advance();
l70:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 423:         plevel = 1;
ld	(ix+-3),1
;macro.c: 424:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 425:         skipws();
global	_skipws
call	_skipws
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 426:         while (1) {
jp	l71
l72:
;macro.c: 430:             if (curchar == '\'' || curchar == '\"') {
ld	a,(_curchar)
cp	.low.39
jp	nz,20f
jp	21f
20:
ld	a,(_curchar)
cp	.low.34
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l74
11:
;macro.c: 431:                 c = curchar;
ld	a,(_curchar)
ld	(ix+-27),a
;macro.c: 432:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 433:                 *d++ = c;
ld	a,(ix+-27)
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 434:                 while (curchar != c) {
jp	l75
l76:
;macro.c: 435:                     *d++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 436:                     if (curchar == '\\') {
ld	a,(_curchar)
cp	.low.92
jp	nz,l78
;macro.c: 437:                         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 438:                         *d++ = curchar;
ld	a,(_curchar)
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 439:                     }
;macro.c: 440:                     advance();
l78:
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 441:                 }
l75:
ld	a,(_curchar)
ld	e,a
ld	a,(ix+-27)
cp	e
jp	nz,l76
l77:
;macro.c: 442:             }
;macro.c: 443:             if (curchar == '(') {
l74:
ld	a,(_curchar)
cp	.low.40
jp	nz,l79
;macro.c: 444:                 plevel++;
inc	(ix+-3)
;macro.c: 445:             }
;macro.c: 446:             if (curchar == ')') {
l79:
ld	a,(_curchar)
cp	.low.41
jp	nz,l80
;macro.c: 447:                 plevel--;
dec	(ix+-3)
;macro.c: 448:             }
;macro.c: 452:             if (((plevel == 1) && (curchar == ',')) ||
l80:
;macro.c: 453:                 ((plevel == 0) && (curchar == ')'))) {
ld	a,(ix+-3)
cp	.low.1
jp	nz,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.44
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+-3)
or	a
jp	lnz,30f
jp	31f
31:
ld	a,(_curchar)
cp	.low.41
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l81
11:
;macro.c: 454:                 *d++ = 0;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),0
;macro.c: 457:                 if (d > macbuffer + 1 || args > 0) {
ld	e,(ix+-5)
ld	d,(ix+1+-5)
ld	hl,(_macbuffer)
inc	hl
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
20:
ld	e,(ix+-6)
ld	d,0
ld	hl,0
global	wrelop
call	wrelop
jp	lge,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l82
11:
;macro.c: 458:                     parms[args++] = strdup(macbuffer);
ld	hl,(_macbuffer)
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
push	ix
pop	de
ld	a,(ix+-6)
inc	(ix+-6)
ld	l,a
ld	h,0
add	hl,hl
add	hl,de
ld	de,-26
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;macro.c: 459:                 }
;macro.c: 460:                 if (curchar == ')') {
l82:
ld	a,(_curchar)
cp	.low.41
jp	nz,l83
;macro.c: 461:                     break;
jp	l73
;macro.c: 462:                 }
;macro.c: 463:                 d = macbuffer;
l83:
ld	hl,(_macbuffer)
ld	(ix+-5),l
ld	(ix+1+-5),h
;macro.c: 464:                 advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 465:                 skipws();
call	_skipws
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 466:                 continue;
jp	l71
;macro.c: 467:             }
;macro.c: 468:             *d++ = curchar;
l81:
ld	a,(_curchar)
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 469:             *d = 0;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
ld	(hl),0
;macro.c: 470:             advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;macro.c: 471:         }
l71:
ld	a,.low.1
or	.low.0
jp	nz,l72
l73:
;macro.c: 472:     }
;macro.c: 474:     if (args != m->parmcount) {
l66:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(ix+-6)
cp	(hl)
jp	z,l84
;macro.c: 475:         gripe(    12      );
ld	hl,12
push	hl
call	_gripe
ld	hl,2
add	hl,sp
ld	sp,hl
;macro.c: 476:         return 0;
ld	l,.low.0
jp	l63
;macro.c: 477:     }
;macro.c: 483:     d = macbuffer;
l84:
ld	hl,(_macbuffer)
ld	(ix+-5),l
ld	(ix+1+-5),h
;macro.c: 484:     *d = '\0';
ld	l,(ix+-5)
ld	h,(ix+1+-5)
ld	(hl),0
;macro.c: 485:     s = m->mactext;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+6),c
ld	(ix+1+6),b
;macro.c: 487:     while (*s) {
jp	l85
l86:
;macro.c: 488:         c = *s;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	l,(hl)
ld	(ix+-27),l
;macro.c: 490:         if ((c == '\'') || (c == '\"')) {
ld	a,(ix+-27)
cp	.low.39
jp	nz,20f
jp	21f
20:
ld	a,(ix+-27)
cp	.low.34
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l88
11:
;macro.c: 491:             *d++ = *s++;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 492:             while (*s != c) {
jp	l89
l90:
;macro.c: 494:                 if (*s == '\\' && s[1] == c) {
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
cp	.low.92
jp	nz,20f
jp	21f
21:
ld	e,(ix+-27)
ld	d,0
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
ld	l,(hl)
ld	h,0
or	a
sbc	hl,de
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l92
11:
;macro.c: 495:                     *d++ = *s++;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 496:                 }
;macro.c: 497:                 *d++ = *s++;
l92:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 498:             }
l89:
ld	e,(ix+-27)
ld	d,0
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	l,(hl)
ld	h,0
or	a
sbc	hl,de
jp	nz,l90
l91:
;macro.c: 499:             *d++ = *s++;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 500:             continue;
jp	l85
;macro.c: 501:         }
;macro.c: 504:         stringify = 0;
l88:
ld	(ix+-31),0
;macro.c: 505:         if (c == '#') {
ld	a,(ix+-27)
cp	.low.35
jp	nz,l93
;macro.c: 506:             c = *++s;
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(hl)
ld	(ix+-27),l
;macro.c: 507:             if (c == '#') {
ld	a,(ix+-27)
cp	.low.35
jp	nz,l94
;macro.c: 508:                 c = *++s;
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(hl)
ld	(ix+-27),l
;macro.c: 509:             } else {
jp	l95
l94:
;macro.c: 510:                 stringify = 1;
ld	(ix+-31),1
;macro.c: 511:             }
l95:
;macro.c: 512:         }
;macro.c: 515:         if (((c >= 'A') && (c <= 'Z')) || 
l93:
;macro.c: 516:             ((c >= 'a') && (c <= 'z')) ||
;macro.c: 517:             (c == '_')) {
ld	de,65
ld	l,(ix+-27)
ld	h,0
global	wrelop
call	wrelop
jp	llt,40f
jp	41f
41:
ld	e,(ix+-27)
ld	d,0
ld	hl,90
global	wrelop
call	wrelop
jp	llt,40f
jp	41f
41:jp	31f
40:jp	30f
30:
ld	de,97
ld	l,(ix+-27)
ld	h,0
global	wrelop
call	wrelop
jp	llt,40f
jp	41f
41:
ld	e,(ix+-27)
ld	d,0
ld	hl,122
global	wrelop
call	wrelop
jp	llt,40f
jp	41f
41:jp	31f
40:jp	30f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+-27)
cp	.low.95
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l96
11:
;macro.c: 518:             n = strbuf;
global	_strbuf
ld	hl,_strbuf
ld	(ix+-29),l
ld	(ix+1+-29),h
;macro.c: 519:             while ((c = *s) && 
jp	l97
l98:
;macro.c: 520:                    (((c >= 'A') && (c <= 'Z')) || 
;macro.c: 521:                     ((c >= 'a') && (c <= 'z')) ||
;macro.c: 522:                     ((c >= '0') && (c <= '9')) ||
;macro.c: 523:                     (c == '_'))) {
;macro.c: 524:                 *n++ = *s++;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(ix+-29)
ld	h,(ix+1+-29)
inc	hl
ld	(ix+-29),l
ld	(ix+1+-29),h
dec	hl
ld	(hl),a
;macro.c: 525:             }
l97:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
ld	(ix+-27),a
or	a
jp	lnz,20f
jp	21f
20:
ld	de,65
ld	l,(ix+-27)
ld	h,0
global	wrelop
call	wrelop
jp	lge,60f
jp	61f
60:
ld	e,(ix+-27)
ld	d,0
ld	hl,90
global	wrelop
call	wrelop
jp	lge,60f
jp	61f
61:jp	51f
60:jp	50f
51:
ld	de,97
ld	l,(ix+-27)
ld	h,0
global	wrelop
call	wrelop
jp	lge,60f
jp	61f
60:
ld	e,(ix+-27)
ld	d,0
ld	hl,122
global	wrelop
call	wrelop
jp	lge,60f
jp	61f
61:jp	51f
60:jp	50f
51:jp	41f
50:jp	40f
41:
ld	de,48
ld	l,(ix+-27)
ld	h,0
global	wrelop
call	wrelop
jp	lge,50f
jp	51f
50:
ld	e,(ix+-27)
ld	d,0
ld	hl,57
global	wrelop
call	wrelop
jp	lge,50f
jp	51f
51:jp	41f
50:jp	40f
41:jp	31f
40:jp	30f
31:
ld	a,(ix+-27)
cp	.low.95
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:jp	11f
20:jp	10f
10:
jp	l98
11:
l99:
;macro.c: 526:             *n++ = 0;
ld	l,(ix+-29)
ld	h,(ix+1+-29)
inc	hl
ld	(ix+-29),l
ld	(ix+1+-29),h
dec	hl
ld	(hl),0
;macro.c: 527:             n = strbuf;
ld	hl,_strbuf
ld	(ix+-29),l
ld	(ix+1+-29),h
;macro.c: 529:             for (i = 0; i < args; i++) {
ld	(ix+-30),0
jp	l103
l100:
;macro.c: 530:                 if (strcmp(m->parms[i], strbuf) == 0) {
ld	hl,_strbuf
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+-30)
ld	h,0
add	hl,hl
add	hl,bc
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
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
jp	nz,l104
;macro.c: 531:                     n = parms[i];
push	ix
pop	de
ld	l,(ix+-30)
ld	h,0
add	hl,hl
add	hl,de
ld	de,-26
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-29),c
ld	(ix+1+-29),b
;macro.c: 532:                     break;
jp	l101
;macro.c: 533:                 }
;macro.c: 534:             }
l104:
l102:
inc	(ix+-30)
l103:
ld	e,(ix+-6)
ld	d,0
ld	l,(ix+-30)
ld	h,0
global	wrelop
call	wrelop
jp	llt,l100
l101:
;macro.c: 535:             if (stringify) {
ld	a,(ix+-31)
or	a
jp	az,l105
;macro.c: 536:                 *d++ = '\"';
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),34
;macro.c: 537:             }
;macro.c: 538:             while (*n) {
l105:
jp	l106
l107:
;macro.c: 539:                 *d++ = *n++;
ld	l,(ix+-29)
ld	h,(ix+1+-29)
ld	a,(hl)
inc	hl
ld	(ix+-29),l
ld	(ix+1+-29),h
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 540:             }
l106:
ld	l,(ix+-29)
ld	h,(ix+1+-29)
ld	a,(hl)
or	a
jp	anz,l107
l108:
;macro.c: 541:             if (stringify) {
ld	a,(ix+-31)
or	a
jp	az,l109
;macro.c: 542:                 *d++ = '\"';
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),34
;macro.c: 543:             }
;macro.c: 544:             continue;
l109:
jp	l85
;macro.c: 545:         }
;macro.c: 546:         *d++ = *s++;
l96:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
inc	hl
ld	(ix+6),l
ld	(ix+1+6),h
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),a
;macro.c: 547:     }
l85:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
or	a
jp	anz,l86
l87:
;macro.c: 548:     *d = 0;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
ld	(hl),0
;macro.c: 555:     if (d > macbuffer && d[-1] != ' ' && d[-1] != '\t') {
ld	e,(ix+-5)
ld	d,(ix+1+-5)
ld	hl,(_macbuffer)
global	wrelop
call	wrelop
jp	lge,30f
jp	31f
31:
ld	l,(ix+-5)
ld	h,(ix+1+-5)
dec	hl
ld	a,(hl)
cp	.low.32
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	l,(ix+-5)
ld	h,(ix+1+-5)
dec	hl
ld	a,(hl)
cp	.low.9
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l110
11:
;macro.c: 556:         *d++ = ' ';
ld	l,(ix+-5)
ld	h,(ix+1+-5)
inc	hl
ld	(ix+-5),l
ld	(ix+1+-5),h
dec	hl
ld	(hl),32
;macro.c: 557:         *d = 0;
ld	l,(ix+-5)
ld	h,(ix+1+-5)
ld	(hl),0
;macro.c: 558:     }
;macro.c: 560:     insertmacro(m->name, macbuffer);
l110:
global	_insertmacro
ld	hl,(_macbuffer)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_insertmacro
ld	hl,2+2
add	hl,sp
ld	sp,hl
;macro.c: 561:     return 1;
ld	l,.low.1
jp	l63
;macro.c: 562: }
l63:
jp	cret
f110	equ	-31
psect	data
19:
defb	49,0
psect	bss
_macbuffer:
	defs	2
_macros:
	defs	2
