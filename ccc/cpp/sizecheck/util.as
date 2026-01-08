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
;../../libsrc/include/stdarg.h: 3: typedef void *	va_list[1];
;../../libsrc/include/unistd.h: 25: extern char access(char *pathname, unsigned char mode);
;../../libsrc/include/unistd.h: 26: extern unsigned int alarm(unsigned int seconds);
;../../libsrc/include/unistd.h: 27: extern int chdir(char *path);
;../../libsrc/include/unistd.h: 28: extern int chmod(char *path, int mode);
;../../libsrc/include/unistd.h: 29: extern int chown(char *path, int owner);
;../../libsrc/include/unistd.h: 30: extern int close(unsigned char fd);
;../../libsrc/include/unistd.h: 31: extern int creat(char *path, int mode);
;../../libsrc/include/unistd.h: 32: extern int dup(unsigned char fd);
;../../libsrc/include/unistd.h: 33: extern int exec(char *path, char *argv[]);
;../../libsrc/include/unistd.h: 34: extern int execv(char *path, char *argv[]);
;../../libsrc/include/unistd.h: 35: extern void exit(int status);
;../../libsrc/include/unistd.h: 36: extern int fork(void);
;../../libsrc/include/unistd.h: 37: extern int fstat(unsigned char fd, char *buf);
;../../libsrc/include/unistd.h: 38: extern int getpid(void);
;../../libsrc/include/unistd.h: 39: extern int getuid(void);
;../../libsrc/include/unistd.h: 40: extern int gtty(unsigned char fd, char *buf);
;../../libsrc/include/unistd.h: 41: extern int kill(int pid, int sig);
;../../libsrc/include/unistd.h: 42: extern int link(char *oldpath, char *newpath);
;../../libsrc/include/unistd.h: 43: extern long lseek(unsigned char fd, long offset, int whence);
;../../libsrc/include/unistd.h: 44: extern int mknod(char *path, int mode, int dev);
;../../libsrc/include/unistd.h: 45: extern int mount(char *dev, char *dir, int flags);
;../../libsrc/include/unistd.h: 46: extern int nice(int inc);
;../../libsrc/include/unistd.h: 47: extern int open(char *path, int flags);
;../../libsrc/include/unistd.h: 48: extern int pause(void);
;../../libsrc/include/unistd.h: 49: extern int pipe(int *fds);
;../../libsrc/include/unistd.h: 50: extern int read(unsigned char fd, char *buf, int count);
;../../libsrc/include/unistd.h: 51: extern long lseek(unsigned char fd, long offset, int whence);
;../../libsrc/include/unistd.h: 52: extern int seek(unsigned char fd, int offset, int whence);
;../../libsrc/include/unistd.h: 53: extern int setuid(int uid);
;../../libsrc/include/unistd.h: 54: extern void *sbrk(int incr);
;../../libsrc/include/unistd.h: 55: extern int brk(void *addr);
;../../libsrc/include/unistd.h: 56: extern int stat(char *path, char *buf);
;../../libsrc/include/unistd.h: 57: extern int stime(long *tp);
;../../libsrc/include/unistd.h: 58: extern int stty(unsigned char fd, char *buf);
;../../libsrc/include/unistd.h: 59: extern void sync(void);
;../../libsrc/include/unistd.h: 60: extern int time(long *tp);
;../../libsrc/include/unistd.h: 61: extern int umount(char *target);
;../../libsrc/include/unistd.h: 62: extern int unlink(char *pathname);
;../../libsrc/include/unistd.h: 63: extern int wait(int *status);
;../../libsrc/include/unistd.h: 64: extern int write(unsigned char fd, char *buf, int count);
;../../libsrc/include/unistd.h: 65: extern int sleep(unsigned int seconds);
;util.c: 11: char *
;util.c: 12: strdup(char *s)
;util.c: 13: {
psect	text
global	_strdup
_strdup:
global	ncsv, cret, indir
call	ncsv
defw	f143
;util.c: 14:     char *p = malloc(strlen(s) + 1);
global	_malloc
global	_strlen
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
inc	hl
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
;util.c: 15:     if (p)
ld	a,(ix+-2)
or	(ix+1+-2)
jp	z,l8
;util.c: 16:         strcpy(p, s);
global	_strcpy
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_strcpy
ld	hl,2+2
add	hl,sp
ld	sp,hl
;util.c: 17:     return p;
l8:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l7
;util.c: 18: }
l7:
jp	cret
f143	equ	-2
;util.c: 23: static char *errmsgs[] = {
psect	data
_errmsgs:
;util.c: 24:     "unknown error",
defw	19f
;util.c: 25:     "invalid escape sequence",
defw	29f
;util.c: 26:     "bad character constant",
defw	39f
;util.c: 27:     "bad numeric constant",
defw	49f
;util.c: 28:     "token too long",
defw	59f
;util.c: 29:     "macro name expected",
defw	69f
;util.c: 30:     "#elif without #if",
defw	79f
;util.c: 31:     "missing #endif",
defw	89f
;util.c: 32:     "invalid directive",
defw	99f
;util.c: 33:     "bad digit",
defw	109f
;util.c: 34:     "unknown token",
defw	119f
;util.c: 35:     "defined requires identifier",
defw	129f
;util.c: 36:     "macro argument count mismatch",
defw	139f
;util.c: 37:     "symbol truncated (warning)",
defw	149f
;util.c: 38: };
;util.c: 40: extern int exitCode;
;util.c: 45: void
;util.c: 46: gripe(error_t err)
;util.c: 47: {
psect	text
global	_gripe
_gripe:
call	ncsv
defw	f145
;util.c: 48:     char *msg = (err < sizeof(errmsgs)/sizeof(errmsgs[0])) ? errmsgs[err] : "unknown error";
ld	de,14
ld	l,(ix+6)
ld	h,(ix+1+6)
global	wrelop
call	wrelop
jp	alt,10f
jp	11f
11:
ld	hl,159f
jp	12f
10:
ld	de,_errmsgs
ld	l,(ix+6)
ld	h,(ix+1+6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
12:
ld	(ix+-2),l
ld	(ix+1+-2),h
;util.c: 49:     fprintf(stderr, "%s:%d: %s\n", filename ? filename : "?", lineno, msg);
global	_fprintf
global	_stderr
global	_filename
global	_lineno
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
ld	hl,(_lineno)
push	hl
ld	hl,(_filename)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,179f
jp	12f
10:
ld	hl,(_filename)
12:
push	hl
ld	hl,169f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;util.c: 50:     if (err <     13    )
ld	de,13
ld	l,(ix+6)
ld	h,(ix+1+6)
global	wrelop
call	wrelop
jp	age,l10
;util.c: 51:         exitCode = 1;
global	_exitCode
ld	hl,1
ld	(_exitCode),hl
;util.c: 52: }
l10:
l9:
jp	cret
f145	equ	-2
;util.c: 58: unsigned char
;util.c: 59: lookupc(char *s, unsigned char c)
;util.c: 60: {
global	_lookupc
_lookupc:
call	ncsv
defw	f150
;util.c: 61:     unsigned char i;
;util.c: 62:     for (i = 0; s[i]; i++) {
ld	(ix+-1),0
jp	l15
l12:
;util.c: 63:         if (c == (unsigned char)s[i]) {
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+-1)
ld	h,0
add	hl,de
ld	a,(ix+8)
cp	(hl)
jp	nz,l16
;util.c: 64:             return i;
ld	l,(ix+-1)
jp	l11
;util.c: 65:         }
;util.c: 66:     }
l16:
l14:
inc	(ix+-1)
l15:
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+-1)
ld	h,0
add	hl,de
ld	a,(hl)
or	a
jp	anz,l12
l13:
;util.c: 67:     return 0xff;
ld	l,.low.-1
jp	l11
;util.c: 68: }
l11:
jp	cret
f150	equ	-1
;util.c: 70: char printbuf[128];
;util.c: 74: int
;util.c: 75: fdprintf(int fd, char *fmt, ...)
;util.c: 76: {
global	_fdprintf
_fdprintf:
call	ncsv
defw	f152
;util.c: 77:     va_list ap;
;util.c: 78:     int len;
;util.c: 80:     	*ap = (char *)& fmt + sizeof  fmt;
push	ix
pop	de
ld	hl,10
add	hl,de
ld	(ix+-2),l
ld	(ix+1+-2),h
;util.c: 81:     len = vsprintf(printbuf, fmt, ap);
global	_vsprintf
global	_printbuf
push	ix
pop	hl
dec	hl
dec hl
push	hl
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	hl,_printbuf
push	hl
call	_vsprintf
exx
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-4),l
ld	(ix+1+-4),h
;util.c: 82:     ;
;util.c: 84:     write(fd, printbuf, len);
global	_write
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
ld	hl,_printbuf
push	hl
ld	l,(ix+6)
push	hl
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;util.c: 85:     return len;
ld	l,(ix+-4)
ld	h,(ix+1+-4)
jp	l17
;util.c: 86: }
l17:
jp	cret
f152	equ	-4
;util.c: 92: long
;util.c: 93: parseConst(token_t stop)
;util.c: 94: {
global	_parseConst
_parseConst:
call	ncsv
defw	f154
;util.c: 95:     long val = 0;
ld	(ix+-4),.low.0
ld	(ix+1+-4),.high.0
ld	(ix+2+-4),.low.0
ld	(ix+3+-4),.high.0
;util.c: 96:     long term;
;util.c: 97:     char op = 0;
ld	(ix+-9),0
;util.c: 99:     while (cur.type != stop && cur.type !=   0) {
jp	l19
l20:
;util.c: 101:         if (cur.type ==  21) {
global	_cur
ld	a,(_cur)
cp	.low.21
jp	nz,l22
;util.c: 102:             term = cur.v.numeric;
ld	de,(_cur+5)
ld	hl,(_cur+2+5)
ld	(ix+-8),e
ld	(ix+1+-8),d
ld	(ix+2+-8),l
ld	(ix+3+-8),h
;util.c: 103:         } else if (cur.type ==     20) {
jp	l23
l22:
ld	a,(_cur)
cp	.low.20
jp	nz,l24
;util.c: 105:             term = 0;
ld	(ix+-8),.low.0
ld	(ix+1+-8),.high.0
ld	(ix+2+-8),.low.0
ld	(ix+3+-8),.high.0
;util.c: 106:         } else if (cur.type ==    6) {
jp	l25
l24:
ld	a,(_cur)
cp	.low.6
jp	nz,l26
;util.c: 107:             gettoken();
global	_gettoken
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 108:             term = parseConst(   7);
ld	l,.low.7
push	hl
call	_parseConst
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	(ix+-8),e
ld	(ix+1+-8),d
ld	(ix+2+-8),l
ld	(ix+3+-8),h
;util.c: 109:             if (cur.type ==    7)
ld	a,(_cur)
cp	.low.7
jp	nz,l27
;util.c: 110:                 gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 111:         } else if (cur.type ==    34) {
l27:
jp	l28
l26:
ld	a,(_cur)
cp	.low.34
jp	nz,l29
;util.c: 112:             gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 113:             term = !parseConst(stop);
ld	l,(ix+6)
push	hl
call	_parseConst
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	a,e
or	d
or	l
or	h
ld	de,1
jp	z,20f
dec	de
20:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-8),e
ld	(ix+1+-8),d
ld	(ix+2+-8),l
ld	(ix+3+-8),h
;util.c: 114:             continue;
jp	l19
;util.c: 115:         } else if (cur.type == 38) {
jp	l30
l29:
ld	a,(_cur)
cp	.low.38
jp	nz,l31
;util.c: 116:             gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 117:             term = ~parseConst(stop);
ld	l,(ix+6)
push	hl
call	_parseConst
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
push	hl
push	de
ld	hl,-1
pop	bc
or	a
sbc	hl,bc
pop	bc
ex	de,hl
ld	hl,-1
sbc	hl,bc
ld	(ix+-8),e
ld	(ix+1+-8),d
ld	(ix+2+-8),l
ld	(ix+3+-8),h
;util.c: 118:             continue;
jp	l19
;util.c: 119:         } else if (cur.type ==   41) {
jp	l32
l31:
ld	a,(_cur)
cp	.low.41
jp	nz,l33
;util.c: 120:             gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 121:             term = -parseConst(stop);
ld	l,(ix+6)
push	hl
call	_parseConst
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
push	hl
push	de
ld	hl,0
pop	bc
or	a
sbc	hl,bc
pop	bc
ex	de,hl
ld	hl,0
sbc	hl,bc
ld	(ix+-8),e
ld	(ix+1+-8),d
ld	(ix+2+-8),l
ld	(ix+3+-8),h
;util.c: 122:             continue;
jp	l19
;util.c: 123:         } else {
jp	l34
l33:
;util.c: 124:             break;
jp	l21
;util.c: 125:         }
l34:
l32:
l30:
l28:
l25:
l23:
;util.c: 128:         switch (op) {
jp	l36
;util.c: 129:         case 0:   val = term; break;
l37:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 130:         case '+': val = val + term; break;
l38:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	aladd
call	aladd
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 131:         case '-': val = val - term; break;
l39:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	alsub
call	alsub
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 132:         case '*': val = val * term; break;
l40:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	almul
call	almul
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 133:         case '/': val = term ? val / term : 0; break;
l41:
ld	a,(ix+-8)
or	(ix+1+-8)
or	(ix+2+-8)
or	(ix+3+-8)
jp	nz,10f
jp	11f
11:
ld	de,0
ld	hl,0
jp	12f
10:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	aldiv
call	aldiv
12:
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 134:         case '%': val = term ? val % term : 0; break;
l42:
ld	a,(ix+-8)
or	(ix+1+-8)
or	(ix+2+-8)
or	(ix+3+-8)
jp	nz,10f
jp	11f
11:
ld	de,0
ld	hl,0
jp	12f
10:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	almod
call	almod
12:
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 135:         case '&': val = val & term; break;
l43:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	aland
call	aland
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 136:         case '|': val = val | term; break;
l44:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	alor
call	alor
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 137:         case '^': val = val ^ term; break;
l45:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	alxor
call	alxor
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 138:         case '<': val = val < term; break;
l46:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	arelop
call	arelop
jp	alt,20f
jp	21f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 139:         case '>': val = val > term; break;
l47:
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
push	hl
push	de
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
global	arelop
call	arelop
jp	alt,20f
jp	21f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 140:         case 'Q': val = val == term; break;
l48:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	arelop
call	arelop
jp	az,20f
jp	21f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 141:         case 'n': val = val != term; break;
l49:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	arelop
call	arelop
jp	anz,20f
jp	21f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 142:         case 'L': val = val <= term; break;
l50:
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
push	hl
push	de
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
global	arelop
call	arelop
jp	age,20f
jp	21f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 143:         case 'g': val = val >= term; break;
l51:
ld	e,(ix+-8)
ld	d,(ix+1+-8)
ld	l,(ix+2+-8)
ld	h,(ix+3+-8)
push	hl
push	de
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	arelop
call	arelop
jp	age,20f
jp	21f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 144:         case 'j': val = val && term; break;
l52:
ld	a,(ix+-4)
or	(ix+1+-4)
or	(ix+2+-4)
or	(ix+3+-4)
jp	nz,30f
jp	31f
30:
ld	a,(ix+-8)
or	(ix+1+-8)
or	(ix+2+-8)
or	(ix+3+-8)
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 145:         case 'h': val = val || term; break;
l53:
ld	a,(ix+-4)
or	(ix+1+-4)
or	(ix+2+-4)
or	(ix+3+-4)
jp	nz,30f
jp	31f
31:
ld	a,(ix+-8)
or	(ix+1+-8)
or	(ix+2+-8)
or	(ix+3+-8)
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	de,1
jp	22f
21:
ld	de,0
22:
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 146:         case 'y': val = val << term; break;
l54:
ld	b,(ix+-8)
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	allsh
call	allsh
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 147:         case 'w': val = val >> term; break;
l55:
ld	b,(ix+-8)
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
global	alrsh
call	alrsh
ld	(ix+-4),e
ld	(ix+1+-4),d
ld	(ix+2+-4),l
ld	(ix+3+-4),h
jp	l35
;util.c: 148:         }
jp	l35
l36:
ld	a,(ix+-9)
or	a
jp	z,l37
cp	37
jp	z,l42
cp	38
jp	z,l43
cp	42
jp	z,l40
cp	43
jp	z,l38
cp	45
jp	z,l39
cp	47
jp	z,l41
cp	60
jp	z,l46
cp	62
jp	z,l47
cp	76
jp	z,l50
cp	81
jp	z,l48
cp	94
jp	z,l45
cp	103
jp	z,l51
cp	104
jp	z,l53
cp	106
jp	z,l52
cp	110
jp	z,l49
cp	119
jp	z,l55
cp	121
jp	z,l54
cp	124
jp	z,l44
jp	l35
l35:
;util.c: 150:         gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 153:         if (cur.type ==    40 || cur.type ==   41 ||
;util.c: 154:             cur.type ==    36 || cur.type ==     43 || cur.type ==     44 ||
;util.c: 155:             cur.type ==     47 || cur.type ==      48 || cur.type ==     49 ||
;util.c: 156:             cur.type ==      63 || cur.type ==      65 ||
;util.c: 157:             cur.type ==      60 || cur.type ==     61 ||
;util.c: 158:             cur.type ==      62 || cur.type ==      64 ||
;util.c: 159:             cur.type ==    53 || cur.type ==     54 ||
;util.c: 160:             cur.type ==  46 || cur.type ==  45) {
ld	a,(_cur)
cp	.low.40
jp	nz,180f
jp	181f
180:
ld	a,(_cur)
cp	.low.41
jp	nz,180f
jp	181f
181:jp	171f
180:jp	170f
170:
ld	a,(_cur)
cp	.low.36
jp	nz,170f
jp	171f
171:jp	161f
170:jp	160f
160:
ld	a,(_cur)
cp	.low.43
jp	nz,160f
jp	161f
161:jp	151f
160:jp	150f
150:
ld	a,(_cur)
cp	.low.44
jp	nz,150f
jp	151f
151:jp	141f
150:jp	140f
140:
ld	a,(_cur)
cp	.low.47
jp	nz,140f
jp	141f
141:jp	131f
140:jp	130f
130:
ld	a,(_cur)
cp	.low.48
jp	nz,130f
jp	131f
131:jp	121f
130:jp	120f
120:
ld	a,(_cur)
cp	.low.49
jp	nz,120f
jp	121f
121:jp	111f
120:jp	110f
110:
ld	a,(_cur)
cp	.low.63
jp	nz,110f
jp	111f
111:jp	101f
110:jp	100f
100:
ld	a,(_cur)
cp	.low.65
jp	nz,100f
jp	101f
101:jp	91f
100:jp	90f
90:
ld	a,(_cur)
cp	.low.60
jp	nz,90f
jp	91f
91:jp	81f
90:jp	80f
80:
ld	a,(_cur)
cp	.low.61
jp	nz,80f
jp	81f
81:jp	71f
80:jp	70f
70:
ld	a,(_cur)
cp	.low.62
jp	nz,70f
jp	71f
71:jp	61f
70:jp	60f
60:
ld	a,(_cur)
cp	.low.64
jp	nz,60f
jp	61f
61:jp	51f
60:jp	50f
50:
ld	a,(_cur)
cp	.low.53
jp	nz,50f
jp	51f
51:jp	41f
50:jp	40f
40:
ld	a,(_cur)
cp	.low.54
jp	nz,40f
jp	41f
41:jp	31f
40:jp	30f
30:
ld	a,(_cur)
cp	.low.46
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(_cur)
cp	.low.45
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l56
11:
;util.c: 161:             op = cur.type;
ld	a,(_cur)
ld	(ix+-9),a
;util.c: 162:             gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;util.c: 163:         } else {
jp	l57
l56:
;util.c: 164:             break;
jp	l21
;util.c: 165:         }
l57:
;util.c: 166:     }
l19:
ld	a,(_cur)
ld	e,a
ld	a,(ix+6)
cp	e
jp	nz,20f
jp	21f
20:
ld	a,(_cur)
or	a
jp	lnz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l20
11:
l21:
;util.c: 168:     return val;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+2+-4)
ld	h,(ix+3+-4)
jp	l18
;util.c: 169: }
l18:
jp	cret
f154	equ	-9
psect	data
19:
defb	117,110,107,110,111,119,110,32,101,114,114,111,114,0
29:
defb	105,110,118,97,108,105,100,32,101,115,99,97,112,101,32,115
defb	101,113,117,101,110,99,101,0
39:
defb	98,97,100,32,99,104,97,114,97,99,116,101,114,32,99,111
defb	110,115,116,97,110,116,0
49:
defb	98,97,100,32,110,117,109,101,114,105,99,32,99,111,110,115
defb	116,97,110,116,0
59:
defb	116,111,107,101,110,32,116,111,111,32,108,111,110,103,0
69:
defb	109,97,99,114,111,32,110,97,109,101,32,101,120,112,101,99
defb	116,101,100,0
79:
defb	35,101,108,105,102,32,119,105,116,104,111,117,116,32,35,105
defb	102,0
89:
defb	109,105,115,115,105,110,103,32,35,101,110,100,105,102,0
99:
defb	105,110,118,97,108,105,100,32,100,105,114,101,99,116,105,118
defb	101,0
109:
defb	98,97,100,32,100,105,103,105,116,0
119:
defb	117,110,107,110,111,119,110,32,116,111,107,101,110,0
129:
defb	100,101,102,105,110,101,100,32,114,101,113,117,105,114,101,115
defb	32,105,100,101,110,116,105,102,105,101,114,0
139:
defb	109,97,99,114,111,32,97,114,103,117,109,101,110,116,32,99
defb	111,117,110,116,32,109,105,115,109,97,116,99,104,0
149:
defb	115,121,109,98,111,108,32,116,114,117,110,99,97,116,101,100
defb	32,40,119,97,114,110,105,110,103,41,0
159:
defb	117,110,107,110,111,119,110,32,101,114,114,111,114,0
169:
defb	37,115,58,37,100,58,32,37,115,10,0
179:
defb	63,0
psect	bss
_printbuf:
	defs	128
