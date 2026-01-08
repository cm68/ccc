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
;emit.c: 10: char lexFd = -1;
psect	data
global	_lexFd
_lexFd:
defb	-1
;emit.c: 11: char ppFd = -1;
global	_ppFd
_ppFd:
defb	-1
;emit.c: 14: static int lastLine = 0;
_lastLine:
defw	0
;emit.c: 15: static int lastLinePP = 0;
_lastLinePP:
defw	0
;emit.c: 16: static char *lastName = ((void *)0);
_lastName:
defw	0
;emit.c: 19: void emitLine(int line, char *file);
;emit.c: 20: void emitLinePP(int line, char *file);
;emit.c: 25: void
;emit.c: 26: emitFileStart(char *file)
;emit.c: 27: {
psect	text
global	_emitFileStart
_emitFileStart:
global	ncsv, cret, indir
call	ncsv
defw	f150
;emit.c: 28:     char buf[300];
;emit.c: 29:     if (!noLineMarkers) {
global	_noLineMarkers
ld	a,(_noLineMarkers)
or	a
jp	anz,l8
;emit.c: 30:         emitLine(1, file);
global	_emitLine
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,1
push	hl
call	_emitLine
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 32:         sprintf(buf, "# %d \"%s\"\n", 1, file);
global	_sprintf
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,1
push	hl
ld	hl,19f
push	hl
push	ix
pop	de
ld	hl,-300
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+2+2
add	hl,sp
ld	sp,hl
;emit.c: 33:         emitPPStr(buf);
global	_emitPPStr
push	ix
pop	de
ld	hl,-300
add	hl,de
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 34:         lastLine = 1;
ld	hl,1
ld	(_lastLine),hl
;emit.c: 35:         lastLinePP = 1;
ld	hl,1
ld	(_lastLinePP),hl
;emit.c: 36:         lastName = filename;
global	_filename
ld	hl,(_filename)
ld	(_lastName),hl
;emit.c: 37:     }
;emit.c: 38: }
l8:
l7:
jp	cret
f150	equ	-300
;emit.c: 43: void
;emit.c: 44: emitToken(unsigned char tok)
;emit.c: 45: {
global	_emitToken
_emitToken:
call	ncsv
defw	f153
;emit.c: 46:     outbufWrite(&tok, 1);
global	_outbufWrite
ld	hl,1
push	hl
push	ix
pop	de
ld	hl,6
add	hl,de
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 47: }
l9:
jp	cret
f153	equ	0
;emit.c: 52: void
;emit.c: 53: emitKeyword(unsigned char kwtok)
;emit.c: 54: {
global	_emitKeyword
_emitKeyword:
call	ncsv
defw	f154
;emit.c: 55:     outbufWrite(&kwtok, 1);
ld	hl,1
push	hl
push	ix
pop	de
ld	hl,6
add	hl,de
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 56: }
l10:
jp	cret
f154	equ	0
;emit.c: 61: void
;emit.c: 62: emitSym(char *name)
;emit.c: 63: {
global	_emitSym
_emitSym:
call	ncsv
defw	f155
;emit.c: 64:     unsigned char hdr[2];
;emit.c: 65:     int len = strlen(name);
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
ld	(ix+-4),l
ld	(ix+1+-4),h
;emit.c: 66:     if (len > 255) len = 255;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,255
global	wrelop
call	wrelop
jp	age,l12
ld	(ix+-4),.low.255
ld	(ix+1+-4),.high.255
;emit.c: 67:     hdr[0] =     20;
l12:
ld	(ix+-2),20
;emit.c: 68:     hdr[1] = len;
ld	a,(ix+-4)
ld	(ix+-1),a
;emit.c: 69:     outbufWrite(hdr, 2);
ld	hl,2
push	hl
push	ix
pop	hl
dec	hl
dec hl
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 70:     outbufWrite(name, len);
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 71: }
l11:
jp	cret
f155	equ	-4
;emit.c: 76: void
;emit.c: 77: emitNumber(long val)
;emit.c: 78: {
global	_emitNumber
_emitNumber:
call	ncsv
defw	f156
;emit.c: 79:     unsigned char buf[5];
;emit.c: 80:     buf[0] =  21;
ld	(ix+-5),21
;emit.c: 81:     buf[1] = val & 0xff;
ld	a,(ix+6)
ld	(ix+-4),a
;emit.c: 82:     buf[2] = (val >> 8) & 0xff;
ld	b,.low.8
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
global	alrsh
call	alrsh
ld	e,e
ld	(ix+-3),e
;emit.c: 83:     buf[3] = (val >> 16) & 0xff;
ld	b,.low.16
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
global	alrsh
call	alrsh
ld	e,e
ld	(ix+-2),e
;emit.c: 84:     buf[4] = (val >> 24) & 0xff;
ld	b,.low.24
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
global	alrsh
call	alrsh
ld	e,e
ld	(ix+-1),e
;emit.c: 85:     outbufWrite(buf, 5);
ld	hl,5
push	hl
push	ix
pop	de
ld	hl,-5
add	hl,de
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 86: }
l13:
jp	cret
f156	equ	-5
;emit.c: 91: void
;emit.c: 92: emitFNumber(float val)
;emit.c: 93: {
global	_emitFNumber
_emitFNumber:
call	ncsv
defw	f157
;emit.c: 94:     unsigned char buf[5];
;emit.c: 95:     union { float f; unsigned long l; } u;
;emit.c: 96:     u.f = val;
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
ld	(ix+-9),e
ld	(ix+1+-9),d
ld	(ix+2+-9),l
ld	(ix+3+-9),h
;emit.c: 97:     buf[0] = 23;
ld	(ix+-5),23
;emit.c: 98:     buf[1] = u.l & 0xff;
ld	a,(ix+-9)
ld	(ix+-4),a
;emit.c: 99:     buf[2] = (u.l >> 8) & 0xff;
ld	b,.low.8
ld	e,(ix+-9)
ld	d,(ix+1+-9)
ld	l,(ix+2+-9)
ld	h,(ix+3+-9)
global	llrsh
call	llrsh
ld	e,e
ld	(ix+-3),e
;emit.c: 100:     buf[3] = (u.l >> 16) & 0xff;
ld	b,.low.16
ld	e,(ix+-9)
ld	d,(ix+1+-9)
ld	l,(ix+2+-9)
ld	h,(ix+3+-9)
global	llrsh
call	llrsh
ld	e,e
ld	(ix+-2),e
;emit.c: 101:     buf[4] = (u.l >> 24) & 0xff;
ld	b,.low.24
ld	e,(ix+-9)
ld	d,(ix+1+-9)
ld	l,(ix+2+-9)
ld	h,(ix+3+-9)
global	llrsh
call	llrsh
ld	e,e
ld	(ix+-1),e
;emit.c: 102:     outbufWrite(buf, 5);
ld	hl,5
push	hl
push	ix
pop	de
ld	hl,-5
add	hl,de
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 103: }
l15:
jp	cret
f157	equ	-9
;emit.c: 109: static void
;emit.c: 110: emitStr2(unsigned char tok, char *str, int len)
;emit.c: 111: {
_emitStr2:
call	ncsv
defw	f158
;emit.c: 112:     unsigned char hdr[3];
;emit.c: 113:     if (len > 65535) len = 65535;
ld	e,(ix+10)
ld	d,(ix+1+10)
ld	hl,-1
global	wrelop
call	wrelop
jp	age,l17
ld	(ix+10),.low.-1
ld	(ix+1+10),.high.-1
;emit.c: 114:     hdr[0] = tok;
l17:
ld	a,(ix+6)
ld	(ix+-3),a
;emit.c: 115:     hdr[1] = len & 0xff;
ld	a,(ix+10)
ld	(ix+-2),a
;emit.c: 116:     hdr[2] = (len >> 8) & 0xff;
ld	b,.low.8
ld	l,(ix+10)
ld	h,(ix+1+10)
global	shar
call	shar
ld	l,l
ld	(ix+-1),l
;emit.c: 117:     outbufWrite(hdr, 3);
ld	hl,3
push	hl
push	ix
pop	hl
dec	hl
dec hl
dec hl
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 118:     outbufWrite(str, len);
ld	l,(ix+10)
ld	h,(ix+1+10)
push	hl
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 119: }
l16:
jp	cret
f158	equ	-3
;emit.c: 124: void
;emit.c: 125: emitString(char *str, int len)
;emit.c: 126: {
global	_emitString
_emitString:
call	ncsv
defw	f159
;emit.c: 127:     emitStr2( 22, str, len);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,.low.22
push	hl
call	_emitStr2
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;emit.c: 128: }
l18:
jp	cret
f159	equ	0
;emit.c: 133: void
;emit.c: 134: emitAsmString(char *str, int len)
;emit.c: 135: {
global	_emitAsmString
_emitAsmString:
call	ncsv
defw	f160
;emit.c: 136:     emitStr2( 118, str, len);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,.low.118
push	hl
call	_emitStr2
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;emit.c: 137: }
l19:
jp	cret
f160	equ	0
;emit.c: 142: void
;emit.c: 143: emitLabel(char *name)
;emit.c: 144: {
global	_emitLabel
_emitLabel:
call	ncsv
defw	f161
;emit.c: 145:     unsigned char hdr[2];
;emit.c: 146:     int len = strlen(name);
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
ld	(ix+-4),l
ld	(ix+1+-4),h
;emit.c: 147:     if (len > 255) len = 255;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,255
global	wrelop
call	wrelop
jp	age,l21
ld	(ix+-4),.low.255
ld	(ix+1+-4),.high.255
;emit.c: 148:     hdr[0] =   112;
l21:
ld	(ix+-2),112
;emit.c: 149:     hdr[1] = len;
ld	a,(ix+-4)
ld	(ix+-1),a
;emit.c: 150:     outbufWrite(hdr, 2);
ld	hl,2
push	hl
push	ix
pop	hl
dec	hl
dec hl
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 151:     outbufWrite(name, len);
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 152: }
l20:
jp	cret
f161	equ	-4
;emit.c: 157: void
;emit.c: 158: emitNewline(void)
;emit.c: 159: {
global	_emitNewline
_emitNewline:
call	ncsv
defw	f162
;emit.c: 160:     unsigned char c = 117;
ld	(ix+-1),117
;emit.c: 161:     outbufWrite(&c, 1);
ld	hl,1
push	hl
push	ix
pop	hl
dec	hl
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 162: }
l22:
jp	cret
f162	equ	-1
;emit.c: 167: void
;emit.c: 168: emitLine(int line, char *file)
;emit.c: 169: {
_emitLine:
call	ncsv
defw	f163
;emit.c: 170:     unsigned char hdr[4];
;emit.c: 171:     int len = strlen(file);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-6),l
ld	(ix+1+-6),h
;emit.c: 173:     if (len > 255) len = 255;
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	hl,255
global	wrelop
call	wrelop
jp	age,l24
ld	(ix+-6),.low.255
ld	(ix+1+-6),.high.255
;emit.c: 174:     hdr[0] =  116;
l24:
ld	(ix+-4),116
;emit.c: 175:     hdr[1] = line & 0xff;
ld	a,(ix+6)
ld	(ix+-3),a
;emit.c: 176:     hdr[2] = (line >> 8) & 0xff;
ld	b,.low.8
ld	l,(ix+6)
ld	h,(ix+1+6)
global	shar
call	shar
ld	l,l
ld	(ix+-2),l
;emit.c: 177:     hdr[3] = len;
ld	a,(ix+-6)
ld	(ix+-1),a
;emit.c: 178:     outbufWrite(hdr, 4);
ld	hl,4
push	hl
push	ix
pop	hl
dec	hl
dec hl
dec hl
dec hl
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 179:     outbufWrite(file, len);
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 180: }
l23:
jp	cret
f163	equ	-6
;emit.c: 185: void
;emit.c: 186: emitLinePP(int line, char *file)
;emit.c: 187: {
global	_emitLinePP
_emitLinePP:
call	ncsv
defw	f164
;emit.c: 188:     char buf[300];
;emit.c: 189:     sprintf(buf, "\n# %d \"%s\"\n", line, file);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,29f
push	hl
push	ix
pop	de
ld	hl,-300
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+2+2
add	hl,sp
ld	sp,hl
;emit.c: 190:     emitPPStr(buf);
push	ix
pop	de
ld	hl,-300
add	hl,de
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 191: }
l25:
jp	cret
f164	equ	-300
;emit.c: 196: void
;emit.c: 197: emitAsm(char *text, int len)
;emit.c: 198: {
global	_emitAsm
_emitAsm:
call	ncsv
defw	f165
;emit.c: 199:     emitKeyword(        157);
ld	l,.low.-99
push	hl
call	_emitKeyword
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 200:     emitString(text, len);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_emitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 201: }
l26:
jp	cret
f165	equ	0
;emit.c: 206: void
;emit.c: 207: emitPP(char *text, int len)
;emit.c: 208: {
global	_emitPP
_emitPP:
call	ncsv
defw	f166
;emit.c: 209:     if (ppFd >= 0)
ld	a,(_ppFd)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l28
;emit.c: 210:         write(ppFd, text, len);
global	_write
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;emit.c: 211: }
l28:
l27:
jp	cret
f166	equ	0
;emit.c: 216: void
;emit.c: 217: emitPPString(char *text, int len)
;emit.c: 218: {
global	_emitPPString
_emitPPString:
call	ncsv
defw	f167
;emit.c: 219:     int i;
;emit.c: 220:     char c, esc[3];
;emit.c: 222:     if (ppFd < 0) return;
ld	a,(_ppFd)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l30
jp	l29
;emit.c: 224:     esc[0] = '\\';
l30:
ld	(ix+-6),92
;emit.c: 225:     esc[2] = 0;
ld	(ix+-4),0
;emit.c: 226:     for (i = 0; i < len; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l34
l31:
;emit.c: 227:         c = text[i];
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
add	hl,de
ld	l,(hl)
ld	(ix+-3),l
;emit.c: 228:         switch (c) {
jp	l36
;emit.c: 229:         case '\n': esc[1] = 'n'; write(ppFd, esc, 2); break;
l37:
ld	(ix+-5),110
ld	hl,2
push	hl
push	ix
pop	de
ld	hl,-6
add	hl,de
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
jp	l35
;emit.c: 230:         case '\t': esc[1] = 't'; write(ppFd, esc, 2); break;
l38:
ld	(ix+-5),116
ld	hl,2
push	hl
push	ix
pop	de
ld	hl,-6
add	hl,de
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
jp	l35
;emit.c: 231:         case '\r': esc[1] = 'r'; write(ppFd, esc, 2); break;
l39:
ld	(ix+-5),114
ld	hl,2
push	hl
push	ix
pop	de
ld	hl,-6
add	hl,de
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
jp	l35
;emit.c: 232:         case '\\': esc[1] = '\\'; write(ppFd, esc, 2); break;
l40:
ld	(ix+-5),92
ld	hl,2
push	hl
push	ix
pop	de
ld	hl,-6
add	hl,de
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
jp	l35
;emit.c: 233:         case '"':  esc[1] = '"'; write(ppFd, esc, 2); break;
l41:
ld	(ix+-5),34
ld	hl,2
push	hl
push	ix
pop	de
ld	hl,-6
add	hl,de
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
jp	l35
;emit.c: 234:         default:   write(ppFd, &c, 1); break;
l42:
ld	hl,1
push	hl
push	ix
pop	hl
dec	hl
dec hl
dec hl
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
jp	l35
;emit.c: 235:         }
jp	l35
l36:
ld	a,(ix+-3)
cp	9
jp	z,l38
cp	10
jp	z,l37
cp	13
jp	z,l39
cp	34
jp	z,l41
cp	92
jp	z,l40
jp	l42
l35:
;emit.c: 236:     }
l33:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l34:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l31
l32:
;emit.c: 237: }
l29:
jp	cret
f167	equ	-6
;emit.c: 239: void
;emit.c: 240: emitPPStr(char *text)
;emit.c: 241: {
_emitPPStr:
call	ncsv
defw	f168
;emit.c: 242:     if (ppFd >= 0)
ld	a,(_ppFd)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l44
;emit.c: 243:         write(ppFd, text, strlen(text));
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
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	a,(_ppFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;emit.c: 244: }
l44:
l43:
jp	cret
f168	equ	0
;emit.c: 249: static char *
;emit.c: 250: kw2str(unsigned char kw)
;emit.c: 251: {
_kw2str:
call	ncsv
defw	f169
;emit.c: 252:     switch (kw) {
jp	l47
;emit.c: 253:     case         128: return "int";
l48:
ld	hl,39f
jp	l45
;emit.c: 254:     case        129: return "char";
l49:
ld	hl,49f
jp	l45
;emit.c: 255:     case        134: return "long";
l50:
ld	hl,59f
jp	l45
;emit.c: 256:     case       130: return "float";
l51:
ld	hl,69f
jp	l45
;emit.c: 257:     case      131: return "double";
l52:
ld	hl,79f
jp	l45
;emit.c: 258:     case        138: return "void";
l53:
ld	hl,89f
jp	l45
;emit.c: 259:     case    135: return "unsigned";
l54:
ld	hl,99f
jp	l45
;emit.c: 260:     case      143: return "static";
l55:
ld	hl,109f
jp	l45
;emit.c: 261:     case      142: return "extern";
l56:
ld	hl,119f
jp	l45
;emit.c: 262:     case        141: return "auto";
l57:
ld	hl,129f
jp	l45
;emit.c: 263:     case    144: return "register";
l58:
ld	hl,139f
jp	l45
;emit.c: 264:     case     140: return "typedef";
l59:
ld	hl,149f
jp	l45
;emit.c: 265:     case      132: return "struct";
l60:
ld	hl,159f
jp	l45
;emit.c: 266:     case       136: return "union";
l61:
ld	hl,169f
jp	l45
;emit.c: 267:     case        139: return "enum";
l62:
ld	hl,179f
jp	l45
;emit.c: 268:     case          147: return "if";
l63:
ld	hl,189f
jp	l45
;emit.c: 269:     case        149: return "else";
l64:
ld	hl,199f
jp	l45
;emit.c: 270:     case       148: return "while";
l65:
ld	hl,209f
jp	l45
;emit.c: 271:     case          154: return "do";
l66:
ld	hl,219f
jp	l45
;emit.c: 272:     case         156: return "for";
l67:
ld	hl,229f
jp	l45
;emit.c: 273:     case      150: return "switch";
l68:
ld	hl,239f
jp	l45
;emit.c: 274:     case        151: return "case";
l69:
ld	hl,249f
jp	l45
;emit.c: 275:     case     155: return "default";
l70:
ld	hl,259f
jp	l45
;emit.c: 276:     case       152: return "break";
l71:
ld	hl,269f
jp	l45
;emit.c: 277:     case    153: return "continue";
l72:
ld	hl,279f
jp	l45
;emit.c: 278:     case      146: return "return";
l73:
ld	hl,289f
jp	l45
;emit.c: 279:     case        145: return "goto";
l74:
ld	hl,299f
jp	l45
;emit.c: 280:     case         157: return "asm";
l75:
ld	hl,309f
jp	l45
;emit.c: 281:     default: return "?kw?";
l76:
ld	hl,319f
jp	l45
;emit.c: 282:     }
jp	l46
l47:
ld	a,(ix+6)
add	a,.low.-128
ld	l,a
ld	h,0
ld	a,0
cp	h
jp	c,l76
jp	nz,1f
ld	a,29
cp	l
jp	c,l76
1:add	hl,hl
ld	de,S170
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
jp	(hl)
psect	data
S170:
defw	l48
defw	l49
defw	l51
defw	l52
defw	l60
defw	l76
defw	l50
defw	l54
defw	l61
defw	l76
defw	l53
defw	l62
defw	l59
defw	l57
defw	l56
defw	l55
defw	l58
defw	l74
defw	l73
defw	l63
defw	l65
defw	l64
defw	l68
defw	l69
defw	l71
defw	l72
defw	l66
defw	l70
defw	l67
defw	l75
psect	text
l46:
;emit.c: 283: }
l45:
jp	cret
f169	equ	0
;emit.c: 288: static char *
;emit.c: 289: op2str(token_t t)
;emit.c: 290: {
_op2str:
call	ncsv
defw	f171
;emit.c: 291:     switch (t) {
jp	l79
;emit.c: 292:     case  91: return "sizeof";
l80:
ld	hl,329f
jp	l77
;emit.c: 293:     case      60: return "==";
l81:
ld	hl,339f
jp	l77
;emit.c: 294:     case     61: return "!=";
l82:
ld	hl,349f
jp	l77
;emit.c: 295:     case      62: return "<=";
l83:
ld	hl,359f
jp	l77
;emit.c: 296:     case      64: return ">=";
l84:
ld	hl,369f
jp	l77
;emit.c: 297:     case    53: return "&&";
l85:
ld	hl,379f
jp	l77
;emit.c: 298:     case     54: return "||";
l86:
ld	hl,389f
jp	l77
;emit.c: 299:     case  46: return "<<";
l87:
ld	hl,399f
jp	l77
;emit.c: 300:     case  45: return ">>";
l88:
ld	hl,409f
jp	l77
;emit.c: 301:     case    30: return "++";
l89:
ld	hl,419f
jp	l77
;emit.c: 302:     case    31: return "--";
l90:
ld	hl,429f
jp	l77
;emit.c: 303:     case   50: return "->";
l91:
ld	hl,439f
jp	l77
;emit.c: 304:     case  70: return "+=";
l92:
ld	hl,449f
jp	l77
;emit.c: 305:     case   71: return "-=";
l93:
ld	hl,459f
jp	l77
;emit.c: 306:     case  72: return "*=";
l94:
ld	hl,469f
jp	l77
;emit.c: 307:     case   73: return "/=";
l95:
ld	hl,479f
jp	l77
;emit.c: 308:     case   74: return "%=";
l96:
ld	hl,489f
jp	l77
;emit.c: 309:     case   77: return "&=";
l97:
ld	hl,499f
jp	l77
;emit.c: 310:     case    78: return "|=";
l98:
ld	hl,509f
jp	l77
;emit.c: 311:     case   79: return "^=";
l99:
ld	hl,519f
jp	l77
;emit.c: 312:     case 76: return "<<=";
l100:
ld	hl,529f
jp	l77
;emit.c: 313:     case 75: return ">>=";
l101:
ld	hl,539f
jp	l77
;emit.c: 314:     default: return ((void *)0);
l102:
ld	hl,0
jp	l77
;emit.c: 315:     }
jp	l78
l79:
ld	a,(ix+6)
cp	30
jp	z,l89
cp	31
jp	z,l90
cp	45
jp	z,l88
cp	46
jp	z,l87
cp	50
jp	z,l91
cp	53
jp	z,l85
cp	54
jp	z,l86
cp	60
jp	z,l81
cp	61
jp	z,l82
cp	62
jp	z,l83
cp	64
jp	z,l84
cp	70
jp	z,l92
cp	71
jp	z,l93
cp	72
jp	z,l94
cp	73
jp	z,l95
cp	74
jp	z,l96
cp	75
jp	z,l101
cp	76
jp	z,l100
cp	77
jp	z,l97
cp	78
jp	z,l98
cp	79
jp	z,l99
cp	91
jp	z,l80
jp	l102
l78:
;emit.c: 316: }
l77:
jp	cret
f171	equ	0
;emit.c: 322: void
;emit.c: 323: emitCurToken(void)
;emit.c: 324: {
global	_emitCurToken
_emitCurToken:
call	ncsv
defw	f172
;emit.c: 325:     char buf[32];
;emit.c: 326:     char *op;
;emit.c: 329:     if (!noLineMarkers) {
ld	a,(_noLineMarkers)
or	a
jp	anz,l104
;emit.c: 330:         if (lastName != cur.filename) {
global	_cur
ld	de,(_lastName)
ld	hl,(_cur+3)
or	a
sbc	hl,de
jp	z,l105
;emit.c: 332:             emitLine(cur.lineno, cur.filename ? cur.filename : "");
ld	hl,(_cur+3)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,549f
jp	12f
10:
ld	hl,(_cur+3)
12:
push	hl
ld	hl,(_cur+1)
push	hl
call	_emitLine
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 333:             emitLinePP(cur.lineno, cur.filename ? cur.filename : "");
ld	hl,(_cur+3)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,559f
jp	12f
10:
ld	hl,(_cur+3)
12:
push	hl
ld	hl,(_cur+1)
push	hl
call	_emitLinePP
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 334:             lastLine = cur.lineno;
ld	hl,(_cur+1)
ld	(_lastLine),hl
;emit.c: 335:             lastLinePP = cur.lineno;
ld	hl,(_cur+1)
ld	(_lastLinePP),hl
;emit.c: 336:             lastName = cur.filename;
ld	hl,(_cur+3)
ld	(_lastName),hl
;emit.c: 337:         } else if (cur.lineno == lastLine + 1) {
jp	l106
l105:
ld	de,(_cur+1)
ld	hl,(_lastLine)
inc	hl
or	a
sbc	hl,de
jp	nz,l107
;emit.c: 339:             emitNewline();
call	_emitNewline
ld	hl,0
add	hl,sp
ld	sp,hl
;emit.c: 340:             lastLine = cur.lineno;
ld	hl,(_cur+1)
ld	(_lastLine),hl
;emit.c: 341:         } else if (cur.lineno != lastLine) {
jp	l108
l107:
ld	de,(_lastLine)
ld	hl,(_cur+1)
or	a
sbc	hl,de
jp	z,l109
;emit.c: 343:             emitLine(cur.lineno, cur.filename ? cur.filename : "");
ld	hl,(_cur+3)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,569f
jp	12f
10:
ld	hl,(_cur+3)
12:
push	hl
ld	hl,(_cur+1)
push	hl
call	_emitLine
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 344:             lastLine = cur.lineno;
ld	hl,(_cur+1)
ld	(_lastLine),hl
;emit.c: 345:         }
;emit.c: 347:         while (lastLinePP < cur.lineno) {
l109:
l108:
l106:
jp	l110
l111:
;emit.c: 348:             emitPPStr("\n");
ld	hl,579f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 349:             lastLinePP++;
ld	hl,(_lastLinePP)
inc	hl
ld	(_lastLinePP),hl
;emit.c: 350:         }
l110:
ld	de,(_cur+1)
ld	hl,(_lastLinePP)
global	wrelop
call	wrelop
jp	alt,l111
l112:
;emit.c: 351:     }
;emit.c: 359:     if (cur.type >=    128 && cur.type <=     160) {
l104:
ld	de,128
ld	a,(_cur)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:
ld	a,(_cur)
ld	e,a
ld	d,0
ld	hl,160
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l113
11:
;emit.c: 361:         if (cur.type ==   160)
ld	a,(_cur)
cp	.low.-96
jp	nz,l114
;emit.c: 362:             knrFilterToken( 91);
global	_knrFilterToken
ld	l,.low.91
push	hl
call	_knrFilterToken
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 363:         else if (cur.type ==       158 || cur.type ==    159)
jp	l115
l114:
ld	a,(_cur)
cp	.low.-98
jp	nz,20f
jp	21f
20:
ld	a,(_cur)
cp	.low.-97
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l116
11:
;emit.c: 364:             ;
;emit.c: 365:         else
jp	l117
l116:
;emit.c: 366:             knrFiltKw(cur.type);
global	_knrFiltKw
ld	a,(_cur)
ld	c,a
push	bc
call	_knrFiltKw
ld	hl,2
add	hl,sp
ld	sp,hl
l117:
l115:
;emit.c: 367:     } else switch (cur.type) {
jp	l118
l113:
jp	l120
;emit.c: 368:     case     20:
l121:
;emit.c: 369:         knrFilterSym(cur.v.name);
global	_knrFilterSym
ld	hl,(_cur+5)
push	hl
call	_knrFilterSym
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 370:         break;
jp	l119
;emit.c: 371:     case  21:
l122:
;emit.c: 372:         knrFiltNum(cur.v.numeric);
global	_knrFiltNum
ld	de,(_cur+5)
ld	hl,(_cur+2+5)
push	hl
push	de
call	_knrFiltNum
ld	hl,4
add	hl,sp
ld	sp,hl
;emit.c: 373:         break;
jp	l119
;emit.c: 374:     case 23:
l123:
;emit.c: 375:         knrFiltFNum(cur.v.fval);
global	_knrFiltFNum
ld	de,(_cur+5)
ld	hl,(_cur+2+5)
push	hl
push	de
call	_knrFiltFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;emit.c: 376:         break;
jp	l119
;emit.c: 377:     case  22:
l124:
;emit.c: 379:         {
;emit.c: 380:             int len = (unsigned char)cur.v.str[0] |
;emit.c: 381:                       ((unsigned char)cur.v.str[1] << 8);
ld	b,.low.8
ld	iy,(_cur+5)
ld	l,(iy+1)
ld	h,0
global	shal
call	shal
ex	de,hl
ld	hl,(_cur+5)
ld	l,(hl)
ld	h,0
ld	a,l
or	e
ld	l,a
ld	a,h
or	d
ld	h,a
ld	(ix+-36),l
ld	(ix+1+-36),h
;emit.c: 382:             knrFiltStr(cur.v.str + 2, len);
global	_knrFiltStr
ld	l,(ix+-36)
ld	h,(ix+1+-36)
push	hl
ld	hl,(_cur+5)
inc	hl
inc hl
push	hl
call	_knrFiltStr
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 383:         }
;emit.c: 384:         break;
jp	l119
;emit.c: 385:     case  118:
l125:
;emit.c: 388:         emitAsmString(cur.v.name, strlen(cur.v.name));
ld	hl,(_cur+5)
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
push	hl
ld	hl,(_cur+5)
push	hl
call	_emitAsmString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 389:         break;
jp	l119
;emit.c: 390:     case   112:
l126:
;emit.c: 392:         emitLabel(cur.v.name);
ld	hl,(_cur+5)
push	hl
call	_emitLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 393:         break;
jp	l119
;emit.c: 394:     case   0:
l127:
;emit.c: 395:         knrFilterToken(  0);
ld	l,.low.0
push	hl
call	_knrFilterToken
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 396:         break;
jp	l119
;emit.c: 397:     default:
l128:
;emit.c: 399:         knrFilterToken(cur.type);
ld	a,(_cur)
ld	c,a
push	bc
call	_knrFilterToken
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 400:         break;
jp	l119
;emit.c: 401:     }
jp	l119
l120:
ld	a,(_cur)
or	a
jp	z,l127
cp	20
jp	z,l121
cp	21
jp	z,l122
cp	22
jp	z,l124
cp	23
jp	z,l123
cp	112
jp	z,l126
cp	118
jp	z,l125
jp	l128
l119:
l118:
;emit.c: 404:     if (cur.type >=    128 && cur.type <=     160) {
ld	de,128
ld	a,(_cur)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:
ld	a,(_cur)
ld	e,a
ld	d,0
ld	hl,160
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l129
11:
;emit.c: 405:         if (cur.type ==   160)
ld	a,(_cur)
cp	.low.-96
jp	nz,l130
;emit.c: 406:             emitPPStr("sizeof ");
ld	hl,589f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 407:         else {
jp	l131
l130:
;emit.c: 408:             emitPPStr(kw2str(cur.type));
ld	a,(_cur)
ld	c,a
push	bc
call	_kw2str
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 409:             emitPPStr(" ");
ld	hl,599f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 410:         }
l131:
;emit.c: 411:     } else if ((op = op2str(cur.type)) != ((void *)0)) {
jp	l132
l129:
ld	a,(_cur)
ld	c,a
push	bc
call	_op2str
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-34),l
ld	(ix+1+-34),h
ld	a,l
or	h
jp	z,l133
;emit.c: 412:         emitPPStr(op);
ld	l,(ix+-34)
ld	h,(ix+1+-34)
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 413:         emitPPStr(" ");
ld	hl,609f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 414:     } else {
jp	l134
l133:
;emit.c: 415:         switch (cur.type) {
jp	l136
;emit.c: 416:         case     20:
l137:
;emit.c: 417:             emitPPStr(cur.v.name);
ld	hl,(_cur+5)
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 418:             emitPPStr(" ");
ld	hl,619f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 419:             break;
jp	l135
;emit.c: 420:         case  21:
l138:
;emit.c: 421:             sprintf(buf, "%ld ", cur.v.numeric);
ld	de,(_cur+5)
ld	hl,(_cur+2+5)
push	hl
push	de
ld	hl,629f
push	hl
push	ix
pop	de
ld	hl,-32
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+4
add	hl,sp
ld	sp,hl
;emit.c: 422:             emitPPStr(buf);
push	ix
pop	de
ld	hl,-32
add	hl,de
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 423:             break;
jp	l135
;emit.c: 424:         case 23:
l139:
;emit.c: 425:             sprintf(buf, "%g ", cur.v.fval);
ld	de,(_cur+5)
ld	hl,(_cur+2+5)
push	hl
push	de
ld	hl,639f
push	hl
push	ix
pop	de
ld	hl,-32
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+4
add	hl,sp
ld	sp,hl
;emit.c: 426:             emitPPStr(buf);
push	ix
pop	de
ld	hl,-32
add	hl,de
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 427:             break;
jp	l135
;emit.c: 428:         case  22:
l140:
;emit.c: 429:             {
;emit.c: 430:                 int len = (unsigned char)cur.v.str[0] |
;emit.c: 431:                           ((unsigned char)cur.v.str[1] << 8);
ld	b,.low.8
ld	iy,(_cur+5)
ld	l,(iy+1)
ld	h,0
global	shal
call	shal
ex	de,hl
ld	hl,(_cur+5)
ld	l,(hl)
ld	h,0
ld	a,l
or	e
ld	l,a
ld	a,h
or	d
ld	h,a
ld	(ix+-38),l
ld	(ix+1+-38),h
;emit.c: 432:                 emitPPStr("\"");
ld	hl,649f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 433:                 emitPPString(cur.v.str + 2, len);
ld	l,(ix+-38)
ld	h,(ix+1+-38)
push	hl
ld	hl,(_cur+5)
inc	hl
inc hl
push	hl
call	_emitPPString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 434:                 emitPPStr("\" ");
ld	hl,659f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 435:             }
;emit.c: 436:             break;
jp	l135
;emit.c: 437:         case  118:
l141:
;emit.c: 438:             emitPPStr("{ ");
ld	hl,669f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 439:             emitPPString(cur.v.name, strlen(cur.v.name));
ld	hl,(_cur+5)
push	hl
call	_strlen
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
push	hl
ld	hl,(_cur+5)
push	hl
call	_emitPPString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;emit.c: 440:             emitPPStr(" } ");
ld	hl,679f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 441:             break;
jp	l135
;emit.c: 442:         case   112:
l142:
;emit.c: 443:             emitPPStr(cur.v.name);
ld	hl,(_cur+5)
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 444:             emitPPStr(": ");
ld	hl,689f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 445:             break;
jp	l135
;emit.c: 446:         case    1:
l143:
;emit.c: 447:             emitPPStr("; ");
ld	hl,699f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 448:             break;
jp	l135
;emit.c: 449:         case   2:
l144:
;emit.c: 450:             emitPPStr("{ ");
ld	hl,709f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 451:             break;
jp	l135
;emit.c: 452:         case     3:
l145:
;emit.c: 453:             emitPPStr("} ");
ld	hl,719f
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 454:             break;
jp	l135
;emit.c: 455:         case   0:
l146:
;emit.c: 456:             break;
jp	l135
;emit.c: 457:         default:
l147:
;emit.c: 459:             buf[0] = 0;
ld	(ix+-32),0
;emit.c: 460:             switch (cur.type) {
jp	l149
;emit.c: 461:             case    40: buf[0] = '+'; break;
l150:
ld	(ix+-32),43
jp	l148
;emit.c: 462:             case   41: buf[0] = '-'; break;
l151:
ld	(ix+-32),45
jp	l148
;emit.c: 463:             case    36: case   42: buf[0] = '*'; break;
l152:
l153:
ld	(ix+-32),42
jp	l148
;emit.c: 464:             case     43: buf[0] = '/'; break;
l154:
ld	(ix+-32),47
jp	l148
;emit.c: 465:             case     44: buf[0] = '%'; break;
l155:
ld	(ix+-32),37
jp	l148
;emit.c: 466:             case   35: case     47: buf[0] = '&'; break;
l156:
l157:
ld	(ix+-32),38
jp	l148
;emit.c: 467:             case      48: buf[0] = '|'; break;
l158:
ld	(ix+-32),124
jp	l148
;emit.c: 468:             case     49: buf[0] = '^'; break;
l159:
ld	(ix+-32),94
jp	l148
;emit.c: 469:             case      63: buf[0] = '<'; break;
l160:
ld	(ix+-32),60
jp	l148
;emit.c: 470:             case      65: buf[0] = '>'; break;
l161:
ld	(ix+-32),62
jp	l148
;emit.c: 471:             case    34: buf[0] = '!'; break;
l162:
ld	(ix+-32),33
jp	l148
;emit.c: 472:             case 38: buf[0] = '~'; break;
l163:
ld	(ix+-32),126
jp	l148
;emit.c: 473:             case    90: buf[0] = '?'; break;
l164:
ld	(ix+-32),63
jp	l148
;emit.c: 474:             case   8: buf[0] = ':'; break;
l165:
ld	(ix+-32),58
jp	l148
;emit.c: 475:             case     39: buf[0] = '.'; break;
l166:
ld	(ix+-32),46
jp	l148
;emit.c: 476:             case  80: buf[0] = '='; break;
l167:
ld	(ix+-32),61
jp	l148
;emit.c: 477:             case    6: buf[0] = '('; break;
l168:
ld	(ix+-32),40
jp	l148
;emit.c: 478:             case    7: buf[0] = ')'; break;
l169:
ld	(ix+-32),41
jp	l148
;emit.c: 479:             case  4: buf[0] = '['; break;
l170:
ld	(ix+-32),91
jp	l148
;emit.c: 480:             case  5: buf[0] = ']'; break;
l171:
ld	(ix+-32),93
jp	l148
;emit.c: 481:             case   9: buf[0] = ','; break;
l172:
ld	(ix+-32),44
jp	l148
;emit.c: 482:             }
jp	l148
l149:
ld	a,(_cur)
cp	4
jp	z,l170
cp	5
jp	z,l171
cp	6
jp	z,l168
cp	7
jp	z,l169
cp	8
jp	z,l165
cp	9
jp	z,l172
cp	34
jp	z,l162
cp	35
jp	z,l156
cp	36
jp	z,l152
cp	38
jp	z,l163
cp	39
jp	z,l166
cp	40
jp	z,l150
cp	41
jp	z,l151
cp	42
jp	z,l153
cp	43
jp	z,l154
cp	44
jp	z,l155
cp	47
jp	z,l157
cp	48
jp	z,l158
cp	49
jp	z,l159
cp	63
jp	z,l160
cp	65
jp	z,l161
cp	80
jp	z,l167
cp	90
jp	z,l164
jp	l148
l148:
;emit.c: 483:             if (buf[0]) {
ld	a,(ix+-32)
or	a
jp	az,l173
;emit.c: 484:                 buf[1] = ' ';
ld	(ix+-31),32
;emit.c: 485:                 buf[2] = 0;
ld	(ix+-30),0
;emit.c: 486:                 emitPPStr(buf);
push	ix
pop	de
ld	hl,-32
add	hl,de
push	hl
call	_emitPPStr
ld	hl,2
add	hl,sp
ld	sp,hl
;emit.c: 487:             }
;emit.c: 488:             break;
l173:
jp	l135
;emit.c: 489:         }
jp	l135
l136:
ld	a,(_cur)
or	a
jp	z,l146
cp	1
jp	z,l143
cp	2
jp	z,l144
cp	3
jp	z,l145
cp	20
jp	z,l137
cp	21
jp	z,l138
cp	22
jp	z,l140
cp	23
jp	z,l139
cp	112
jp	z,l142
cp	118
jp	z,l141
jp	l147
l135:
;emit.c: 490:     }
l134:
l132:
;emit.c: 491: }
l103:
jp	cret
f172	equ	-38
psect	data
19:
defb	35,32,37,100,32,34,37,115,34,10,0
29:
defb	10,35,32,37,100,32,34,37,115,34,10,0
39:
defb	105,110,116,0
49:
defb	99,104,97,114,0
59:
defb	108,111,110,103,0
69:
defb	102,108,111,97,116,0
79:
defb	100,111,117,98,108,101,0
89:
defb	118,111,105,100,0
99:
defb	117,110,115,105,103,110,101,100,0
109:
defb	115,116,97,116,105,99,0
119:
defb	101,120,116,101,114,110,0
129:
defb	97,117,116,111,0
139:
defb	114,101,103,105,115,116,101,114,0
149:
defb	116,121,112,101,100,101,102,0
159:
defb	115,116,114,117,99,116,0
169:
defb	117,110,105,111,110,0
179:
defb	101,110,117,109,0
189:
defb	105,102,0
199:
defb	101,108,115,101,0
209:
defb	119,104,105,108,101,0
219:
defb	100,111,0
229:
defb	102,111,114,0
239:
defb	115,119,105,116,99,104,0
249:
defb	99,97,115,101,0
259:
defb	100,101,102,97,117,108,116,0
269:
defb	98,114,101,97,107,0
279:
defb	99,111,110,116,105,110,117,101,0
289:
defb	114,101,116,117,114,110,0
299:
defb	103,111,116,111,0
309:
defb	97,115,109,0
319:
defb	63,107,119,63,0
329:
defb	115,105,122,101,111,102,0
339:
defb	61,61,0
349:
defb	33,61,0
359:
defb	60,61,0
369:
defb	62,61,0
379:
defb	38,38,0
389:
defb	124,124,0
399:
defb	60,60,0
409:
defb	62,62,0
419:
defb	43,43,0
429:
defb	45,45,0
439:
defb	45,62,0
449:
defb	43,61,0
459:
defb	45,61,0
469:
defb	42,61,0
479:
defb	47,61,0
489:
defb	37,61,0
499:
defb	38,61,0
509:
defb	124,61,0
519:
defb	94,61,0
529:
defb	60,60,61,0
539:
defb	62,62,61,0
549:
defb	0
559:
defb	0
569:
defb	0
579:
defb	10,0
589:
defb	115,105,122,101,111,102,32,0
599:
defb	32,0
609:
defb	32,0
619:
defb	32,0
629:
defb	37,108,100,32,0
639:
defb	37,103,32,0
649:
defb	34,0
659:
defb	34,32,0
669:
defb	123,32,0
679:
defb	32,125,32,0
689:
defb	58,32,0
699:
defb	59,32,0
709:
defb	123,32,0
719:
defb	125,32,0
