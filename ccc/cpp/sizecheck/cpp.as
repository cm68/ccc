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
;../../libsrc/include/fcntl.h: 20: extern int open(char *path, int flags);
;../../libsrc/include/fcntl.h: 21: extern int creat(char *path, int mode);
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
;cpp.c: 17: char *curFile;
;cpp.c: 18: int lineNo;
;cpp.c: 19: char exitCode = 0;
psect	data
global	_exitCode
_exitCode:
defb	0
;cpp.c: 20: char noLineMarkers = 0;
global	_noLineMarkers
_noLineMarkers:
defb	0
;cpp.c: 24: char *includePaths[32];
;cpp.c: 25: unsigned char numIncludes = 0;
global	_numIncludes
_numIncludes:
defb	0
;cpp.c: 30: void
;cpp.c: 31: error(char *msg)
;cpp.c: 32: {
psect	text
global	_error
_error:
global	ncsv, cret, indir
call	ncsv
defw	f151
;cpp.c: 33:     fprintf(stderr, "%s:%d: error: %s\n", filename ? filename : curFile, lineno, msg);
global	_fprintf
global	_stderr
global	_filename
global	_curFile
global	_lineno
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,(_lineno)
push	hl
ld	hl,(_filename)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,(_curFile)
jp	12f
10:
ld	hl,(_filename)
12:
push	hl
ld	hl,19f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;cpp.c: 34:     exitCode = 1;
ld	a,.low.1
ld	(_exitCode),a
;cpp.c: 35: }
l7:
jp	cret
f151	equ	0
;cpp.c: 37: void
;cpp.c: 38: fatal(char *msg)
;cpp.c: 39: {
global	_fatal
_fatal:
call	ncsv
defw	f155
;cpp.c: 40:     fprintf(stderr, "%s:%d: fatal: %s\n", filename ? filename : curFile, lineno, msg);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,(_lineno)
push	hl
ld	hl,(_filename)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,(_curFile)
jp	12f
10:
ld	hl,(_filename)
12:
push	hl
ld	hl,29f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;cpp.c: 41:     exit(1);
global	_exit
ld	hl,1
push	hl
call	_exit
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 42: }
l8:
jp	cret
f155	equ	0
;cpp.c: 44: void
;cpp.c: 45: usage(void)
;cpp.c: 46: {
global	_usage
_usage:
call	ncsv
defw	f156
;cpp.c: 47:     fprintf(stderr, "usage: cpp [options] <source.c>\n");
ld	hl,39f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 48:     fprintf(stderr, "  -o <base>      Output base name (produces <base>.x and <base>.i)\n");
ld	hl,49f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 49:     fprintf(stderr, "  -I<dir>        Add include directory\n");
ld	hl,59f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 50:     fprintf(stderr, "  -i<dir>        System include directory\n");
ld	hl,69f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 51:     fprintf(stderr, "  -D<name>[=val] Define macro\n");
ld	hl,79f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 52:     fprintf(stderr, "  -E             Preprocess only (output to stdout)\n");
ld	hl,89f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 53:     exit(1);
ld	hl,1
push	hl
call	_exit
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 54: }
l9:
jp	cret
f156	equ	0
;cpp.c: 59: void
;cpp.c: 60: process(char *sourcefile)
;cpp.c: 61: {
global	_process
_process:
call	ncsv
defw	f157
;cpp.c: 62:     curFile = sourcefile;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(_curFile),hl
;cpp.c: 65:     pushfile(sourcefile);
global	_pushfile
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_pushfile
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 66:     ioinit();
global	_ioinit
call	_ioinit
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 69:     emitFileStart(sourcefile);
global	_emitFileStart
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_emitFileStart
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 72:     gettoken();
global	_gettoken
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 73:     gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 76:     while (cur.type !=   0) {
jp	l11
l12:
;cpp.c: 77:         emitCurToken();
global	_emitCurToken
call	_emitCurToken
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 78:         gettoken();
call	_gettoken
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 79:     }
l11:
global	_cur
ld	a,(_cur)
or	a
jp	lnz,l12
l13:
;cpp.c: 82:     emitToken(  0);
global	_emitToken
ld	l,.low.0
push	hl
call	_emitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 83: }
l10:
jp	cret
f157	equ	0
;cpp.c: 85: char lexFile[128];
;cpp.c: 86: char ppFile[128];
;cpp.c: 88: int
;cpp.c: 89: main(int argc, char **argv)
;cpp.c: 90: {
global	_main
_main:
call	ncsv
defw	f161
;cpp.c: 91:     char *source = ((void *)0);
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
;cpp.c: 92:     char *outbase = ((void *)0);
ld	(ix+-4),.low.0
ld	(ix+1+-4),.high.0
;cpp.c: 93:     int i;
;cpp.c: 94:     int ppOnly = 0;
ld	(ix+-8),.low.0
ld	(ix+1+-8),.high.0
;cpp.c: 97:     for (i = 1; i < argc; i++) {
ld	(ix+-6),.low.1
ld	(ix+1+-6),.high.1
jp	l18
l15:
;cpp.c: 98:         if (strcmp(argv[i], "-o") == 0) {
global	_strcmp
ld	hl,99f
push	hl
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
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
jp	nz,l19
;cpp.c: 99:             if (++i >= argc) usage();
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
inc	hl
ld	(ix+-6),l
ld	(ix+1+-6),h
global	wrelop
call	wrelop
jp	alt,l20
call	_usage
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 100:             outbase = argv[i];
l20:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-4),c
ld	(ix+1+-4),b
;cpp.c: 101:         } else if (argv[i][0] == '-' && argv[i][1] == 'I') {
jp	l21
l19:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
cp	.low.45
jp	nz,20f
jp	21f
21:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
inc	hl
ld	a,(hl)
cp	.low.73
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l22
11:
;cpp.c: 103:             if (numIncludes < 32)
ld	de,32
ld	a,(_numIncludes)
ld	l,a
ld	h,0
global	wrelop
call	wrelop
jp	lge,l23
;cpp.c: 104:                 includePaths[numIncludes++] = argv[i] + 2;
global	_includePaths
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
inc	bc
inc bc
ld	a,(_numIncludes)
ld	e,a
inc	a
ld	(_numIncludes),a
ld	l,e
ld	h,0
add	hl,hl
ld	de,_includePaths
add	hl,de
ld	(hl),c
inc	hl
ld	(hl),b
;cpp.c: 105:         } else if (argv[i][0] == '-' && argv[i][1] == 'i') {
l23:
jp	l24
l22:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
cp	.low.45
jp	nz,20f
jp	21f
21:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
inc	hl
ld	a,(hl)
cp	.low.105
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l25
11:
;cpp.c: 107:             sysIncPath = argv[i] + 2;
global	_sysIncPath
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
inc	bc
inc bc
ld	(_sysIncPath),bc
;cpp.c: 108:         } else if (argv[i][0] == '-' && argv[i][1] == 'D') {
jp	l26
l25:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
cp	.low.45
jp	nz,20f
jp	21f
21:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
inc	hl
ld	a,(hl)
cp	.low.68
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l27
11:
;cpp.c: 110:             addDefine(argv[i] + 2);
global	_addDefine
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
inc	bc
inc bc
push	bc
call	_addDefine
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 111:         } else if (strcmp(argv[i], "-E") == 0) {
jp	l28
l27:
ld	hl,109f
push	hl
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
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
jp	nz,l29
;cpp.c: 112:             ppOnly = 1;
ld	(ix+-8),.low.1
ld	(ix+1+-8),.high.1
;cpp.c: 113:         } else if (strcmp(argv[i], "-N") == 0) {
jp	l30
l29:
ld	hl,119f
push	hl
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
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
jp	nz,l31
;cpp.c: 114:             noLineMarkers = 1;
ld	a,.low.1
ld	(_noLineMarkers),a
;cpp.c: 115:         } else if (argv[i][0] == '-') {
jp	l32
l31:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
cp	.low.45
jp	nz,l33
;cpp.c: 116:             fprintf(stderr, "Unknown option: %s\n", argv[i]);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	hl,129f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;cpp.c: 117:             usage();
call	_usage
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 118:         } else {
jp	l34
l33:
;cpp.c: 119:             source = argv[i];
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-2),c
ld	(ix+1+-2),b
;cpp.c: 120:         }
l34:
l32:
l30:
l28:
l26:
l24:
l21:
;cpp.c: 121:     }
l17:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
inc	hl
ld	(ix+-6),l
ld	(ix+1+-6),h
l18:
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
global	wrelop
call	wrelop
jp	alt,l15
l16:
;cpp.c: 123:     if (!source) {
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l35
;cpp.c: 124:         fprintf(stderr, "No source file specified\n");
ld	hl,139f
push	hl
ld	hl,(_stderr)
push	hl
call	_fprintf
ld	hl,2+2
add	hl,sp
ld	sp,hl
;cpp.c: 125:         usage();
call	_usage
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 126:     }
;cpp.c: 129:     if (!outbase) {
l35:
ld	a,(ix+-4)
or	(ix+1+-4)
jp	nz,l36
;cpp.c: 130:         char *dot;
;cpp.c: 131:         outbase = strdup(source);
global	_strdup
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_strdup
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-4),l
ld	(ix+1+-4),h
;cpp.c: 132:         dot = strrchr(outbase, '.');
global	_strrchr
ld	hl,46
push	hl
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
call	_strrchr
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-10),l
ld	(ix+1+-10),h
;cpp.c: 133:         if (dot) *dot = '\0';
ld	a,(ix+-10)
or	(ix+1+-10)
jp	z,l37
ld	l,(ix+-10)
ld	h,(ix+1+-10)
ld	(hl),0
;cpp.c: 134:     }
l37:
;cpp.c: 137:     sprintf(lexFile, "%s.x", outbase);
l36:
global	_sprintf
global	_lexFile
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
ld	hl,149f
push	hl
ld	hl,_lexFile
push	hl
call	_sprintf
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;cpp.c: 138:     sprintf(ppFile, "%s.i", outbase);
global	_ppFile
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
ld	hl,159f
push	hl
ld	hl,_ppFile
push	hl
call	_sprintf
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;cpp.c: 141:     lexFd = creat(lexFile, 0644);
global	_lexFd
global	_creat
ld	hl,420
push	hl
ld	hl,_lexFile
push	hl
call	_creat
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	(_lexFd),a
;cpp.c: 142:     if (lexFd < 0) {
ld	a,(_lexFd)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l38
;cpp.c: 143:         perror(lexFile);
global	_perror
ld	hl,_lexFile
push	hl
call	_perror
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 144:         exit(1);
ld	hl,1
push	hl
call	_exit
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 145:     }
;cpp.c: 147:     ppFd = creat(ppFile, 0644);
l38:
global	_ppFd
ld	hl,420
push	hl
ld	hl,_ppFile
push	hl
call	_creat
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	(_ppFd),a
;cpp.c: 148:     if (ppFd < 0) {
ld	a,(_ppFd)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l39
;cpp.c: 149:         perror(ppFile);
ld	hl,_ppFile
push	hl
call	_perror
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 150:         exit(1);
ld	hl,1
push	hl
call	_exit
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 151:     }
;cpp.c: 154:     addInclude("");
l39:
global	_addInclude
ld	hl,169f
push	hl
call	_addInclude
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 155:     for (i = 0; i < numIncludes; i++) {
ld	(ix+-6),.low.0
ld	(ix+1+-6),.high.0
jp	l43
l40:
;cpp.c: 156:         addInclude(includePaths[i]);
ld	de,_includePaths
ld	l,(ix+-6)
ld	h,(ix+1+-6)
add	hl,hl
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_addInclude
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 157:     }
l42:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
inc	hl
ld	(ix+-6),l
ld	(ix+1+-6),h
l43:
ld	a,(_numIncludes)
ld	e,a
ld	d,0
ld	l,(ix+-6)
ld	h,(ix+1+-6)
global	wrelop
call	wrelop
jp	llt,l40
l41:
;cpp.c: 160:     knrInit();
global	_knrInit
call	_knrInit
ld	hl,0
add	hl,sp
ld	sp,hl
;cpp.c: 163:     (void)ppOnly;
ld	l,(ix+-8)
ld	h,(ix+1+-8)
;cpp.c: 164:     process(source);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_process
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 166:     close(lexFd);
global	_close
ld	a,(_lexFd)
ld	c,a
push	bc
call	_close
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 167:     close(ppFd);
ld	a,(_ppFd)
ld	c,a
push	bc
call	_close
ld	hl,2
add	hl,sp
ld	sp,hl
;cpp.c: 169:     return exitCode;
ld	a,(_exitCode)
ld	l,a
rla
sbc	a,a
ld	h,a
jp	l14
;cpp.c: 170: }
l14:
jp	cret
f161	equ	-10
psect	data
19:
defb	37,115,58,37,100,58,32,101,114,114,111,114,58,32,37,115
defb	10,0
29:
defb	37,115,58,37,100,58,32,102,97,116,97,108,58,32,37,115
defb	10,0
39:
defb	117,115,97,103,101,58,32,99,112,112,32,91,111,112,116,105
defb	111,110,115,93,32,60,115,111,117,114,99,101,46,99,62,10
defb	0
49:
defb	32,32,45,111,32,60,98,97,115,101,62,32,32,32,32,32
defb	32,79,117,116,112,117,116,32,98,97,115,101,32,110,97,109
defb	101,32,40,112,114,111,100,117,99,101,115,32,60,98,97,115
defb	101,62,46,120,32,97,110,100,32,60,98,97,115,101,62,46
defb	105,41,10,0
59:
defb	32,32,45,73,60,100,105,114,62,32,32,32,32,32,32,32
defb	32,65,100,100,32,105,110,99,108,117,100,101,32,100,105,114
defb	101,99,116,111,114,121,10,0
69:
defb	32,32,45,105,60,100,105,114,62,32,32,32,32,32,32,32
defb	32,83,121,115,116,101,109,32,105,110,99,108,117,100,101,32
defb	100,105,114,101,99,116,111,114,121,10,0
79:
defb	32,32,45,68,60,110,97,109,101,62,91,61,118,97,108,93
defb	32,68,101,102,105,110,101,32,109,97,99,114,111,10,0
89:
defb	32,32,45,69,32,32,32,32,32,32,32,32,32,32,32,32
defb	32,80,114,101,112,114,111,99,101,115,115,32,111,110,108,121
defb	32,40,111,117,116,112,117,116,32,116,111,32,115,116,100,111
defb	117,116,41,10,0
99:
defb	45,111,0
109:
defb	45,69,0
119:
defb	45,78,0
129:
defb	85,110,107,110,111,119,110,32,111,112,116,105,111,110,58,32
defb	37,115,10,0
139:
defb	78,111,32,115,111,117,114,99,101,32,102,105,108,101,32,115
defb	112,101,99,105,102,105,101,100,10,0
149:
defb	37,115,46,120,0
159:
defb	37,115,46,105,0
169:
defb	0
psect	bss
_curFile:
	defs	2
_ppFile:
	defs	128
global	_lineNo
_lineNo:
	defs	2
_lexFile:
	defs	128
_includePaths:
	defs	64
