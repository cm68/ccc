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
;io.c: 20: unsigned char curchar;
;io.c: 21: unsigned char nextchar;
;io.c: 22: int lineno;
;io.c: 23: char *filename;
;io.c: 24: char column;
;io.c: 25: char nextcol = 0;
psect	data
global	_nextcol
_nextcol:
defb	0
;io.c: 26: char namebuf[128];
;io.c: 35: struct textbuf *tbtop;
;io.c: 85: struct include {
;io.c: 86:     char *path;
;io.c: 87:     struct include *next;
;io.c: 88: } *includes;
;io.c: 94: char *sysIncPath = "/usr/include";
global	_sysIncPath
_sysIncPath:
defw	19f
;io.c: 117: void
;io.c: 118: addInclude(char *s)
;io.c: 119: {
psect	text
global	_addInclude
_addInclude:
global	ncsv, cret, indir
call	ncsv
defw	f154
;io.c: 120:     struct include *i, *ip;
;io.c: 121:     i = malloc(sizeof(*i));
global	_malloc
ld	hl,4
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
;io.c: 122:     i->path = strdup(s);
global	_strdup
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
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 123:     i->next = 0;
ld	de,0
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 124:     if (includes) {
global	_includes
ld	hl,(_includes)
ld	a,l
or	h
jp	z,l9
;io.c: 125:         ip = includes;
ld	hl,(_includes)
ld	(ix+-4),l
ld	(ix+1+-4),h
;io.c: 126:         while (ip->next) {
jp	l10
l11:
;io.c: 127:             ip = ip->next;
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-4),c
ld	(ix+1+-4),b
;io.c: 128:         }
l10:
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
inc hl
ld	a,(hl)
inc	hl
or	(hl)
jp	nz,l11
l12:
;io.c: 129:         ip->next = i;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 130:     } else {
jp	l13
l9:
;io.c: 131:         includes = i;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_includes),hl
;io.c: 132:     }
l13:
;io.c: 138: }
l8:
jp	cret
f154	equ	-4
;io.c: 143: void
;io.c: 144: pushfile(char *name)
;io.c: 145: {
global	_pushfile
_pushfile:
call	ncsv
defw	f155
;io.c: 146:     struct textbuf *t;
;io.c: 148:     t = malloc(sizeof(*t));
ld	hl,19
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
;io.c: 149:     t->fd = open(name, 0);
global	_open
ld	hl,0
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_open
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),a
;io.c: 150:     if (t->fd < 0) {
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l15
;io.c: 151:         fdprintf(2, "cannot open: %s\n", name);
global	_fdprintf
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,29f
push	hl
ld	hl,2
push	hl
call	_fdprintf
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 152:         exit(1);
global	_exit
ld	hl,1
push	hl
call	_exit
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 153:     }
;io.c: 154:     t->name = strdup(name);
l15:
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
;io.c: 155:     t->offset = t->valid = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 156:     t->lineno = 1;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,9
add	hl,de
ld	de,1
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 157:     t->storage = malloc(512          );
ld	hl,512
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
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 158:     t->saved_column = column;
global	_column
ld	a,(_column)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,15
add	hl,de
ld	(hl),a
;io.c: 159:     t->prev = tbtop;
global	_tbtop
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	de,(_tbtop)
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 160:     tbtop = t;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_tbtop),hl
;io.c: 161:     filename = t->name;
global	_filename
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_filename),bc
;io.c: 162:     lineno = 1;
global	_lineno
ld	hl,1
ld	(_lineno),hl
;io.c: 163: }
l14:
jp	cret
f155	equ	-2
;io.c: 197: void
;io.c: 198: insertfile(char *name, int sys)
;io.c: 199: {
global	_insertfile
_insertfile:
call	ncsv
defw	f156
;io.c: 200: 	struct textbuf *t;
;io.c: 201:     struct include *i;
;io.c: 217: 	t = malloc(sizeof(*t));
ld	hl,19
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
;io.c: 218:     t->fd = -1;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),-1
;io.c: 223:     if (sys && sysIncPath) {
ld	a,(ix+8)
or	(ix+1+8)
jp	z,20f
jp	21f
21:
ld	hl,(_sysIncPath)
ld	a,l
or	h
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l17
11:
;io.c: 224:         strcpy(namebuf, sysIncPath);
global	_strcpy
global	_namebuf
ld	hl,(_sysIncPath)
push	hl
ld	hl,_namebuf
push	hl
call	_strcpy
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 225:         strcat(namebuf, "/");
global	_strcat
ld	hl,39f
push	hl
ld	hl,_namebuf
push	hl
call	_strcat
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 226:         strcat(namebuf, name);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,_namebuf
push	hl
call	_strcat
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 227:         t->fd = open(namebuf, 0);
ld	hl,0
push	hl
ld	hl,_namebuf
push	hl
call	_open
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),a
;io.c: 228:         if (t->fd > 0) {
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
ld	e,a
rla
sbc	a,a
ld	d,a
ld	hl,0
global	wrelop
call	wrelop
jp	age,l18
;io.c: 229:             goto found;
jp	l19
;io.c: 230:         }
;io.c: 231:     }
l18:
;io.c: 236:     for (i = includes; i; i = i->next) {
l17:
ld	hl,(_includes)
ld	(ix+-4),l
ld	(ix+1+-4),h
jp	l23
l20:
;io.c: 237:         if (i->path[0]) {
ld	l,(ix+-4)
ld	h,(ix+1+-4)
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
or	a
jp	az,l24
;io.c: 238:             strcpy(namebuf, i->path);
ld	l,(ix+-4)
ld	h,(ix+1+-4)
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	hl,_namebuf
push	hl
call	_strcpy
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 239:             strcat(namebuf, "/");
ld	hl,49f
push	hl
ld	hl,_namebuf
push	hl
call	_strcat
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 240:             strcat(namebuf, name);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,_namebuf
push	hl
call	_strcat
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 241:         } else {
jp	l25
l24:
;io.c: 243:             strcpy(namebuf, name);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,_namebuf
push	hl
call	_strcpy
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 244:         }
l25:
;io.c: 245:         t->fd = open(namebuf, 0);
ld	hl,0
push	hl
ld	hl,_namebuf
push	hl
call	_open
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),a
;io.c: 246:         if (t->fd > 0) {
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
ld	e,a
rla
sbc	a,a
ld	d,a
ld	hl,0
global	wrelop
call	wrelop
jp	age,l26
;io.c: 247:             break;
jp	l21
;io.c: 248:         }
;io.c: 249:     }
l26:
l22:
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-4),c
ld	(ix+1+-4),b
l23:
ld	a,(ix+-4)
or	(ix+1+-4)
jp	nz,l20
l21:
;io.c: 250:     if (t->fd == -1) {
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
cp	.low.-1
jp	nz,l27
;io.c: 251:         free(t);
global	_free
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 252:         fdprintf(2, "cannot find include file: %s\n", name);
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,59f
push	hl
ld	hl,2
push	hl
call	_fdprintf
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 253:         exit(1);
ld	hl,1
push	hl
call	_exit
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 254:     }
;io.c: 255: found:
l27:
l19:
;io.c: 256: 	t->name = strdup(namebuf);
ld	hl,_namebuf
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
;io.c: 257: 	t->offset = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 258: 	t->lineno = 1;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,9
add	hl,de
ld	de,1
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 259: 	t->storage = malloc(512          );
ld	hl,512
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
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 260: 	t->saved_column = column;
ld	a,(_column)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,15
add	hl,de
ld	(hl),a
;io.c: 261: 	t->prev = tbtop;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	de,(_tbtop)
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 262: 	tbtop = t;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_tbtop),hl
;io.c: 263:     filename = t->name;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_filename),bc
;io.c: 264:     lineno = 1;
ld	hl,1
ld	(_lineno),hl
;io.c: 266:     t->valid = read(t->fd, t->storage, 512          );
global	_read
ld	hl,512
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	l,(hl)
push	hl
call	_read
exx
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 267:     if (t->valid > 0) {
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l28
;io.c: 268:         curchar = t->storage[t->offset++];
global	_curchar
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ex	de,hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
inc	hl
ex	de,hl
ld	(hl),d
dec	hl
ld	(hl),e
ex	de,hl
dec	hl
add	hl,bc
ld	a,(hl)
ld	(_curchar),a
;io.c: 269:         nextchar = (t->offset < t->valid) ? t->storage[t->offset] : 0;
global	_nextchar
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
pop	de
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
21:
ld	hl,0
jp	22f
20:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
add	hl,bc
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
22:
ld	a,l
ld	(_nextchar),a
;io.c: 271:         column = 0;
ld	a,.low.0
ld	(_column),a
;io.c: 272:         nextcol = 1;
ld	a,.low.1
ld	(_nextcol),a
;io.c: 273:     } else {
jp	l29
l28:
;io.c: 274:         curchar = nextchar = 0;
ld	a,.low.0
ld	(_nextchar),a
ld	(_curchar),a
;io.c: 275:         column = nextcol = 0;
ld	a,.low.0
ld	(_nextcol),a
ld	(_column),a
;io.c: 276:     }
l29:
;io.c: 277: }
l16:
jp	cret
f156	equ	-4
;io.c: 314: void
;io.c: 315: insertmacro(char *name, char *macbuf)
;io.c: 316: {
global	_insertmacro
_insertmacro:
call	ncsv
defw	f157
;io.c: 317: 	struct textbuf *t;
;io.c: 318:     int l;
;io.c: 321:     l = strlen(macbuf);
global	_strlen
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
ld	(ix+-4),l
ld	(ix+1+-4),h
;io.c: 327:     t = tbtop;
ld	hl,(_tbtop)
ld	(ix+-2),l
ld	(ix+1+-2),h
;io.c: 330:     if (t->offset > l) {
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	wrelop
call	wrelop
jp	age,l31
;io.c: 331:         ;
;io.c: 332:         t->offset -= l;
ld	c,(ix+-4)
ld	b,(ix+1+-4)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ex	de,hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
or	a
sbc	hl,bc
ex	de,hl
ld	(hl),d
dec	hl
ld	(hl),e
ex	de,hl
;io.c: 333:         strncpy(&t->storage[t->offset], macbuf, l);
global	_strncpy
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
add	hl,bc
push	hl
call	_strncpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 334:         curchar = t->storage[t->offset++];
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ex	de,hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
inc	hl
ex	de,hl
ld	(hl),d
dec	hl
ld	(hl),e
ex	de,hl
dec	hl
add	hl,bc
ld	a,(hl)
ld	(_curchar),a
;io.c: 335:         nextchar = t->storage[t->offset];
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
add	hl,bc
ld	a,(hl)
ld	(_nextchar),a
;io.c: 336:         ;
;io.c: 337:         return;
jp	l30
;io.c: 338:     }
;io.c: 341: 	t = malloc(sizeof(*t));
l31:
ld	hl,19
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
;io.c: 342: 	t->fd = -1;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),-1
;io.c: 343: 	t->name = strdup(name);
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
;io.c: 344: 	t->lineno = lineno;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,9
add	hl,de
ld	de,(_lineno)
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 345: 	t->offset = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 346: 	t->storage = strdup(macbuf);
ld	l,(ix+8)
ld	h,(ix+1+8)
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
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 347: 	t->valid = strlen(t->storage);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_strlen
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
ld	hl,7
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 348: 	t->saved_column = column;
ld	a,(_column)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,15
add	hl,de
ld	(hl),a
;io.c: 349: 	t->prev = tbtop;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	de,(_tbtop)
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 350: 	tbtop = t;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_tbtop),hl
;io.c: 351:     filename = name;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(_filename),hl
;io.c: 353:     curchar = t->storage[t->offset++];
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ex	de,hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
inc	hl
ex	de,hl
ld	(hl),d
dec	hl
ld	(hl),e
ex	de,hl
dec	hl
add	hl,bc
ld	a,(hl)
ld	(_curchar),a
;io.c: 354:     nextchar = t->storage[t->offset];
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
add	hl,bc
ld	a,(hl)
ld	(_nextchar),a
;io.c: 355: }
l30:
jp	cret
f157	equ	-4
;io.c: 400: void
;io.c: 401: advance()
;io.c: 402: {
global	_advance
_advance:
call	ncsv
defw	f158
;io.c: 403: 	struct textbuf *t;
;io.c: 405: again:
l33:
;io.c: 406:     t = tbtop;
ld	hl,(_tbtop)
ld	(ix+-2),l
ld	(ix+1+-2),h
;io.c: 408:     curchar = nextchar;
ld	a,(_nextchar)
ld	(_curchar),a
;io.c: 411:     if (!t) {
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l34
;io.c: 412:         nextchar = 0;
ld	a,.low.0
ld	(_nextchar),a
;io.c: 413:         goto done;
jp	l35
;io.c: 414:     }
;io.c: 417: 	if (t->offset + 1 < t->valid) {
l34:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
inc	hl
pop	de
global	wrelop
call	wrelop
jp	age,l36
;io.c: 418:             nextchar = t->storage[++t->offset];
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ex	de,hl
ex	de,hl
ld	e,(hl)
inc	hl
ld	d,(hl)
ex	de,hl
inc	hl
ex	de,hl
ld	(hl),d
dec	hl
ld	(hl),e
ex	de,hl
add	hl,bc
ld	a,(hl)
ld	(_nextchar),a
;io.c: 428:             goto done;
jp	l35
;io.c: 429: 	}
;io.c: 432:     if (t->fd != -1) {
l36:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
cp	.low.-1
jp	z,l37
;io.c: 433:         t->valid = read(t->fd, t->storage, 512          );
ld	hl,512
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	l,(hl)
push	hl
call	_read
exx
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 434:         t->offset = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 435:         if (t->valid > 0) {
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l38
;io.c: 436:             nextchar = t->storage[0];
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
ld	a,(hl)
ld	(_nextchar),a
;io.c: 437:             goto done;
jp	l35
;io.c: 438:         }
;io.c: 439:         close(t->fd);
l38:
global	_close
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	l,(hl)
push	hl
call	_close
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 440:         t->fd = -1;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),-1
;io.c: 441:         nextchar = 0;
ld	a,.low.0
ld	(_nextchar),a
;io.c: 442:         goto done;
jp	l35
;io.c: 443:     }
;io.c: 445:     tbtop = t->prev;
l37:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_tbtop),bc
;io.c: 446:     if (tbtop) {
ld	hl,(_tbtop)
ld	a,l
or	h
jp	z,l39
;io.c: 454:         column = 0;
ld	a,.low.0
ld	(_column),a
;io.c: 455:         nextcol = 0;
ld	a,.low.0
ld	(_nextcol),a
;io.c: 456:         lineno = tbtop->lineno;
ld	iy,(_tbtop)
ld	l,(iy+9)
ld	h,(iy+9+1)
ld	(_lineno),hl
;io.c: 457:         filename = tbtop->name;
ld	iy,(_tbtop)
ld	l,(iy+1)
ld	h,(iy+1+1)
ld	(_filename),hl
;io.c: 463:         if (tbtop->offset < tbtop->valid) {
ld	iy,(_tbtop)
ld	e,(iy+7)
ld	d,(iy+7+1)
ld	iy,(_tbtop)
ld	l,(iy+5)
ld	h,(iy+5+1)
global	wrelop
call	wrelop
jp	age,l40
;io.c: 464:             nextchar = tbtop->storage[tbtop->offset];
ld	iy,(_tbtop)
ld	e,(iy+3)
ld	d,(iy+3+1)
ld	iy,(_tbtop)
ld	l,(iy+5)
ld	h,(iy+5+1)
add	hl,de
ld	a,(hl)
ld	(_nextchar),a
;io.c: 465:         } else if (tbtop->fd != -1) {
jp	l41
l40:
ld	hl,(_tbtop)
ld	a,(hl)
cp	.low.-1
jp	z,l42
;io.c: 467:             tbtop->valid = read(tbtop->fd, tbtop->storage, 512          );
ld	hl,512
push	hl
ld	iy,(_tbtop)
ld	l,(iy+3)
ld	h,(iy+3+1)
push	hl
ld	hl,(_tbtop)
ld	l,(hl)
push	hl
call	_read
exx
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	iy,(_tbtop)
ld	(iy+7),l
ld	(iy+7+1),h
;io.c: 468:             tbtop->offset = 0;
ld	iy,(_tbtop)
ld	(iy+5),.low.0
ld	(iy+5+1),.high.0
;io.c: 469:             if (tbtop->valid > 0) {
ld	iy,(_tbtop)
ld	e,(iy+7)
ld	d,(iy+7+1)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l43
;io.c: 470:                 nextchar = tbtop->storage[0];
ld	iy,(_tbtop)
ld	l,(iy+3)
ld	h,(iy+3+1)
ld	a,(hl)
ld	(_nextchar),a
;io.c: 471:             } else {
jp	l44
l43:
;io.c: 472:                 nextchar = 0;
ld	a,.low.0
ld	(_nextchar),a
;io.c: 473:             }
l44:
;io.c: 474:         } else {
jp	l45
l42:
;io.c: 475:             nextchar = 0;
ld	a,.low.0
ld	(_nextchar),a
;io.c: 476:         }
l45:
l41:
;io.c: 477:     }
;io.c: 478:     free(t->storage);
l39:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
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
;io.c: 479:     if (tbtop) {
ld	hl,(_tbtop)
ld	a,l
or	h
jp	z,l46
;io.c: 480:         free(t->name);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 481:     }
;io.c: 483:     free(t);
l46:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 484:     if (!tbtop) {
ld	hl,(_tbtop)
ld	a,l
or	h
jp	nz,l47
;io.c: 486:         nextchar = 0;
ld	a,.low.0
ld	(_nextchar),a
;io.c: 487:     } else if (curchar == 0) {
jp	l48
l47:
ld	a,(_curchar)
or	a
jp	lnz,l49
;io.c: 492:         goto again;
jp	l33
;io.c: 493:     }
;io.c: 494: done:
l49:
l48:
l35:
;io.c: 495:     column = nextcol;
ld	a,(_nextcol)
ld	(_column),a
;io.c: 496:     if (curchar == 0) {
ld	a,(_curchar)
or	a
jp	lnz,l50
;io.c: 497:         nextcol = 0;
ld	a,.low.0
ld	(_nextcol),a
;io.c: 498:     } else if (curchar == '\n') {
jp	l51
l50:
ld	a,(_curchar)
cp	.low.10
jp	nz,l52
;io.c: 499:         nextcol = 0;
ld	a,.low.0
ld	(_nextcol),a
;io.c: 500:         lineno++;
ld	hl,(_lineno)
inc	hl
ld	(_lineno),hl
;io.c: 501:         if (tbtop) {
ld	hl,(_tbtop)
ld	a,l
or	h
jp	z,l53
;io.c: 502:             tbtop->lineno = lineno;
ld	hl,(_lineno)
ld	iy,(_tbtop)
ld	(iy+9),l
ld	(iy+9+1),h
;io.c: 503:         }
;io.c: 504:     } else {
l53:
jp	l54
l52:
;io.c: 505:         if (nextcol < 2) nextcol++;
ld	de,2
ld	a,(_nextcol)
ld	l,a
rla
sbc	a,a
ld	h,a
global	wrelop
call	wrelop
jp	age,l55
ld	a,(_nextcol)
add	a,.low.1
ld	(_nextcol),a
;io.c: 506:     }
l55:
l54:
l51:
;io.c: 507:     if (nextchar == '\t') nextchar = ' ';
ld	a,(_nextchar)
cp	.low.9
jp	nz,l56
ld	a,.low.32
ld	(_nextchar),a
;io.c: 514:     if (curchar == '\\' && nextchar == '\n') {
l56:
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
jp	l57
11:
;io.c: 515:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;io.c: 516:         advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;io.c: 517:         return;
jp	l32
;io.c: 518:     }
;io.c: 530:     ;
l57:
;io.c: 531: }
l32:
jp	cret
f158	equ	-2
;io.c: 548: void
;io.c: 549: ioinit()
;io.c: 550: {
global	_ioinit
_ioinit:
call	ncsv
defw	f159
;io.c: 551:     lineno = 1;
ld	hl,1
ld	(_lineno),hl
;io.c: 552:     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;io.c: 553:     advance();
call	_advance
ld	hl,0
add	hl,sp
ld	sp,hl
;io.c: 554:     column = 0;
ld	a,.low.0
ld	(_column),a
;io.c: 555:     nextcol = 1;
ld	a,.low.1
ld	(_nextcol),a
;io.c: 556: }
l58:
jp	cret
f159	equ	0
;io.c: 569: struct textbuf *obtop = ((void *)0);
psect	data
global	_obtop
_obtop:
defw	0
;io.c: 572: char spillfile[] = "/tmp/cppXXXXXX";
global	_spillfile
_spillfile:
defb	47
defb	116
defb	109
defb	112
defb	47
defb	99
defb	112
defb	112
defb	88
defb	88
defb	88
defb	88
defb	88
defb	88
defb	0
;io.c: 577: static void
;io.c: 578: tbSpill(struct textbuf *t)
;io.c: 579: {
psect	text
_tbSpill:
call	ncsv
defw	f162
;io.c: 580:     if (t->fd < 0) {
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l60
;io.c: 581: 		strcpy(spillfile, "/tmp/cppXXXXXX");
ld	hl,69f
push	hl
ld	hl,_spillfile
push	hl
call	_strcpy
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 582:         t->fd = mkstemp(spillfile);
global	_mkstemp
ld	hl,_spillfile
push	hl
call	_mkstemp
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(hl),a
;io.c: 583:         if (t->fd >= 0)
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l61
;io.c: 584:             unlink(spillfile);
global	_unlink
ld	hl,_spillfile
push	hl
call	_unlink
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 585:     }
l61:
;io.c: 586:     if (t->fd >= 0) {
l60:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l62
;io.c: 587:         write(t->fd, t->storage, t->offset);
global	_write
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,5
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	l,(hl)
push	hl
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 588:         t->file_size += t->offset;
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,5
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
push	hl
push	de
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,11
add	hl,de
global	asaladd
call	asaladd
;io.c: 589:         t->offset = 0;
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,5
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 590:     }
;io.c: 591: }
l62:
l59:
jp	cret
f162	equ	0
;io.c: 596: static int
;io.c: 597: tbFill(struct textbuf *t)
;io.c: 598: {
_tbFill:
call	ncsv
defw	f164
;io.c: 599:     if (t->fd < 0)
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l64
;io.c: 600:         return 0;
ld	hl,0
jp	l63
;io.c: 601:     t->valid = read(t->fd, t->storage, 512          );
l64:
ld	hl,512
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	l,(hl)
push	hl
call	_read
exx
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,7
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 602:     t->offset = 0;
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,5
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 603:     return t->valid;
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	hl,7
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
jp	l63
;io.c: 604: }
l63:
jp	cret
f164	equ	0
;io.c: 609: void
;io.c: 610: outbufPush(void)
;io.c: 611: {
global	_outbufPush
_outbufPush:
call	ncsv
defw	f165
;io.c: 612:     struct textbuf *t = malloc(sizeof(*t));
ld	hl,19
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
;io.c: 613:     t->fd = -1;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(hl),-1
;io.c: 614:     t->name = ((void *)0);
ld	de,0
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 615:     t->storage = malloc(512          );
ld	hl,512
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
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 616:     t->offset = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 617:     t->valid = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 618:     t->lineno = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,9
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 619:     t->file_size = 0;
ld	de,0
ld	hl,0
push	hl
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,11
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
inc	hl
pop	bc
ld	(hl),c
inc	hl
ld	(hl),b
ld	l,c
ld	h,b
;io.c: 620:     t->saved_column = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,15
add	hl,de
ld	(hl),0
;io.c: 621:     t->direction = 'w';
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	(hl),119
;io.c: 622:     t->prev = obtop;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	de,(_obtop)
ld	(hl),e
inc	hl
ld	(hl),d
;io.c: 623:     obtop = t;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_obtop),hl
;io.c: 624: }
l65:
jp	cret
f165	equ	-2
;io.c: 629: void
;io.c: 630: outbufWrite(void *data, int len)
;io.c: 631: {
global	_outbufWrite
_outbufWrite:
call	ncsv
defw	f166
;io.c: 632:     extern char lexFd;
;io.c: 634:     if (!obtop) {
ld	hl,(_obtop)
ld	a,l
or	h
jp	nz,l67
;io.c: 635:         write(lexFd, data, len);
global	_lexFd
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	a,(_lexFd)
ld	c,a
push	bc
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 636:         return;
jp	l66
;io.c: 637:     }
;io.c: 639:     if (obtop->offset + len > 512          )
l67:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	iy,(_obtop)
ld	l,(iy+5)
ld	h,(iy+5+1)
add	hl,de
ex	de,hl
ld	hl,512
global	wrelop
call	wrelop
jp	age,l68
;io.c: 640:         tbSpill(obtop);
ld	hl,(_obtop)
push	hl
call	_tbSpill
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 642:     if (len > 512          ) {
l68:
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	hl,512
global	wrelop
call	wrelop
jp	age,l69
;io.c: 643:         if (obtop->fd < 0) {
ld	hl,(_obtop)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	z,l70
;io.c: 644: 			strcpy(spillfile, "/tmp/cppXXXXXX");
ld	hl,79f
push	hl
ld	hl,_spillfile
push	hl
call	_strcpy
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 645:             obtop->fd = mkstemp(spillfile);
ld	hl,_spillfile
push	hl
call	_mkstemp
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
ld	hl,(_obtop)
ld	(hl),a
;io.c: 646:             if (obtop->fd >= 0)
ld	hl,(_obtop)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l71
;io.c: 647:                 unlink(spillfile);
ld	hl,_spillfile
push	hl
call	_unlink
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 648:         }
l71:
;io.c: 649:         if (obtop->fd >= 0) {
l70:
ld	hl,(_obtop)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l72
;io.c: 650:             write(obtop->fd, data, len);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	hl,(_obtop)
ld	l,(hl)
push	hl
call	_write
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 651:             obtop->file_size += len;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	a,d
rla
sbc	a,a
ld	l,a
ld	h,a
push	hl
push	de
ld	de,(_obtop)
ld	hl,11
add	hl,de
global	asaladd
call	asaladd
;io.c: 652:         }
;io.c: 653:         return;
l72:
jp	l66
;io.c: 654:     }
;io.c: 655:     memcpy(obtop->storage + obtop->offset, data, len);
l69:
global	_memcpy
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	iy,(_obtop)
ld	e,(iy+3)
ld	d,(iy+3+1)
ld	iy,(_obtop)
ld	l,(iy+5)
ld	h,(iy+5+1)
add	hl,de
push	hl
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 656:     obtop->offset += len;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	iy,(_obtop)
ld	l,(iy+5)
ld	h,(iy+5+1)
add	hl,de
ld	(iy+5),l
ld	(iy+5+1),h
;io.c: 657: }
l66:
jp	cret
f166	equ	0
;io.c: 662: void
;io.c: 663: outbufReplay(void)
;io.c: 664: {
global	_outbufReplay
_outbufReplay:
call	ncsv
defw	f168
;io.c: 665:     struct textbuf *t = obtop;
ld	hl,(_obtop)
ld	(ix+-2),l
ld	(ix+1+-2),h
;io.c: 666:     char *membuf = ((void *)0);
ld	(ix+-4),.low.0
ld	(ix+1+-4),.high.0
;io.c: 667:     int memlen = 0;
ld	(ix+-6),.low.0
ld	(ix+1+-6),.high.0
;io.c: 669:     if (!t)
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l74
;io.c: 670:         return;
jp	l73
;io.c: 671:     obtop = t->prev;
l74:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_obtop),bc
;io.c: 674:     if (t->fd >= 0 && t->offset > 0) {
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,20f
jp	21f
21:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	hl,0
global	wrelop
call	wrelop
jp	age,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l75
11:
;io.c: 675:         memlen = t->offset;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-6),c
ld	(ix+1+-6),b
;io.c: 676:         membuf = malloc(memlen);
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-4),l
ld	(ix+1+-4),h
;io.c: 677:         memcpy(membuf, t->storage, memlen);
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;io.c: 678:     }
;io.c: 681:     if (t->fd >= 0) {
l75:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l76
;io.c: 682:         lseek(t->fd, 0, 0);
global	_lseek
ld	hl,0
push	hl
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	l,(hl)
push	hl
call	_lseek
ld	hl,2+4+2
add	hl,sp
ld	sp,hl
;io.c: 683:         while (tbFill(t) > 0)
jp	l77
l78:
;io.c: 684:             outbufWrite(t->storage, t->valid);
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,7
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
l77:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_tbFill
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
ld	hl,0
global	wrelop
call	wrelop
jp	alt,l78
l79:
;io.c: 685:         close(t->fd);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	l,(hl)
push	hl
call	_close
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 686:     }
;io.c: 689:     if (membuf) {
l76:
ld	a,(ix+-4)
or	(ix+1+-4)
jp	z,l80
;io.c: 690:         outbufWrite(membuf, memlen);
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 691:         free(membuf);
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 692:     } else if (t->offset > 0) {
jp	l81
l80:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l82
;io.c: 694:         outbufWrite(t->storage, t->offset);
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,5
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_outbufWrite
ld	hl,2+2
add	hl,sp
ld	sp,hl
;io.c: 695:     }
;io.c: 697:     free(t->storage);
l82:
l81:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
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
;io.c: 698:     free(t);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 699: }
l73:
jp	cret
f168	equ	-6
;io.c: 704: void
;io.c: 705: outbufPop(void)
;io.c: 706: {
global	_outbufPop
_outbufPop:
call	ncsv
defw	f169
;io.c: 707:     struct textbuf *t = obtop;
ld	hl,(_obtop)
ld	(ix+-2),l
ld	(ix+1+-2),h
;io.c: 709:     if (!t)
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l84
;io.c: 710:         return;
jp	l83
;io.c: 711:     obtop = t->prev;
l84:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,17
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_obtop),bc
;io.c: 712:     if (t->fd >= 0)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	a,(hl)
ld	l,a
rla
sbc	a,a
ld	h,a
bit	7,h
jp	nz,l85
;io.c: 713:         close(t->fd);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	l,(hl)
push	hl
call	_close
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 714:     free(t->storage);
l85:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
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
;io.c: 715:     free(t);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;io.c: 716: }
l83:
jp	cret
f169	equ	-2
psect	data
19:
defb	47,117,115,114,47,105,110,99,108,117,100,101,0
29:
defb	99,97,110,110,111,116,32,111,112,101,110,58,32,37,115,10
defb	0
39:
defb	47,0
49:
defb	47,0
59:
defb	99,97,110,110,111,116,32,102,105,110,100,32,105,110,99,108
defb	117,100,101,32,102,105,108,101,58,32,37,115,10,0
69:
defb	47,116,109,112,47,99,112,112,88,88,88,88,88,88,0
79:
defb	47,116,109,112,47,99,112,112,88,88,88,88,88,88,0
psect	bss
_lineno:
	defs	2
_tbtop:
	defs	2
_curchar:
	defs	1
_namebuf:
	defs	128
_includes:
	defs	2
_filename:
	defs	2
_column:
	defs	1
_nextchar:
	defs	1
