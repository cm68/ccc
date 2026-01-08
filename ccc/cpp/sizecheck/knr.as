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
;knr.c: 39: static unsigned char state =    0		;
psect	data
_state:
defb	0
;knr.c: 40: static int brace_depth = 0;
_brace_depth:
defw	0
;knr.c: 51: static int ctrlParenDep = 0;
_ctrlParenDep:
defw	0
;knr.c: 52: static int ctrlBodyDep = 0;
_ctrlBodyDep:
defw	0
;knr.c: 53: static unsigned char ctrl_type = 0;
_ctrl_type:
defb	0
;knr.c: 54: static unsigned char saved_state = 0;
_saved_state:
defb	0
;knr.c: 58: struct ctrl_frame {
;knr.c: 59: 	unsigned char ctrl_type;
;knr.c: 60: 	int ctrlBodyDep;
;knr.c: 61: };
;knr.c: 62: static struct ctrl_frame ctrl_stack[8];
;knr.c: 63: static int ctrl_sp = 0;
_ctrl_sp:
defw	0
;knr.c: 68: static int typedef_depth = 0;
_typedef_depth:
defw	0
;knr.c: 69: static char typedef_name[16];
;knr.c: 70: static int tdefNameDepth = 0;
_tdefNameDepth:
defw	0
;knr.c: 76: struct buftok {
;knr.c: 77: 	unsigned char type;
;knr.c: 78: 	short lineno;
;knr.c: 79: 	union {
;knr.c: 80: 		long num;
;knr.c: 81: 		float fnum;
;knr.c: 82: 		char *str;
;knr.c: 83: 	} v;
;knr.c: 84: };
;knr.c: 85: static struct buftok *tokbuf = ((void *)0);
_tokbuf:
defw	0
;knr.c: 86: static int num_tokens = 0;
_num_tokens:
defw	0
;knr.c: 89: static int lastEmitLine = 0;
_lastEmitLine:
defw	0
;knr.c: 95: struct param {
;knr.c: 96: 	char name[16];
;knr.c: 97: 	struct buftok *type_toks;
;knr.c: 98: 	int num_type_toks;
;knr.c: 99: 	char has_type;
;knr.c: 100: 	struct param *next;
;knr.c: 101: };
;knr.c: 102: static struct param *params = ((void *)0);
_params:
defw	0
;knr.c: 103: static int num_params = 0;
_num_params:
defw	0
;knr.c: 104: static int paren_depth = 0;
_paren_depth:
defw	0
;knr.c: 109: struct typedef_node {
;knr.c: 110: 	char *name;
;knr.c: 111: 	struct typedef_node *next;
;knr.c: 112: };
;knr.c: 113: static struct typedef_node *typedefs = ((void *)0);
_typedefs:
defw	0
;knr.c: 125: struct local_init {
;knr.c: 126: 	char name[16];
;knr.c: 127: 	struct buftok *inittoks;
;knr.c: 128: 	int num_inittoks;
;knr.c: 129: };
;knr.c: 130: static struct local_init *locInits = ((void *)0);
_locInits:
defw	0
;knr.c: 131: static int numLocInits = 0;
_numLocInits:
defw	0
;knr.c: 134: static struct buftok *locDecl = ((void *)0);
_locDecl:
defw	0
;knr.c: 135: static int numLocDecl = 0;
_numLocDecl:
defw	0
;knr.c: 138: static int locDeclParen = 0;
_locDeclParen:
defw	0
;knr.c: 139: static int locInInit = 0;
_locInInit:
defw	0
;knr.c: 140: static char locCurName[16];
;knr.c: 154: struct loop_frame {
;knr.c: 155: 	unsigned char type;
;knr.c: 156: 	unsigned char savedState;
;knr.c: 157: 	int label_num;
;knr.c: 158: 	int body_depth;
;knr.c: 159: 	int saved_brace;
;knr.c: 161: 	struct buftok *savedIncr;
;knr.c: 162: 	int numSavedIncr;
;knr.c: 163: };
;knr.c: 165: static struct loop_frame loop_stack[8];
;knr.c: 166: static int loop_sp = 0;
_loop_sp:
defw	0
;knr.c: 167: static int next_loop_num = 1;
_next_loop_num:
defw	1
;knr.c: 171: static struct buftok *loop_cond = ((void *)0);
_loop_cond:
defw	0
;knr.c: 172: static int num_loop_cond = 0;
_num_loop_cond:
defw	0
;knr.c: 173: static int loopParen = 0;
_loopParen:
defw	0
;knr.c: 176: static struct buftok *loop_init = ((void *)0);
_loop_init:
defw	0
;knr.c: 177: static int num_loop_init = 0;
_num_loop_init:
defw	0
;knr.c: 178: static struct buftok *loop_incr = ((void *)0);
_loop_incr:
defw	0
;knr.c: 179: static int num_loop_incr = 0;
_num_loop_incr:
defw	0
;knr.c: 180: static int for_part = 0;
_for_part:
defw	0
;knr.c: 188: static int switchBraceStk[8];
;knr.c: 189: static int switchStkTop = 0;
_switchStkTop:
defw	0
;knr.c: 190: static int switchParen = 0;
_switchParen:
defw	0
;knr.c: 202: static void
;knr.c: 203: realEmitToken(unsigned char tok)
;knr.c: 204: {
psect	text
_realEmitToken:
global	ncsv, cret, indir
call	ncsv
defw	f182
;knr.c: 205: 	emitToken(tok);
global	_emitToken
ld	l,(ix+6)
push	hl
call	_emitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 206: }
l14:
jp	cret
f182	equ	0
;knr.c: 208: static void
;knr.c: 209: realEmitKw(unsigned char kw)
;knr.c: 210: {
_realEmitKw:
call	ncsv
defw	f183
;knr.c: 211: 	emitKeyword(kw);
global	_emitKeyword
ld	l,(ix+6)
push	hl
call	_emitKeyword
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 212: }
l15:
jp	cret
f183	equ	0
;knr.c: 214: static void
;knr.c: 215: realEmitSym(char *name)
;knr.c: 216: {
_realEmitSym:
call	ncsv
defw	f184
;knr.c: 217: 	emitSym(name);
global	_emitSym
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_emitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 218: }
l16:
jp	cret
f184	equ	0
;knr.c: 220: static void
;knr.c: 221: realEmitNumber(long val)
;knr.c: 222: {
_realEmitNumber:
call	ncsv
defw	f185
;knr.c: 223: 	emitNumber(val);
global	_emitNumber
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
push	hl
push	de
call	_emitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 224: }
l17:
jp	cret
f185	equ	0
;knr.c: 226: static void
;knr.c: 227: realEmitFNum(float val)
;knr.c: 228: {
_realEmitFNum:
call	ncsv
defw	f186
;knr.c: 229: 	emitFNumber(val);
global	_emitFNumber
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
push	hl
push	de
call	_emitFNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 230: }
l18:
jp	cret
f186	equ	0
;knr.c: 232: static void
;knr.c: 233: realEmitString(char *str, int len)
;knr.c: 234: {
_realEmitString:
call	ncsv
defw	f187
;knr.c: 235: 	emitString(str, len);
global	_emitString
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
;knr.c: 236: }
l19:
jp	cret
f187	equ	0
;knr.c: 241: void
;knr.c: 242: knrAddTypedef(char *name)
;knr.c: 243: {
global	_knrAddTypedef
_knrAddTypedef:
call	ncsv
defw	f188
;knr.c: 244: 	struct typedef_node *node = malloc(sizeof(*node));
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
;knr.c: 245: 	node->name = strdup(name);
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
;knr.c: 246: 	node->next = typedefs;
ld	de,(_typedefs)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 247: 	typedefs = node;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
ld	(_typedefs),hl
;knr.c: 248: }
l20:
jp	cret
f188	equ	-2
;knr.c: 253: static int
;knr.c: 254: isTypedef(char *name)
;knr.c: 255: {
_isTypedef:
call	ncsv
defw	f189
;knr.c: 256: 	struct typedef_node *node;
;knr.c: 257: 	for (node = typedefs; node; node = node->next) {
ld	hl,(_typedefs)
ld	(ix+-2),l
ld	(ix+1+-2),h
jp	l25
l22:
;knr.c: 258: 		if (strcmp(node->name, name) == 0)
global	_strcmp
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
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
jp	nz,l26
;knr.c: 259: 			return 1;
ld	hl,1
jp	l21
;knr.c: 260: 	}
l26:
l24:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-2),c
ld	(ix+1+-2),b
l25:
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l22
l23:
;knr.c: 261: 	return 0;
ld	hl,0
jp	l21
;knr.c: 262: }
l21:
jp	cret
f189	equ	-2
;knr.c: 267: static int
;knr.c: 268: isTypeTok(unsigned char type, char *name)
;knr.c: 269: {
_isTypeTok:
call	ncsv
defw	f190
;knr.c: 271: 	if (type >=         128 && type <=        138)
ld	de,128
ld	l,(ix+6)
ld	h,0
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:
ld	e,(ix+6)
ld	d,0
ld	hl,138
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l28
11:
;knr.c: 272: 		return 1;
ld	hl,1
jp	l27
;knr.c: 273: 	if (type >=     140 && type <=    144)
l28:
ld	de,140
ld	l,(ix+6)
ld	h,0
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:
ld	e,(ix+6)
ld	d,0
ld	hl,144
global	wrelop
call	wrelop
jp	llt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l29
11:
;knr.c: 274: 		return 1;
ld	hl,1
jp	l27
;knr.c: 275: 	if (type ==        139 || type ==       158 || type ==    159)
l29:
ld	a,(ix+6)
cp	.low.-117
jp	nz,30f
jp	31f
30:
ld	a,(ix+6)
cp	.low.-98
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+6)
cp	.low.-97
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l30
11:
;knr.c: 276: 		return 1;
ld	hl,1
jp	l27
;knr.c: 277: 	if (type ==     20 && name && isTypedef(name))
l30:
ld	a,(ix+6)
cp	.low.20
jp	nz,30f
jp	31f
31:
ld	a,(ix+8)
or	(ix+1+8)
jp	z,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
call	_isTypedef
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l31
11:
;knr.c: 278: 		return 1;
ld	hl,1
jp	l27
;knr.c: 279: 	return 0;
l31:
ld	hl,0
jp	l27
;knr.c: 280: }
l27:
jp	cret
f190	equ	0
;knr.c: 285: static void
;knr.c: 286: bufToken(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 287: {
_bufToken:
call	ncsv
defw	f191
;knr.c: 288: 	if (num_tokens >= 256)
ld	de,256
ld	hl,(_num_tokens)
global	wrelop
call	wrelop
jp	alt,l33
;knr.c: 289: 		return;
jp	l32
;knr.c: 290: 	if (!tokbuf)
l33:
ld	hl,(_tokbuf)
ld	a,l
or	h
jp	nz,l34
;knr.c: 291: 		tokbuf = malloc(256 * sizeof(struct buftok));
ld	hl,1792
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_tokbuf),hl
;knr.c: 293: 	tokbuf[num_tokens].type = type;
l34:
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 294: 	tokbuf[num_tokens].lineno = lineno;
global	_lineno
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
ld	de,(_lineno)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 295: 	if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l35
;knr.c: 296: 		tokbuf[num_tokens].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 297: 	} else if (type ==  22) {
jp	l36
l35:
ld	a,(ix+6)
cp	.low.22
jp	nz,l37
;knr.c: 298: 		tokbuf[num_tokens].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 299: 		memcpy(tokbuf[num_tokens].v.str, str, slen);
global	_memcpy
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 300: 		tokbuf[num_tokens].v.str[slen] = 0;
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+18)
ld	h,(ix+1+18)
add	hl,bc
ld	(hl),0
;knr.c: 301: 	} else if (type ==  21) {
jp	l38
l37:
ld	a,(ix+6)
cp	.low.21
jp	nz,l39
;knr.c: 302: 		tokbuf[num_tokens].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 303: 	} else if (type == 23) {
jp	l40
l39:
ld	a,(ix+6)
cp	.low.23
jp	nz,l41
;knr.c: 304: 		tokbuf[num_tokens].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	de,7
ld	hl,(_num_tokens)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 305: 	}
;knr.c: 306: 	num_tokens++;
l41:
l40:
l38:
l36:
ld	hl,(_num_tokens)
inc	hl
ld	(_num_tokens),hl
;knr.c: 307: }
l32:
jp	cret
f191	equ	0
;knr.c: 312: static void
;knr.c: 313: emitBufTok(struct buftok *t)
;knr.c: 314: {
_emitBufTok:
call	ncsv
defw	f193
;knr.c: 315: 	switch (t->type) {
jp	l44
;knr.c: 316: 	case     20:
l45:
;knr.c: 317: 		realEmitSym(t->v.str);
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 318: 		break;
jp	l43
;knr.c: 319: 	case  21:
l46:
;knr.c: 320: 		realEmitNumber(t->v.num);
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	e,(hl)
inc	hl
ld	d,(hl)
inc	hl
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 321: 		break;
jp	l43
;knr.c: 322: 	case 23:
l47:
;knr.c: 323: 		realEmitFNum(t->v.fnum);
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	e,(hl)
inc	hl
ld	d,(hl)
inc	hl
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 324: 		break;
jp	l43
;knr.c: 325: 	case  22:
l48:
;knr.c: 326: 		realEmitString(t->v.str, strlen(t->v.str));
global	_strlen
ld	l,(ix+6)
ld	h,(ix+1+6)
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
ld	l,l
ld	h,h
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
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 327: 		break;
jp	l43
;knr.c: 328: 	default:
l49:
;knr.c: 329: 		realEmitToken(t->type);
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	l,(hl)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 330: 		break;
jp	l43
;knr.c: 331: 	}
jp	l43
l44:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
cp	20
jp	z,l45
cp	21
jp	z,l46
cp	22
jp	z,l48
cp	23
jp	z,l47
jp	l49
l43:
;knr.c: 332: }
l42:
jp	cret
f193	equ	0
;knr.c: 337: static void
;knr.c: 338: freeBufTok(struct buftok *t)
;knr.c: 339: {
_freeBufTok:
call	ncsv
defw	f194
;knr.c: 340: 	if (t->type ==     20 || t->type ==  22) {
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
cp	.low.20
jp	nz,20f
jp	21f
20:
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	a,(hl)
cp	.low.22
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l51
11:
;knr.c: 341: 		if (t->v.str)
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	a,(hl)
inc	hl
or	(hl)
jp	z,l52
;knr.c: 342: 			free(t->v.str);
global	_free
ld	l,(ix+6)
ld	h,(ix+1+6)
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
;knr.c: 343: 		t->v.str = ((void *)0);
l52:
ld	de,0
ld	l,(ix+6)
ld	h,(ix+1+6)
inc	hl
inc hl
inc hl
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 344: 	}
;knr.c: 345: }
l51:
l50:
jp	cret
f194	equ	0
;knr.c: 351: static void
;knr.c: 352: flushBuf(void)
;knr.c: 353: {
_flushBuf:
call	ncsv
defw	f195
;knr.c: 354: 	int i;
;knr.c: 355: 	for (i = 0; i < num_tokens; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l57
l54:
;knr.c: 356: 		if (tokbuf[i].type ==   2) {
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
ld	a,(hl)
cp	.low.2
jp	nz,l58
;knr.c: 357: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 361: 		} else if (tokbuf[i].type ==     3) {
jp	l59
l58:
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
ld	a,(hl)
cp	.low.3
jp	nz,l60
;knr.c: 362: 			brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 366: 		}
;knr.c: 367: 		emitBufTok(&tokbuf[i]);
l60:
l59:
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 368: 		freeBufTok(&tokbuf[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 369: 	}
l56:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l57:
ld	de,(_num_tokens)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l54
l55:
;knr.c: 370: 	num_tokens = 0;
ld	hl,0
ld	(_num_tokens),hl
;knr.c: 371: }
l53:
jp	cret
f195	equ	-2
;knr.c: 376: static void
;knr.c: 377: clearBuf(void)
;knr.c: 378: {
_clearBuf:
call	ncsv
defw	f196
;knr.c: 379: 	int i;
;knr.c: 380: 	for (i = 0; i < num_tokens; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l65
l62:
;knr.c: 381: 		freeBufTok(&tokbuf[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 382: 	}
l64:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l65:
ld	de,(_num_tokens)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l62
l63:
;knr.c: 383: 	num_tokens = 0;
ld	hl,0
ld	(_num_tokens),hl
;knr.c: 384: }
l61:
jp	cret
f196	equ	-2
;knr.c: 389: static void
;knr.c: 390: bufLocDecl(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 391: {
_bufLocDecl:
call	ncsv
defw	f197
;knr.c: 392: 	if (numLocDecl >= 64)
ld	de,64
ld	hl,(_numLocDecl)
global	wrelop
call	wrelop
jp	alt,l67
;knr.c: 393: 		return;
jp	l66
;knr.c: 394: 	if (!locDecl)
l67:
ld	hl,(_locDecl)
ld	a,l
or	h
jp	nz,l68
;knr.c: 395: 		locDecl = malloc(64 * sizeof(struct buftok));
ld	hl,448
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_locDecl),hl
;knr.c: 397: 	locDecl[numLocDecl].type = type;
l68:
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 398: 	if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l69
;knr.c: 399: 		locDecl[numLocDecl].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 400: 	} else if (type ==  22) {
jp	l70
l69:
ld	a,(ix+6)
cp	.low.22
jp	nz,l71
;knr.c: 401: 		locDecl[numLocDecl].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 402: 		memcpy(locDecl[numLocDecl].v.str, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 403: 		locDecl[numLocDecl].v.str[slen] = 0;
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+18)
ld	h,(ix+1+18)
add	hl,bc
ld	(hl),0
;knr.c: 404: 	} else if (type ==  21) {
jp	l72
l71:
ld	a,(ix+6)
cp	.low.21
jp	nz,l73
;knr.c: 405: 		locDecl[numLocDecl].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 406: 	} else if (type == 23) {
jp	l74
l73:
ld	a,(ix+6)
cp	.low.23
jp	nz,l75
;knr.c: 407: 		locDecl[numLocDecl].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	de,7
ld	hl,(_numLocDecl)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 408: 	}
;knr.c: 409: 	numLocDecl++;
l75:
l74:
l72:
l70:
ld	hl,(_numLocDecl)
inc	hl
ld	(_numLocDecl),hl
;knr.c: 410: }
l66:
jp	cret
f197	equ	0
;knr.c: 415: static void
;knr.c: 416: bufLocInit(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 417: {
_bufLocInit:
call	ncsv
defw	f198
;knr.c: 418: 	struct local_init *li;
;knr.c: 419: 	int idx;
;knr.c: 421: 	if (numLocInits == 0)
ld	hl,(_numLocInits)
ld	a,l
or	h
jp	nz,l77
;knr.c: 422: 		return;
jp	l76
;knr.c: 423: 	li = &locInits[numLocInits - 1];
l77:
ld	de,20
ld	hl,(_numLocInits)
dec	hl
global	amul
call	amul
ld	de,(_locInits)
add	hl,de
ld	(ix+-2),l
ld	(ix+1+-2),h
;knr.c: 424: 	idx = li->num_inittoks;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,18
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-4),c
ld	(ix+1+-4),b
;knr.c: 425: 	if (idx >= 16)
ld	de,16
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	wrelop
call	wrelop
jp	alt,l78
;knr.c: 426: 		return;
jp	l76
;knr.c: 427: 	if (!li->inittoks)
l78:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp	nz,l79
;knr.c: 428: 		li->inittoks = malloc(16 * sizeof(struct buftok));
ld	hl,112
push	hl
call	_malloc
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
ld	hl,16
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 430: 	li->inittoks[idx].type = type;
l79:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
ld	a,(ix+6)
ld	(hl),a
;knr.c: 431: 	if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l80
;knr.c: 432: 		li->inittoks[idx].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 433: 	} else if (type ==  22) {
jp	l81
l80:
ld	a,(ix+6)
cp	.low.22
jp	nz,l82
;knr.c: 434: 		li->inittoks[idx].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 435: 		memcpy(li->inittoks[idx].v.str, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 436: 		li->inittoks[idx].v.str[slen] = 0;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+18)
ld	h,(ix+1+18)
add	hl,bc
ld	(hl),0
;knr.c: 437: 	} else if (type ==  21) {
jp	l83
l82:
ld	a,(ix+6)
cp	.low.21
jp	nz,l84
;knr.c: 438: 		li->inittoks[idx].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
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
;knr.c: 439: 	} else if (type == 23) {
jp	l85
l84:
ld	a,(ix+6)
cp	.low.23
jp	nz,l86
;knr.c: 440: 		li->inittoks[idx].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
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
;knr.c: 441: 	}
;knr.c: 442: 	li->num_inittoks++;
l86:
l85:
l83:
l81:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,18
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
inc	bc
ld	(hl),b
dec	hl
ld	(hl),c
;knr.c: 443: }
l76:
jp	cret
f198	equ	-4
;knr.c: 449: static void
;knr.c: 450: emitLocDecl(void)
;knr.c: 451: {
_emitLocDecl:
call	ncsv
defw	f199
;knr.c: 452: 	int i;
;knr.c: 453: 	if (!locDecl) {
ld	hl,(_locDecl)
ld	a,l
or	h
jp	nz,l88
;knr.c: 454: 		numLocDecl = 0;
ld	hl,0
ld	(_numLocDecl),hl
;knr.c: 455: 		return;
jp	l87
;knr.c: 456: 	}
;knr.c: 457: 	for (i = 0; i < numLocDecl; i++) {
l88:
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l92
l89:
;knr.c: 458: 		if (locDecl[i].type ==   2)
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
ld	a,(hl)
cp	.low.2
jp	nz,l93
;knr.c: 459: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 460: 		else if (locDecl[i].type ==     3)
jp	l94
l93:
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
ld	a,(hl)
cp	.low.3
jp	nz,l95
;knr.c: 461: 			brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 462: 		emitBufTok(&locDecl[i]);
l95:
l94:
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 463: 		freeBufTok(&locDecl[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_locDecl)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 464: 	}
l91:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l92:
ld	de,(_numLocDecl)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l89
l90:
;knr.c: 465: 	numLocDecl = 0;
ld	hl,0
ld	(_numLocDecl),hl
;knr.c: 466: }
l87:
jp	cret
f199	equ	-2
;knr.c: 472: static void
;knr.c: 473: flushLocInits(void)
;knr.c: 474: {
_flushLocInits:
call	ncsv
defw	f200
;knr.c: 475: 	int i, j;
;knr.c: 476: 	struct local_init *li;
;knr.c: 478: 	if (!locInits) {
ld	hl,(_locInits)
ld	a,l
or	h
jp	nz,l97
;knr.c: 479: 		numLocInits = 0;
ld	hl,0
ld	(_numLocInits),hl
;knr.c: 480: 		return;
jp	l96
;knr.c: 481: 	}
;knr.c: 482: 	for (i = 0; i < numLocInits; i++) {
l97:
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l101
l98:
;knr.c: 483: 		li = &locInits[i];
ld	de,20
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_locInits)
add	hl,de
ld	(ix+-6),l
ld	(ix+1+-6),h
;knr.c: 484: 		realEmitSym(li->name);
ld	l,(ix+-6)
ld	h,(ix+1+-6)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 485: 		realEmitToken( 80);
ld	l,.low.80
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 486: 		if (li->inittoks) {
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	hl,16
add	hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp	z,l102
;knr.c: 487: 			for (j = 0; j < li->num_inittoks; j++) {
ld	(ix+-4),.low.0
ld	(ix+1+-4),.high.0
jp	l106
l103:
;knr.c: 488: 				emitBufTok(&li->inittoks[j]);
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 489: 				freeBufTok(&li->inittoks[j]);
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
pop	bc
add	hl,bc
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 490: 			}
l105:
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
ld	(ix+-4),l
ld	(ix+1+-4),h
l106:
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	hl,18
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	wrelop
call	wrelop
jp	alt,l103
l104:
;knr.c: 491: 		}
;knr.c: 492: 		realEmitToken(   1);
l102:
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 493: 		li->num_inittoks = 0;
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	hl,18
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 494: 	}
l100:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l101:
ld	de,(_numLocInits)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l98
l99:
;knr.c: 495: 	numLocInits = 0;
ld	hl,0
ld	(_numLocInits),hl
;knr.c: 496: }
l96:
jp	cret
f200	equ	-6
;knr.c: 501: static void
;knr.c: 502: startLocInit(char *name)
;knr.c: 503: {
_startLocInit:
call	ncsv
defw	f201
;knr.c: 504: 	if (numLocInits >= 8)
ld	de,8
ld	hl,(_numLocInits)
global	wrelop
call	wrelop
jp	alt,l108
;knr.c: 505: 		return;
jp	l107
;knr.c: 506: 	if (!locInits)
l108:
ld	hl,(_locInits)
ld	a,l
or	h
jp	nz,l109
;knr.c: 507: 		locInits = calloc(8, sizeof(struct local_init));
global	_calloc
ld	hl,20
push	hl
ld	hl,8
push	hl
call	_calloc
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_locInits),hl
;knr.c: 508: 	strncpy(locInits[numLocInits].name, name, 15);
l109:
global	_strncpy
ld	hl,15
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	de,20
ld	hl,(_numLocInits)
global	amul
call	amul
ld	de,(_locInits)
add	hl,de
push	hl
call	_strncpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 509: 	locInits[numLocInits].name[15] = 0;
ld	de,20
ld	hl,(_numLocInits)
global	amul
call	amul
ld	de,(_locInits)
add	hl,de
ld	de,15
add	hl,de
ld	(hl),0
;knr.c: 510: 	locInits[numLocInits].num_inittoks = 0;
ld	de,20
ld	hl,(_numLocInits)
global	amul
call	amul
ld	de,(_locInits)
add	hl,de
ld	de,18
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 511: 	numLocInits++;
ld	hl,(_numLocInits)
inc	hl
ld	(_numLocInits),hl
;knr.c: 512: 	locInInit = 1;
ld	hl,1
ld	(_locInInit),hl
;knr.c: 513: }
l107:
jp	cret
f201	equ	0
;knr.c: 520: static void
;knr.c: 521: bufLoopCond(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 522: {
_bufLoopCond:
call	ncsv
defw	f202
;knr.c: 523: 	if (num_loop_cond >= 64)
ld	de,64
ld	hl,(_num_loop_cond)
global	wrelop
call	wrelop
jp	alt,l111
;knr.c: 524: 		return;
jp	l110
;knr.c: 525: 	if (!loop_cond)
l111:
ld	hl,(_loop_cond)
ld	a,l
or	h
jp	nz,l112
;knr.c: 526: 		loop_cond = malloc(64 * sizeof(struct buftok));
ld	hl,448
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_loop_cond),hl
;knr.c: 527: 	loop_cond[num_loop_cond].type = type;
l112:
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 528: 	loop_cond[num_loop_cond].lineno = lineno;
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
ld	de,(_lineno)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 529: 	if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l113
;knr.c: 530: 		loop_cond[num_loop_cond].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 531: 	} else if (type ==  22) {
jp	l114
l113:
ld	a,(ix+6)
cp	.low.22
jp	nz,l115
;knr.c: 532: 		loop_cond[num_loop_cond].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 533: 		memcpy(loop_cond[num_loop_cond].v.str, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 534: 		loop_cond[num_loop_cond].v.str[slen] = 0;
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+18)
ld	h,(ix+1+18)
add	hl,bc
ld	(hl),0
;knr.c: 535: 	} else if (type ==  21) {
jp	l116
l115:
ld	a,(ix+6)
cp	.low.21
jp	nz,l117
;knr.c: 536: 		loop_cond[num_loop_cond].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 537: 	} else if (type == 23) {
jp	l118
l117:
ld	a,(ix+6)
cp	.low.23
jp	nz,l119
;knr.c: 538: 		loop_cond[num_loop_cond].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	de,7
ld	hl,(_num_loop_cond)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 539: 	}
;knr.c: 540: 	num_loop_cond++;
l119:
l118:
l116:
l114:
ld	hl,(_num_loop_cond)
inc	hl
ld	(_num_loop_cond),hl
;knr.c: 541: }
l110:
jp	cret
f202	equ	0
;knr.c: 544: static void
;knr.c: 545: bufLoopInit(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 546: {
_bufLoopInit:
call	ncsv
defw	f203
;knr.c: 547: 	if (num_loop_init >= 64)
ld	de,64
ld	hl,(_num_loop_init)
global	wrelop
call	wrelop
jp	alt,l121
;knr.c: 548: 		return;
jp	l120
;knr.c: 549: 	if (!loop_init)
l121:
ld	hl,(_loop_init)
ld	a,l
or	h
jp	nz,l122
;knr.c: 550: 		loop_init = malloc(64 * sizeof(struct buftok));
ld	hl,448
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_loop_init),hl
;knr.c: 551: 	loop_init[num_loop_init].type = type;
l122:
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 552: 	loop_init[num_loop_init].lineno = lineno;
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
ld	de,(_lineno)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 553: 	if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l123
;knr.c: 554: 		loop_init[num_loop_init].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 555: 	} else if (type ==  22) {
jp	l124
l123:
ld	a,(ix+6)
cp	.low.22
jp	nz,l125
;knr.c: 556: 		loop_init[num_loop_init].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 557: 		memcpy(loop_init[num_loop_init].v.str, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 558: 		loop_init[num_loop_init].v.str[slen] = 0;
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+18)
ld	h,(ix+1+18)
add	hl,bc
ld	(hl),0
;knr.c: 559: 	} else if (type ==  21) {
jp	l126
l125:
ld	a,(ix+6)
cp	.low.21
jp	nz,l127
;knr.c: 560: 		loop_init[num_loop_init].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 561: 	} else if (type == 23) {
jp	l128
l127:
ld	a,(ix+6)
cp	.low.23
jp	nz,l129
;knr.c: 562: 		loop_init[num_loop_init].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	de,7
ld	hl,(_num_loop_init)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 563: 	}
;knr.c: 564: 	num_loop_init++;
l129:
l128:
l126:
l124:
ld	hl,(_num_loop_init)
inc	hl
ld	(_num_loop_init),hl
;knr.c: 565: }
l120:
jp	cret
f203	equ	0
;knr.c: 568: static void
;knr.c: 569: bufLoopIncr(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 570: {
_bufLoopIncr:
call	ncsv
defw	f204
;knr.c: 571: 	if (num_loop_incr >= 64)
ld	de,64
ld	hl,(_num_loop_incr)
global	wrelop
call	wrelop
jp	alt,l131
;knr.c: 572: 		return;
jp	l130
;knr.c: 573: 	if (!loop_incr)
l131:
ld	hl,(_loop_incr)
ld	a,l
or	h
jp	nz,l132
;knr.c: 574: 		loop_incr = malloc(64 * sizeof(struct buftok));
ld	hl,448
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_loop_incr),hl
;knr.c: 575: 	loop_incr[num_loop_incr].type = type;
l132:
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 576: 	loop_incr[num_loop_incr].lineno = lineno;
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
ld	de,(_lineno)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 577: 	if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l133
;knr.c: 578: 		loop_incr[num_loop_incr].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 579: 	} else if (type ==  22) {
jp	l134
l133:
ld	a,(ix+6)
cp	.low.22
jp	nz,l135
;knr.c: 580: 		loop_incr[num_loop_incr].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 581: 		memcpy(loop_incr[num_loop_incr].v.str, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 582: 		loop_incr[num_loop_incr].v.str[slen] = 0;
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	l,(ix+18)
ld	h,(ix+1+18)
add	hl,bc
ld	(hl),0
;knr.c: 583: 	} else if (type ==  21) {
jp	l136
l135:
ld	a,(ix+6)
cp	.low.21
jp	nz,l137
;knr.c: 584: 		loop_incr[num_loop_incr].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 585: 	} else if (type == 23) {
jp	l138
l137:
ld	a,(ix+6)
cp	.low.23
jp	nz,l139
;knr.c: 586: 		loop_incr[num_loop_incr].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 587: 	}
;knr.c: 588: 	num_loop_incr++;
l139:
l138:
l136:
l134:
ld	hl,(_num_loop_incr)
inc	hl
ld	(_num_loop_incr),hl
;knr.c: 589: }
l130:
jp	cret
f204	equ	0
;knr.c: 592: static void
;knr.c: 593: clearLoopCond(void)
;knr.c: 594: {
_clearLoopCond:
call	ncsv
defw	f205
;knr.c: 595: 	int i;
;knr.c: 596: 	for (i = 0; i < num_loop_cond; i++)
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l144
l141:
;knr.c: 597: 		freeBufTok(&loop_cond[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
l143:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l144:
ld	de,(_num_loop_cond)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l141
l142:
;knr.c: 598: 	num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 599: }
l140:
jp	cret
f205	equ	-2
;knr.c: 602: static void
;knr.c: 603: clearLoopInit(void)
;knr.c: 604: {
_clearLoopInit:
call	ncsv
defw	f206
;knr.c: 605: 	int i;
;knr.c: 606: 	for (i = 0; i < num_loop_init; i++)
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l149
l146:
;knr.c: 607: 		freeBufTok(&loop_init[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
l148:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l149:
ld	de,(_num_loop_init)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l146
l147:
;knr.c: 608: 	num_loop_init = 0;
ld	hl,0
ld	(_num_loop_init),hl
;knr.c: 609: }
l145:
jp	cret
f206	equ	-2
;knr.c: 612: static void
;knr.c: 613: clearLoopIncr(void)
;knr.c: 614: {
_clearLoopIncr:
call	ncsv
defw	f207
;knr.c: 615: 	int i;
;knr.c: 616: 	for (i = 0; i < num_loop_incr; i++)
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l154
l151:
;knr.c: 617: 		freeBufTok(&loop_incr[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
l153:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l154:
ld	de,(_num_loop_incr)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l151
l152:
;knr.c: 618: 	num_loop_incr = 0;
ld	hl,0
ld	(_num_loop_incr),hl
;knr.c: 619: }
l150:
jp	cret
f207	equ	-2
;knr.c: 625: static void
;knr.c: 626: syncLine(int line)
;knr.c: 627: {
_syncLine:
call	ncsv
defw	f208
;knr.c: 628: 	if (line != lastEmitLine) {
ld	de,(_lastEmitLine)
ld	l,(ix+6)
ld	h,(ix+1+6)
or	a
sbc	hl,de
jp	z,l156
;knr.c: 629: 		emitLine(line, filename ? filename : "");
global	_emitLine
global	_filename
ld	hl,(_filename)
ld	a,l
or	h
jp	nz,10f
jp	11f
11:
ld	hl,19f
jp	12f
10:
ld	hl,(_filename)
12:
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
call	_emitLine
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 630: 		lastEmitLine = line;
ld	l,(ix+6)
ld	h,(ix+1+6)
ld	(_lastEmitLine),hl
;knr.c: 631: 	}
;knr.c: 632: }
l156:
l155:
jp	cret
f208	equ	0
;knr.c: 635: static void
;knr.c: 636: emitLoopCond(void)
;knr.c: 637: {
_emitLoopCond:
call	ncsv
defw	f210
;knr.c: 638: 	int i;
;knr.c: 639: 	for (i = 0; i < num_loop_cond; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l161
l158:
;knr.c: 640: 		syncLine(loop_cond[i].lineno);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_syncLine
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 641: 		emitBufTok(&loop_cond[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_cond)
add	hl,de
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 642: 	}
l160:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l161:
ld	de,(_num_loop_cond)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l158
l159:
;knr.c: 643: }
l157:
jp	cret
f210	equ	-2
;knr.c: 646: static void
;knr.c: 647: emitLoopInit(void)
;knr.c: 648: {
_emitLoopInit:
call	ncsv
defw	f211
;knr.c: 649: 	int i;
;knr.c: 650: 	for (i = 0; i < num_loop_init; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l166
l163:
;knr.c: 651: 		syncLine(loop_init[i].lineno);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_syncLine
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 652: 		emitBufTok(&loop_init[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_init)
add	hl,de
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 653: 	}
l165:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l166:
ld	de,(_num_loop_init)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l163
l164:
;knr.c: 654: }
l162:
jp	cret
f211	equ	-2
;knr.c: 657: static void
;knr.c: 658: emitLoopIncr(void)
;knr.c: 659: {
_emitLoopIncr:
call	ncsv
defw	f212
;knr.c: 660: 	int i;
;knr.c: 661: 	for (i = 0; i < num_loop_incr; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l171
l168:
;knr.c: 662: 		syncLine(loop_incr[i].lineno);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
inc	hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_syncLine
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 663: 		emitBufTok(&loop_incr[i]);
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 664: 	}
l170:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l171:
ld	de,(_num_loop_incr)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l168
l169:
;knr.c: 665: }
l167:
jp	cret
f212	equ	-2
;knr.c: 668: static void
;knr.c: 669: pushLoop(unsigned char type)
;knr.c: 670: {
_pushLoop:
call	ncsv
defw	f213
;knr.c: 671: 	int i;
;knr.c: 672: 	if (loop_sp >= 8)
ld	de,8
ld	hl,(_loop_sp)
global	wrelop
call	wrelop
jp	alt,l173
;knr.c: 673: 		return;
jp	l172
;knr.c: 674: 	loop_stack[loop_sp].type = type;
l173:
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 675: 	loop_stack[loop_sp].savedState = state;
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+1
add	hl,de
ld	a,(_state)
ld	(hl),a
;knr.c: 676: 	loop_stack[loop_sp].label_num = next_loop_num++;
ld	hl,(_next_loop_num)
inc	hl
ld	(_next_loop_num),hl
dec	hl
ex	de,hl
push	de
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+2
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 677: 	loop_stack[loop_sp].body_depth = 0;
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 678: 	loop_stack[loop_sp].saved_brace = brace_depth;
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+6
add	hl,de
ld	de,(_brace_depth)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 680: 	if (num_loop_incr > 0) {
ld	de,(_num_loop_incr)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l174
;knr.c: 681: 		loop_stack[loop_sp].savedIncr = malloc(num_loop_incr * sizeof(struct buftok));
ld	de,7
ld	hl,(_num_loop_incr)
global	amul
call	amul
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	e,l
ld	d,h
push	de
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 682: 		for (i = 0; i < num_loop_incr; i++)
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l178
l175:
;knr.c: 683: 			loop_stack[loop_sp].savedIncr[i] = loop_incr[i];
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
push	hl
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
pop	bc
add	hl,bc
ex	de,hl
pop	hl
push	hl
ld	bc,7
ldir
pop	hl
l177:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l178:
ld	de,(_num_loop_incr)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l175
l176:
;knr.c: 684: 		loop_stack[loop_sp].numSavedIncr = num_loop_incr;
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+10
add	hl,de
ld	de,(_num_loop_incr)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 685: 		num_loop_incr = 0;
ld	hl,0
ld	(_num_loop_incr),hl
;knr.c: 686: 	} else {
jp	l179
l174:
;knr.c: 687: 		loop_stack[loop_sp].savedIncr = ((void *)0);
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 688: 		loop_stack[loop_sp].numSavedIncr = 0;
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+10
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 689: 	}
l179:
;knr.c: 690: 	loop_sp++;
ld	hl,(_loop_sp)
inc	hl
ld	(_loop_sp),hl
;knr.c: 691: }
l172:
jp	cret
f213	equ	-2
;knr.c: 694: static void
;knr.c: 695: popLoop(void)
;knr.c: 696: {
_popLoop:
call	ncsv
defw	f214
;knr.c: 697: 	int i;
;knr.c: 698: 	if (loop_sp > 0) {
ld	de,(_loop_sp)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l181
;knr.c: 699: 		loop_sp--;
ld	hl,(_loop_sp)
dec	hl
ld	(_loop_sp),hl
;knr.c: 701: 		if (loop_stack[loop_sp].savedIncr) {
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp	z,l182
;knr.c: 702: 			for (i = 0; i < loop_stack[loop_sp].numSavedIncr; i++)
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l186
l183:
;knr.c: 703: 				loop_incr[i] = loop_stack[loop_sp].savedIncr[i];
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
pop	bc
add	hl,bc
push	hl
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_loop_incr)
add	hl,de
ex	de,hl
pop	hl
push	hl
ld	bc,7
ldir
pop	hl
l185:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l186:
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+10
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l183
l184:
;knr.c: 704: 			num_loop_incr = loop_stack[loop_sp].numSavedIncr;
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+10
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_num_loop_incr),bc
;knr.c: 705: 			free(loop_stack[loop_sp].savedIncr);
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 706: 			loop_stack[loop_sp].savedIncr = ((void *)0);
ld	de,12
ld	hl,(_loop_sp)
global	amul
call	amul
ld	de,_loop_stack+8
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 707: 		}
;knr.c: 708: 	}
l182:
;knr.c: 709: }
l181:
l180:
jp	cret
f214	equ	-2
;knr.c: 712: static char
;knr.c: 713: loopLabelChar(void)
;knr.c: 714: {
_loopLabelChar:
call	ncsv
defw	f215
;knr.c: 715: 	if (loop_sp == 0)
ld	hl,(_loop_sp)
ld	a,l
or	h
jp	nz,l188
;knr.c: 716: 		return '?';
ld	l,.low.63
jp	l187
;knr.c: 717: 	switch (loop_stack[loop_sp - 1].type) {
l188:
jp	l190
;knr.c: 718: 	case       148: return 'W';
l191:
ld	l,.low.87
jp	l187
;knr.c: 719: 	case         156:   return 'F';
l192:
ld	l,.low.70
jp	l187
;knr.c: 720: 	case          154:    return 'D';
l193:
ld	l,.low.68
jp	l187
;knr.c: 721: 	default:    return '?';
l194:
ld	l,.low.63
jp	l187
;knr.c: 722: 	}
jp	l189
l190:
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	148
jp	z,l191
cp	154
jp	z,l193
cp	156
jp	z,l192
jp	l194
l189:
;knr.c: 723: }
l187:
jp	cret
f215	equ	0
;knr.c: 726: static void
;knr.c: 727: emitLoopLabel(char suffix)
;knr.c: 728: {
_emitLoopLabel:
call	ncsv
defw	f216
;knr.c: 729: 	char buf[16];
;knr.c: 730: 	if (loop_sp == 0)
ld	hl,(_loop_sp)
ld	a,l
or	h
jp	nz,l196
;knr.c: 731: 		return;
jp	l195
;knr.c: 732: 	sprintf(buf, "__%c%d%c", loopLabelChar(),
l196:
;knr.c: 733: 	        loop_stack[loop_sp - 1].label_num, suffix);
global	_sprintf
ld	a,(ix+6)
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+2
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_loopLabelChar
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	hl,29f
push	hl
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 734: 	realEmitSym(buf);
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 735: 	realEmitToken(  8);
ld	l,.low.8
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 736: 	realEmitToken(   1);
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 737: }
l195:
jp	cret
f216	equ	-16
;knr.c: 740: static void
;knr.c: 741: emitLoopGoto(char suffix)
;knr.c: 742: {
_emitLoopGoto:
call	ncsv
defw	f217
;knr.c: 743: 	char buf[16];
;knr.c: 744: 	if (loop_sp == 0)
ld	hl,(_loop_sp)
ld	a,l
or	h
jp	nz,l198
;knr.c: 745: 		return;
jp	l197
;knr.c: 746: 	sprintf(buf, "__%c%d%c", loopLabelChar(),
l198:
;knr.c: 747: 	        loop_stack[loop_sp - 1].label_num, suffix);
ld	a,(ix+6)
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+2
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_loopLabelChar
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	a,l
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	hl,39f
push	hl
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 748: 	realEmitKw(       145);
ld	l,.low.-111
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 749: 	realEmitSym(buf);
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 750: 	realEmitToken(   1);
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 751: }
l197:
jp	cret
f217	equ	-16
;knr.c: 754: static int
;knr.c: 755: findInnerLoop(void)
;knr.c: 756: {
_findInnerLoop:
call	ncsv
defw	f218
;knr.c: 757: 	int i;
;knr.c: 758: 	for (i = loop_sp - 1; i >= 0; i--) {
ld	hl,(_loop_sp)
dec	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
jp	l203
l200:
;knr.c: 759: 		if (loop_stack[i].type ==       148 ||
;knr.c: 760: 		    loop_stack[i].type ==         156 ||
;knr.c: 761: 		    loop_stack[i].type ==          154)
ld	de,12
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-108
jp	nz,30f
jp	31f
30:
ld	de,12
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-100
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	de,12
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-102
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l204
11:
;knr.c: 762: 			return i;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l199
;knr.c: 763: 	}
l204:
l202:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
dec	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l203:
bit	7,(ix+1+-2)
jp	z,l200
l201:
;knr.c: 764: 	return -1;
ld	hl,-1
jp	l199
;knr.c: 765: }
l199:
jp	cret
f218	equ	-2
;knr.c: 769: static void
;knr.c: 770: emitBreakGoto(int idx)
;knr.c: 771: {
_emitBreakGoto:
call	ncsv
defw	f219
;knr.c: 772: 	char buf[16];
;knr.c: 773: 	char c;
;knr.c: 774: 	if (idx < 0)
bit	7,(ix+1+6)
jp	z,l206
;knr.c: 775: 		return;
jp	l205
;knr.c: 776: 	switch (loop_stack[idx].type) {
l206:
jp	l208
;knr.c: 777: 	case       148: c = 'W'; break;
l209:
ld	(ix+-17),87
jp	l207
;knr.c: 778: 	case         156:   c = 'F'; break;
l210:
ld	(ix+-17),70
jp	l207
;knr.c: 779: 	case          154:    c = 'D'; break;
l211:
ld	(ix+-17),68
jp	l207
;knr.c: 780: 	default:    return;
l212:
jp	l205
;knr.c: 781: 	}
jp	l207
l208:
ld	de,12
ld	l,(ix+6)
ld	h,(ix+1+6)
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	148
jp	z,l209
cp	154
jp	z,l211
cp	156
jp	z,l210
jp	l212
l207:
;knr.c: 782: 	sprintf(buf, "__%c%d%c", c, loop_stack[idx].label_num, 'B');
ld	hl,66
push	hl
ld	de,12
ld	l,(ix+6)
ld	h,(ix+1+6)
global	amul
call	amul
ld	de,_loop_stack+2
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	a,(ix+-17)
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	hl,49f
push	hl
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 783: 	realEmitKw(       145);
ld	l,.low.-111
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 784: 	realEmitSym(buf);
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 785: }
l205:
jp	cret
f219	equ	-17
;knr.c: 789: static void
;knr.c: 790: emitContGoto(int idx)
;knr.c: 791: {
_emitContGoto:
call	ncsv
defw	f220
;knr.c: 792: 	char buf[16];
;knr.c: 793: 	char c, suffix;
;knr.c: 794: 	if (idx < 0)
bit	7,(ix+1+6)
jp	z,l214
;knr.c: 795: 		return;
jp	l213
;knr.c: 796: 	switch (loop_stack[idx].type) {
l214:
jp	l216
;knr.c: 797: 	case       148: c = 'W'; suffix = 'T'; break;
l217:
ld	(ix+-17),87
ld	(ix+-18),84
jp	l215
;knr.c: 798: 	case         156:   c = 'F'; suffix = 'C'; break;
l218:
ld	(ix+-17),70
ld	(ix+-18),67
jp	l215
;knr.c: 799: 	case          154:    c = 'D'; suffix = 'C'; break;
l219:
ld	(ix+-17),68
ld	(ix+-18),67
jp	l215
;knr.c: 800: 	default:    return;
l220:
jp	l213
;knr.c: 801: 	}
jp	l215
l216:
ld	de,12
ld	l,(ix+6)
ld	h,(ix+1+6)
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	148
jp	z,l217
cp	154
jp	z,l219
cp	156
jp	z,l218
jp	l220
l215:
;knr.c: 802: 	sprintf(buf, "__%c%d%c", c, loop_stack[idx].label_num, suffix);
ld	a,(ix+-18)
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	de,12
ld	l,(ix+6)
ld	h,(ix+1+6)
global	amul
call	amul
ld	de,_loop_stack+2
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	a,(ix+-17)
ld	l,a
rla
sbc	a,a
ld	h,a
push	hl
ld	hl,59f
push	hl
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_sprintf
ld	hl,2+2+2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 803: 	realEmitKw(       145);
ld	l,.low.-111
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 804: 	realEmitSym(buf);
push	ix
pop	de
ld	hl,-16
add	hl,de
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 805: }
l213:
jp	cret
f220	equ	-18
;knr.c: 810: static struct param *
;knr.c: 811: findParam(char *name)
;knr.c: 812: {
_findParam:
call	ncsv
defw	f221
;knr.c: 813: 	struct param *p;
;knr.c: 814: 	for (p = params; p; p = p->next) {
ld	hl,(_params)
ld	(ix+-2),l
ld	(ix+1+-2),h
jp	l225
l222:
;knr.c: 815: 		if (strcmp(p->name, name) == 0)
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
ld	l,(ix+-2)
ld	h,(ix+1+-2)
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
jp	nz,l226
;knr.c: 816: 			return p;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l221
;knr.c: 817: 	}
l226:
l224:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,21
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-2),c
ld	(ix+1+-2),b
l225:
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l222
l223:
;knr.c: 818: 	return ((void *)0);
ld	hl,0
jp	l221
;knr.c: 819: }
l221:
jp	cret
f221	equ	-2
;knr.c: 824: static void
;knr.c: 825: emitAnsiHeader(void)
;knr.c: 826: {
_emitAnsiHeader:
call	ncsv
defw	f222
;knr.c: 827: 	struct param *p, *next;
;knr.c: 828: 	int i, k, first;
;knr.c: 831: 	for (i = 0; i < num_tokens; i++) {
ld	(ix+-6),.low.0
ld	(ix+1+-6),.high.0
jp	l231
l228:
;knr.c: 832: 		if (tokbuf[i].type ==    6)
ld	de,7
ld	l,(ix+-6)
ld	h,(ix+1+-6)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
ld	a,(hl)
cp	.low.6
jp	nz,l232
;knr.c: 833: 			break;
jp	l229
;knr.c: 834: 		emitBufTok(&tokbuf[i]);
l232:
ld	de,7
ld	l,(ix+-6)
ld	h,(ix+1+-6)
global	amul
call	amul
ld	de,(_tokbuf)
add	hl,de
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 835: 	}
l230:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
inc	hl
ld	(ix+-6),l
ld	(ix+1+-6),h
l231:
ld	de,(_num_tokens)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
global	wrelop
call	wrelop
jp	alt,l228
l229:
;knr.c: 837: 	realEmitToken(   6);
ld	l,.low.6
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 840: 	first = 1;
ld	(ix+-10),.low.1
ld	(ix+1+-10),.high.1
;knr.c: 841: 	for (p = params; p; p = p->next) {
ld	hl,(_params)
ld	(ix+-2),l
ld	(ix+1+-2),h
jp	l236
l233:
;knr.c: 842: 		if (!first)
ld	a,(ix+-10)
or	(ix+1+-10)
jp	nz,l237
;knr.c: 843: 			realEmitToken(  9);
ld	l,.low.9
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 844: 		first = 0;
l237:
ld	(ix+-10),.low.0
ld	(ix+1+-10),.high.0
;knr.c: 846: 		if (p->has_type) {
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,20
add	hl,de
ld	a,(hl)
or	a
jp	az,l238
;knr.c: 847: 			for (k = 0; k < p->num_type_toks; k++) {
ld	(ix+-8),.low.0
ld	(ix+1+-8),.high.0
jp	l242
l239:
;knr.c: 848: 				emitBufTok(&p->type_toks[k]);
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-8)
ld	h,(ix+1+-8)
global	amul
call	amul
pop	bc
add	hl,bc
push	hl
call	_emitBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 849: 			}
l241:
ld	l,(ix+-8)
ld	h,(ix+1+-8)
inc	hl
ld	(ix+-8),l
ld	(ix+1+-8),h
l242:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,18
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	l,(ix+-8)
ld	h,(ix+1+-8)
global	wrelop
call	wrelop
jp	alt,l239
l240:
;knr.c: 850: 		} else {
jp	l243
l238:
;knr.c: 852: 			realEmitKw(        128);
ld	l,.low.-128
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 853: 			realEmitSym(p->name);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 854: 		}
l243:
;knr.c: 855: 	}
l235:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,21
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-2),c
ld	(ix+1+-2),b
l236:
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l233
l234:
;knr.c: 857: 	realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 870: 	clearBuf();
call	_clearBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 873: 	for (p = params; p; p = next) {
ld	hl,(_params)
ld	(ix+-2),l
ld	(ix+1+-2),h
jp	l247
l244:
;knr.c: 874: 		next = p->next;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,21
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(ix+-4),c
ld	(ix+1+-4),b
;knr.c: 875: 		for (k = 0; k < p->num_type_toks; k++)
ld	(ix+-8),.low.0
ld	(ix+1+-8),.high.0
jp	l251
l248:
;knr.c: 876: 			freeBufTok(&p->type_toks[k]);
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-8)
ld	h,(ix+1+-8)
global	amul
call	amul
pop	bc
add	hl,bc
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
l250:
ld	l,(ix+-8)
ld	h,(ix+1+-8)
inc	hl
ld	(ix+-8),l
ld	(ix+1+-8),h
l251:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,18
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
ld	l,(ix+-8)
ld	h,(ix+1+-8)
global	wrelop
call	wrelop
jp	alt,l248
l249:
;knr.c: 877: 		free(p->type_toks);
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 878: 		free(p);
ld	l,(ix+-2)
ld	h,(ix+1+-2)
push	hl
call	_free
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 879: 	}
l246:
ld	l,(ix+-4)
ld	h,(ix+1+-4)
ld	(ix+-2),l
ld	(ix+1+-2),h
l247:
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l244
l245:
;knr.c: 880: 	params = ((void *)0);
ld	hl,0
ld	(_params),hl
;knr.c: 881: 	num_params = 0;
ld	hl,0
ld	(_num_params),hl
;knr.c: 882: }
l227:
jp	cret
f222	equ	-10
;knr.c: 889: static struct buftok *decl_toks = ((void *)0);
psect	data
_decl_toks:
defw	0
;knr.c: 890: static int num_decl_toks;
;knr.c: 900: static int
;knr.c: 901: findBaseEnd(void)
;knr.c: 902: {
psect	text
_findBaseEnd:
call	ncsv
defw	f225
;knr.c: 903: 	int i;
;knr.c: 904: 	for (i = 0; i < num_decl_toks; i++) {
ld	(ix+-2),.low.0
ld	(ix+1+-2),.high.0
jp	l256
l253:
;knr.c: 905: 		unsigned char t = decl_toks[i].type;
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	l,(hl)
ld	(ix+-3),l
;knr.c: 909: 		if (t ==    36 || t ==   42 || t ==    6) {
ld	a,(ix+-3)
cp	.low.36
jp	nz,30f
jp	31f
30:
ld	a,(ix+-3)
cp	.low.42
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+-3)
cp	.low.6
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l257
11:
;knr.c: 910: 			return i;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l252
;knr.c: 911: 		}
;knr.c: 912: 		if (t ==     20) {
l257:
ld	a,(ix+-3)
cp	.low.20
jp	nz,l258
;knr.c: 914: 			if (findParam(decl_toks[i].v.str) != ((void *)0)) {
ld	de,7
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_findParam
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,l259
;knr.c: 915: 				return i;
ld	l,(ix+-2)
ld	h,(ix+1+-2)
jp	l252
;knr.c: 916: 			}
;knr.c: 918: 		}
l259:
;knr.c: 919: 	}
l258:
l255:
ld	l,(ix+-2)
ld	h,(ix+1+-2)
inc	hl
ld	(ix+-2),l
ld	(ix+1+-2),h
l256:
ld	de,(_num_decl_toks)
ld	l,(ix+-2)
ld	h,(ix+1+-2)
global	wrelop
call	wrelop
jp	alt,l253
l254:
;knr.c: 920: 	return num_decl_toks;
ld	hl,(_num_decl_toks)
jp	l252
;knr.c: 921: }
l252:
jp	cret
f225	equ	-3
;knr.c: 923: static int
;knr.c: 924: procDeclTok(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 925: {
_procDeclTok:
call	ncsv
defw	f226
;knr.c: 926: 	struct param *p;
;knr.c: 927: 	int i, base_end, decl_start;
;knr.c: 930: 	if (type ==   2) {
ld	a,(ix+6)
cp	.low.2
jp	nz,l261
;knr.c: 931: 		emitAnsiHeader();
call	_emitAnsiHeader
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 932: 		realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 933: 		brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 934: 		state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 935: 		return 0;
ld	hl,0
jp	l260
;knr.c: 936: 	}
;knr.c: 939: 	if (num_decl_toks < 64) {
l261:
ld	de,64
ld	hl,(_num_decl_toks)
global	wrelop
call	wrelop
jp	age,l262
;knr.c: 940: 		if (!decl_toks)
ld	hl,(_decl_toks)
ld	a,l
or	h
jp	nz,l263
;knr.c: 941: 			decl_toks = malloc(64 * sizeof(struct buftok));
ld	hl,448
push	hl
call	_malloc
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(_decl_toks),hl
;knr.c: 942: 		decl_toks[num_decl_toks].type = type;
l263:
ld	de,7
ld	hl,(_num_decl_toks)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(ix+6)
ld	(hl),a
;knr.c: 943: 		if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l264
;knr.c: 944: 			decl_toks[num_decl_toks].v.str = strdup(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
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
ld	de,7
ld	hl,(_num_decl_toks)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 945: 		} else if (type ==  22) {
jp	l265
l264:
ld	a,(ix+6)
cp	.low.22
jp	nz,l266
;knr.c: 946: 			decl_toks[num_decl_toks].v.str = malloc(slen + 1);
ld	l,(ix+18)
ld	h,(ix+1+18)
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
push	de
ld	de,7
ld	hl,(_num_decl_toks)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 947: 			memcpy(decl_toks[num_decl_toks].v.str, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	de,7
ld	hl,(_num_decl_toks)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_memcpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 948: 		} else if (type ==  21) {
jp	l267
l266:
ld	a,(ix+6)
cp	.low.21
jp	nz,l268
;knr.c: 949: 			decl_toks[num_decl_toks].v.num = num;
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	de,7
ld	hl,(_num_decl_toks)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 950: 		} else if (type == 23) {
jp	l269
l268:
ld	a,(ix+6)
cp	.low.23
jp	nz,l270
;knr.c: 951: 			decl_toks[num_decl_toks].v.fnum = fnum;
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	de,7
ld	hl,(_num_decl_toks)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
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
;knr.c: 952: 		}
;knr.c: 953: 		num_decl_toks++;
l270:
l269:
l267:
l265:
ld	hl,(_num_decl_toks)
inc	hl
ld	(_num_decl_toks),hl
;knr.c: 954: 	}
;knr.c: 957: 	if (type ==    6)
l262:
ld	a,(ix+6)
cp	.low.6
jp	nz,l271
;knr.c: 958: 		paren_depth++;
ld	hl,(_paren_depth)
inc	hl
ld	(_paren_depth),hl
;knr.c: 959: 	else if (type ==    7)
jp	l272
l271:
ld	a,(ix+6)
cp	.low.7
jp	nz,l273
;knr.c: 960: 		paren_depth--;
ld	hl,(_paren_depth)
dec	hl
ld	(_paren_depth),hl
;knr.c: 963: 	if (type ==    1 && paren_depth == 0) {
l273:
l272:
ld	a,(ix+6)
cp	.low.1
jp	nz,20f
jp	21f
21:
ld	hl,(_paren_depth)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l274
11:
;knr.c: 969: 		base_end = findBaseEnd();
call	_findBaseEnd
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-6),l
ld	(ix+1+-6),h
;knr.c: 970: 		decl_start = base_end;
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	(ix+-8),l
ld	(ix+1+-8),h
;knr.c: 982: 		for (i = base_end; i < num_decl_toks; i++) {
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	(ix+-4),l
ld	(ix+1+-4),h
jp	l278
l275:
;knr.c: 984: 			if (decl_toks[i].type ==   9) {
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.9
jp	nz,l279
;knr.c: 985: 				decl_start = i + 1;
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
ld	(ix+-8),l
ld	(ix+1+-8),h
;knr.c: 986: 				continue;
jp	l277
;knr.c: 987: 			}
;knr.c: 988: 			if (decl_toks[i].type ==    1)
l279:
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.1
jp	nz,l280
;knr.c: 989: 				break;
jp	l276
;knr.c: 991: 			if (decl_toks[i].type ==     20) {
l280:
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.20
jp	nz,l281
;knr.c: 992: 				p = findParam(decl_toks[i].v.str);
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
call	_findParam
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-2),l
ld	(ix+1+-2),h
;knr.c: 996: 				if (p && !p->has_type) {
ld	a,(ix+-2)
or	(ix+1+-2)
jp	z,20f
jp	21f
21:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,20
add	hl,de
ld	a,(hl)
or	a
jp	anz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l282
11:
;knr.c: 997: 					int k = 0, j;
ld	(ix+-10),.low.0
ld	(ix+1+-10),.high.0
;knr.c: 999: 					if (!p->type_toks)
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp	nz,l283
;knr.c: 1000: 						p->type_toks = malloc(32 * sizeof(struct buftok));
ld	hl,224
push	hl
call	_malloc
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
ld	hl,16
add	hl,de
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1002: 					for (j = 0; j < base_end && k < 32; j++) {
l283:
ld	(ix+-12),.low.0
ld	(ix+1+-12),.high.0
jp	l287
l284:
;knr.c: 1003: 						p->type_toks[k].type = decl_toks[j].type;
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
push	af
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
pop	af
ld	(hl),a
;knr.c: 1004: 						if (decl_toks[j].type ==     20)
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.20
jp	nz,l288
;knr.c: 1005: 							p->type_toks[k].v.str = strdup(decl_toks[j].v.str);
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
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
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1006: 						else
jp	l289
l288:
;knr.c: 1007: 							p->type_toks[k].v = decl_toks[j].v;
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
push	hl
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
ex	de,hl
pop	hl
push	hl
ld	bc,4
ldir
pop	hl
l289:
;knr.c: 1008: 						k++;
ld	l,(ix+-10)
ld	h,(ix+1+-10)
inc	hl
ld	(ix+-10),l
ld	(ix+1+-10),h
;knr.c: 1009: 					}
l286:
ld	l,(ix+-12)
ld	h,(ix+1+-12)
inc	hl
ld	(ix+-12),l
ld	(ix+1+-12),h
l287:
ld	e,(ix+-6)
ld	d,(ix+1+-6)
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
20:
ld	de,32
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l284
11:
l285:
;knr.c: 1011: 					for (j = decl_start; j <= i && k < 32; j++) {
ld	l,(ix+-8)
ld	h,(ix+1+-8)
ld	(ix+-12),l
ld	(ix+1+-12),h
jp	l293
l290:
;knr.c: 1012: 						p->type_toks[k].type = decl_toks[j].type;
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
push	af
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
pop	af
ld	(hl),a
;knr.c: 1013: 						if (decl_toks[j].type ==     20)
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.20
jp	nz,l294
;knr.c: 1014: 							p->type_toks[k].v.str = strdup(decl_toks[j].v.str);
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
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
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1015: 						else
jp	l295
l294:
;knr.c: 1016: 							p->type_toks[k].v = decl_toks[j].v;
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
push	hl
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
ex	de,hl
pop	hl
push	hl
ld	bc,4
ldir
pop	hl
l295:
;knr.c: 1017: 						k++;
ld	l,(ix+-10)
ld	h,(ix+1+-10)
inc	hl
ld	(ix+-10),l
ld	(ix+1+-10),h
;knr.c: 1018: 					}
l292:
ld	l,(ix+-12)
ld	h,(ix+1+-12)
inc	hl
ld	(ix+-12),l
ld	(ix+1+-12),h
l293:
ld	e,(ix+-12)
ld	d,(ix+1+-12)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	wrelop
call	wrelop
jp	age,20f
jp	21f
20:
ld	de,32
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l290
11:
l291:
;knr.c: 1020: 					for (j = i + 1; j < num_decl_toks && k < 32; j++) {
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
ld	(ix+-12),l
ld	(ix+1+-12),h
jp	l299
l296:
;knr.c: 1021: 						if (decl_toks[j].type ==   9)
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.9
jp	nz,l300
;knr.c: 1022: 							break;
jp	l297
;knr.c: 1023: 						if (decl_toks[j].type ==    1)
l300:
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.1
jp	nz,l301
;knr.c: 1024: 							break;
jp	l297
;knr.c: 1025: 						p->type_toks[k].type = decl_toks[j].type;
l301:
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
push	af
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
pop	af
ld	(hl),a
;knr.c: 1026: 						if (decl_toks[j].type ==     20)
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
ld	a,(hl)
cp	.low.20
jp	nz,l302
;knr.c: 1027: 							p->type_toks[k].v.str = strdup(decl_toks[j].v.str);
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
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
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
pop	de
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1028: 						else
jp	l303
l302:
;knr.c: 1029: 							p->type_toks[k].v = decl_toks[j].v;
ld	de,7
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
inc	hl
inc hl
inc hl
push	hl
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,16
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
push	bc
ld	de,7
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	amul
call	amul
pop	bc
add	hl,bc
inc	hl
inc hl
inc hl
ex	de,hl
pop	hl
push	hl
ld	bc,4
ldir
pop	hl
l303:
;knr.c: 1030: 						k++;
ld	l,(ix+-10)
ld	h,(ix+1+-10)
inc	hl
ld	(ix+-10),l
ld	(ix+1+-10),h
;knr.c: 1031: 					}
l298:
ld	l,(ix+-12)
ld	h,(ix+1+-12)
inc	hl
ld	(ix+-12),l
ld	(ix+1+-12),h
l299:
ld	de,(_num_decl_toks)
ld	l,(ix+-12)
ld	h,(ix+1+-12)
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
20:
ld	de,32
ld	l,(ix+-10)
ld	h,(ix+1+-10)
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l296
11:
l297:
;knr.c: 1032: 					p->num_type_toks = k;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,18
add	hl,de
ld	e,(ix+-10)
ld	d,(ix+1+-10)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1033: 					p->has_type = 1;
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,20
add	hl,de
ld	(hl),1
;knr.c: 1034: 				}
;knr.c: 1035: 			}
l282:
;knr.c: 1036: 		}
l281:
l277:
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
ld	(ix+-4),l
ld	(ix+1+-4),h
l278:
ld	de,(_num_decl_toks)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	wrelop
call	wrelop
jp	alt,l275
l276:
;knr.c: 1039: 		for (i = 0; i < num_decl_toks; i++) {
ld	(ix+-4),.low.0
ld	(ix+1+-4),.high.0
jp	l307
l304:
;knr.c: 1040: 			freeBufTok(&decl_toks[i]);
ld	de,7
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	amul
call	amul
ld	de,(_decl_toks)
add	hl,de
push	hl
call	_freeBufTok
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1041: 		}
l306:
ld	l,(ix+-4)
ld	h,(ix+1+-4)
inc	hl
ld	(ix+-4),l
ld	(ix+1+-4),h
l307:
ld	de,(_num_decl_toks)
ld	l,(ix+-4)
ld	h,(ix+1+-4)
global	wrelop
call	wrelop
jp	alt,l304
l305:
;knr.c: 1042: 		num_decl_toks = 0;
ld	hl,0
ld	(_num_decl_toks),hl
;knr.c: 1043: 		paren_depth = 0;
ld	hl,0
ld	(_paren_depth),hl
;knr.c: 1044: 		return 1;
ld	hl,1
jp	l260
;knr.c: 1045: 	}
;knr.c: 1047: 	return 1;
l274:
ld	hl,1
jp	l260
;knr.c: 1048: }
l260:
jp	cret
f226	equ	-12
;knr.c: 1056: void
;knr.c: 1057: knrFilterToken(unsigned char type)
;knr.c: 1058: {
global	_knrFilterToken
_knrFilterToken:
call	ncsv
defw	f227
;knr.c: 1059: 	knrFilter(type, 0, 0.0, ((void *)0), 0);
global	_knrFilter
ld	hl,0
push	hl
ld	hl,0
push	hl
psect	data
e228:	deff 0.0
psect	text
ld	de,(e228)
ld	hl,(e228+2)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1060: }
l308:
jp	cret
f227	equ	0
;knr.c: 1062: void
;knr.c: 1063: knrFiltKw(unsigned char kw)
;knr.c: 1064: {
global	_knrFiltKw
_knrFiltKw:
call	ncsv
defw	f229
;knr.c: 1065: 	knrFilter(kw, 0, 0.0, ((void *)0), 0);
ld	hl,0
push	hl
ld	hl,0
push	hl
psect	data
e230:	deff 0.0
psect	text
ld	de,(e230)
ld	hl,(e230+2)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1066: }
l309:
jp	cret
f229	equ	0
;knr.c: 1068: void
;knr.c: 1069: knrFilterSym(char *name)
;knr.c: 1070: {
global	_knrFilterSym
_knrFilterSym:
call	ncsv
defw	f231
;knr.c: 1071: 	knrFilter(    20, 0, 0.0, name, strlen(name));
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
psect	data
e232:	deff 0.0
psect	text
ld	de,(e232)
ld	hl,(e232+2)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,.low.20
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1072: }
l310:
jp	cret
f231	equ	0
;knr.c: 1074: void
;knr.c: 1075: knrFiltNum(long val)
;knr.c: 1076: {
global	_knrFiltNum
_knrFiltNum:
call	ncsv
defw	f233
;knr.c: 1077: 	knrFilter( 21, val, 0.0, ((void *)0), 0);
ld	hl,0
push	hl
ld	hl,0
push	hl
psect	data
e234:	deff 0.0
psect	text
ld	de,(e234)
ld	hl,(e234+2)
push	hl
push	de
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
push	hl
push	de
ld	l,.low.21
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1078: }
l311:
jp	cret
f233	equ	0
;knr.c: 1080: void
;knr.c: 1081: knrFiltFNum(float val)
;knr.c: 1082: {
global	_knrFiltFNum
_knrFiltFNum:
call	ncsv
defw	f235
;knr.c: 1083: 	knrFilter(23, 0, val, ((void *)0), 0);
ld	hl,0
push	hl
ld	hl,0
push	hl
ld	e,(ix+6)
ld	d,(ix+1+6)
ld	l,(ix+2+6)
ld	h,(ix+3+6)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,.low.23
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1084: }
l312:
jp	cret
f235	equ	0
;knr.c: 1086: void
;knr.c: 1087: knrFiltStr(char *str, int len)
;knr.c: 1088: {
global	_knrFiltStr
_knrFiltStr:
call	ncsv
defw	f236
;knr.c: 1089: 	knrFilter( 22, 0, 0.0, str, len);
ld	l,(ix+8)
ld	h,(ix+1+8)
push	hl
ld	l,(ix+6)
ld	h,(ix+1+6)
push	hl
psect	data
e237:	deff 0.0
psect	text
ld	de,(e237)
ld	hl,(e237+2)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,.low.22
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1090: }
l313:
jp	cret
f236	equ	0
;knr.c: 1099: void
;knr.c: 1100: knrFilter(unsigned char type, long num, float fnum, char *str, int slen)
;knr.c: 1101: {
_knrFilter:
call	ncsv
defw	f238
;knr.c: 1102: 	int cur_depth = brace_depth;
ld	hl,(_brace_depth)
ld	(ix+-2),l
ld	(ix+1+-2),h
;knr.c: 1112: 	switch (state) {
jp	l316
;knr.c: 1113: 	case    0		:
l317:
;knr.c: 1121: 		if (cur_depth == 0) {
ld	a,(ix+-2)
or	(ix+1+-2)
jp	nz,l318
;knr.c: 1123: 			if (type ==     140) {
ld	a,(ix+6)
cp	.low.-116
jp	nz,l319
;knr.c: 1125: 				typedef_depth = 0;
ld	hl,0
ld	(_typedef_depth),hl
;knr.c: 1126: 				typedef_name[0] = 0;
ld	a,.low.0
ld	(_typedef_name),a
;knr.c: 1127: 				state =   5		;
ld	a,.low.5
ld	(_state),a
;knr.c: 1128: 				realEmitKw(    140);
ld	l,.low.-116
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1129: 				return;
jp	l314
;knr.c: 1130: 			}
;knr.c: 1136: 			if (type ==     20 && !isTypedef(str)) {
l319:
ld	a,(ix+6)
cp	.low.20
jp	nz,20f
jp	21f
21:
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_isTypedef
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l320
11:
;knr.c: 1137: 				bufToken(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufToken
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1138: 				state = 1		;
ld	a,.low.1
ld	(_state),a
;knr.c: 1139: 				return;
jp	l314
;knr.c: 1140: 			}
;knr.c: 1143: 			if (isTypeTok(type, str)) {
l320:
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	l,(ix+6)
push	hl
call	_isTypeTok
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,l321
;knr.c: 1144: 				bufToken(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufToken
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1145: 				state = 1		;
ld	a,.low.1
ld	(_state),a
;knr.c: 1146: 				return;
jp	l314
;knr.c: 1147: 			}
;knr.c: 1148: 		}
l321:
;knr.c: 1156: 		if (cur_depth > 0) {
l318:
ld	e,(ix+-2)
ld	d,(ix+1+-2)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l322
;knr.c: 1158: 			if (type ==       148) {
ld	a,(ix+6)
cp	.low.-108
jp	nz,l323
;knr.c: 1160: 				pushLoop(      148);
ld	l,.low.-108
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1161: 				num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 1162: 				loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 1163: 				state =  12	;
ld	a,.low.12
ld	(_state),a
;knr.c: 1164: 				return;
jp	l314
;knr.c: 1165: 			}
;knr.c: 1167: 			if (type ==         156) {
l323:
ld	a,(ix+6)
cp	.low.-100
jp	nz,l324
;knr.c: 1168: 				pushLoop(        156);
ld	l,.low.-100
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1169: 				num_loop_init = 0;
ld	hl,0
ld	(_num_loop_init),hl
;knr.c: 1170: 				num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 1171: 				num_loop_incr = 0;
ld	hl,0
ld	(_num_loop_incr),hl
;knr.c: 1172: 				loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 1173: 				for_part = 0;
ld	hl,0
ld	(_for_part),hl
;knr.c: 1174: 				state =  12	;
ld	a,.low.12
ld	(_state),a
;knr.c: 1175: 				return;
jp	l314
;knr.c: 1176: 			}
;knr.c: 1178: 			if (type ==          154) {
l324:
ld	a,(ix+6)
cp	.low.-102
jp	nz,l325
;knr.c: 1179: 				pushLoop(         154);
ld	l,.low.-102
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1181: 				lastEmitLine = lineno;
ld	hl,(_lineno)
ld	(_lastEmitLine),hl
;knr.c: 1183: 				realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1184: 				brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1185: 				emitLoopLabel('T');
ld	l,.low.84
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1187: 				outbufPush();
global	_outbufPush
call	_outbufPush
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1188: 				loop_stack[loop_sp - 1].body_depth = 0;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1189: 				state =    14	;
ld	a,.low.14
ld	(_state),a
;knr.c: 1190: 				return;
jp	l314
;knr.c: 1191: 			}
;knr.c: 1193: 			if (type ==          147) {
l325:
ld	a,(ix+6)
cp	.low.-109
jp	nz,l326
;knr.c: 1194: 				realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1195: 				ctrl_type = type;
ld	a,(ix+6)
ld	(_ctrl_type),a
;knr.c: 1196: 				ctrlParenDep = 0;
ld	hl,0
ld	(_ctrlParenDep),hl
;knr.c: 1197: 				saved_state = state;
ld	a,(_state)
ld	(_saved_state),a
;knr.c: 1198: 				state = 6		;
ld	a,.low.6
ld	(_state),a
;knr.c: 1199: 				return;
jp	l314
;knr.c: 1200: 			}
;knr.c: 1202: 			if (type ==        149) {
l326:
ld	a,(ix+6)
cp	.low.-107
jp	nz,l327
;knr.c: 1203: 				realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1204: 				ctrl_type = type;
ld	a,(ix+6)
ld	(_ctrl_type),a
;knr.c: 1205: 				saved_state = state;
ld	a,(_state)
ld	(_saved_state),a
;knr.c: 1206: 				state = 7		;
ld	a,.low.7
ld	(_state),a
;knr.c: 1207: 				return;
jp	l314
;knr.c: 1208: 			}
;knr.c: 1213: 			if (isTypeTok(type, str)) {
l327:
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	l,(ix+6)
push	hl
call	_isTypeTok
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,l328
;knr.c: 1214: 				bufLocDecl(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLocDecl
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1215: 				locDeclParen = 0;
ld	hl,0
ld	(_locDeclParen),hl
;knr.c: 1216: 				locInInit = 0;
ld	hl,0
ld	(_locInInit),hl
;knr.c: 1217: 				locCurName[0] = 0;
ld	a,.low.0
ld	(_locCurName),a
;knr.c: 1218: 				state = 10	;
ld	a,.low.10
ld	(_state),a
;knr.c: 1219: 				return;
jp	l314
;knr.c: 1220: 			}
;knr.c: 1221: 		}
l328:
;knr.c: 1224: 		if (type ==   2)
l322:
ld	a,(ix+6)
cp	.low.2
jp	nz,l329
;knr.c: 1225: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1226: 		else if (type ==     3)
jp	l330
l329:
ld	a,(ix+6)
cp	.low.3
jp	nz,l331
;knr.c: 1227: 			brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1229: 		if (type ==     20)
l331:
l330:
ld	a,(ix+6)
cp	.low.20
jp	nz,l332
;knr.c: 1230: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1231: 		else if (type ==  21)
jp	l333
l332:
ld	a,(ix+6)
cp	.low.21
jp	nz,l334
;knr.c: 1232: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1233: 		else if (type == 23)
jp	l335
l334:
ld	a,(ix+6)
cp	.low.23
jp	nz,l336
;knr.c: 1234: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1235: 		else if (type ==  22)
jp	l337
l336:
ld	a,(ix+6)
cp	.low.22
jp	nz,l338
;knr.c: 1236: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1237: 		else
jp	l339
l338:
;knr.c: 1238: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l339:
l337:
l335:
l333:
;knr.c: 1239: 		break;
jp	l315
;knr.c: 1241: 	case 1		:
l340:
;knr.c: 1243: 		bufToken(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufToken
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1245: 		if (type ==    6) {
ld	a,(ix+6)
cp	.low.6
jp	nz,l341
;knr.c: 1246: 			state =    2		;
ld	a,.low.2
ld	(_state),a
;knr.c: 1247: 			num_params = 0;
ld	hl,0
ld	(_num_params),hl
;knr.c: 1248: 			return;
jp	l314
;knr.c: 1249: 		}
;knr.c: 1252: 		if (type ==    1 || type ==   2) {
l341:
ld	a,(ix+6)
cp	.low.1
jp	nz,20f
jp	21f
20:
ld	a,(ix+6)
cp	.low.2
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l342
11:
;knr.c: 1253: 			flushBuf();
call	_flushBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1254: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1255: 			return;
jp	l314
;knr.c: 1256: 		}
;knr.c: 1259: 		break;
l342:
jp	l315
;knr.c: 1261: 	case    2		:
l343:
;knr.c: 1263: 		bufToken(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufToken
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1265: 		if (type ==     20) {
ld	a,(ix+6)
cp	.low.20
jp	nz,l344
;knr.c: 1266: 			if (!isTypedef(str)) {
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_isTypedef
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	nz,l345
;knr.c: 1268: 				struct param *newp = malloc(sizeof(struct param));
ld	hl,23
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
;knr.c: 1269: 				struct param **pp;
;knr.c: 1270: 				strncpy(newp->name, str, 15);
ld	hl,15
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	l,(ix+-4)
ld	h,(ix+1+-4)
push	hl
call	_strncpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1271: 				newp->name[15] = 0;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,15
add	hl,de
ld	(hl),0
;knr.c: 1272: 				newp->has_type = 0;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,20
add	hl,de
ld	(hl),0
;knr.c: 1273: 				newp->num_type_toks = 0;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,18
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1274: 				newp->type_toks = ((void *)0);
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,16
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1275: 				newp->next = ((void *)0);
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	hl,21
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1277: 				for (pp = &params; *pp; pp = &(*pp)->next)
ld	hl,_params
ld	(ix+-6),l
ld	(ix+1+-6),h
jp	l349
l346:
;knr.c: 1278: 					;
l348:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	hl,21
add	hl,bc
ld	(ix+-6),l
ld	(ix+1+-6),h
l349:
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	a,(hl)
inc	hl
or	(hl)
jp	nz,l346
l347:
;knr.c: 1279: 				*pp = newp;
ld	e,(ix+-4)
ld	d,(ix+1+-4)
ld	l,(ix+-6)
ld	h,(ix+1+-6)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1280: 				num_params++;
ld	hl,(_num_params)
inc	hl
ld	(_num_params),hl
;knr.c: 1281: 			} else {
jp	l350
l345:
;knr.c: 1283: 				flushBuf();
call	_flushBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1284: 				state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1285: 				return;
jp	l314
;knr.c: 1286: 			}
l350:
;knr.c: 1287: 		} else if (type ==    7) {
jp	l351
l344:
ld	a,(ix+6)
cp	.low.7
jp	nz,l352
;knr.c: 1288: 			state = 3		;
ld	a,.low.3
ld	(_state),a
;knr.c: 1289: 			return;
jp	l314
;knr.c: 1290: 		} else if (type ==   9) {
jp	l353
l352:
ld	a,(ix+6)
cp	.low.9
jp	nz,l354
;knr.c: 1292: 		} else if (isTypeTok(type, ((void *)0))) {
jp	l355
l354:
ld	hl,0
push	hl
ld	l,(ix+6)
push	hl
call	_isTypeTok
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,l356
;knr.c: 1294: 			flushBuf();
call	_flushBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1295: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1296: 			return;
jp	l314
;knr.c: 1297: 		} else if (type ==    36 || type ==  4 || type == 92) {
jp	l357
l356:
ld	a,(ix+6)
cp	.low.36
jp	nz,30f
jp	31f
30:
ld	a,(ix+6)
cp	.low.4
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+6)
cp	.low.92
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l358
11:
;knr.c: 1299: 			flushBuf();
call	_flushBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1300: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1301: 			return;
jp	l314
;knr.c: 1302: 		}
;knr.c: 1303: 		break;
l358:
l357:
l355:
l353:
l351:
jp	l315
;knr.c: 1305: 	case 3		:
l359:
;knr.c: 1307: 		if (type ==   2) {
ld	a,(ix+6)
cp	.low.2
jp	nz,l360
;knr.c: 1309: 			emitAnsiHeader();
call	_emitAnsiHeader
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1310: 			realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1311: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1312: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1313: 		} else if (type ==    1) {
jp	l361
l360:
ld	a,(ix+6)
cp	.low.1
jp	nz,l362
;knr.c: 1315: 			flushBuf();
call	_flushBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1316: 			realEmitToken(   1);
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1317: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1318: 		} else if (isTypeTok(type, str)) {
jp	l363
l362:
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	l,(ix+6)
push	hl
call	_isTypeTok
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,l364
;knr.c: 1320: 			state =     4		;
ld	a,.low.4
ld	(_state),a
;knr.c: 1321: 			num_decl_toks = 0;
ld	hl,0
ld	(_num_decl_toks),hl
;knr.c: 1322: 			paren_depth = 0;
ld	hl,0
ld	(_paren_depth),hl
;knr.c: 1323: 			procDeclTok(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_procDeclTok
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1324: 		} else {
jp	l365
l364:
;knr.c: 1326: 			flushBuf();
call	_flushBuf
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1328: 			if (type ==   2)
ld	a,(ix+6)
cp	.low.2
jp	nz,l366
;knr.c: 1329: 				brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1330: 			else if (type ==     3)
jp	l367
l366:
ld	a,(ix+6)
cp	.low.3
jp	nz,l368
;knr.c: 1331: 				brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1333: 			if (type ==     20)
l368:
l367:
ld	a,(ix+6)
cp	.low.20
jp	nz,l369
;knr.c: 1334: 				realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1335: 			else if (type ==  21)
jp	l370
l369:
ld	a,(ix+6)
cp	.low.21
jp	nz,l371
;knr.c: 1336: 				realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1337: 			else if (type == 23)
jp	l372
l371:
ld	a,(ix+6)
cp	.low.23
jp	nz,l373
;knr.c: 1338: 				realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1339: 			else if (type ==  22)
jp	l374
l373:
ld	a,(ix+6)
cp	.low.22
jp	nz,l375
;knr.c: 1340: 				realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1341: 			else
jp	l376
l375:
;knr.c: 1342: 				realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l376:
l374:
l372:
l370:
;knr.c: 1343: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1344: 		}
l365:
l363:
l361:
;knr.c: 1345: 		break;
jp	l315
;knr.c: 1347: 	case     4		:
l377:
;knr.c: 1349: 		procDeclTok(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_procDeclTok
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1350: 		break;
jp	l315
;knr.c: 1352: 	case   5		:
l378:
;knr.c: 1360: 		if (type ==    6 || type ==  4)
ld	a,(ix+6)
cp	.low.6
jp	nz,20f
jp	21f
20:
ld	a,(ix+6)
cp	.low.4
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l379
11:
;knr.c: 1361: 			typedef_depth++;
ld	hl,(_typedef_depth)
inc	hl
ld	(_typedef_depth),hl
;knr.c: 1362: 		else if (type ==    7 || type ==  5)
jp	l380
l379:
ld	a,(ix+6)
cp	.low.7
jp	nz,20f
jp	21f
20:
ld	a,(ix+6)
cp	.low.5
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l381
11:
;knr.c: 1363: 			typedef_depth--;
ld	hl,(_typedef_depth)
dec	hl
ld	(_typedef_depth),hl
;knr.c: 1365: 		if (type ==     20) {
l381:
l380:
ld	a,(ix+6)
cp	.low.20
jp	nz,l382
;knr.c: 1372: 			strncpy(typedef_name, str, 15);
ld	hl,15
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	hl,_typedef_name
push	hl
call	_strncpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1373: 			typedef_name[15] = 0;
ld	hl,_typedef_name+15
ld	(hl),0
;knr.c: 1374: 			tdefNameDepth = typedef_depth;
ld	hl,(_typedef_depth)
ld	(_tdefNameDepth),hl
;knr.c: 1375: 		}
;knr.c: 1377: 		if (type ==    1) {
l382:
ld	a,(ix+6)
cp	.low.1
jp	nz,l383
;knr.c: 1379: 			if (typedef_name[0]) {
ld	a,(_typedef_name)
or	a
jp	az,l384
;knr.c: 1380: 				knrAddTypedef(typedef_name);
ld	hl,_typedef_name
push	hl
call	_knrAddTypedef
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1384: 			}
;knr.c: 1385: 			typedef_name[0] = 0;
l384:
ld	a,.low.0
ld	(_typedef_name),a
;knr.c: 1386: 			typedef_depth = 0;
ld	hl,0
ld	(_typedef_depth),hl
;knr.c: 1387: 			state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1388: 		}
;knr.c: 1391: 		if (type ==   2)
l383:
ld	a,(ix+6)
cp	.low.2
jp	nz,l385
;knr.c: 1392: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1393: 		else if (type ==     3)
jp	l386
l385:
ld	a,(ix+6)
cp	.low.3
jp	nz,l387
;knr.c: 1394: 			brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1396: 		if (type ==     20)
l387:
l386:
ld	a,(ix+6)
cp	.low.20
jp	nz,l388
;knr.c: 1397: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1398: 		else if (type ==  21)
jp	l389
l388:
ld	a,(ix+6)
cp	.low.21
jp	nz,l390
;knr.c: 1399: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1400: 		else if (type == 23)
jp	l391
l390:
ld	a,(ix+6)
cp	.low.23
jp	nz,l392
;knr.c: 1401: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1402: 		else if (type ==  22)
jp	l393
l392:
ld	a,(ix+6)
cp	.low.22
jp	nz,l394
;knr.c: 1403: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1404: 		else
jp	l395
l394:
;knr.c: 1405: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l395:
l393:
l391:
l389:
;knr.c: 1406: 		break;
jp	l315
;knr.c: 1408: 	case 6		:
l396:
;knr.c: 1413: 		if (type ==    6)
ld	a,(ix+6)
cp	.low.6
jp	nz,l397
;knr.c: 1414: 			ctrlParenDep++;
ld	hl,(_ctrlParenDep)
inc	hl
ld	(_ctrlParenDep),hl
;knr.c: 1415: 		else if (type ==    7) {
jp	l398
l397:
ld	a,(ix+6)
cp	.low.7
jp	nz,l399
;knr.c: 1416: 			ctrlParenDep--;
ld	hl,(_ctrlParenDep)
dec	hl
ld	(_ctrlParenDep),hl
;knr.c: 1417: 			if (ctrlParenDep == 0) {
ld	hl,(_ctrlParenDep)
ld	a,l
or	h
jp	nz,l400
;knr.c: 1419: 				realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1420: 				state = 7		;
ld	a,.low.7
ld	(_state),a
;knr.c: 1421: 				return;
jp	l314
;knr.c: 1422: 			}
;knr.c: 1423: 		}
l400:
;knr.c: 1425: 		if (type ==     20)
l399:
l398:
ld	a,(ix+6)
cp	.low.20
jp	nz,l401
;knr.c: 1426: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1427: 		else if (type ==  21)
jp	l402
l401:
ld	a,(ix+6)
cp	.low.21
jp	nz,l403
;knr.c: 1428: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1429: 		else if (type == 23)
jp	l404
l403:
ld	a,(ix+6)
cp	.low.23
jp	nz,l405
;knr.c: 1430: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1431: 		else if (type ==  22)
jp	l406
l405:
ld	a,(ix+6)
cp	.low.22
jp	nz,l407
;knr.c: 1432: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1433: 		else
jp	l408
l407:
;knr.c: 1434: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l408:
l406:
l404:
l402:
;knr.c: 1435: 		break;
jp	l315
;knr.c: 1437: 	case 7		:
l409:
;knr.c: 1443: 		if (ctrl_type == 0) {
ld	a,(_ctrl_type)
or	a
jp	lnz,l410
;knr.c: 1445: 			state = saved_state;
ld	a,(_saved_state)
ld	(_state),a
;knr.c: 1447: 			if (type ==     20)
ld	a,(ix+6)
cp	.low.20
jp	nz,l411
;knr.c: 1448: 				realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1449: 			else if (type ==  21)
jp	l412
l411:
ld	a,(ix+6)
cp	.low.21
jp	nz,l413
;knr.c: 1450: 				realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1451: 			else if (type == 23)
jp	l414
l413:
ld	a,(ix+6)
cp	.low.23
jp	nz,l415
;knr.c: 1452: 				realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1453: 			else if (type ==  22)
jp	l416
l415:
ld	a,(ix+6)
cp	.low.22
jp	nz,l417
;knr.c: 1454: 				realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1455: 			else
jp	l418
l417:
;knr.c: 1456: 				realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l418:
l416:
l414:
l412:
;knr.c: 1457: 			break;
jp	l315
;knr.c: 1458: 		}
;knr.c: 1459: 		if (type ==   2) {
l410:
ld	a,(ix+6)
cp	.low.2
jp	nz,l419
;knr.c: 1461: 			realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1462: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1463: 			if (ctrl_type ==          154) {
ld	a,(_ctrl_type)
cp	.low.-102
jp	nz,l420
;knr.c: 1465: 				ctrlBodyDep = 1;
ld	hl,1
ld	(_ctrlBodyDep),hl
;knr.c: 1466: 				state =  9		;
ld	a,.low.9
ld	(_state),a
;knr.c: 1467: 			} else {
jp	l421
l420:
;knr.c: 1468: 				state = saved_state;
ld	a,(_saved_state)
ld	(_state),a
;knr.c: 1469: 			}
l421:
;knr.c: 1470: 		} else if (ctrl_type ==        149 && type ==          147) {
jp	l422
l419:
ld	a,(_ctrl_type)
cp	.low.-107
jp	nz,20f
jp	21f
21:
ld	a,(ix+6)
cp	.low.-109
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l423
11:
;knr.c: 1472: 			realEmitToken(         147);
ld	l,.low.-109
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1473: 			ctrl_type =          147;
ld	a,.low.-109
ld	(_ctrl_type),a
;knr.c: 1474: 			ctrlParenDep = 0;
ld	hl,0
ld	(_ctrlParenDep),hl
;knr.c: 1475: 			state = 6		;
ld	a,.low.6
ld	(_state),a
;knr.c: 1476: 		} else {
jp	l424
l423:
;knr.c: 1478: 			realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1479: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1480: 			ctrlBodyDep = 0;
ld	hl,0
ld	(_ctrlBodyDep),hl
;knr.c: 1481: 			state = 8		;
ld	a,.low.8
ld	(_state),a
;knr.c: 1483: 			knrFilter(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1484: 		}
l424:
l422:
;knr.c: 1485: 		break;
jp	l315
;knr.c: 1487: 	case 8		:
l425:
;knr.c: 1492: 		if (type ==   2 || type ==    6 || type ==  4)
ld	a,(ix+6)
cp	.low.2
jp	nz,30f
jp	31f
30:
ld	a,(ix+6)
cp	.low.6
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+6)
cp	.low.4
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l426
11:
;knr.c: 1493: 			ctrlBodyDep++;
ld	hl,(_ctrlBodyDep)
inc	hl
ld	(_ctrlBodyDep),hl
;knr.c: 1494: 		else if (type ==     3 || type ==    7 || type ==  5)
jp	l427
l426:
ld	a,(ix+6)
cp	.low.3
jp	nz,30f
jp	31f
30:
ld	a,(ix+6)
cp	.low.7
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+6)
cp	.low.5
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l428
11:
;knr.c: 1495: 			ctrlBodyDep--;
ld	hl,(_ctrlBodyDep)
dec	hl
ld	(_ctrlBodyDep),hl
;knr.c: 1498: 		if (ctrlBodyDep == 0) {
l428:
l427:
ld	hl,(_ctrlBodyDep)
ld	a,l
or	h
jp	nz,l429
;knr.c: 1499: 			if (ctrl_type ==          154 && type ==       148) {
ld	a,(_ctrl_type)
cp	.low.-102
jp	nz,20f
jp	21f
21:
ld	a,(ix+6)
cp	.low.-108
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l430
11:
;knr.c: 1501: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1502: 				brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1503: 				realEmitToken(      148);
ld	l,.low.-108
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1505: 				ctrlParenDep = 0;
ld	hl,0
ld	(_ctrlParenDep),hl
;knr.c: 1506: 				state = 6		;
ld	a,.low.6
ld	(_state),a
;knr.c: 1507: 				ctrl_type = 0;
ld	a,.low.0
ld	(_ctrl_type),a
;knr.c: 1508: 				return;
jp	l314
;knr.c: 1509: 			}
;knr.c: 1510: 			if (type ==    1 && ctrl_type !=          154) {
l430:
ld	a,(ix+6)
cp	.low.1
jp	nz,20f
jp	21f
21:
ld	a,(_ctrl_type)
cp	.low.-102
jp	z,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l431
11:
;knr.c: 1513: 				realEmitToken(   1);
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1514: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1515: 				brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1517: 				while (ctrl_sp > 0 &&
jp	l432
l433:
;knr.c: 1518: 				       ctrl_stack[ctrl_sp - 1].ctrl_type !=          154) {
;knr.c: 1519: 					ctrl_sp--;
ld	hl,(_ctrl_sp)
dec	hl
ld	(_ctrl_sp),hl
;knr.c: 1521: 					realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1522: 					brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1523: 				}
l432:
ld	de,(_ctrl_sp)
ld	hl,0
global	wrelop
call	wrelop
jp	alt,20f
jp	21f
20:
ld	de,3
ld	hl,(_ctrl_sp)
dec	hl
global	amul
call	amul
ld	de,_ctrl_stack
add	hl,de
ld	a,(hl)
cp	.low.-102
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l433
11:
l434:
;knr.c: 1525: 				if (ctrl_sp > 0) {
ld	de,(_ctrl_sp)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l435
;knr.c: 1526: 					ctrl_sp--;
ld	hl,(_ctrl_sp)
dec	hl
ld	(_ctrl_sp),hl
;knr.c: 1527: 					ctrl_type = ctrl_stack[ctrl_sp].ctrl_type;
ld	de,3
ld	hl,(_ctrl_sp)
global	amul
call	amul
ld	de,_ctrl_stack
add	hl,de
ld	a,(hl)
ld	(_ctrl_type),a
;knr.c: 1528: 					ctrlBodyDep = ctrl_stack[ctrl_sp].ctrlBodyDep;
ld	de,3
ld	hl,(_ctrl_sp)
global	amul
call	amul
ld	de,_ctrl_stack+1
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	(_ctrlBodyDep),bc
;knr.c: 1530: 				} else {
jp	l436
l435:
;knr.c: 1531: 					state = saved_state;
ld	a,(_saved_state)
ld	(_state),a
;knr.c: 1532: 					return;
jp	l314
;knr.c: 1533: 				}
l436:
;knr.c: 1534: 				return;
jp	l314
;knr.c: 1535: 			}
;knr.c: 1537: 			if (type ==          147 || type ==       148 || type ==         156) {
l431:
ld	a,(ix+6)
cp	.low.-109
jp	nz,30f
jp	31f
30:
ld	a,(ix+6)
cp	.low.-108
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
20:
ld	a,(ix+6)
cp	.low.-100
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l437
11:
;knr.c: 1539: 				if (ctrl_sp < 8) {
ld	de,8
ld	hl,(_ctrl_sp)
global	wrelop
call	wrelop
jp	age,l438
;knr.c: 1540: 					ctrl_stack[ctrl_sp].ctrl_type = ctrl_type;
ld	de,3
ld	hl,(_ctrl_sp)
global	amul
call	amul
ld	de,_ctrl_stack
add	hl,de
ld	a,(_ctrl_type)
ld	(hl),a
;knr.c: 1541: 					ctrl_stack[ctrl_sp].ctrlBodyDep = ctrlBodyDep;
ld	de,3
ld	hl,(_ctrl_sp)
global	amul
call	amul
ld	de,_ctrl_stack+1
add	hl,de
ld	de,(_ctrlBodyDep)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1542: 					ctrl_sp++;
ld	hl,(_ctrl_sp)
inc	hl
ld	(_ctrl_sp),hl
;knr.c: 1543: 				}
;knr.c: 1544: 				realEmitToken(type);
l438:
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1545: 				ctrl_type = type;
ld	a,(ix+6)
ld	(_ctrl_type),a
;knr.c: 1546: 				ctrlParenDep = 0;
ld	hl,0
ld	(_ctrlParenDep),hl
;knr.c: 1547: 				state = 6		;
ld	a,.low.6
ld	(_state),a
;knr.c: 1548: 				return;
jp	l314
;knr.c: 1549: 			}
;knr.c: 1550: 			if (type ==        149 && ctrl_type ==          147) {
l437:
ld	a,(ix+6)
cp	.low.-107
jp	nz,20f
jp	21f
21:
ld	a,(_ctrl_type)
cp	.low.-109
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l439
11:
;knr.c: 1552: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1553: 				brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1555: 				realEmitToken(       149);
ld	l,.low.-107
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1556: 				ctrl_type =        149;
ld	a,.low.-107
ld	(_ctrl_type),a
;knr.c: 1557: 				state = 7		;
ld	a,.low.7
ld	(_state),a
;knr.c: 1558: 				return;
jp	l314
;knr.c: 1559: 			}
;knr.c: 1560: 			if (type ==          154 || type ==        149) {
l439:
ld	a,(ix+6)
cp	.low.-102
jp	nz,20f
jp	21f
20:
ld	a,(ix+6)
cp	.low.-107
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l440
11:
;knr.c: 1562: 				if (ctrl_sp < 8) {
ld	de,8
ld	hl,(_ctrl_sp)
global	wrelop
call	wrelop
jp	age,l441
;knr.c: 1563: 					ctrl_stack[ctrl_sp].ctrl_type = ctrl_type;
ld	de,3
ld	hl,(_ctrl_sp)
global	amul
call	amul
ld	de,_ctrl_stack
add	hl,de
ld	a,(_ctrl_type)
ld	(hl),a
;knr.c: 1564: 					ctrl_stack[ctrl_sp].ctrlBodyDep = ctrlBodyDep;
ld	de,3
ld	hl,(_ctrl_sp)
global	amul
call	amul
ld	de,_ctrl_stack+1
add	hl,de
ld	de,(_ctrlBodyDep)
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1565: 					ctrl_sp++;
ld	hl,(_ctrl_sp)
inc	hl
ld	(_ctrl_sp),hl
;knr.c: 1566: 				}
;knr.c: 1567: 				realEmitToken(type);
l441:
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1568: 				ctrl_type = type;
ld	a,(ix+6)
ld	(_ctrl_type),a
;knr.c: 1569: 				state = 7		;
ld	a,.low.7
ld	(_state),a
;knr.c: 1570: 				return;
jp	l314
;knr.c: 1571: 			}
;knr.c: 1572: 		}
l440:
;knr.c: 1575: 		if (type ==     20)
l429:
ld	a,(ix+6)
cp	.low.20
jp	nz,l442
;knr.c: 1576: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1577: 		else if (type ==  21)
jp	l443
l442:
ld	a,(ix+6)
cp	.low.21
jp	nz,l444
;knr.c: 1578: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1579: 		else if (type == 23)
jp	l445
l444:
ld	a,(ix+6)
cp	.low.23
jp	nz,l446
;knr.c: 1580: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1581: 		else if (type ==  22)
jp	l447
l446:
ld	a,(ix+6)
cp	.low.22
jp	nz,l448
;knr.c: 1582: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1583: 		else
jp	l449
l448:
;knr.c: 1584: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l449:
l447:
l445:
l443:
;knr.c: 1585: 		break;
jp	l315
;knr.c: 1587: 	case  9		:
l450:
;knr.c: 1592: 		if (type ==   2)
ld	a,(ix+6)
cp	.low.2
jp	nz,l451
;knr.c: 1593: 			ctrlBodyDep++;
ld	hl,(_ctrlBodyDep)
inc	hl
ld	(_ctrlBodyDep),hl
;knr.c: 1594: 		else if (type ==     3)
jp	l452
l451:
ld	a,(ix+6)
cp	.low.3
jp	nz,l453
;knr.c: 1595: 			ctrlBodyDep--;
ld	hl,(_ctrlBodyDep)
dec	hl
ld	(_ctrlBodyDep),hl
;knr.c: 1598: 		if (type ==     20)
l453:
l452:
ld	a,(ix+6)
cp	.low.20
jp	nz,l454
;knr.c: 1599: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1600: 		else if (type ==  21)
jp	l455
l454:
ld	a,(ix+6)
cp	.low.21
jp	nz,l456
;knr.c: 1601: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1602: 		else if (type == 23)
jp	l457
l456:
ld	a,(ix+6)
cp	.low.23
jp	nz,l458
;knr.c: 1603: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1604: 		else if (type ==  22)
jp	l459
l458:
ld	a,(ix+6)
cp	.low.22
jp	nz,l460
;knr.c: 1605: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1606: 		else
jp	l461
l460:
;knr.c: 1607: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l461:
l459:
l457:
l455:
;knr.c: 1610: 		if (type ==     3 && ctrlBodyDep == 0) {
ld	a,(ix+6)
cp	.low.3
jp	nz,20f
jp	21f
21:
ld	hl,(_ctrlBodyDep)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l462
11:
;knr.c: 1611: 			ctrl_type = 0;
ld	a,.low.0
ld	(_ctrl_type),a
;knr.c: 1612: 			state = 6		;
ld	a,.low.6
ld	(_state),a
;knr.c: 1613: 			ctrlParenDep = 0;
ld	hl,0
ld	(_ctrlParenDep),hl
;knr.c: 1614: 		}
;knr.c: 1615: 		break;
l462:
jp	l315
;knr.c: 1617: 	case 10	:
l463:
;knr.c: 1629: 		if (type ==    6)
ld	a,(ix+6)
cp	.low.6
jp	nz,l464
;knr.c: 1630: 			locDeclParen++;
ld	hl,(_locDeclParen)
inc	hl
ld	(_locDeclParen),hl
;knr.c: 1631: 		else if (type ==    7)
jp	l465
l464:
ld	a,(ix+6)
cp	.low.7
jp	nz,l466
;knr.c: 1632: 			locDeclParen--;
ld	hl,(_locDeclParen)
dec	hl
ld	(_locDeclParen),hl
;knr.c: 1634: 		if (locDeclParen == 0 && type ==    1) {
l466:
l465:
ld	hl,(_locDeclParen)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:
ld	a,(ix+6)
cp	.low.1
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l467
11:
;knr.c: 1636: 			if (locInInit)
ld	hl,(_locInInit)
ld	a,l
or	h
jp	z,l468
;knr.c: 1637: 				locInInit = 0;
ld	hl,0
ld	(_locInInit),hl
;knr.c: 1638: 			bufLocDecl(   1, 0, 0, ((void *)0), 0);
l468:
ld	hl,0
push	hl
ld	hl,0
push	hl
psect	data
e239:	deff 0
psect	text
ld	de,(e239)
ld	hl,(e239+2)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,.low.1
push	hl
call	_bufLocDecl
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1639: 			emitLocDecl();
call	_emitLocDecl
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1640: 			locCurName[0] = 0;
ld	a,.low.0
ld	(_locCurName),a
;knr.c: 1641: 			state = 11	;
ld	a,.low.11
ld	(_state),a
;knr.c: 1642: 			return;
jp	l314
;knr.c: 1643: 		}
;knr.c: 1645: 		if (locDeclParen == 0 && type ==   9) {
l467:
ld	hl,(_locDeclParen)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:
ld	a,(ix+6)
cp	.low.9
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l469
11:
;knr.c: 1647: 			if (locInInit)
ld	hl,(_locInInit)
ld	a,l
or	h
jp	z,l470
;knr.c: 1648: 				locInInit = 0;
ld	hl,0
ld	(_locInInit),hl
;knr.c: 1649: 			bufLocDecl(  9, 0, 0, ((void *)0), 0);
l470:
ld	hl,0
push	hl
ld	hl,0
push	hl
psect	data
e240:	deff 0
psect	text
ld	de,(e240)
ld	hl,(e240+2)
push	hl
push	de
ld	de,0
ld	hl,0
push	hl
push	de
ld	l,.low.9
push	hl
call	_bufLocDecl
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1650: 			locCurName[0] = 0;
ld	a,.low.0
ld	(_locCurName),a
;knr.c: 1651: 			return;
jp	l314
;knr.c: 1652: 		}
;knr.c: 1654: 		if (locDeclParen == 0 && type ==  80 && !locInInit) {
l469:
ld	hl,(_locDeclParen)
ld	a,l
or	h
jp	nz,30f
jp	31f
31:
ld	a,(ix+6)
cp	.low.80
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	hl,(_locInInit)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l471
11:
;knr.c: 1656: 			startLocInit(locCurName);
ld	hl,_locCurName
push	hl
call	_startLocInit
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1657: 			return;
jp	l314
;knr.c: 1658: 		}
;knr.c: 1660: 		if (locInInit) {
l471:
ld	hl,(_locInInit)
ld	a,l
or	h
jp	z,l472
;knr.c: 1662: 			bufLocInit(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLocInit
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1663: 		} else {
jp	l473
l472:
;knr.c: 1665: 			bufLocDecl(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLocDecl
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1666: 			if (type ==     20 && !isTypedef(str)) {
ld	a,(ix+6)
cp	.low.20
jp	nz,20f
jp	21f
21:
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_isTypedef
exx
ld	hl,2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l474
11:
;knr.c: 1668: 				strncpy(locCurName, str, 15);
ld	hl,15
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	hl,_locCurName
push	hl
call	_strncpy
ld	hl,2+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1669: 				locCurName[15] = 0;
ld	hl,_locCurName+15
ld	(hl),0
;knr.c: 1670: 			}
;knr.c: 1671: 		}
l474:
l473:
;knr.c: 1672: 		break;
jp	l315
;knr.c: 1674: 	case 11	:
l475:
;knr.c: 1680: 		if (isTypeTok(type, str)) {
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	l,(ix+6)
push	hl
call	_isTypeTok
exx
ld	hl,2+2
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	a,l
or	h
jp	z,l476
;knr.c: 1682: 			bufLocDecl(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLocDecl
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1683: 			locDeclParen = 0;
ld	hl,0
ld	(_locDeclParen),hl
;knr.c: 1684: 			locInInit = 0;
ld	hl,0
ld	(_locInInit),hl
;knr.c: 1685: 			locCurName[0] = 0;
ld	a,.low.0
ld	(_locCurName),a
;knr.c: 1686: 			state = 10	;
ld	a,.low.10
ld	(_state),a
;knr.c: 1687: 			return;
jp	l314
;knr.c: 1688: 		}
;knr.c: 1691: 		flushLocInits();
l476:
call	_flushLocInits
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1692: 		state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 1695: 		knrFilter(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_knrFilter
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1696: 		break;
jp	l315
;knr.c: 1698: 	case  12	:
l477:
;knr.c: 1712: 		if (type ==    6) {
ld	a,(ix+6)
cp	.low.6
jp	nz,l478
;knr.c: 1713: 			loopParen++;
ld	hl,(_loopParen)
inc	hl
ld	(_loopParen),hl
;knr.c: 1714: 			if (loopParen == 1)
ld	de,1
ld	hl,(_loopParen)
or	a
sbc	hl,de
jp	nz,l479
;knr.c: 1715: 				return;
jp	l314
;knr.c: 1716: 		} else if (type ==    7) {
l479:
jp	l480
l478:
ld	a,(ix+6)
cp	.low.7
jp	nz,l481
;knr.c: 1717: 			loopParen--;
ld	hl,(_loopParen)
dec	hl
ld	(_loopParen),hl
;knr.c: 1718: 			if (loopParen == 0) {
ld	hl,(_loopParen)
ld	a,l
or	h
jp	nz,l482
;knr.c: 1721: 				lastEmitLine = lineno;
ld	hl,(_lineno)
ld	(_lastEmitLine),hl
;knr.c: 1722: 				if (loop_stack[loop_sp - 1].type ==       148) {
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-108
jp	nz,l483
;knr.c: 1724: 					realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1725: 					brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1726: 					emitLoopLabel('T');
ld	l,.low.84
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1727: 					if (num_loop_cond > 0) {
ld	de,(_num_loop_cond)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l484
;knr.c: 1728: 						realEmitKw(         147);
ld	l,.low.-109
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1729: 						realEmitToken(   6);
ld	l,.low.6
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1730: 						realEmitToken(   34);
ld	l,.low.34
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1731: 						realEmitToken(   6);
ld	l,.low.6
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1732: 						emitLoopCond();
call	_emitLoopCond
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1733: 						realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1734: 						realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1735: 						realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1736: 						emitLoopGoto('B');
ld	l,.low.66
push	hl
call	_emitLoopGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1737: 						realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1738: 					}
;knr.c: 1739: 					clearLoopCond();
l484:
call	_clearLoopCond
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1740: 				} else {
jp	l485
l483:
;knr.c: 1742: 					realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1743: 					brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1744: 					if (num_loop_init > 0) {
ld	de,(_num_loop_init)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l486
;knr.c: 1745: 						emitLoopInit();
call	_emitLoopInit
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1746: 						realEmitToken(   1);
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1747: 					}
;knr.c: 1748: 					clearLoopInit();
l486:
call	_clearLoopInit
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1749: 					emitLoopLabel('T');
ld	l,.low.84
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1750: 					if (num_loop_cond > 0) {
ld	de,(_num_loop_cond)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l487
;knr.c: 1751: 						realEmitKw(         147);
ld	l,.low.-109
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1752: 						realEmitToken(   6);
ld	l,.low.6
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1753: 						realEmitToken(   34);
ld	l,.low.34
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1754: 						realEmitToken(   6);
ld	l,.low.6
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1755: 						emitLoopCond();
call	_emitLoopCond
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1756: 						realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1757: 						realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1758: 						realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1759: 						emitLoopGoto('B');
ld	l,.low.66
push	hl
call	_emitLoopGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1760: 						realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1761: 					}
;knr.c: 1762: 					clearLoopCond();
l487:
call	_clearLoopCond
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1763: 				}
l485:
;knr.c: 1765: 				outbufPush();
call	_outbufPush
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1766: 				loop_stack[loop_sp - 1].body_depth = 0;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1767: 				state =  13	;
ld	a,.low.13
ld	(_state),a
;knr.c: 1768: 				return;
jp	l314
;knr.c: 1769: 			}
;knr.c: 1770: 		}
l482:
;knr.c: 1773: 		if (loop_stack[loop_sp - 1].type ==         156 &&
l481:
l480:
;knr.c: 1774: 		    type ==    1 && loopParen == 1) {
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-100
jp	nz,30f
jp	31f
31:
ld	a,(ix+6)
cp	.low.1
jp	nz,30f
jp	31f
31:jp	21f
30:jp	20f
21:
ld	de,1
ld	hl,(_loopParen)
or	a
sbc	hl,de
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l488
11:
;knr.c: 1775: 			for_part++;
ld	hl,(_for_part)
inc	hl
ld	(_for_part),hl
;knr.c: 1776: 			return;
jp	l314
;knr.c: 1777: 		}
;knr.c: 1780: 		if (loop_stack[loop_sp - 1].type ==         156) {
l488:
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-100
jp	nz,l489
;knr.c: 1781: 			switch (for_part) {
jp	l491
;knr.c: 1782: 			case 0:
l492:
;knr.c: 1783: 				bufLoopInit(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLoopInit
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1784: 				break;
jp	l490
;knr.c: 1785: 			case 1:
l493:
;knr.c: 1786: 				bufLoopCond(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLoopCond
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1787: 				break;
jp	l490
;knr.c: 1788: 			case 2:
l494:
;knr.c: 1789: 				bufLoopIncr(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLoopIncr
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1790: 				break;
jp	l490
;knr.c: 1791: 			}
jp	l490
l491:
ld	hl,(_for_part)
ld	a,h
or	a
jp	nz,1f
ld	a,l
or	a
jp	z,l492
cp	1
jp	z,l493
cp	2
jp	z,l494
1:
jp	l490
l490:
;knr.c: 1792: 		} else {
jp	l495
l489:
;knr.c: 1793: 			bufLoopCond(type, num, fnum, str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLoopCond
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 1794: 		}
l495:
;knr.c: 1795: 		break;
jp	l315
;knr.c: 1797: 	case  13	:
l496:
;knr.c: 1808: 		if (type ==   2) {
ld	a,(ix+6)
cp	.low.2
jp	nz,l497
;knr.c: 1809: 			loop_stack[loop_sp - 1].body_depth++;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
inc	bc
ld	(hl),b
dec	hl
ld	(hl),c
;knr.c: 1810: 		} else if (type ==     3) {
jp	l498
l497:
ld	a,(ix+6)
cp	.low.3
jp	nz,l499
;knr.c: 1811: 			loop_stack[loop_sp - 1].body_depth--;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
dec	bc
ld	(hl),b
dec	hl
ld	(hl),c
;knr.c: 1812: 			if (loop_stack[loop_sp - 1].body_depth == 0) {
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp	nz,l500
;knr.c: 1814: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1816: 				outbufReplay();
global	_outbufReplay
call	_outbufReplay
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1818: 				if (loop_stack[loop_sp - 1].type ==       148) {
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack
add	hl,de
ld	a,(hl)
cp	.low.-108
jp	nz,l501
;knr.c: 1820: 					emitLoopGoto('T');
ld	l,.low.84
push	hl
call	_emitLoopGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1821: 					emitLoopLabel('B');
ld	l,.low.66
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1822: 				} else {
jp	l502
l501:
;knr.c: 1824: 					emitLoopLabel('C');
ld	l,.low.67
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1825: 					if (num_loop_incr > 0) {
ld	de,(_num_loop_incr)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l503
;knr.c: 1827: 						lastEmitLine = -1;
ld	hl,-1
ld	(_lastEmitLine),hl
;knr.c: 1828: 						emitLoopIncr();
call	_emitLoopIncr
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1829: 						realEmitToken(   1);
ld	l,.low.1
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1830: 					}
;knr.c: 1831: 					clearLoopIncr();
l503:
call	_clearLoopIncr
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1832: 					emitLoopGoto('T');
ld	l,.low.84
push	hl
call	_emitLoopGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1833: 					emitLoopLabel('B');
ld	l,.low.66
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1834: 				}
l502:
;knr.c: 1835: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1836: 				brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 1838: 				state = loop_stack[loop_sp - 1].savedState;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+1
add	hl,de
ld	a,(hl)
ld	(_state),a
;knr.c: 1839: 				popLoop();
call	_popLoop
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1840: 				return;
jp	l314
;knr.c: 1841: 			}
;knr.c: 1842: 		}
l500:
;knr.c: 1845: 		if (type ==       148) {
l499:
l498:
ld	a,(ix+6)
cp	.low.-108
jp	nz,l504
;knr.c: 1846: 			pushLoop(      148);
ld	l,.low.-108
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1847: 			num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 1848: 			loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 1849: 			state =  12	;
ld	a,.low.12
ld	(_state),a
;knr.c: 1850: 			return;
jp	l314
;knr.c: 1851: 		}
;knr.c: 1852: 		if (type ==         156) {
l504:
ld	a,(ix+6)
cp	.low.-100
jp	nz,l505
;knr.c: 1853: 			pushLoop(        156);
ld	l,.low.-100
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1854: 			num_loop_init = 0;
ld	hl,0
ld	(_num_loop_init),hl
;knr.c: 1855: 			num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 1856: 			num_loop_incr = 0;
ld	hl,0
ld	(_num_loop_incr),hl
;knr.c: 1857: 			loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 1858: 			for_part = 0;
ld	hl,0
ld	(_for_part),hl
;knr.c: 1859: 			state =  12	;
ld	a,.low.12
ld	(_state),a
;knr.c: 1860: 			return;
jp	l314
;knr.c: 1861: 		}
;knr.c: 1862: 		if (type ==          154) {
l505:
ld	a,(ix+6)
cp	.low.-102
jp	nz,l506
;knr.c: 1863: 			pushLoop(         154);
ld	l,.low.-102
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1864: 			lastEmitLine = lineno;
ld	hl,(_lineno)
ld	(_lastEmitLine),hl
;knr.c: 1865: 			realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1866: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1867: 			emitLoopLabel('T');
ld	l,.low.84
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1868: 			outbufPush();
call	_outbufPush
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1869: 			loop_stack[loop_sp - 1].body_depth = 0;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1870: 			state =    14	;
ld	a,.low.14
ld	(_state),a
;knr.c: 1871: 			return;
jp	l314
;knr.c: 1872: 		}
;knr.c: 1878: 		if (type ==      150) {
l506:
ld	a,(ix+6)
cp	.low.-106
jp	nz,l507
;knr.c: 1879: 			switchParen = 0;
ld	hl,0
ld	(_switchParen),hl
;knr.c: 1881: 		} else if (switchParen >= 0) {
jp	l508
l507:
ld	hl,(_switchParen)
bit	7,h
jp	nz,l509
;knr.c: 1883: 			if (type ==    6) {
ld	a,(ix+6)
cp	.low.6
jp	nz,l510
;knr.c: 1884: 				switchParen++;
ld	hl,(_switchParen)
inc	hl
ld	(_switchParen),hl
;knr.c: 1885: 			} else if (type ==    7) {
jp	l511
l510:
ld	a,(ix+6)
cp	.low.7
jp	nz,l512
;knr.c: 1886: 				switchParen--;
ld	hl,(_switchParen)
dec	hl
ld	(_switchParen),hl
;knr.c: 1888: 			} else if (type ==   2 && switchParen == 0) {
jp	l513
l512:
ld	a,(ix+6)
cp	.low.2
jp	nz,20f
jp	21f
21:
ld	hl,(_switchParen)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l514
11:
;knr.c: 1890: 				if (switchStkTop < 8)
ld	de,8
ld	hl,(_switchStkTop)
global	wrelop
call	wrelop
jp	age,l515
;knr.c: 1891: 					switchBraceStk[switchStkTop++] = loop_stack[loop_sp - 1].body_depth;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	de,_switchBraceStk
ld	hl,(_switchStkTop)
inc	hl
ld	(_switchStkTop),hl
dec	hl
add	hl,hl
add	hl,de
ld	(hl),c
inc	hl
ld	(hl),b
;knr.c: 1892: 				switchParen = -1;
l515:
ld	hl,-1
ld	(_switchParen),hl
;knr.c: 1893: 			}
;knr.c: 1894: 		}
l514:
l513:
l511:
;knr.c: 1896: 		if (type ==     3 && switchStkTop > 0) {
l509:
l508:
ld	a,(ix+6)
cp	.low.3
jp	nz,20f
jp	21f
21:
ld	de,(_switchStkTop)
ld	hl,0
global	wrelop
call	wrelop
jp	age,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l516
11:
;knr.c: 1898: 			if (loop_stack[loop_sp - 1].body_depth + 1 == switchBraceStk[switchStkTop - 1])
ld	de,_switchBraceStk
ld	hl,(_switchStkTop)
dec	hl
add	hl,hl
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
push	de
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
inc	hl
pop	de
or	a
sbc	hl,de
jp	nz,l517
;knr.c: 1899: 				switchStkTop--;
ld	hl,(_switchStkTop)
dec	hl
ld	(_switchStkTop),hl
;knr.c: 1900: 		}
l517:
;knr.c: 1903: 		if (type ==       152) {
l516:
ld	a,(ix+6)
cp	.low.-104
jp	nz,l518
;knr.c: 1905: 			if (switchStkTop == 0) {
ld	hl,(_switchStkTop)
ld	a,l
or	h
jp	nz,l519
;knr.c: 1906: 				int idx = findInnerLoop();
call	_findInnerLoop
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-8),l
ld	(ix+1+-8),h
;knr.c: 1907: 				if (idx >= 0) {
bit	7,(ix+1+-8)
jp	nz,l520
;knr.c: 1908: 					emitBreakGoto(idx);
ld	l,(ix+-8)
ld	h,(ix+1+-8)
push	hl
call	_emitBreakGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1909: 					return;
jp	l314
;knr.c: 1910: 				}
;knr.c: 1911: 			}
l520:
;knr.c: 1913: 		}
l519:
;knr.c: 1914: 		if (type ==    153) {
l518:
ld	a,(ix+6)
cp	.low.-103
jp	nz,l521
;knr.c: 1915: 			int idx = findInnerLoop();
call	_findInnerLoop
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-10),l
ld	(ix+1+-10),h
;knr.c: 1916: 			if (idx >= 0) {
bit	7,(ix+1+-10)
jp	nz,l522
;knr.c: 1917: 				emitContGoto(idx);
ld	l,(ix+-10)
ld	h,(ix+1+-10)
push	hl
call	_emitContGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1918: 				return;
jp	l314
;knr.c: 1919: 			}
;knr.c: 1920: 		}
l522:
;knr.c: 1923: 		if (type ==     20)
l521:
ld	a,(ix+6)
cp	.low.20
jp	nz,l523
;knr.c: 1924: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1925: 		else if (type ==  21)
jp	l524
l523:
ld	a,(ix+6)
cp	.low.21
jp	nz,l525
;knr.c: 1926: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1927: 		else if (type == 23)
jp	l526
l525:
ld	a,(ix+6)
cp	.low.23
jp	nz,l527
;knr.c: 1928: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 1929: 		else if (type ==  22)
jp	l528
l527:
ld	a,(ix+6)
cp	.low.22
jp	nz,l529
;knr.c: 1930: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 1931: 		else
jp	l530
l529:
;knr.c: 1932: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l530:
l528:
l526:
l524:
;knr.c: 1933: 		break;
jp	l315
;knr.c: 1935: 	case    14	:
l531:
;knr.c: 1946: 		if (type ==   2) {
ld	a,(ix+6)
cp	.low.2
jp	nz,l532
;knr.c: 1947: 			loop_stack[loop_sp - 1].body_depth++;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
inc	bc
ld	(hl),b
dec	hl
ld	(hl),c
;knr.c: 1948: 		} else if (type ==     3) {
jp	l533
l532:
ld	a,(ix+6)
cp	.low.3
jp	nz,l534
;knr.c: 1949: 			loop_stack[loop_sp - 1].body_depth--;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
dec	bc
ld	(hl),b
dec	hl
ld	(hl),c
;knr.c: 1950: 			if (loop_stack[loop_sp - 1].body_depth == 0) {
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	a,(hl)
inc	hl
or	(hl)
jp	nz,l535
;knr.c: 1952: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1954: 				outbufReplay();
call	_outbufReplay
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1956: 				emitLoopLabel('C');
ld	l,.low.67
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1958: 				state =    15	;
ld	a,.low.15
ld	(_state),a
;knr.c: 1959: 				loopParen = -1;
ld	hl,-1
ld	(_loopParen),hl
;knr.c: 1960: 				return;
jp	l314
;knr.c: 1961: 			}
;knr.c: 1962: 		}
l535:
;knr.c: 1965: 		if (type ==       148) {
l534:
l533:
ld	a,(ix+6)
cp	.low.-108
jp	nz,l536
;knr.c: 1966: 			pushLoop(      148);
ld	l,.low.-108
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1967: 			num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 1968: 			loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 1969: 			state =  12	;
ld	a,.low.12
ld	(_state),a
;knr.c: 1970: 			return;
jp	l314
;knr.c: 1971: 		}
;knr.c: 1972: 		if (type ==         156) {
l536:
ld	a,(ix+6)
cp	.low.-100
jp	nz,l537
;knr.c: 1973: 			pushLoop(        156);
ld	l,.low.-100
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1974: 			num_loop_init = 0;
ld	hl,0
ld	(_num_loop_init),hl
;knr.c: 1975: 			num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 1976: 			num_loop_incr = 0;
ld	hl,0
ld	(_num_loop_incr),hl
;knr.c: 1977: 			loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 1978: 			for_part = 0;
ld	hl,0
ld	(_for_part),hl
;knr.c: 1979: 			state =  12	;
ld	a,.low.12
ld	(_state),a
;knr.c: 1980: 			return;
jp	l314
;knr.c: 1981: 		}
;knr.c: 1983: 		if (type ==          154) {
l537:
ld	a,(ix+6)
cp	.low.-102
jp	nz,l538
;knr.c: 1984: 			pushLoop(         154);
ld	l,.low.-102
push	hl
call	_pushLoop
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1985: 			lastEmitLine = lineno;
ld	hl,(_lineno)
ld	(_lastEmitLine),hl
;knr.c: 1986: 			realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1987: 			brace_depth++;
ld	hl,(_brace_depth)
inc	hl
ld	(_brace_depth),hl
;knr.c: 1988: 			emitLoopLabel('T');
ld	l,.low.84
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 1989: 			outbufPush();
call	_outbufPush
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 1990: 			loop_stack[loop_sp - 1].body_depth = 0;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	de,0
ld	(hl),e
inc	hl
ld	(hl),d
;knr.c: 1991: 			state =    14	;
ld	a,.low.14
ld	(_state),a
;knr.c: 1992: 			return;
jp	l314
;knr.c: 1993: 		}
;knr.c: 1998: 		if (type ==      150) {
l538:
ld	a,(ix+6)
cp	.low.-106
jp	nz,l539
;knr.c: 1999: 			switchParen = 0;
ld	hl,0
ld	(_switchParen),hl
;knr.c: 2000: 		} else if (switchParen >= 0) {
jp	l540
l539:
ld	hl,(_switchParen)
bit	7,h
jp	nz,l541
;knr.c: 2001: 			if (type ==    6) {
ld	a,(ix+6)
cp	.low.6
jp	nz,l542
;knr.c: 2002: 				switchParen++;
ld	hl,(_switchParen)
inc	hl
ld	(_switchParen),hl
;knr.c: 2003: 			} else if (type ==    7) {
jp	l543
l542:
ld	a,(ix+6)
cp	.low.7
jp	nz,l544
;knr.c: 2004: 				switchParen--;
ld	hl,(_switchParen)
dec	hl
ld	(_switchParen),hl
;knr.c: 2005: 			} else if (type ==   2 && switchParen == 0) {
jp	l545
l544:
ld	a,(ix+6)
cp	.low.2
jp	nz,20f
jp	21f
21:
ld	hl,(_switchParen)
ld	a,l
or	h
jp	nz,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l546
11:
;knr.c: 2006: 				if (switchStkTop < 8)
ld	de,8
ld	hl,(_switchStkTop)
global	wrelop
call	wrelop
jp	age,l547
;knr.c: 2007: 					switchBraceStk[switchStkTop++] = loop_stack[loop_sp - 1].body_depth;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	c,(hl)
inc	hl
ld	b,(hl)
ld	de,_switchBraceStk
ld	hl,(_switchStkTop)
inc	hl
ld	(_switchStkTop),hl
dec	hl
add	hl,hl
add	hl,de
ld	(hl),c
inc	hl
ld	(hl),b
;knr.c: 2008: 				switchParen = -1;
l547:
ld	hl,-1
ld	(_switchParen),hl
;knr.c: 2009: 			}
;knr.c: 2010: 		}
l546:
l545:
l543:
;knr.c: 2011: 		if (type ==     3 && switchStkTop > 0) {
l541:
l540:
ld	a,(ix+6)
cp	.low.3
jp	nz,20f
jp	21f
21:
ld	de,(_switchStkTop)
ld	hl,0
global	wrelop
call	wrelop
jp	age,20f
jp	21f
21:jp	11f
20:jp	10f
10:
jp	l548
11:
;knr.c: 2012: 			if (loop_stack[loop_sp - 1].body_depth + 1 == switchBraceStk[switchStkTop - 1])
ld	de,_switchBraceStk
ld	hl,(_switchStkTop)
dec	hl
add	hl,hl
add	hl,de
ld	e,(hl)
inc	hl
ld	d,(hl)
push	de
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+4
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
inc	hl
pop	de
or	a
sbc	hl,de
jp	nz,l549
;knr.c: 2013: 				switchStkTop--;
ld	hl,(_switchStkTop)
dec	hl
ld	(_switchStkTop),hl
;knr.c: 2014: 		}
l549:
;knr.c: 2017: 		if (type ==       152) {
l548:
ld	a,(ix+6)
cp	.low.-104
jp	nz,l550
;knr.c: 2018: 			if (switchStkTop == 0) {
ld	hl,(_switchStkTop)
ld	a,l
or	h
jp	nz,l551
;knr.c: 2019: 				int idx = findInnerLoop();
call	_findInnerLoop
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-12),l
ld	(ix+1+-12),h
;knr.c: 2020: 				if (idx >= 0) {
bit	7,(ix+1+-12)
jp	nz,l552
;knr.c: 2021: 					emitBreakGoto(idx);
ld	l,(ix+-12)
ld	h,(ix+1+-12)
push	hl
call	_emitBreakGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2022: 					return;
jp	l314
;knr.c: 2023: 				}
;knr.c: 2024: 			}
l552:
;knr.c: 2025: 		}
l551:
;knr.c: 2026: 		if (type ==    153) {
l550:
ld	a,(ix+6)
cp	.low.-103
jp	nz,l553
;knr.c: 2027: 			int idx = findInnerLoop();
call	_findInnerLoop
exx
ld	hl,0
add	hl,sp
ld	sp,hl
exx
ld	l,l
ld	h,h
ld	(ix+-14),l
ld	(ix+1+-14),h
;knr.c: 2028: 			if (idx >= 0) {
bit	7,(ix+1+-14)
jp	nz,l554
;knr.c: 2029: 				emitContGoto(idx);
ld	l,(ix+-14)
ld	h,(ix+1+-14)
push	hl
call	_emitContGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2030: 				return;
jp	l314
;knr.c: 2031: 			}
;knr.c: 2032: 		}
l554:
;knr.c: 2035: 		if (type ==     20)
l553:
ld	a,(ix+6)
cp	.low.20
jp	nz,l555
;knr.c: 2036: 			realEmitSym(str);
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitSym
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2037: 		else if (type ==  21)
jp	l556
l555:
ld	a,(ix+6)
cp	.low.21
jp	nz,l557
;knr.c: 2038: 			realEmitNumber(num);
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
call	_realEmitNumber
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 2039: 		else if (type == 23)
jp	l558
l557:
ld	a,(ix+6)
cp	.low.23
jp	nz,l559
;knr.c: 2040: 			realEmitFNum(fnum);
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
call	_realEmitFNum
ld	hl,4
add	hl,sp
ld	sp,hl
;knr.c: 2041: 		else if (type ==  22)
jp	l560
l559:
ld	a,(ix+6)
cp	.low.22
jp	nz,l561
;knr.c: 2042: 			realEmitString(str, slen);
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
call	_realEmitString
ld	hl,2+2
add	hl,sp
ld	sp,hl
;knr.c: 2043: 		else
jp	l562
l561:
;knr.c: 2044: 			realEmitToken(type);
ld	l,(ix+6)
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
l562:
l560:
l558:
l556:
;knr.c: 2045: 		break;
jp	l315
;knr.c: 2047: 	case    15	:
l563:
;knr.c: 2057: 		if (loopParen == -1) {
ld	de,-1
ld	hl,(_loopParen)
or	a
sbc	hl,de
jp	nz,l564
;knr.c: 2059: 			if (type ==       148) {
ld	a,(ix+6)
cp	.low.-108
jp	nz,l565
;knr.c: 2060: 				loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 2061: 				num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 2062: 			}
;knr.c: 2064: 			return;
l565:
jp	l314
;knr.c: 2065: 		}
;knr.c: 2067: 		if (type ==    6) {
l564:
ld	a,(ix+6)
cp	.low.6
jp	nz,l566
;knr.c: 2068: 			loopParen++;
ld	hl,(_loopParen)
inc	hl
ld	(_loopParen),hl
;knr.c: 2069: 			if (loopParen == 1)
ld	de,1
ld	hl,(_loopParen)
or	a
sbc	hl,de
jp	nz,l567
;knr.c: 2070: 				return;
jp	l314
;knr.c: 2071: 		} else if (type ==    7) {
l567:
jp	l568
l566:
ld	a,(ix+6)
cp	.low.7
jp	nz,l569
;knr.c: 2072: 			loopParen--;
ld	hl,(_loopParen)
dec	hl
ld	(_loopParen),hl
;knr.c: 2073: 			if (loopParen == 0) {
ld	hl,(_loopParen)
ld	a,l
or	h
jp	nz,l570
;knr.c: 2076: 				if (num_loop_cond > 0) {
ld	de,(_num_loop_cond)
ld	hl,0
global	wrelop
call	wrelop
jp	age,l571
;knr.c: 2078: 					lastEmitLine = -1;
ld	hl,-1
ld	(_lastEmitLine),hl
;knr.c: 2079: 					realEmitKw(         147);
ld	l,.low.-109
push	hl
call	_realEmitKw
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2080: 					realEmitToken(   6);
ld	l,.low.6
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2081: 					emitLoopCond();
call	_emitLoopCond
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 2082: 					realEmitToken(   7);
ld	l,.low.7
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2083: 					realEmitToken(  2);
ld	l,.low.2
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2084: 					emitLoopGoto('T');
ld	l,.low.84
push	hl
call	_emitLoopGoto
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2085: 					realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2086: 				}
;knr.c: 2087: 				clearLoopCond();
l571:
call	_clearLoopCond
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 2088: 				emitLoopLabel('B');
ld	l,.low.66
push	hl
call	_emitLoopLabel
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2089: 				realEmitToken(    3);
ld	l,.low.3
push	hl
call	_realEmitToken
ld	hl,2
add	hl,sp
ld	sp,hl
;knr.c: 2090: 				brace_depth--;
ld	hl,(_brace_depth)
dec	hl
ld	(_brace_depth),hl
;knr.c: 2092: 				loopParen = loop_stack[loop_sp - 1].savedState;
ld	de,12
ld	hl,(_loop_sp)
dec	hl
global	amul
call	amul
ld	de,_loop_stack+1
add	hl,de
ld	l,(hl)
ld	h,0
ld	(_loopParen),hl
;knr.c: 2093: 				popLoop();
call	_popLoop
ld	hl,0
add	hl,sp
ld	sp,hl
;knr.c: 2095: 				return;
jp	l314
;knr.c: 2096: 			}
;knr.c: 2097: 		} else if (type ==    1) {
l570:
jp	l572
l569:
ld	a,(ix+6)
cp	.low.1
jp	nz,l573
;knr.c: 2100: 			state = loopParen;
ld	a,(_loopParen)
ld	(_state),a
;knr.c: 2101: 			return;
jp	l314
;knr.c: 2102: 		}
;knr.c: 2105: 		bufLoopCond(type, num, fnum, str, slen);
l573:
l572:
l568:
ld	l,(ix+18)
ld	h,(ix+1+18)
push	hl
ld	l,(ix+16)
ld	h,(ix+1+16)
push	hl
ld	e,(ix+12)
ld	d,(ix+1+12)
ld	l,(ix+2+12)
ld	h,(ix+3+12)
push	hl
push	de
ld	e,(ix+8)
ld	d,(ix+1+8)
ld	l,(ix+2+8)
ld	h,(ix+3+8)
push	hl
push	de
ld	l,(ix+6)
push	hl
call	_bufLoopCond
ld	hl,2+4+4+2+2
add	hl,sp
ld	sp,hl
;knr.c: 2106: 		break;
jp	l315
;knr.c: 2107: 	}
jp	l315
l316:
ld	a,(_state)
ld	l,a
ld	h,0
ld	a,0
cp	h
jp	c,l315
jp	nz,1f
ld	a,15
cp	l
jp	c,l315
1:add	hl,hl
ld	de,S241
add	hl,de
ld	a,(hl)
inc	hl
ld	h,(hl)
ld	l,a
jp	(hl)
psect	data
S241:
defw	l317
defw	l340
defw	l343
defw	l359
defw	l377
defw	l378
defw	l396
defw	l409
defw	l425
defw	l450
defw	l463
defw	l475
defw	l477
defw	l496
defw	l531
defw	l563
psect	text
l315:
;knr.c: 2108: }
l314:
jp	cret
f238	equ	-14
;knr.c: 2113: void
;knr.c: 2114: knrInit(void)
;knr.c: 2115: {
global	_knrInit
_knrInit:
call	ncsv
defw	f242
;knr.c: 2116: 	state =    0		;
ld	a,.low.0
ld	(_state),a
;knr.c: 2117: 	brace_depth = 0;
ld	hl,0
ld	(_brace_depth),hl
;knr.c: 2118: 	num_tokens = 0;
ld	hl,0
ld	(_num_tokens),hl
;knr.c: 2119: 	num_params = 0;
ld	hl,0
ld	(_num_params),hl
;knr.c: 2120: 	typedefs = ((void *)0);
ld	hl,0
ld	(_typedefs),hl
;knr.c: 2121: 	typedef_depth = 0;
ld	hl,0
ld	(_typedef_depth),hl
;knr.c: 2122: 	typedef_name[0] = 0;
ld	a,.low.0
ld	(_typedef_name),a
;knr.c: 2123: 	ctrlParenDep = 0;
ld	hl,0
ld	(_ctrlParenDep),hl
;knr.c: 2124: 	ctrlBodyDep = 0;
ld	hl,0
ld	(_ctrlBodyDep),hl
;knr.c: 2125: 	ctrl_type = 0;
ld	a,.low.0
ld	(_ctrl_type),a
;knr.c: 2126: 	saved_state = 0;
ld	a,.low.0
ld	(_saved_state),a
;knr.c: 2127: 	ctrl_sp = 0;
ld	hl,0
ld	(_ctrl_sp),hl
;knr.c: 2129: 	numLocInits = 0;
ld	hl,0
ld	(_numLocInits),hl
;knr.c: 2130: 	numLocDecl = 0;
ld	hl,0
ld	(_numLocDecl),hl
;knr.c: 2131: 	locDeclParen = 0;
ld	hl,0
ld	(_locDeclParen),hl
;knr.c: 2132: 	locInInit = 0;
ld	hl,0
ld	(_locInInit),hl
;knr.c: 2133: 	locCurName[0] = 0;
ld	a,.low.0
ld	(_locCurName),a
;knr.c: 2135: 	loop_sp = 0;
ld	hl,0
ld	(_loop_sp),hl
;knr.c: 2136: 	next_loop_num = 1;
ld	hl,1
ld	(_next_loop_num),hl
;knr.c: 2137: 	num_loop_cond = 0;
ld	hl,0
ld	(_num_loop_cond),hl
;knr.c: 2138: 	num_loop_init = 0;
ld	hl,0
ld	(_num_loop_init),hl
;knr.c: 2139: 	num_loop_incr = 0;
ld	hl,0
ld	(_num_loop_incr),hl
;knr.c: 2140: 	loopParen = 0;
ld	hl,0
ld	(_loopParen),hl
;knr.c: 2141: 	for_part = 0;
ld	hl,0
ld	(_for_part),hl
;knr.c: 2142: 	lastEmitLine = 0;
ld	hl,0
ld	(_lastEmitLine),hl
;knr.c: 2144: 	switchStkTop = 0;
ld	hl,0
ld	(_switchStkTop),hl
;knr.c: 2145: 	switchParen = -1;
ld	hl,-1
ld	(_switchParen),hl
;knr.c: 2146: }
l574:
jp	cret
f242	equ	0
psect	data
19:
defb	0
29:
defb	95,95,37,99,37,100,37,99,0
39:
defb	95,95,37,99,37,100,37,99,0
49:
defb	95,95,37,99,37,100,37,99,0
59:
defb	95,95,37,99,37,100,37,99,0
psect	bss
_loop_stack:
	defs	96
_switchBraceStk:
	defs	16
_locCurName:
	defs	16
_ctrl_stack:
	defs	24
_typedef_name:
	defs	16
_num_decl_toks:
	defs	2
