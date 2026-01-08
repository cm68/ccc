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
;kwtab.c: 4: unsigned char ckw[] = {
psect	data
global	_ckw
_ckw:
;kwtab.c: 5: 	'i'|0x80, 8, 'f'|0x80, 2, 0xff,          147,
defb	-23
defb	8
defb	-26
defb	2
defb	-1
defb	-109
;kwtab.c: 6: 	  'n', 't', 0xff,         128,
defb	110
defb	116
defb	-1
defb	-128
;kwtab.c: 7: 	'c'|0x80, 26, 'h'|0x80, 4, 'a', 'r', 0xff,        129,
defb	-29
defb	26
defb	-24
defb	4
defb	97
defb	114
defb	-1
defb	-127
;kwtab.c: 8: 	  'o'|0x80, 13, 'n', 's'|0x80, 3, 't', 0xff,       158,
defb	-17
defb	13
defb	110
defb	-13
defb	3
defb	116
defb	-1
defb	-98
;kwtab.c: 9: 	      't', 'i', 'n', 'u', 'e', 0xff,    153,
defb	116
defb	105
defb	110
defb	117
defb	101
defb	-1
defb	-103
;kwtab.c: 10: 	  'a', 's', 'e', 0xff,        151,
defb	97
defb	115
defb	101
defb	-1
defb	-105
;kwtab.c: 11: 	'r'|0x80, 16, 'e', 't'|0x80, 5, 'u', 'r', 'n', 0xff,      146,
defb	-14
defb	16
defb	101
defb	-12
defb	5
defb	117
defb	114
defb	110
defb	-1
defb	-110
;kwtab.c: 12: 	    'g', 'i', 's', 't', 'e', 'r', 0xff,    144,
defb	103
defb	105
defb	115
defb	116
defb	101
defb	114
defb	-1
defb	-112
;kwtab.c: 13: 	'f'|0x80, 11, 'o'|0x80, 3, 'r', 0xff,         156,
defb	-26
defb	11
defb	-17
defb	3
defb	114
defb	-1
defb	-100
;kwtab.c: 14: 	  'l', 'o', 'a', 't', 0xff,       130,
defb	108
defb	111
defb	97
defb	116
defb	-1
defb	-126
;kwtab.c: 15: 	'w'|0x80, 6, 'h', 'i', 'l', 'e', 0xff,       148,
defb	-9
defb	6
defb	104
defb	105
defb	108
defb	101
defb	-1
defb	-108
;kwtab.c: 16: 	'e'|0x80, 19, 'l'|0x80, 4, 's', 'e', 0xff,        149,
defb	-27
defb	19
defb	-20
defb	4
defb	115
defb	101
defb	-1
defb	-107
;kwtab.c: 17: 	  'n'|0x80, 4, 'u', 'm', 0xff,        139,
defb	-18
defb	4
defb	117
defb	109
defb	-1
defb	-117
;kwtab.c: 18: 	  'x', 't', 'e', 'r', 'n', 0xff,      142,
defb	120
defb	116
defb	101
defb	114
defb	110
defb	-1
defb	-114
;kwtab.c: 19: 	'v'|0x80, 14, 'o', 'i'|0x80, 3, 'd', 0xff,        138,
defb	-10
defb	14
defb	111
defb	-23
defb	3
defb	100
defb	-1
defb	-118
;kwtab.c: 20: 	    'l', 'a', 't', 'i', 'l', 'e', 0xff,    159,
defb	108
defb	97
defb	116
defb	105
defb	108
defb	101
defb	-1
defb	-97
;kwtab.c: 21: 	's'|0x80, 44, 't'|0x80, 13, 'a'|0x80, 5, 't', 'i', 'c', 0xff,      143,
defb	-13
defb	44
defb	-12
defb	13
defb	-31
defb	5
defb	116
defb	105
defb	99
defb	-1
defb	-113
;kwtab.c: 22: 	    'r', 'u', 'c', 't', 0xff,      132,
defb	114
defb	117
defb	99
defb	116
defb	-1
defb	-124
;kwtab.c: 23: 	  'i'|0x80, 13, 'g'|0x80, 5, 'n', 'e', 'd', 0xff,      133,
defb	-23
defb	13
defb	-25
defb	5
defb	110
defb	101
defb	100
defb	-1
defb	-123
;kwtab.c: 24: 	    'z', 'e', 'o', 'f', 0xff,   160,
defb	122
defb	101
defb	111
defb	102
defb	-1
defb	-96
;kwtab.c: 25: 	  'h'|0x80, 5, 'o', 'r', 't', 0xff,       137,
defb	-24
defb	5
defb	111
defb	114
defb	116
defb	-1
defb	-119
;kwtab.c: 26: 	  'w', 'i', 't', 'c', 'h', 0xff,      150,
defb	119
defb	105
defb	116
defb	99
defb	104
defb	-1
defb	-106
;kwtab.c: 27: 	'u'|0x80, 15, 'n', 's'|0x80, 7, 'i', 'g', 'n', 'e', 'd', 0xff,    135,
defb	-11
defb	15
defb	110
defb	-13
defb	7
defb	105
defb	103
defb	110
defb	101
defb	100
defb	-1
defb	-121
;kwtab.c: 28: 	    'i', 'o', 'n', 0xff,       136,
defb	105
defb	111
defb	110
defb	-1
defb	-120
;kwtab.c: 29: 	'l'|0x80, 5, 'o', 'n', 'g', 0xff,        134,
defb	-20
defb	5
defb	111
defb	110
defb	103
defb	-1
defb	-122
;kwtab.c: 30: 	'b'|0x80, 6, 'r', 'e', 'a', 'k', 0xff,       152,
defb	-30
defb	6
defb	114
defb	101
defb	97
defb	107
defb	-1
defb	-104
;kwtab.c: 31: 	'd'|0x80, 18, 'e'|0x80, 7, 'f', 'a', 'u', 'l', 't', 0xff,     155,
defb	-28
defb	18
defb	-27
defb	7
defb	102
defb	97
defb	117
defb	108
defb	116
defb	-1
defb	-101
;kwtab.c: 32: 	  'o', 0xfe,          154,
defb	111
defb	-2
defb	-102
;kwtab.c: 33: 	    'u', 'b', 'l', 'e', 0xff,      131,
defb	117
defb	98
defb	108
defb	101
defb	-1
defb	-125
;kwtab.c: 34: 	'g'|0x80, 5, 'o', 't', 'o', 0xff,        145,
defb	-25
defb	5
defb	111
defb	116
defb	111
defb	-1
defb	-111
;kwtab.c: 35: 	't'|0x80, 8, 'y', 'p', 'e', 'd', 'e', 'f', 0xff,     140,
defb	-12
defb	8
defb	121
defb	112
defb	101
defb	100
defb	101
defb	102
defb	-1
defb	-116
;kwtab.c: 36: 	'a', 'u'|0x80, 4, 't', 'o', 0xff,        141,
defb	97
defb	-11
defb	4
defb	116
defb	111
defb	-1
defb	-115
;kwtab.c: 37: 	  's', 'm', 0xff,         157,
defb	115
defb	109
defb	-1
defb	-99
;kwtab.c: 38: 	0
;kwtab.c: 39: };
defb	0
