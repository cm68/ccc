/*
 * global defs for curt's c compiler
 */
#include "type.h"

#define	DEBUG

#ifdef DEBUG
extern int verbose;

#define V_LEX   (1 << 0)
#define	V_IO	(1 << 1)
#endif

/*
 * these are keywords that are recognized by the lexer.
 * they all have values that are printable for debug 
 * purposes and for cheap serializing into intermediate files
 */
typedef enum token {
	E_O_F = 0,
	/* C keywords */
	ASM = 'A', AUTO = 'o', 
	BOOLEAN = 'b' , BREAK = 'B', 
	CASE = 'C', CHAR = 'c', CONST = 'k', CONTINUE = 'N',
	DEFAULT = 'O', DO = 'D', DOUBLE = 'd', 
	ELSE = 'E', ENUM = 'e', EXTERN = 'x', 
	FLOAT = 'f', FOR = 'F', 
	GOTO = 'G',
	IF = 'I', INT = 'i', 
	LONG = 'l', 
	REGISTER = 'r', RETURN = 'R', 
	SIZEOF = 'z', SHORT = 's', STATIC = 'p', STRUCT = 'a', SWITCH = 'S',
	TYPEDEF = 't',
	UNION = 'm', UNSIGNED = 'u', 
	VOID = 'v', VOLATILE = '4', 
	WHILE = 'W',

	/* syntactic cogs */
	BEGIN = '{', END = '}',
 	LBRACK = '[', RBRACK = ']',
	LPAR = '(', RPAR = ')', 
	SEMI = ';', COMMA = ',',
	LABEL = '3',

	/* terminals */
	SYM = '5', NUMBER = '9', STRING = '\"',

	/* operators */
	ASSIGN = '=', DOT = '.', DEREF = 'M',
	PLUS = '+', MINUS = '-', STAR = '*', DIV = '/', MOD = '%',
	AND = '&', OR = '|', XOR = '^',
	LT = '<', GT = '>', BANG = '!', TWIDDLE = '~',
	QUES = '?', OTHER = ':',
	INC = 'U', DEC = 'V',
	LSHIFT = 'y' , RSHIFT = 'w',
	LOR = 'h', LAND = 'j',
	EQ = 'Q', NEQ = 'n', LE = 'L', GE = 'g',
	PLUSEQ = 'P', SUBEQ = '_', MULTEQ = 'T', DIVEQ = '2', MODEQ = '7',
	ANDEQ = '@', OREQ = '1', XOREQ = 'X',
	LANDEQ = 'J', LOREQ = 'H', 
	RSHIFTEQ = '6', LSHIFTEQ = '0',

	/* CPP */
	INCLUDE = '#',
	DEFINE = '$', UNDEF = 'K', 
	IFDEF = 'Y', ENDIF = 'Z', ELIF = '8'
} token_t;

/* kw.c */
extern char cppkw[];
extern char ckw[];
extern char asmkw[];
extern char kwlook(char *str, char *table);

/* lex.c */
extern token_t curtok;
extern token_t nexttok;
extern long curval;
extern long nextval;
extern char *curstr;
extern char *nextstr;
extern char strbuf[];
extern char match(token_t t);

/* io.c */
extern void pushfile(char *name);
extern void insert_macro(char *name, char *macbuf);
extern char getnext();
extern char readchar();

extern char prevchar;
extern char curchar;
extern char nextchar;
extern int lineno;
extern char *filename;

/*
 * error numbers for formatting
 * these should match the indexes for the errmsg array
 */
typedef enum error {
	ER_C_NX,	// number format error
	ER_C_BC, 	// bad character
	ER_C_CD,	// bad literal 
	ER_C_UT,	// unknown character
	ER_C_CU,	// endif without if
	ER_C_ME,	// too many elses
	ER_C_MN,	// no macro name
	ER_C_ID,	// bad include name format
	ER_C_DP,	// bad macro param list
	ER_C_CE,	// cpp const required
	ER_C_BD,	// unknown cpp operation
	ER_C_END	// last error
} error_t;

/* main.c */
extern void err(error_t errcode);
extern void fatal(error_t errcode);
extern void recover(error_t errcode, token_t skipto);
extern void need(token_t check, token_t skipto, error_t errcode);
extern void err(error_t errcode);
int main(int argc, char **argv);
void process(char *f);
void usage(char *complaint, char *p);

/* macro.c */
struct macro {
	char parmcount;
	char *name;
	char **parms;
	char *mactext;
	struct macro *next;
};
extern struct macro *macros;
char *macbuffer;

/* util.c */
extern char lookupc(char *s, char c);

/* type.c */
extern struct scope *scope;
void push_scope(char *name);
void pop_scope();
struct type *findtype(char *name, kind_t kind);

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
