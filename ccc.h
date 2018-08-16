/*
 * global defs for curt's c compiler
 */

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
enum token {
	EOF = 0,
	/* C keywords */
	ASM = 'a', AUTO = 'A', 
	BOOLEAN = 'b' , BREAK = 'B', 
	CASE = 'C', CHAR = 'c', CONST = '9', CONTINUE = 'g',
	DEFAULT = 'o', DO = 'D', DOUBLE = 'd', 
	ELSE = 'E', ENUM = 'e', EXTERN = 'x', 
	FLOAT = 'f', FOR = 'F', 
	GOTO = 'J',
	IF = 'I', INT = 'i', 
	LONG = 'l', 
	REGISTER = 'r', RETURN = 'R', 
	SIZEOF = 'z', SHORT = 's', STATIC = 'L', STRUCT = 'X', 
	TYPEDEF = 't', 
	UNION = 'U', UNSIGNED = 'u', 
	VOID = 'v', VOLATILE = 'V', 
	WHILE = 'W',

	/* syntactic cogs */
	OPEN = '{', CLOSE = '}',
 	LBRACK = '[', RBRACK = ']',
	LPAR = '(', RPAR = ')', 
	SEMI = ';', COMMA = ',',

	/* terminals */
	SYM = '7', NUMBER = '8', STRING = '9',

	/* operators */
	ASSIGN = '=', DOT = '.', DEREF = 'M',
	PLUS = '+', MINUS = '-', STAR = '*', DIV = '/', MOD = '%',
	AND = '&', OR = '|', XOR = '^',
	LT = '<', GT = '>', NOT = '!', COMP = '~',
	QUES = '?', OTHER = ':',
	INC = 'p', DEC = 'm',
	LSHIFT = 'K' , RSHIFT = 'k',
	LOR = 'n', LAND = 'N',
	EQ = 'Q', NEQ = 'q', LE = 'x', GE = 'X',
	PLUSEQ, SUBEQ, MULTEQ, DIVEQ, MODEQ, 
	ANDEQ, OREQ, XOREQ,
	LANDEQ, LOREQ, 
	RSHIFTEQ, LSHIFTEQ,
	/* CPP */	
	INCLUDE = '0',
	DEFINE = '1', UNDEF = '2', 
	IFDEF = '3', ENDIF = '4', ELIF = '5'
};

/* kw.c */
extern char *cppkw;
extern char *ckw;
extern char *asmkw;
extern char kwlook(char *str, char *table);

/* lex.c */
extern enum token curtok;
extern enum token nexttok;
extern long curval;
extern long nextval;
extern char *curstr;
extern char *nextstr;

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

/* main.c */
extern void err(char errcode);
extern void fatal(char errcode);
extern void recover(char errcode, char skipto);
extern void need(char check, char skipto, char errcode);
extern void err(char errcode);
int main(int argc, char **argv);
void process(char *f);
void usage(char *complaint, char *p);

/* error numbers for formatting */
enum errors {
	ER_C_NX,	// number format error
	ER_C_BC, 	// bad character
	ER_C_CD,	// bad literal 
	ER_C_UT,	// unknown character
	ER_C_CU,	// endif without if
	ER_C_ME,	// too many elses
	ER_C_MN,	// no macro name
	ER_C_ID,	// bad include name format
	ER_C_IT,	// unterminated include name
	ER_C_DP,	// bad macro param list
	ER_C_CE,	// cpp const required
	ER_C_BD,	// unknown cpp operation
	ER_C_END	// last error
};

