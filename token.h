/*
 * these are keywords that are recognized by the lexer.
 * they all have values that are printable for debug 
 * purposes and for cheap serializing into intermediate files
 */
typedef enum {
	/* pseudo-keywords, and expression operators */
    E_O_F = 0,
	EXPR = 0x01,
	NEG = 0x02,
	NOT = 0x03,
	CALL = 0x04,
	CAST = 0x05,
    NONE = ' ',

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
    ASSIGN = '=', DOT = '.', ARROW = 'q', DEREF = 'M',
    PLUS = '+', MINUS = '-', STAR = '*', DIV = '/', MOD = '%',
    AND = '&', OR = '|', XOR = '^',
    LT = '<', GT = '>', BANG = '!', TWIDDLE = '~',
    QUES = '?', COLON = ':',
    INCR = 'U', DECR = 'V',
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
    IFDEF = 'Y', IFNDEF = '7', ENDIF = 'Z', ELIF = '8'
} token_t;
