/*
 * these are keywords that are recognized by the lexer.
 * they all have values that are printable for debug
 * purposes and for cheap serializing into intermediate files
 */
typedef unsigned char token_t;

enum {
    E_O_F = 0,
    NONE = ' ',

    /* C keywords */
    ASM = 'A', AUTO = 'o',
    BREAK = 'B', 
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
    SYM = '5', NUMBER = '9', FNUMBER = 'b', STRING = '\"',

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
    PLUSEQ = 'P', SUBEQ = 0xdf, MULTEQ = 'T', DIVEQ = '2', MODEQ = 0xfe,
    ANDEQ = 0xc6, OREQ = '1', XOREQ = 'X',
    LANDEQ = 'J', LOREQ = 'H',
    RSHIFTEQ = '6', LSHIFTEQ = '0',
    EXPR = '`', NEG = '\\', NOT = '\'', CALL = '@',
    NARROW = '_', WIDEN = 0xb6, SEXT = 0xab, COPY = 0xbb, INITLIST = 0xb7,
    PREINC = 0xcf, POSTINC = 0xef, PREDEC = 0xd6, POSTDEC = 0xf6,
    BFEXTRACT = 0xa7, BFASSIGN = 0xdd,

    /* AST operator aliases - reuse keyword values for .ast serialization */
    AST_SEXT = 'x',         /* EXTERN - sign extend */
    AST_PREINC = '(',       /* LPAR - pre-increment */
    AST_POSTINC = ')',      /* RPAR - post-increment */
    AST_PREDEC = '{',       /* BEGIN - pre-decrement */
    AST_POSTDEC = '}',      /* END - post-decrement */
    AST_SUBEQ = 'o',        /* AUTO - subtract-equals */
    AST_ANDEQ = 'a',        /* STRUCT - and-equals */
    AST_MODEQ = 'm',        /* UNION - mod-equals */
    AST_BFEXTRACT = 'e',    /* ENUM - bitfield extract */
    AST_BFASSIGN = 'f',     /* FLOAT - bitfield assign */

    /* CPP */
    INCLUDE = '#',
    DEFINE = '$', UNDEF = 'K',
    IFDEF = 'Y', IFNDEF = '7', ENDIF = 'Z', ELIF = '8'
};
