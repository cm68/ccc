/*
 * lexeme.h - Token definitions for lexeme stream
 *
 * Shared between cpp (emitter) and pass1 (reader).
 * Binary .x format uses these token values directly.
 */
#ifndef LEXEME_H
#define LEXEME_H

/* Delimiters (0-9) */
#define E_O_F   0
#define SEMI    1
#define BEGIN   2
#define END     3
#define LBRACK  4
#define RBRACK  5
#define LPAR    6
#define RPAR    7
#define COLON   8
#define COMMA   9

/* Terminals (20-25) */
#define SYM     20
#define NUMBER  21
#define STRING  22
#define FNUMBER 23
#define LNUMBER 25

/* Unary/Binary operators (30-54) */
#define INCR    30
#define DECR    31
#define BANG    34
#define AMPER   35
#define STAR    36
#define TWIDDLE 38
#define DOT     39
#define PLUS    40
#define MINUS   41
#define TIMES   42
#define DIV     43
#define MOD     44
#define RSHIFT  45
#define LSHIFT  46
#define AND     47
#define OR      48
#define XOR     49
#define ARROW   50
#define LAND    53
#define LOR     54

/* Relational (60-65) */
#define EQ      60
#define NEQ     61
#define LE      62
#define LT      63
#define GE      64
#define GT      65

/* Assignment operators (70-80) */
#define PLUSEQ  70
#define SUBEQ   71
#define MULTEQ  72
#define DIVEQ   73
#define MODEQ   74
#define RSHIFTEQ 75
#define LSHIFTEQ 76
#define ANDEQ   77
#define OREQ    78
#define XOREQ   79
#define ASSIGN  80

/* Special (90-92) */
#define QUES    90
#define SIZEOF  91
#define ELLIPSIS 92

/* Line tracking (112-118) */
#define LABEL   112
#define LINENO  116
#define NEWLINE 117
#define ASMSTR  118

/* Keyword tokens (128-160) */
#define INT         128
#define CHAR        129
#define FLOAT       130
#define DOUBLE      131
#define STRUCT      132
#define SIGNED      133
#define LONG        134
#define UNSIGNED    135
#define UNION       136
#define SHORT       137
#define VOID        138
#define ENUM        139

#define	SCLASS_MIN	TYPEDEF
#define TYPEDEF     140
#define AUTO        141
#define EXTERN      142
#define STATIC      143
#define REGISTER    144
#define	SCLASS_MAX	REGISTER

#define GOTO        145
#define RETURN      146
#define IF          147
#define WHILE       148
#define ELSE        149
#define SWITCH      150
#define CASE        151
#define BREAK       152
#define CONTINUE    153
#define DO          154
#define DEFAULT     155
#define FOR         156
#define ASM         157
#define CONST       158
#define VOLATILE    159
#define SIZEOF_KW   160

/* Keyword range for is-keyword check */
#define KW_FIRST    128
#define KW_LAST     160

/* Internal tokens - not from lexer, used in AST (200+) */
#define TOK_NONE    200
#define DEREF       201
#define EXPR        202
#define NEG         203
#define NOT         204
#define CALL        205
#define NARROW      206
#define WIDEN       207
#define SEXT        208
#define COPY        209
#define INITLIST    210
#define PREINC      211
#define POSTINC     212
#define PREDEC      213
#define POSTDEC     214
#define BFEXTRACT   215
#define BFASSIGN    216
#define REGVAR      217   /* pass2: register variable */
#define LOCALVAR    218   /* pass2: local variable (IY/IX indexed) */
#define ARGNODE     219   /* pass2: function call argument wrapper */
#define TERNBRANCH  220   /* pass2: ternary then/else container */
#define URSHIFT     221   /* unsigned right shift >>> */

/* AST format uses lexeme token values directly (SYM, DEREF, BANG, etc.) */
/* Special markers for AST that have no lexeme equivalent */
#define AST_CONST   '#'   /* numeric constant marker */

#endif /* LEXEME_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
