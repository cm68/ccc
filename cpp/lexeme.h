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
#define TYPEDEF     137
#define VOID        138
#define SHORT       139
#define AUTO        140
#define EXTERN      141
#define STATIC      142
#define REGISTER    143
#define GOTO        144
#define RETURN      145
#define IF          146
#define WHILE       147
#define ELSE        148
#define SWITCH      149
#define CASE        150
#define BREAK       151
#define CONTINUE    152
#define DO          153
#define DEFAULT     154
#define FOR         155
#define ENUM        156
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

/* AST markers that don't conflict with tokens (tokens use 30+) */
#define AST_SYM     4     /* symbol reference marker */

/* AST inc/dec use printable chars to distinguish pre/post */
#define AST_PREINC  '('
#define AST_POSTINC ')'
#define AST_PREDEC  '{'
#define AST_POSTDEC '}'

#endif /* LEXEME_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
