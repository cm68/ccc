/*
 * Token values for pass1.
 * Includes lexeme.h for CPP token values.
 * Internal tokens (not from lexer) use values >= 200.
 */

/* Get shared lexeme definitions from cpp */
#include "../../cpp/lexeme.h"

typedef unsigned char token_t;

/* Alias for const - lexeme.h uses CONST, pass1 used KW_CONST */
#define KW_CONST CONST

enum {
    /* Internal tokens - not from lexer (200+) */
    NONE = 200,
    DEREF = 201,
    EXPR = 202,
    NEG = 203,
    NOT = 204,
    CALL = 205,
    NARROW = 206,
    WIDEN = 207,
    SEXT = 208,
    COPY = 209,
    INITLIST = 210,
    PREINC = 211,
    POSTINC = 212,
    PREDEC = 213,
    POSTDEC = 214,
    BFEXTRACT = 215,
    BFASSIGN = 216,
    /* Note: CONST (158) is a keyword from lexeme.h */

    /* AST operator aliases - printable chars for .ast serialization */
    AST_SEXT = 'x',
    AST_PREINC = '(',
    AST_POSTINC = ')',
    AST_PREDEC = '{',
    AST_POSTDEC = '}',
    AST_SUBEQ = 'o',
    AST_ANDEQ = 'a',
    AST_MODEQ = 'm',
    AST_BFEXTRACT = 'e',
    AST_BFASSIGN = 'f'
};
