/*
 * lexdata.c - Token classification data
 */
#include "lexeme.h"

/*
 * Sized to KW_LAST+1 (161 entries).  Filters only see lexer-produced
 * tokens (0..160); synthetic AST tokens (200+) never reach token_props.
 */
unsigned char token_props[KW_LAST + 1] = {
	[INT] = TF_TYPE, [CHAR] = TF_TYPE, [FLOAT] = TF_TYPE, [DOUBLE] = TF_TYPE,
	[STRUCT] = TF_TYPE, [SIGNED] = TF_TYPE, [LONG] = TF_TYPE, [UNSIGNED] = TF_TYPE,
	[UNION] = TF_TYPE, [SHORT] = TF_TYPE, [VOID] = TF_TYPE, [ENUM] = TF_TYPE,
	[TYPEDEF] = TF_TYPE, [AUTO] = TF_TYPE, [EXTERN] = TF_TYPE, [STATIC] = TF_TYPE,
	[REGISTER] = TF_TYPE, [CONST] = TF_TYPE, [VOLATILE] = TF_TYPE,

	[IF] = TF_COND, [WHILE] = TF_COND, [FOR] = TF_COND, [SWITCH] = TF_COND,
	[DO] = TF_DO,
	[ELSE] = TF_ELSE,

	[SEMI] = TF_TERM, [END] = TF_TERM | TF_CLOSE,

	[LPAR] = TF_OPEN, [LBRACK] = TF_OPEN, [BEGIN] = TF_OPEN,
	[RPAR] = TF_CLOSE, [RBRACK] = TF_CLOSE
};
