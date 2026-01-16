/*
 * filttest.c - Test harness for filter pipeline
 *
 * Reads tokens, runs through filtbrace -> filtctrl, prints results.
 * Usage: ./filttest < input.toks
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cpp.h"
#include "lexeme.h"

/* Globals needed by filters */
int lineno = 1;
char *filename = "test";

/* Token input queue (simulates lexer) */
#define INPUT_MAX 1024
static struct token input_toks[INPUT_MAX];
static int input_len = 0;
static int input_pos = 0;

/* Get token from input queue - pointer-out style */
static void
get_input(struct token *out)
{
	if (input_pos < input_len) {
		tokcpy(out, &input_toks[input_pos++]);
		return;
	}
	/* EOF */
	out->type = 0;
	out->v.numeric = 0;
	out->lineno = lineno;
	out->filename = filename;
}

/* External filter functions - pointer-out style */
extern void filtknr_init(void (*up)(struct token *));
extern void filtknr(struct token *out);
extern void filtdecl_init(void (*up)(struct token *));
extern void filtdecl(struct token *out);
extern void filtbrace_init(void (*up)(struct token *));
extern void filtbrace(struct token *out);
extern void filtctrl_init(void (*up)(struct token *));
extern void filtctrl(struct token *out);

/* Token name table */
static char *
tokname(int t)
{
	switch (t) {
	case 0: return "EOF";
	case IF: return "IF";
	case ELSE: return "ELSE";
	case WHILE: return "WHILE";
	case FOR: return "FOR";
	case DO: return "DO";
	case SWITCH: return "SWITCH";
	case CASE: return "CASE";
	case DEFAULT: return "DEFAULT";
	case BREAK: return "BREAK";
	case CONTINUE: return "CONTINUE";
	case GOTO: return "GOTO";
	case RETURN: return "RETURN";
	case BEGIN: return "{";
	case END: return "}";
	case LPAR: return "(";
	case RPAR: return ")";
	case SEMI: return ";";
	case COLON: return ":";
	case LABEL: return "LABEL";
	case SYM: return "SYM";
	case NUMBER: return "NUM";
	case NOT: return "!";
	case LT: return "<";
	case GT: return ">";
	case EQ: return "==";
	case ASSIGN: return "=";
	case PREINC: return "++";
	case COMMA: return ",";
	case STAR: return "*";
	case INT: return "int";
	case CHAR: return "char";
	case VOID: return "void";
	case SHORT: return "short";
	case LONG: return "long";
	case UNSIGNED: return "unsigned";
	case STATIC: return "static";
	case TYPEDEF: return "typedef";
	default: return "?";
	}
}

static void
print_token(struct token *t)
{
	if (t->type == SYM || t->type == LABEL)
		printf("%s:%s ", tokname(t->type), t->v.name ? t->v.name : "?");
	else if (t->type == NUMBER)
		printf("%ld ", t->v.numeric);
	else
		printf("%s ", tokname(t->type));
}

/* Parse simple token description into output token */
static void
parse_tok(char *s, struct token *out)
{
	out->type = 0;
	out->v.numeric = 0;
	out->lineno = lineno;
	out->filename = filename;

	if (strcmp(s, "if") == 0) out->type = IF;
	else if (strcmp(s, "else") == 0) out->type = ELSE;
	else if (strcmp(s, "while") == 0) out->type = WHILE;
	else if (strcmp(s, "for") == 0) out->type = FOR;
	else if (strcmp(s, "do") == 0) out->type = DO;
	else if (strcmp(s, "switch") == 0) out->type = SWITCH;
	else if (strcmp(s, "case") == 0) out->type = CASE;
	else if (strcmp(s, "default") == 0) out->type = DEFAULT;
	else if (strcmp(s, "break") == 0) out->type = BREAK;
	else if (strcmp(s, "continue") == 0) out->type = CONTINUE;
	else if (strcmp(s, "goto") == 0) out->type = GOTO;
	else if (strcmp(s, "return") == 0) out->type = RETURN;
	else if (strcmp(s, "{") == 0) out->type = BEGIN;
	else if (strcmp(s, "}") == 0) out->type = END;
	else if (strcmp(s, "(") == 0) out->type = LPAR;
	else if (strcmp(s, ")") == 0) out->type = RPAR;
	else if (strcmp(s, ";") == 0) out->type = SEMI;
	else if (strcmp(s, ":") == 0) out->type = COLON;
	else if (strcmp(s, "<") == 0) out->type = LT;
	else if (strcmp(s, ">") == 0) out->type = GT;
	else if (strcmp(s, "==") == 0) out->type = EQ;
	else if (strcmp(s, "=") == 0) out->type = ASSIGN;
	else if (strcmp(s, "++") == 0) out->type = PREINC;
	else if (strcmp(s, "!") == 0) out->type = NOT;
	else if (strcmp(s, ",") == 0) out->type = COMMA;
	else if (strcmp(s, "*") == 0) out->type = STAR;
	else if (strcmp(s, "int") == 0) out->type = INT;
	else if (strcmp(s, "char") == 0) out->type = CHAR;
	else if (strcmp(s, "void") == 0) out->type = VOID;
	else if (strcmp(s, "short") == 0) out->type = SHORT;
	else if (strcmp(s, "long") == 0) out->type = LONG;
	else if (strcmp(s, "unsigned") == 0) out->type = UNSIGNED;
	else if (strcmp(s, "static") == 0) out->type = STATIC;
	else if (strcmp(s, "typedef") == 0) out->type = TYPEDEF;
	else if (s[0] >= '0' && s[0] <= '9') {
		out->type = NUMBER;
		out->v.numeric = atol(s);
	} else {
		out->type = SYM;
		out->v.name = strdup(s);
	}
}

int
main(int argc, char **argv)
{
	char line[256];
	struct token t;
	int mode;
	int i;
	char *p;

	mode = 0;	/* 0=full, 1=brace, 2=decl, 3=knr */

	if (argc > 1) {
		if (strcmp(argv[1], "-b") == 0) mode = 1;
		else if (strcmp(argv[1], "-d") == 0) mode = 2;
		else if (strcmp(argv[1], "-k") == 0) mode = 3;
	}

	/* Read tokens from stdin */
	while (fgets(line, sizeof(line), stdin)) {
		p = strtok(line, " \t\n");
		while (p && input_len < INPUT_MAX) {
			parse_tok(p, &input_toks[input_len++]);
			p = strtok(NULL, " \t\n");
		}
	}

	printf("Input: ");
	for (i = 0; i < input_len; i++)
		print_token(&input_toks[i]);
	printf("\n");

	/* Initialize filter chain */
	if (mode == 3) {
		/* K&R filter only */
		filtknr_init(get_input);
		printf("After filtknr: ");
		filtknr(&t);
		while (t.type != 0) {
			print_token(&t);
			filtknr(&t);
		}
	} else if (mode == 2) {
		/* Decl filter only */
		filtdecl_init(get_input);
		printf("After filtdecl: ");
		filtdecl(&t);
		while (t.type != 0) {
			print_token(&t);
			filtdecl(&t);
		}
	} else if (mode == 1) {
		/* Brace filter only */
		filtbrace_init(get_input);
		printf("After filtbrace: ");
		filtbrace(&t);
		while (t.type != 0) {
			print_token(&t);
			filtbrace(&t);
		}
	} else {
		/* Full pipeline: knr -> decl -> brace -> ctrl */
		filtknr_init(get_input);
		filtdecl_init(filtknr);
		filtbrace_init(filtdecl);
		filtctrl_init(filtbrace);
		printf("After filtctrl: ");
		filtctrl(&t);
		while (t.type != 0) {
			print_token(&t);
			filtctrl(&t);
		}
	}
	printf("\n");

	return 0;
}
