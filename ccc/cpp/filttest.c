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

/* Get token from input queue */
static struct token
get_input(void)
{
	if (input_pos < input_len)
		return input_toks[input_pos++];
	/* EOF */
	struct token t = {0};
	return t;
}

/* External filter functions */
extern void filtknr_init(struct token (*up)(void));
extern struct token filtknr(void);
extern void filtdecl_init(struct token (*up)(void));
extern struct token filtdecl(void);
extern void filtbrace_init(struct token (*up)(void));
extern struct token filtbrace(void);
extern void filtctrl_init(struct token (*up)(void));
extern struct token filtctrl(void);

/* Token name table */
static const char *tokname(int t)
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
print_token(struct token t)
{
	if (t.type == SYM || t.type == LABEL)
		printf("%s:%s ", tokname(t.type), t.v.name ? t.v.name : "?");
	else if (t.type == NUMBER)
		printf("%ld ", t.v.numeric);
	else
		printf("%s ", tokname(t.type));
}

/* Parse simple token description */
static struct token
parse_tok(const char *s)
{
	struct token t = {0};
	t.lineno = lineno;
	t.filename = filename;

	if (strcmp(s, "if") == 0) t.type = IF;
	else if (strcmp(s, "else") == 0) t.type = ELSE;
	else if (strcmp(s, "while") == 0) t.type = WHILE;
	else if (strcmp(s, "for") == 0) t.type = FOR;
	else if (strcmp(s, "do") == 0) t.type = DO;
	else if (strcmp(s, "switch") == 0) t.type = SWITCH;
	else if (strcmp(s, "case") == 0) t.type = CASE;
	else if (strcmp(s, "default") == 0) t.type = DEFAULT;
	else if (strcmp(s, "break") == 0) t.type = BREAK;
	else if (strcmp(s, "continue") == 0) t.type = CONTINUE;
	else if (strcmp(s, "goto") == 0) t.type = GOTO;
	else if (strcmp(s, "return") == 0) t.type = RETURN;
	else if (strcmp(s, "{") == 0) t.type = BEGIN;
	else if (strcmp(s, "}") == 0) t.type = END;
	else if (strcmp(s, "(") == 0) t.type = LPAR;
	else if (strcmp(s, ")") == 0) t.type = RPAR;
	else if (strcmp(s, ";") == 0) t.type = SEMI;
	else if (strcmp(s, ":") == 0) t.type = COLON;
	else if (strcmp(s, "<") == 0) t.type = LT;
	else if (strcmp(s, ">") == 0) t.type = GT;
	else if (strcmp(s, "==") == 0) t.type = EQ;
	else if (strcmp(s, "=") == 0) t.type = ASSIGN;
	else if (strcmp(s, "++") == 0) t.type = PREINC;
	else if (strcmp(s, "!") == 0) t.type = NOT;
	else if (strcmp(s, ",") == 0) t.type = COMMA;
	else if (strcmp(s, "*") == 0) t.type = STAR;
	else if (strcmp(s, "int") == 0) t.type = INT;
	else if (strcmp(s, "char") == 0) t.type = CHAR;
	else if (strcmp(s, "void") == 0) t.type = VOID;
	else if (strcmp(s, "short") == 0) t.type = SHORT;
	else if (strcmp(s, "long") == 0) t.type = LONG;
	else if (strcmp(s, "unsigned") == 0) t.type = UNSIGNED;
	else if (strcmp(s, "static") == 0) t.type = STATIC;
	else if (strcmp(s, "typedef") == 0) t.type = TYPEDEF;
	else if (s[0] >= '0' && s[0] <= '9') {
		t.type = NUMBER;
		t.v.numeric = atol(s);
	} else {
		t.type = SYM;
		t.v.name = strdup(s);
	}
	return t;
}

int
main(int argc, char **argv)
{
	char line[256];
	struct token t;
	int mode = 0;	/* 0=full, 1=brace, 2=decl, 3=knr */

	if (argc > 1) {
		if (strcmp(argv[1], "-b") == 0) mode = 1;
		else if (strcmp(argv[1], "-d") == 0) mode = 2;
		else if (strcmp(argv[1], "-k") == 0) mode = 3;
	}

	/* Read tokens from stdin */
	while (fgets(line, sizeof(line), stdin)) {
		char *p = strtok(line, " \t\n");
		while (p && input_len < INPUT_MAX) {
			input_toks[input_len++] = parse_tok(p);
			p = strtok(NULL, " \t\n");
		}
	}

	printf("Input: ");
	for (int i = 0; i < input_len; i++)
		print_token(input_toks[i]);
	printf("\n");

	/* Initialize filter chain */
	if (mode == 3) {
		/* K&R filter only */
		filtknr_init(get_input);
		printf("After filtknr: ");
		while ((t = filtknr()).type != 0)
			print_token(t);
	} else if (mode == 2) {
		/* Decl filter only */
		filtdecl_init(get_input);
		printf("After filtdecl: ");
		while ((t = filtdecl()).type != 0)
			print_token(t);
	} else if (mode == 1) {
		/* Brace filter only */
		filtbrace_init(get_input);
		printf("After filtbrace: ");
		while ((t = filtbrace()).type != 0)
			print_token(t);
	} else {
		/* Full pipeline: knr -> decl -> brace -> ctrl */
		filtknr_init(get_input);
		filtdecl_init(filtknr);
		filtbrace_init(filtdecl);
		filtctrl_init(filtbrace);
		printf("After filtctrl: ");
		while ((t = filtctrl()).type != 0)
			print_token(t);
	}
	printf("\n");

	return 0;
}
