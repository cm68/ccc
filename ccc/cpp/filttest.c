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
	int brace_only = 0;

	if (argc > 1 && strcmp(argv[1], "-b") == 0)
		brace_only = 1;

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
	filtbrace_init(get_input);
	if (brace_only) {
		printf("After filtbrace: ");
		while ((t = filtbrace()).type != 0)
			print_token(t);
	} else {
		filtctrl_init(filtbrace);
		printf("After filtctrl: ");
		while ((t = filtctrl()).type != 0)
			print_token(t);
	}
	printf("\n");

	return 0;
}
