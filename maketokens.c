/*
 * dirty little program generate token names
 * and normalized values
 */ 
#include <stdio.h>
#include "token.h"

char *tokenname[128];

#define check(a) tokenname[a] = #a;
	
int
main(int argc, char **argv)
{
	int i;
	char *s;

#include "enumlist.h"

	printf("char *tokenname[] = {\n");
	for (i = 0; i < 128; i++) {
		printf("/* %d 0x%x \'%c\' */ ", i, i, (i > ' ') && (i < 0x7f) ? i : ' ');
		if (tokenname[i]) {
			printf("\"%s\"", tokenname[i]);
		} else {
			printf("0");
		}
		if (i < 128) printf(",\n");
	}
	printf("};\n");
	printf("char *detoken[] = {\n");
	for (i = 0; i < 128; i++) {
		s = 0;
		switch(i) {
		case E_O_F: s = ""; break;	
		case SYM: s = "symbol"; break;	
		case NUMBER: s = "number"; break;
		case STRING: s = "string"; break;
		
		/* Delimiters and punctuation */
		case BEGIN: s = "{"; break;
		case END: s = "}"; break;
		case SEMI: s = ";"; break;
		case COMMA: s = ","; break;
		case DOT: s = "."; break;
		case LPAR: s = "("; break;
		case RPAR: s = ")"; break;
		case LBRACK: s = "["; break;
		case RBRACK: s = "]"; break;
		case COLON: s = ":"; break;
		case QUES: s = "?"; break;
		
		/* Basic arithmetic operators */
		case PLUS: s = "+"; break;
		case MINUS: s = "-"; break;
		case STAR: s = "*"; break;
		case DIV: s = "/"; break;
		case MOD: s = "%"; break;
		
		/* Bitwise operators */
		case AND: s = "&"; break;
		case OR: s = "|"; break;
		case XOR: s = "^"; break;
		case TWIDDLE: s = "~"; break;
		case LSHIFT: s = "<<"; break;
		case RSHIFT: s = ">>"; break;
		
		/* Logical operators */
		case BANG: s = "!"; break;
		case LAND: s = "&&"; break;
		case LOR: s = "||"; break;
		
		/* Comparison operators */
		case LT: s = "<"; break;
		case GT: s = ">"; break;
		case LE: s = "<="; break;
		case GE: s = ">="; break;
		case EQ: s = "=="; break;
		case NEQ: s = "!="; break;
		
		/* Assignment operators */
		case ASSIGN: s = "="; break;
		case PLUSEQ: s = "+="; break;
		case SUBEQ: s = "-="; break;
		case MULTEQ: s = "*="; break;
		case DIVEQ: s = "/="; break;
		case MODEQ: s = "%="; break;
		case ANDEQ: s = "&="; break;
		case OREQ: s = "|="; break;
		case XOREQ: s = "^="; break;
		case LSHIFTEQ: s = "<<="; break;
		case RSHIFTEQ: s = ">>="; break;
		case LANDEQ: s = "&&="; break;
		case LOREQ: s = "||="; break;
		
		/* Increment/decrement */
		case INCR: s = "++"; break;
		case DECR: s = "--"; break;
		
		/* Member access */
		case DEREF: s = "->"; break;
		
		/* Keywords */
		case INT: s = "int"; break;
		case CHAR: s = "char"; break;
		case SHORT: s = "short"; break;
		case LONG: s = "long"; break;
		case FLOAT: s = "float"; break;
		case DOUBLE: s = "double"; break;
		case VOID: s = "void"; break;
		case UNSIGNED: s = "unsigned"; break;
		case STRUCT: s = "struct"; break;
		case UNION: s = "union"; break;
		case ENUM: s = "enum"; break;
		case SIZEOF: s = "sizeof"; break;
		case TYPEDEF: s = "typedef"; break;
		case CONST: s = "const"; break;
		case VOLATILE: s = "volatile"; break;
		case STATIC: s = "static"; break;
		case EXTERN: s = "extern"; break;
		case REGISTER: s = "register"; break;
		case AUTO: s = "auto"; break;
		case BOOLEAN: s = "_Bool"; break;
		
		/* Control flow keywords */
		case IF: s = "if"; break;
		case ELSE: s = "else"; break;
		case FOR: s = "for"; break;
		case WHILE: s = "while"; break;
		case DO: s = "do"; break;
		case SWITCH: s = "switch"; break;
		case CASE: s = "case"; break;
		case DEFAULT: s = "default"; break;
		case BREAK: s = "break"; break;
		case CONTINUE: s = "continue"; break;
		case RETURN: s = "return"; break;
		case GOTO: s = "goto"; break;
		
		/* Other keywords */
		case ASM: s = "asm"; break;
		
		default:
			break;
		}
		if (s) {
			printf("\"%s \"", s);
		} else {
			printf("0");
		}
		if (i < 128) printf(",\n");
	}
	printf("};\n");

	return 0;
};
