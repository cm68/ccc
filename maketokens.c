/*
 * dirty little program generate token names
 * and normalized values
 */ 
#include "token.h"

char *tokenname[128];

#define check(a) tokenname[a] = strdup(#a);
	
main(int argc, char **argv)
{
	int i;
	char *s;

#include "enumlist.h"

	printf("char *tokenname[] = {\n");
	for (i = 0; i < 128; i++) {
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
		case BEGIN: s = "{"; break;
		case END: s = "}"; break;
		case SEMI: s = ";"; break;
		case STRUCT: s = "struct"; break;
		case INT: s = "int"; break;
		case CHAR: s = "char"; break;
		case SHORT: s = "short"; break;
		case ASSIGN: s = "="; break;
		case COMMA: s = ","; break;
		case DOT: s = "."; break;
		case STAR: s = "*"; break;
		case LPAR: s = "("; break;
		case RPAR: s = ")"; break;
		case LBRACK: s = "["; break;
		case RBRACK: s = "]"; break;
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

	exit(0);
};
