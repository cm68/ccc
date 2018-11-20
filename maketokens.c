/*
 * dirty little program generate token names
 */ 
#include "ccc.h"

char *tokenname[128];

#define check(a) tokenname[a] = strdup(#a);
	
main()
{
	int i;

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
	exit(0);
};
