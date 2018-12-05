/*
 * dirty little program to validate that we don't reuse token values
 */ 
#include "ccc.h"

char ar[128];
int coll;

#define check(a) \
	if (ar[a]) { coll++; printf("duplicate value %s %d %c\n", #a, a, a); } ar[a] = 1
	
int
main(int argc, char **argv)
{
	int i;

#include "enumlist.h"

	for (i = 0x20; i < sizeof(ar); i++) {
		if (ar[i] == 0) {
			printf("unused: %d %c\n", i, i);
		}
	}
	printf("%d collisions\n", coll);
	if (coll) exit(1);
	exit(0);
};
