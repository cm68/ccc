/*
 * Tests struct declarations with bitfields and arrays
 */

int i;
char *k;

struct foo {
	int jj;
	char *s;
	int bfr:3;
	unsigned short k[34];
};

struct foo bar;

