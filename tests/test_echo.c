#include <unistd.h>

void
swr(char *s) {
	while (*s) {
		write(1, s, 1);
		s++;
	}
}

int
main(int argc, char **argv)
{
	argv++;
	while (--argc) {
		swr(*argv++);
		swr(" ");
	}
	swr("\n");
	exit (0);
}

