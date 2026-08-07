#include <stdio.h>
#include <unistd.h>

main()
{
	char *a;
	char *b;
	int n;

	a = uname();
	printf("1: %s\n", a);
	b = uname();
	printf("2: %s\n", b);
	printf("same buffer: %d\n", a == b);
	n = 0;
	while (*a)
		++a, ++n;
	printf("len %d, pid %d\n", n, getpid());
	return 0;
}
