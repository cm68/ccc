#include <unistd.h>
char message[] = "hello, world\n";
char *msg1 = "yes, another\n";

extern int errno;

int
main()
{
	unsigned char i;

	for (i = 0; i < 33; i++) {
		errno = i;
		perror("message");
	}

	write(1, message, sizeof(message) - 1);
	write(1, msg1, 13 );
	msg1 = message;
	write(1, msg1, 13 );

	return 0;
}
