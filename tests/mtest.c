#include <unistd.h>
char message[] = "hello, world\n";
char *msg1 = "yes, another\n";
int
main()
{
	write(1, message, sizeof(message) - 1);
	write(1, msg1, 13 );
	msg1 = message;
	write(1, msg1, 13 );
	return 0;
}
