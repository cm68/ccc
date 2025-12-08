char *message = "hello, world";

int
main()
{
	write(1, message, sizeof(message));
}
