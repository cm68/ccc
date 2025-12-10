/*
 * syscall_alarm test
 */

int alarm();

int main(void)
{
	int prev;

	/* set alarm for 10 seconds */
	prev = alarm(10);
	/* cancel it */
	alarm(0);
	return 0;
}
