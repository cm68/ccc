/*
 * syscall_time test
 */

int time();

long timeval;

int main(void)
{
	if (time(&timeval) < 0)
		return 1;
	/* time should be non-zero */
	if (timeval == 0)
		return 2;
	return 0;
}
