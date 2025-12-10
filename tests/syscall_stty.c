/*
 * syscall_stty test
 */

int gtty();
int stty();

char ttybuf[6];

int main(void)
{
	/* get current settings */
	if (gtty(0, ttybuf) < 0)
		return 1;
	/* set them back unchanged */
	if (stty(0, ttybuf) < 0)
		return 2;
	return 0;
}
