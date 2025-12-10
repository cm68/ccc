/*
 * syscall_getuid test
 */

int getuid();

int main(void)
{
	int uid;

	uid = getuid();
	/* uid can be 0 for root, just check it doesn't fail */
	if (uid < 0)
		return 1;
	return 0;
}
