char nm[6];

int
main()
{
	unsigned short h;
	char *p;

	nm[0] = 'a'; nm[1] = 'l'; nm[2] = 'p'; nm[3] = 'h'; nm[4] = 'a';

	h = 0;
	for (p = nm; *p; p++)
		h = h * 31 + (unsigned char)*p;
	if (h != 45406)
		return 1;
	if (h % 127 != 67)
		return 2;
	if ((unsigned short)((((('a' * 31 + 'l') * 31 + 'p') * 31 + 'h') * 31 + 'a')) != 45406)
		return 3;
	if ((unsigned short)((((('a' * 31 + 'l') * 31 + 'p') * 31 + 'h') * 31 + 'a')) % 127 != 67)
		return 4;
	return 0;
}
