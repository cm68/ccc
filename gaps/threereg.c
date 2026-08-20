char tab[256];
char line[20];
main()
{
	register int i;
	register char *p1, *p2, *p3;

	for (i = 1; i < 256; i++)
		tab[i] = i;
	strcpy(line, "abc");
	p1 = line;
	p2 = tab;
	p3 = tab;
	while (*p1 = p2[*p1])
		p1++;
	printf("%s\n", line);
	return 0;
}
