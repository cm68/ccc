char buffer[9000];
f()
{
	register char *q;

	q = buffer;
	if (q >= &buffer[9000])
		q = buffer;
	return 0;
}
