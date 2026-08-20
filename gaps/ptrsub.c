char buffer[512];
f()
{
	register char *p;

	p = buffer;
	return &buffer[512] - p;
}
