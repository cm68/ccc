f()
{
	register int w;

	w = 6;
	while (w &= (~(-w)))
		;
	return w;
}
