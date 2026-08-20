struct buf {
	struct buf *next, *prev;
	unsigned block;
	char data[1024];
};
struct buf pool[3];
main()
{
	register char *cp;
	register struct buf *bp;
	register int i;

	printf("sizeof=%d\n", sizeof(struct buf));
	cp = (char *)pool;
	bp = pool;
	for (i = 0; i < 3; i++)
	{
		bp = (struct buf *)cp;
		cp = cp + sizeof(struct buf);
		printf("i=%d bp=%u cp=%u stride=%d\n",
			i, (unsigned)bp, (unsigned)cp,
			(int)(cp - (char *)bp));
	}
	return 0;
}
