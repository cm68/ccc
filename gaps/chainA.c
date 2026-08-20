struct buf {
	struct buf *next, *prev;
	unsigned block;
	char data[1024];
};
static struct buf *bufs;
int nbufs;
static struct {
	struct buf *next, *prev;
} buf_anchor;
#define	END_OF_CHAIN	((struct buf *)&buf_anchor)
#define	buf_head	buf_anchor.next
#define	buf_tail	buf_anchor.prev
char *calloc();

init(want)
int want;
{
	register char *cp;
	register struct buf *bp;
	register int i;

	bufs = (struct buf *)calloc(want, sizeof(struct buf));
	nbufs = want;

	cp = (char *)bufs;
	bp = bufs;
	for (i = 0; i < nbufs; i++)
	{
		bp = (struct buf *)cp;
		cp = cp + sizeof(struct buf);
		bp->next = (struct buf *)cp;
		bp->prev = (struct buf *)(cp - 2*sizeof(struct buf));
		bp->block = (unsigned)(-1);
	}
	bufs->prev = END_OF_CHAIN;
	bp->next = END_OF_CHAIN;
	buf_head = bufs;
	buf_tail = bp;
}

relink()
{
	register struct buf *bp;

	bp = buf_tail;
	bp->next->prev = bp->prev;
	bp->prev->next = bp->next;
	bp->next = buf_head;
	bp->prev = END_OF_CHAIN;
	buf_head->prev = bp;
	buf_head = bp;
}

search(want)
unsigned want;
{
	register struct buf *bp;

	for (bp = buf_head;  bp != END_OF_CHAIN;  bp = bp->next)
		if (bp->block == want)
			return (1);
	return (0);
}

main()
{
	struct buf *p;
	int n;

	init(5);
	relink();
	buf_head->block = 3;
	printf("found3=%d\n", search(3));
	printf("absent=%d\n", search(12345));
	n = 0;
	for (p = buf_head; p != END_OF_CHAIN && n < 20; p = p->next)
		n++;
	printf("forward chain %d\n", n);
	n = 0;
	for (p = buf_tail; p != END_OF_CHAIN && n < 20; p = p->prev)
		n++;
	printf("backward chain %d\n", n);
	return 0;
}
