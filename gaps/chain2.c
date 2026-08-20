struct buf {
	struct buf *next, *prev;
	long block;
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
	register struct buf *bp;

	bufs = (struct buf *)calloc(want, sizeof(struct buf));
	nbufs = want;
	for (bp = &bufs[0];  bp < &bufs[nbufs];  bp++)
	{
		bp->next = bp + 1;
		bp->prev = bp - 1;
		bp->block = (long)(-1);
	}
	bufs[0].prev = bufs[nbufs-1].next = END_OF_CHAIN;
	buf_head = &bufs[0];
	buf_tail = &bufs[nbufs-1];
}

main()
{
	init(5);
	printf("[4].next=%x [0].prev=%x anchor=%x\n",
		(unsigned)bufs[4].next, (unsigned)bufs[0].prev,
		(unsigned)&buf_anchor);
	printf("[4+1 spill]=%x\n", (unsigned)bufs[5].next);
	return 0;
}
