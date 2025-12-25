/*
 * c wrapper around _break system call 
 */

unsigned short memtop;	/* no idea who initializes this */

int
brk(unsigned short addr)
{
	unsigned short ret;
    unsigned char array[4];

	if (addr < memtop && addr > array) {
		return -1;
	}
	ret = _break(addr);
	if (ret == -1) {
		return -1;
	}
	memtop = ret;
	return memtop;
}

int
sbrk(unsigned short increment)
{
	return (brk(memtop+increment));
}

/* vim: set tabstop=4 shiftwidth=4 expandtab: */
