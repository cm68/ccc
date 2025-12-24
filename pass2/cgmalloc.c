#include "cgen.h"

/*
 * File - malloc.c Created 09.03.2019 Last Modified 30.05.2020
 */

#ifdef CPM
static uint8_t *freeList; /* wB024 */

#define Len(p)     (uint16_t)((p)[-1] & MALLOC_LARGEFLAG ? *(uint16_t *)((p)-3) : (p)[-1])
/* get the real start of the reserved block */
#define BlkAddr(p) ((p)[-1] & MALLOC_LARGEFLAG ? ((p)-3) : (p)-1)
#define Link(p)    (*(uint8_t **)(p))
/*
 * the memory allocator reserves multiple blocks of memory
 * each block is stored as
 * [block size][data]
 * next points to the next allocated memory block
 * block size is encoded as 0-127 for data lengths 1-128
 * other wise (length - 3) as a word and a flag byte of 0x80
 *
 * When freed the data is replaced by a pointer to the next largest
 * free block.
 * The above implies that a minimum data length is sizeof(uint8_t *)
 *
 */
/* encode the data block size. Return the pointer to the data
 * note the passed in size should account for the block size header
 * and the minimum data length
 */
static uint8_t *setSize(register uint8_t *blkPtr, short size) {
    if (size > MALLOC_LARGEFLAG) {
        *((uint16_t *)blkPtr) = size - 3;
        blkPtr += 3;
        blkPtr[-1] = MALLOC_LARGEFLAG;
    } else {
        *blkPtr = (uint8_t)size - 1;
        blkPtr++;
    }
    return blkPtr;
}

static bool pack() {
    uint8_t *loblk;
    uint8_t *hiblkAddr;
    uint8_t **curLo;
    uint8_t **curHi;
    bool packed;
    register uint8_t *hiblk;

    packed = false;
rescan:
    for (curHi = &freeList; hiblk = *curHi; curHi = &Link(hiblk)) {
        hiblkAddr = BlkAddr(hiblk); /* optimised over original */
        for (curLo = &freeList; loblk = *curLo; curLo = &Link(loblk)) {
            if (hiblkAddr != loblk + Len(loblk)) /* loop until we find adjacent blocks */
                continue;
            /* unlink the blocks to be joined */
            if (&Link(hiblk) == curLo)      /* free list order is hi lo next */
                *curHi = Link(loblk);       /* move the next info to the hi block */
            else if (curHi == &Link(loblk)) /* free list order is low hi next */
                *curLo = Link(hiblk);       /* move the next info to the lo block */
            else {                          /* not adjacent in freelist */
                *curLo = Link(loblk);       /* move the lo -> next info */
                *curHi = Link(hiblk);       /* move the hi -> next info */
            }
            loblk = BlkAddr(loblk); /* make sure we also include the header */
            free(setSize(loblk, hiblk + Len(hiblk) - loblk)); /* join the blocks */
            packed = true;
            goto rescan;
        }
    }
    /* Bug? the original code unintentionally returned int(curHi) + 1 */
    return packed;
}

/*********************************************************
 * malloc v2 OK			Used in: allocMem
 *********************************************************/
void *malloc(size_t size) {
    uint8_t *nextBlk;
    size_t blkLen;
    bool done;
    register uint8_t *curBlk;

    if (size < sizeof(uint8_t *))
        size = sizeof(uint8_t *);
    done = false;
    /*
     * see if we can find a suitable previously allocated block
     * if not consolidate and try again
     * failing that allocate a new block
     */
    do {
        for (curBlk = (uint8_t *)&freeList; nextBlk = Link(curBlk); curBlk = nextBlk) {
            if (Len(nextBlk) >= size) {
                Link(curBlk) = Link(nextBlk);
                curBlk       = nextBlk;
                /* if splitting the block would leave sufficient space
                 * for a new block i.e. header + data/pointer then split it */
                if ((blkLen = Len(curBlk)) > size + 2 + sizeof(uint8_t *)) { /* ? +1 ok */
                    nextBlk = curBlk + size;
                    free(setSize(nextBlk, blkLen - size));
                    curBlk = BlkAddr(curBlk);
                    return setSize(curBlk, size);
                } else
                    return curBlk;
            }
        }
    } while (!done && (done = pack()));
    blkLen = size < 128 ? 1 : 3; /* size of block header */
    if ((curBlk = sbrk(size + blkLen)) == (uint8_t *)-1)
        return 0;

    return setSize(curBlk, size + blkLen);
}

/*********************************************************
 * free v6 OK  Used in: leaveBlock, freeNode, relNodeFrList
 *                       subToAdd, cmalloc
 * free the given block inserting on free chain in size order
 *
 *********************************************************/
void free(void *p) {
    register uint8_t *curBlk;

    for (curBlk = (uint8_t *)&freeList; Link(curBlk) && Len(Link(curBlk)) < Len((uint8_t *)p); curBlk = Link(curBlk))
        ;
    Link(p)      = Link(curBlk);
    Link(curBlk) = (uint8_t *)p;
}
#endif
/* end of file malloc.c */
