/*
 * wsobj.c - Whitesmith's object file format common functions
 *
 * Shared between assembler (asz) and linker (wsld)
 */

#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#define INIT
#else
#define INIT = 0
#endif

#include "wsobj.h"

/*
 * segment name strings for debugging
 */
char *wsSegNames[] = { "undef", "text", "data", "bss", "abs", "ext" };

/*
 * write a byte to file descriptor
 */
void
wsWrByte(fd, b)
int fd;
unsigned char b;
{
    write(fd, &b, 1);
}

/*
 * write a 16-bit little-endian word
 */
void
wsWrWord(fd, val)
int fd;
unsigned short val;
{
    wsWrByte(fd, val & 0xff);
    wsWrByte(fd, val >> 8);
}

/*
 * encode a relocation bump value
 * bump = distance from last relocation to this one
 */
void
wsEncBump(fd, bump)
int fd;
int bump;
{
    while (bump >= REL_BUMP_LIM) {
        wsWrByte(fd, REL_BUMP_EXT + REL_BUMP_MAX);
        wsWrByte(fd, 0xff);
        bump -= REL_BUMP_LIM;
    }
    if (bump >= REL_BUMP_EXT) {
        bump -= REL_BUMP_EXT;
        wsWrByte(fd, (bump >> 8) + REL_BUMP_EXT);
        wsWrByte(fd, bump & 0xff);
    } else if (bump) {
        wsWrByte(fd, bump);
    }
}

/*
 * encode a relocation control byte
 * seg = segment type (SEG_TEXT, SEG_DATA, SEG_BSS, SEG_ABS)
 *       or -1 for symbol reference
 * symidx = symbol index (only used if seg == -1)
 */
void
wsEncReloc(fd, seg, symidx)
int fd;
int seg;
int symidx;
{
    int control;

    switch (seg) {
    case SEG_ABS:
        wsWrByte(fd, REL_ABS);
        break;
    case SEG_TEXT:
        wsWrByte(fd, REL_TEXT);
        break;
    case SEG_DATA:
        wsWrByte(fd, REL_DATA);
        break;
    case SEG_BSS:
        wsWrByte(fd, REL_BSS);
        break;
    default:
        /* symbol reference - encode index */
        control = symidx + REL_SYM_OFS;
        if (control < REL_EXT_THR1) {
            wsWrByte(fd, (control + REL_SYM_SHIFT) << 2);
        } else if (control < REL_EXT_THR2) {
            wsWrByte(fd, REL_SYM_EXT);
            wsWrByte(fd, control - REL_EXT_THR1);
        } else {
            control -= REL_EXT_THR2;
            wsWrByte(fd, REL_SYM_EXT);
            wsWrByte(fd, (control >> 8) + REL_EXT_LONG);
            wsWrByte(fd, control);
        }
        break;
    }
}

/*
 * terminate a relocation table
 */
void
wsEndReloc(fd)
int fd;
{
    wsWrByte(fd, 0);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
