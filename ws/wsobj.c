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
#define void int
#define INIT = 0
#endif

#include "wsobj.h"

/*
 * segment name strings for debugging
 */
char *ws_segnames[] = { "undef", "text", "data", "bss", "abs", "ext" };

/*
 * write a byte to file descriptor
 */
void
ws_write_byte(fd, b)
int fd;
unsigned char b;
{
    write(fd, &b, 1);
}

/*
 * write a 16-bit little-endian word
 */
void
ws_write_word(fd, val)
int fd;
unsigned short val;
{
    ws_write_byte(fd, val & 0xff);
    ws_write_byte(fd, val >> 8);
}

/*
 * encode a relocation bump value
 * bump = distance from last relocation to this one
 */
void
ws_encode_bump(fd, bump)
int fd;
int bump;
{
    while (bump >= REL_BUMP_LIMIT) {
        ws_write_byte(fd, REL_BUMP_EXT + REL_BUMP_MAX);
        ws_write_byte(fd, 0xff);
        bump -= REL_BUMP_LIMIT;
    }
    if (bump >= REL_BUMP_EXT) {
        bump -= REL_BUMP_EXT;
        ws_write_byte(fd, (bump >> 8) + REL_BUMP_EXT);
        ws_write_byte(fd, bump & 0xff);
    } else if (bump) {
        ws_write_byte(fd, bump);
    }
}

/*
 * encode a relocation control byte
 * seg = segment type (SEG_TEXT, SEG_DATA, SEG_BSS, SEG_ABS)
 *       or -1 for symbol reference
 * symidx = symbol index (only used if seg == -1)
 */
void
ws_encode_reloc_type(fd, seg, symidx)
int fd;
int seg;
int symidx;
{
    int control;

    switch (seg) {
    case SEG_ABS:
        ws_write_byte(fd, REL_ABS);
        break;
    case SEG_TEXT:
        ws_write_byte(fd, REL_TEXT);
        break;
    case SEG_DATA:
        ws_write_byte(fd, REL_DATA);
        break;
    case SEG_BSS:
        ws_write_byte(fd, REL_BSS);
        break;
    default:
        /* symbol reference - encode index */
        control = symidx + REL_SYM_OFFSET;
        if (control < REL_EXT_THRESH1) {
            ws_write_byte(fd, (control + REL_SYM_SHIFT) << 2);
        } else if (control < REL_EXT_THRESH2) {
            ws_write_byte(fd, REL_SYM_EXT);
            ws_write_byte(fd, control - REL_EXT_THRESH1);
        } else {
            control -= REL_EXT_THRESH2;
            ws_write_byte(fd, REL_SYM_EXT);
            ws_write_byte(fd, (control >> 8) + REL_EXT_LONG);
            ws_write_byte(fd, control);
        }
        break;
    }
}

/*
 * terminate a relocation table
 */
void
ws_end_relocs(fd)
int fd;
{
    ws_write_byte(fd, 0);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
