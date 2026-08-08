/*
 * cpm3 - a CP/M 3 machine, just big enough to run this compiler
 *
 * The memory map is a banked CP/M 3 with almost all of it given over
 * to the TPA, which is the point: c1 needs the room, and a simulator
 * is free to be as generous as the hardware never was.
 *
 *	0000	warm boot vector    - jp WBOOT
 *	0005	bdos entry          - jp BDOS
 *	0006	address of BDOS, which is what a program reads to find
 *		the top of the TPA and where it puts its stack
 *	005c	fcb 1, parsed from the first argument
 *	006c	fcb 2, parsed from the second
 *	0080	command tail, and the default DMA buffer
 *	0100	the program
 *	 ...	the TPA: heap up from the end of bss, stack down from
 *		BDOS
 *	fe00	the two trap bytes the emulator watches for
 */

#ifndef CPM3_H
#define CPM3_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MEMSIZE		0x10000

/*
 * Where the BDOS would be.  Nothing of it is really there - the two
 * addresses below hold one byte each and the emulator notices when
 * the processor fetches from them - but a program reads 0006 to size
 * the TPA, so this is the number that decides how much room it has.
 */
#define WBOOT_TRAP	(bdosbase)
#define BDOS_TRAP	(bdosbase + 3)

#define TPATOP		BDOS_TRAP

/*
 * Where the system sits, and so how much room a program has.  A real
 * machine's is fixed by how much of the BDOS is resident; -t moves
 * it here, which is how to ask what a program would need rather than
 * only whether it fits.
 */
extern uint16_t	bdosbase;

#define FCB1		0x005c
#define FCB2		0x006c
#define DMABUF		0x0080
#define TPA		0x0100

#define RECLEN		128	/* a CP/M record */

/*
 * The fcb as CP/M lays it out.  The compiler's runtime keeps its own
 * copy of this in libcpm; this is the other side of the same 36 bytes.
 */
#define FCB_DR		0	/* drive, 0 = default */
#define FCB_NAME	1	/* 8 characters, space padded */
#define FCB_TYPE	9	/* 3 characters, space padded */
#define FCB_EX		12	/* extent, 16k each */
#define FCB_S1		13
#define FCB_S2		14	/* extent, high bits */
#define FCB_RC		15	/* records in this extent */
#define FCB_D0		16	/* 16 bytes the system owns */
#define FCB_CR		32	/* record within the extent */
#define FCB_R0		33	/* random record, 3 bytes */
#define FCB_R1		34
#define FCB_R2		35
#define FCB_LEN		36

/* bdos function numbers, the ones this machine answers */
#define B_TERM		0
#define B_CONIN		1
#define B_CONOUT	2
#define B_RAWIO		6
#define B_PRINTS	9
#define B_CONST		11
#define B_VERSION	12
#define B_RESET		13
#define B_SELDSK	14
#define B_OPEN		15
#define B_CLOSE		16
#define B_SFIRST	17
#define B_SNEXT		18
#define B_DELETE	19
#define B_READSEQ	20
#define B_WRITESEQ	21
#define B_MAKE		22
#define B_RENAME	23
#define B_LOGVEC	24
#define B_GETDSK	25
#define B_SETDMA	26
#define B_USER		32
#define B_READRAN	33
#define B_WRITERAN	34
#define B_FILESIZE	35
#define B_SETRAN	36
#define B_MULTISEC	44
#define B_ERRMODE	45
#define B_GETDT		105

/* the version 12 reports: CP/M 3.1, system type plain CP/M */
#define CPM3VERSION	0x0031

extern uint8_t	mem[MEMSIZE];
extern int	verbose;
extern int	errmode;	/* what 45 last set */
extern uint16_t	dma;
extern int	usernum;

/* machine */
extern void	fatal(char *fmt, ...);
extern void	trace(char *fmt, ...);

/* bdos */
extern void	bdos(void);
extern void	diskinit(char *dir);
extern void	diskmap(char *spec);
extern void	diskdone(void);

#endif

/*
 * vim: tabstop=8 shiftwidth=8 noexpandtab:
 */
