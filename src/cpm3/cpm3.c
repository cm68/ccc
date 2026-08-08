/*
 * cpm3 - the machine
 *
 * Loads a .com at 0100, builds the page zero a program expects and
 * runs it until it asks to stop.  The Z80 itself is z80.h, which is
 * Andre Weissflog's, unmodified - see README.md.
 *
 * The BDOS is not code.  0005 jumps to BDOS_TRAP, which holds a
 * single ret; the tick callback watches for the processor fetching
 * an instruction from that address, services the call out of the
 * registers, and lets the ret carry the program back.  A real BDOS
 * would be sitting in the bank the TPA cannot see, which is exactly
 * as visible as this.
 */

#define CHIPS_IMPL
#include "z80.h"

#include "cpm3.h"
#include <stdarg.h>
#include <unistd.h>
#include <ctype.h>

uint8_t		mem[MEMSIZE];
int		verbose;
int		errmode = 0xff;	/* CP/M 3 programs set this; default is
				 * display-and-terminate, but nothing here
				 * terminates a program on our behalf */
uint16_t	dma = DMABUF;
int		usernum;

static z80_t	cpu;
static z80_desc_t desc;
static int	stopped;
static int	foldcase;	/* -u: fold the command tail, as a CCP does */

void
fatal(char *fmt, ...)
{
	va_list ap;

	fflush(stdout);
	fprintf(stderr, "cpm3: ");
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
	exit(1);
}

void
trace(char *fmt, ...)
{
	va_list ap;

	if (!verbose)
		return;
	fflush(stdout);
	fprintf(stderr, "cpm3: ");
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

/*
 * Register access for the bdos code, which needs to read its
 * arguments out of the processor and put its answer back.
 */
uint8_t
reg_c(void)
{
	return z80_c(&cpu);
}

uint8_t
reg_e(void)
{
	return z80_e(&cpu);
}

uint16_t
reg_de(void)
{
	return z80_de(&cpu);
}

/*
 * A bdos call returns in a, and by convention hl carries the same
 * value with h zero - programs read either.  bdoshl.s in libcpm takes
 * the hl form, bdos.s the a form.
 */
void
retval(uint8_t v)
{
	z80_set_a(&cpu, v);
	z80_set_hl(&cpu, v);
	z80_set_b(&cpu, 0);
}

void
retval16(uint16_t v)
{
	z80_set_a(&cpu, v & 0xff);
	z80_set_hl(&cpu, v);
}

void
halt(void)
{
	stopped = 1;
}

/*
 * The processor's only window on the machine.  Memory is flat - a
 * banked CP/M 3 would have the system in bank 0 and the TPA in bank
 * 1, and since nothing here ever looks at the system's bank there is
 * no reason to simulate the switch.  Nothing answers on the I/O
 * ports; the compiler does not use them.
 */
static uint64_t
tick(int num_ticks, uint64_t pins, void *user_data)
{
	uint16_t addr;

	(void)num_ticks;
	(void)user_data;

	if (pins & Z80_MREQ) {
		addr = Z80_GET_ADDR(pins);
		if (pins & Z80_RD) {
			Z80_SET_DATA(pins, mem[addr]);
		} else if (pins & Z80_WR) {
			mem[addr] = Z80_GET_DATA(pins);
		}
	} else if (pins & Z80_IORQ) {
		if (pins & Z80_RD)
			Z80_SET_DATA(pins, 0);
	}
	return pins;
}

/*
 * Page zero, and the two bytes the traps live behind.
 */
static void
setup(void)
{
	memset(mem, 0, sizeof(mem));

	mem[0] = 0xc3;			/* jp WBOOT_TRAP */
	mem[1] = WBOOT_TRAP & 0xff;
	mem[2] = WBOOT_TRAP >> 8;

	mem[5] = 0xc3;			/* jp BDOS_TRAP */
	mem[6] = BDOS_TRAP & 0xff;
	mem[7] = BDOS_TRAP >> 8;

	mem[WBOOT_TRAP] = 0x76;		/* halt, never actually run */
	mem[BDOS_TRAP] = 0xc9;		/* ret */
}

/*
 * A filename into the 12 bytes of an fcb: drive, then 8 and 3 space
 * padded and upper cased.  A name with no dot gets a blank type; one
 * with no name is left blank, which is what the CCP does with an
 * argument that is not a filename at all.
 */
static void
parsefcb(uint16_t at, char *arg, int len)
{
	int i;
	char *dot;

	memset(&mem[at], 0, len);
	memset(&mem[at + FCB_NAME], ' ', 11);
	if (!arg)
		return;

	if (arg[0] && arg[1] == ':') {
		mem[at + FCB_DR] = (toupper(arg[0]) - 'A') + 1;
		arg += 2;
	}

	dot = strrchr(arg, '.');
	for (i = 0; i < 8 && arg[i] && (!dot || arg + i < dot); i++)
		mem[at + FCB_NAME + i] = toupper((unsigned char)arg[i]);
	if (dot) {
		for (i = 0; i < 3 && dot[1 + i]; i++)
			mem[at + FCB_TYPE + i] = toupper((unsigned char)dot[1 + i]);
	}
}

/*
 * The command tail at 0080: a length byte then the arguments with a
 * leading space, the way the CCP hands them over.  The runtime's argv
 * code reads it back out of here.
 *
 * A real CCP folds the tail to upper case before the program sees it,
 * and there is no way to get the original back - which is why CP/M
 * programs take upper case options.  This compiler's are lower case,
 * and -o and -O are different flags to it, so folding by default
 * would mean it could not be driven at all.  -u asks for the faithful
 * behaviour when what is being tested is how a program copes with it.
 */
static void
cmdtail(int argc, char **argv)
{
	int i, n = 0;
	char buf[128];

	buf[0] = 0;
	for (i = 0; i < argc; i++) {
		if (n + strlen(argv[i]) + 2 >= sizeof(buf))
			fatal("command tail too long");
		n += sprintf(buf + n, " %s", argv[i]);
	}
	if (foldcase)
		for (i = 0; i < n; i++)
			buf[i] = toupper((unsigned char)buf[i]);

	mem[DMABUF] = n;
	memcpy(&mem[DMABUF + 1], buf, n);
	mem[DMABUF + 1 + n] = 0;
	trace("command tail %d <%s>", n, buf);
}

static void
loadcom(char *path)
{
	FILE *f;
	size_t n;

	f = fopen(path, "rb");
	if (!f)
		fatal("%s: cannot open", path);
	n = fread(&mem[TPA], 1, MEMSIZE - TPA, f);
	fclose(f);
	if (n == 0)
		fatal("%s: empty", path);
	if (TPA + n > TPATOP)
		fatal("%s: %lu bytes does not fit under the bdos at %04x",
		      path, (unsigned long)n, TPATOP);
	trace("loaded %s, %lu bytes, %04x-%04x",
	      path, (unsigned long)n, TPA, (unsigned)(TPA + n - 1));
}

static void
usage(void)
{
	fprintf(stderr, "usage: cpm3 [-v] [-u] [-d [X=]dir]... program.com [args...]\n");
	fprintf(stderr, "  -v       trace bdos calls on stderr\n");
	fprintf(stderr, "  -d dir       directory drive A maps to (default .)\n");
	fprintf(stderr, "  -d X=dir     directory drive X maps to\n");
	fprintf(stderr, "  -u       fold the command tail to upper case, as a CCP does\n");
	exit(2);
}

int
main(int argc, char **argv)
{
	int mapped = 0;
	int i;

	for (i = 1; i < argc && argv[i][0] == '-'; i++) {
		if (strcmp(argv[i], "-v") == 0) {
			verbose++;
		} else if (strcmp(argv[i], "-u") == 0) {
			foldcase = 1;
		} else if (strcmp(argv[i], "-d") == 0) {
			if (++i >= argc)
				usage();
			diskmap(argv[i]);
			mapped++;
		} else {
			usage();
		}
	}
	if (i >= argc)
		usage();

	setup();
	diskinit(".");		/* an unmapped drive A is here */
	(void)mapped;
	loadcom(argv[i]);
	i++;
	/*
	 * The two default fcbs overlap, which is how CP/M laid page
	 * zero out: the first is a whole 36-byte fcb at 005c and runs
	 * to 007f, and the second is only its 16 name bytes at 006c,
	 * sitting on top of the first one's allocation area.  A
	 * program that wants both has to copy the second somewhere
	 * else before it opens the first.
	 *
	 * Writing a full fcb at 006c instead runs to 008f and takes
	 * the command tail with it - which is what this did, so every
	 * program saw an empty argv.
	 */
	parsefcb(FCB1, i < argc ? argv[i] : NULL, FCB_LEN);
	parsefcb(FCB2, i + 1 < argc ? argv[i + 1] : NULL, 16);
	cmdtail(argc - i, &argv[i]);

	desc.tick_cb = tick;
	z80init(&cpu, &desc);
	z80_set_pc(&cpu, TPA);
	z80_set_sp(&cpu, TPATOP);

	/*
	 * One instruction at a time, checking between them for the two
	 * addresses that mean the system was called.
	 *
	 * The check has to be here rather than in the tick callback.
	 * z80.h keeps the registers in locals for the length of a
	 * z80_exec and only writes them back to the struct at the end
	 * of an instruction, so a bdos call serviced from inside a
	 * memory cycle reads whatever was in the struct beforehand -
	 * which is how the first version of this asked for bdos
	 * function 255.  Between instructions the struct is the truth.
	 */
	while (!stopped) {
		uint16_t pc = z80_pc(&cpu);

		if (pc == WBOOT_TRAP)
			break;
		if (pc == BDOS_TRAP)
			bdos();		/* the ret below returns for us */

		do {
			z80_exec(&cpu, 1);
		} while (!z80_opdone(&cpu));
	}

	diskdone();
	fflush(stdout);

	/*
	 * CP/M has no exit status - the BDOS takes none and the CCP
	 * would have nowhere to put it - so a C runtime that wants one
	 * has to leave it somewhere by agreement.  This one stores it
	 * at 0080h on the way out (see exit.s), which is free by then:
	 * the command tail that lived there was read into argv before
	 * main was called.
	 *
	 * Carrying it out to the host is the whole reason a run of
	 * these can be scripted at all.  Without it cpp reporting
	 * "out of memory" and stopping looked exactly like cpp
	 * succeeding, and a survey of what does and does not compile
	 * counted the failures as passes.
	 *
	 * A program that never calls exit leaves whatever was there,
	 * which is the tail length - so this trusts the convention,
	 * and a program that does not follow it gets a status that
	 * means nothing.  Every program in this tree follows it.
	 */
	return mem[DMABUF] & 0xff;
}

/*
 * vim: tabstop=8 shiftwidth=8 noexpandtab:
 */
