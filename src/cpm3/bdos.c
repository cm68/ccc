/*
 * cpm3 - the BDOS, and the files behind it
 *
 * A CP/M program does file i/o by handing over a 36-byte fcb and
 * asking for one 128-byte record at a time.  There is no directory
 * here and no allocation map: an fcb names a file in one host
 * directory, and the record number in the fcb says where in it to
 * read or write.  Nothing keeps a handle across calls, because a
 * program is free to copy an fcb around and CP/M would not have
 * noticed either - the position lives in the fcb, which is the one
 * place both sides agree on.
 *
 * Open host files are cached by name so that a compiler reading a
 * source a record at a time does not pay an open for each one.
 */

#include "cpm3.h"
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <unistd.h>

extern uint8_t	reg_c(void);
extern uint8_t	reg_e(void);
extern uint16_t	reg_de(void);
extern void	retval(uint8_t v);
extern void	retval16(uint16_t v);
extern void	halt(void);

#define OK	0x00		/* what a bdos file call returns on success */
#define ERR	0xff		/* and on failure */

static char	dirpath[1024];

/*
 * The open file cache.  Small: the compiler has a source, a couple of
 * intermediates and an output going at once.
 */
#define NOPEN	8

static struct open {
	char	name[16];	/* as CP/M spells it, 8.3 upper case */
	FILE	*f;
	int	used;
} opens[NOPEN];

void
diskinit(char *dir)
{
	if (strlen(dir) >= sizeof(dirpath) - 16)
		fatal("directory name too long");
	strcpy(dirpath, dir);
}

void
diskdone(void)
{
	int i;

	for (i = 0; i < NOPEN; i++)
		if (opens[i].used) {
			fclose(opens[i].f);
			opens[i].used = 0;
		}
}

/*
 * The 11 name bytes of an fcb as "NAME.TYP".  The high bit of each
 * type byte is an attribute flag, so they are masked off.
 */
static void
fcbname(uint16_t fcb, char *out)
{
	int i, n = 0;

	for (i = 0; i < 8; i++) {
		int c = mem[fcb + FCB_NAME + i] & 0x7f;
		if (c == ' ')
			break;
		out[n++] = c;
	}
	for (i = 0; i < 3; i++) {
		int c = mem[fcb + FCB_TYPE + i] & 0x7f;
		if (c == ' ')
			break;
		if (i == 0)
			out[n++] = '.';
		out[n++] = c;
	}
	out[n] = 0;
}

/*
 * CP/M is upper case and a Unix directory usually is not.  Try the
 * name as given, then folded down, then folded up, and take the first
 * that is there.  On a create, lower case is what gets made: a tree
 * full of shouting filenames helps nobody.
 */
static int
hostpath(char *cpmname, char *out, int outlen, int forcreate)
{
	char lower[16], upper[16];
	struct stat st;
	int i;

	for (i = 0; cpmname[i]; i++) {
		lower[i] = tolower((unsigned char)cpmname[i]);
		upper[i] = toupper((unsigned char)cpmname[i]);
	}
	lower[i] = upper[i] = 0;

	if (forcreate) {
		snprintf(out, outlen, "%s/%s", dirpath, lower);
		return 1;
	}
	snprintf(out, outlen, "%s/%s", dirpath, cpmname);
	if (stat(out, &st) == 0)
		return 1;
	snprintf(out, outlen, "%s/%s", dirpath, lower);
	if (stat(out, &st) == 0)
		return 1;
	snprintf(out, outlen, "%s/%s", dirpath, upper);
	if (stat(out, &st) == 0)
		return 1;
	return 0;
}

/*
 * Find the cached handle for a name, opening it if this is the first
 * time.  "create" makes the file if it is not there and truncates it
 * if it is, which is what F_MAKE means.
 */
static FILE *
getfile(char *name, int create)
{
	char path[1200];
	int i, slot = -1;

	for (i = 0; i < NOPEN; i++) {
		if (opens[i].used && strcmp(opens[i].name, name) == 0) {
			if (!create)
				return opens[i].f;
			/* F_MAKE on an open file: start it over */
			fclose(opens[i].f);
			opens[i].used = 0;
		}
		if (!opens[i].used && slot < 0)
			slot = i;
	}
	if (slot < 0) {
		/*
		 * Every slot busy.  Close the first and take it - the
		 * position is in the fcb, so reopening later costs
		 * nothing but the open itself.
		 */
		fclose(opens[0].f);
		opens[0].used = 0;
		slot = 0;
	}

	if (!hostpath(name, path, sizeof(path), create))
		return NULL;

	/*
	 * r+ so that a file opened for reading can still be written -
	 * CP/M has one kind of open and decides at the call.  A file
	 * being made is truncated first, then reopened the same way.
	 */
	if (create) {
		FILE *t = fopen(path, "wb");
		if (!t)
			return NULL;
		fclose(t);
	}
	opens[slot].f = fopen(path, "r+b");
	if (!opens[slot].f)
		return NULL;
	strncpy(opens[slot].name, name, sizeof(opens[slot].name) - 1);
	opens[slot].name[sizeof(opens[slot].name) - 1] = 0;
	opens[slot].used = 1;
	return opens[slot].f;
}

/*
 * The size of a file in bytes, or -1 if it is not there.
 */
static long
hostsize(char *name)
{
	char path[1200];
	struct stat st;

	if (!hostpath(name, path, sizeof(path), 0) || stat(path, &st) != 0)
		return -1;
	return (long)st.st_size;
}

/*
 * CP/M 3 records the byte count of a file's last record, which is
 * what lets it say how long a file actually is - CP/M 2 could only
 * count 128-byte records and a program had to guess the rest, or
 * stop at a ^Z and hope the file was text.  It lives in the fcb's s1
 * field, and zero means the last record is full.
 *
 * The compiler's intermediates are binary and exactly as long as they
 * are, so this is not a nicety: without it c0 reads whatever padding
 * follows the lexemes and takes it for more of them.
 */
static void
setbytecount(uint16_t fcb, long size)
{
	mem[fcb + FCB_S1] = (size <= 0) ? 0 : (uint8_t)(size % RECLEN);
}

static void
dropfile(char *name)
{
	int i;

	for (i = 0; i < NOPEN; i++)
		if (opens[i].used && strcmp(opens[i].name, name) == 0) {
			fclose(opens[i].f);
			opens[i].used = 0;
		}
}

/*
 * Where in the file the fcb is pointing.
 *
 * Sequentially that is the extent and the record within it: 128
 * records to an extent, 32 extents before s2 has to carry.  Randomly
 * it is the three byte record number, which says the same thing in
 * one piece.
 */
static long
seqrec(uint16_t fcb)
{
	long ex = mem[fcb + FCB_EX] & 0x1f;
	long s2 = mem[fcb + FCB_S2] & 0x3f;

	return ((s2 * 32) + ex) * 128 + mem[fcb + FCB_CR];
}

static void
setseqrec(uint16_t fcb, long rec)
{
	mem[fcb + FCB_CR] = rec % 128;
	rec /= 128;
	mem[fcb + FCB_EX] = rec % 32;
	mem[fcb + FCB_S2] = rec / 32;
}

static long
ranrec(uint16_t fcb)
{
	return mem[fcb + FCB_R0] |
	       ((long)mem[fcb + FCB_R1] << 8) |
	       ((long)mem[fcb + FCB_R2] << 16);
}

/*
 * Read one record into the dma buffer.  A read that runs off the end
 * of the file returns 1, "reading unwritten data", which is how a
 * program learns where the end is.  A partial last record is padded
 * with ^Z: CP/M has no byte count, so a text file ends at the first
 * one, and that is what the runtime's fgetc looks for.
 */
static int
readrec(uint16_t fcb, long rec)
{
	char name[16];
	FILE *f;
	size_t n;
	uint8_t buf[RECLEN];

	fcbname(fcb, name);
	f = getfile(name, 0);
	if (!f)
		return ERR;
	if (fseek(f, rec * RECLEN, SEEK_SET) != 0)
		return 1;
	memset(buf, 0x1a, sizeof(buf));
	n = fread(buf, 1, RECLEN, f);
	if (n == 0)
		return 1;
	memcpy(&mem[dma], buf, RECLEN);
	return OK;
}

static int
writerec(uint16_t fcb, long rec)
{
	char name[16];
	FILE *f;

	fcbname(fcb, name);
	f = getfile(name, 0);
	if (!f)
		return ERR;
	if (fseek(f, rec * RECLEN, SEEK_SET) != 0)
		return 2;	/* no space */
	if (fwrite(&mem[dma], 1, RECLEN, f) != RECLEN)
		return 2;
	return OK;
}

/*
 * Console.  Output goes where ours goes; input comes from ours.  CP/M
 * echoes what function 1 reads, and the runtime's getch relies on it.
 */
static void
conout(int c)
{
	c &= 0x7f;
	if (c == '\r')
		return;		/* CP/M sends crlf; one is enough here */
	putchar(c);
}

static int
conin(void)
{
	int c = getchar();

	if (c == EOF)
		return 0x1a;
	return c;
}

void
bdos(void)
{
	uint8_t fn = reg_c();
	uint16_t de = reg_de();
	uint16_t fcb = de;
	char name[16], name2[16];
	long rec;
	int rc;

	/*
	 * -vv traces every call, which is the only way to see what a
	 * program is really asking for.  -v keeps to the interesting
	 * ones, since console output would drown everything else.
	 */
	if (verbose > 1) {
		/*
		 * For a call that takes an fcb, show the 12 name bytes
		 * raw as well - a name that came out blank or full of
		 * something unexpected is the usual thing to be looking
		 * for, and fcbname() would hide it.
		 */
		if (fn >= B_OPEN && fn <= B_SETRAN && fn != B_SETDMA) {
			char hex[64];
			int k;

			for (k = 0; k < 12; k++)
				sprintf(hex + k * 3, "%02x ", mem[de + k]);
			trace("bdos %3d fcb=%04x %s", fn, de, hex);
		} else {
			trace("bdos %3d de=%04x", fn, de);
		}
	}

	switch (fn) {

	case B_TERM:			/* system reset - the program is done */
		trace("term");
		halt();
		break;

	case B_CONIN:
		rc = conin();
		conout(rc);
		retval(rc);
		break;

	case B_CONOUT:
		conout(reg_e());
		retval(0);
		break;

	case B_RAWIO:
		if (reg_e() == 0xff) {
			retval(0);	/* no key waiting */
		} else if (reg_e() == 0xfe || reg_e() == 0xfd) {
			retval(0);
		} else {
			conout(reg_e());
			retval(0);
		}
		break;

	case B_PRINTS:			/* '$' terminated string */
		while (mem[de] != '$')
			conout(mem[de++]);
		retval(0);
		break;

	case B_CONST:
		retval(0);		/* never anything waiting */
		break;

	case B_VERSION:
		trace("version");
		retval16(CPM3VERSION);
		break;

	case B_RESET:
	case B_SELDSK:
		retval(0);
		break;

	case B_OPEN:
		fcbname(fcb, name);
		if (getfile(name, 0)) {
			mem[fcb + FCB_CR] = 0;
			mem[fcb + FCB_EX] = 0;
			mem[fcb + FCB_S2] = 0;
			setbytecount(fcb, hostsize(name));
			trace("open %s (%ld bytes)", name, hostsize(name));
			retval(OK);
		} else {
			trace("open %s - not found", name);
			retval(ERR);
		}
		break;

	case B_CLOSE:
		fcbname(fcb, name);
		/*
		 * A program that has been writing says in s1 how much of
		 * the last record it meant.  Records are all that ever
		 * reach the disk, so honour it by cutting the file back
		 * to the length it is really claiming - which is what
		 * CP/M 3 recording a byte count amounts to here.
		 */
		if (mem[fcb + FCB_S1]) {
			long sz = hostsize(name);

			if (sz > 0 && sz % RECLEN == 0) {
				char path[1200];

				sz = sz - RECLEN + mem[fcb + FCB_S1];
				if (hostpath(name, path, sizeof(path), 0)) {
					FILE *f = getfile(name, 0);
					if (f)
						fflush(f);
					dropfile(name);
					if (truncate(path, sz) != 0)
						trace("close %s: truncate failed",
						      name);
				}
			}
		}
		trace("close %s", name);
		/*
		 * Left open in the cache: a program often closes and
		 * reopens, and nothing is buffered on our side that a
		 * close has to push out.  diskdone flushes at exit.
		 */
		retval(OK);
		break;

	case B_MAKE:
		fcbname(fcb, name);
		if (getfile(name, 1)) {
			mem[fcb + FCB_CR] = 0;
			mem[fcb + FCB_EX] = 0;
			mem[fcb + FCB_S2] = 0;
			mem[fcb + FCB_RC] = 0;
			setbytecount(fcb, 0);
			trace("make %s", name);
			retval(OK);
		} else {
			trace("make %s - failed", name);
			retval(ERR);
		}
		break;

	case B_DELETE:
		fcbname(fcb, name);
		dropfile(name);
		{
			char path[1200];
			if (hostpath(name, path, sizeof(path), 0) &&
			    remove(path) == 0) {
				trace("delete %s", name);
				retval(OK);
			} else {
				trace("delete %s - not found", name);
				retval(ERR);
			}
		}
		break;

	case B_RENAME:
		/*
		 * The new name is in the second half of the same fcb,
		 * 16 bytes along.
		 */
		fcbname(fcb, name);
		fcbname(fcb + 16, name2);
		dropfile(name);
		{
			char from[1200], to[1200];
			if (hostpath(name, from, sizeof(from), 0)) {
				hostpath(name2, to, sizeof(to), 1);
				if (rename(from, to) == 0) {
					trace("rename %s %s", name, name2);
					retval(OK);
					break;
				}
			}
			trace("rename %s %s - failed", name, name2);
			retval(ERR);
		}
		break;

	case B_READSEQ:
		rec = seqrec(fcb);
		rc = readrec(fcb, rec);
		if (rc == OK)
			setseqrec(fcb, rec + 1);
		retval(rc);
		break;

	case B_WRITESEQ:
		rec = seqrec(fcb);
		rc = writerec(fcb, rec);
		if (rc == OK)
			setseqrec(fcb, rec + 1);
		retval(rc);
		break;

	case B_READRAN:
		rec = ranrec(fcb);
		rc = readrec(fcb, rec);
		if (rc == OK)
			setseqrec(fcb, rec);
		retval(rc);
		break;

	case B_WRITERAN:
		rec = ranrec(fcb);
		rc = writerec(fcb, rec);
		if (rc == OK)
			setseqrec(fcb, rec);
		retval(rc);
		break;

	case B_FILESIZE:
		/*
		 * The size in records, rounded up, written back into the
		 * random record field.  libcpm's _fsize reads it there
		 * and multiplies by 128, which is why a file whose
		 * length is not a multiple of 128 reads long - CP/M
		 * genuinely does not know any better.
		 */
		fcbname(fcb, name);
		{
			char path[1200];
			struct stat st;
			long recs = 0;

			if (hostpath(name, path, sizeof(path), 0) &&
			    stat(path, &st) == 0)
				recs = (st.st_size + RECLEN - 1) / RECLEN;
			mem[fcb + FCB_R0] = recs & 0xff;
			mem[fcb + FCB_R1] = (recs >> 8) & 0xff;
			mem[fcb + FCB_R2] = (recs >> 16) & 0xff;
			setbytecount(fcb, hostsize(name));
			trace("filesize %s = %ld records", name, recs);
			retval(OK);
		}
		break;

	case B_SETRAN:
		rec = seqrec(fcb);
		mem[fcb + FCB_R0] = rec & 0xff;
		mem[fcb + FCB_R1] = (rec >> 8) & 0xff;
		mem[fcb + FCB_R2] = (rec >> 16) & 0xff;
		retval(OK);
		break;

	case B_SFIRST:
	case B_SNEXT:
		/*
		 * No directory to search.  Nothing in the compiler asks,
		 * and answering "no more files" is honest.
		 */
		retval(ERR);
		break;

	case B_LOGVEC:
		retval16(1);		/* drive A only */
		break;

	case B_GETDSK:
		retval(0);
		break;

	case B_SETDMA:
		dma = de;
		break;

	case B_USER:
		if (reg_e() == 0xff) {
			retval(usernum);
		} else {
			usernum = reg_e();
			retval(0);
		}
		break;

	case B_MULTISEC:
		/*
		 * Accepted and ignored: every transfer here is one
		 * record, and a program that asks for more still gets
		 * correct data, only not in one call.
		 */
		retval(OK);
		break;

	case B_ERRMODE:
		errmode = reg_e();
		trace("error mode %02x", errmode);
		break;

	case B_GETDT:
		/* zero date, zero time - nothing here reads it */
		memset(&mem[de], 0, 4);
		retval(0);
		break;

	default:
		trace("unimplemented bdos call %d", fn);
		retval(ERR);
		break;
	}
}

/*
 * vim: tabstop=8 shiftwidth=8 noexpandtab:
 */
