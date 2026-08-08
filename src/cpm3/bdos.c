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

/*
 * The drives.  CP/M has sixteen, A through P, and each is a directory
 * here - which is the whole of the impedance mismatch between the two
 * systems, once it is said out loud.  A CP/M program cannot say
 * "../cpp/lexeme.h", but it can say "D:lexeme.h", and that is the same
 * statement if D is where cpp's headers are.
 *
 * A drive nobody mapped has no directory and everything on it fails to
 * open, which is what an empty drive does.
 */
#define NDRIVE	16

static char	*drivedir[NDRIVE];
static int	curdrive;		/* 0 = A */

/*
 * The open file cache.  Small: the compiler has a source, a couple of
 * intermediates and an output going at once.
 */
#define NOPEN	8

static struct open {
	int	dr;		/* drive code as it stood in the fcb */
	char	name[16];	/* as CP/M spells it, 8.3 upper case */
	FILE	*f;
	int	used;
} opens[NOPEN];

/*
 * "dir" maps drive A; "X=dir" or "X:dir" maps drive X.
 */
void
diskmap(char *spec)
{
	int d = 0;
	char *dir = spec;

	if (spec[0] && (spec[1] == '=' || spec[1] == ':') &&
	    isalpha((unsigned char)spec[0])) {
		d = toupper((unsigned char)spec[0]) - 'A';
		dir = spec + 2;
		if (d < 0 || d >= NDRIVE)
			fatal("no drive %c: CP/M has A through P", spec[0]);
	}
	if (strlen(dir) >= 1024)
		fatal("directory name too long");
	drivedir[d] = dir;
}

void
diskinit(char *dir)
{
	if (!drivedir[0])
		drivedir[0] = dir;
}

/*
 * Which host directory a drive code means.  In an fcb, 0 is "the
 * current drive" and 1 is A - the off-by-one is CP/M's, not ours.
 */
static char *
dirfor(int dr)
{
	int d = dr ? dr - 1 : curdrive;

	if (d < 0 || d >= NDRIVE)
		return NULL;
	return drivedir[d];
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
hostpath(int dr, char *cpmname, char *out, int outlen, int forcreate)
{
	char lower[16], upper[16];
	struct stat st;
	char *dir = dirfor(dr);
	int i;

	if (!dir)
		return 0;		/* no such drive: an empty one */

	for (i = 0; cpmname[i]; i++) {
		lower[i] = tolower((unsigned char)cpmname[i]);
		upper[i] = toupper((unsigned char)cpmname[i]);
	}
	lower[i] = upper[i] = 0;

	if (forcreate) {
		snprintf(out, outlen, "%s/%s", dir, lower);
		return 1;
	}
	snprintf(out, outlen, "%s/%s", dir, cpmname);
	if (stat(out, &st) == 0)
		return 1;
	snprintf(out, outlen, "%s/%s", dir, lower);
	if (stat(out, &st) == 0)
		return 1;
	snprintf(out, outlen, "%s/%s", dir, upper);
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
getfile(int dr, char *name, int create)
{
	char path[1200];
	int i, slot = -1;

	for (i = 0; i < NOPEN; i++) {
		if (opens[i].used && opens[i].dr == dr &&
		    strcmp(opens[i].name, name) == 0) {
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

	if (!hostpath(dr, name, path, sizeof(path), create))
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
	opens[slot].dr = dr;
	opens[slot].used = 1;
	return opens[slot].f;
}

/*
 * The size of a file in bytes, or -1 if it is not there.
 */
static long
hostsize(int dr, char *name)
{
	char path[1200];
	struct stat st;

	if (!hostpath(dr, name, path, sizeof(path), 0) || stat(path, &st) != 0)
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
dropfile(int dr, char *name)
{
	int i;

	for (i = 0; i < NOPEN; i++)
		if (opens[i].used && opens[i].dr == dr &&
		    strcmp(opens[i].name, name) == 0) {
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
	f = getfile(mem[fcb + FCB_DR], name, 0);
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
	f = getfile(mem[fcb + FCB_DR], name, 0);
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
	int dr = mem[de + FCB_DR];	/* only meaningful for fcb calls */
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
			/*
			 * rwp is libcpm's own field, four bytes past the
			 * 36-byte fcb the BDOS knows about.  It is the
			 * position the runtime thinks it is at, which is
			 * the thing to watch when a write will not move.
			 */
			trace("bdos %3d fcb=%04x rec=%ld cr=%d ex=%d rwp=%ld %s",
			      fn, de,
			      (long)(mem[de+FCB_R0] | (mem[de+FCB_R1]<<8) |
				     ((long)mem[de+FCB_R2]<<16)),
			      mem[de+FCB_CR], mem[de+FCB_EX],
			      (long)(mem[de+36] | (mem[de+37]<<8) |
				     ((long)mem[de+38]<<16) |
				     ((long)mem[de+39]<<24)), hex);
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
		curdrive = 0;
		retval(0);
		break;

	case B_SELDSK:
		/* e holds the drive, 0 = A.  Selecting one nobody
		 * mapped is allowed; opening on it is what fails. */
		if (reg_e() < NDRIVE)
			curdrive = reg_e();
		retval(0);
		break;

	case B_OPEN:
		fcbname(fcb, name);
		if (getfile(dr, name, 0)) {
			mem[fcb + FCB_CR] = 0;
			mem[fcb + FCB_EX] = 0;
			mem[fcb + FCB_S2] = 0;
			setbytecount(fcb, hostsize(dr, name));
			trace("open %s (%ld bytes)", name, hostsize(dr, name));
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
			long sz = hostsize(dr, name);

			if (sz > 0 && sz % RECLEN == 0) {
				char path[1200];

				sz = sz - RECLEN + mem[fcb + FCB_S1];
				if (hostpath(dr, name, path, sizeof(path), 0)) {
					FILE *f = getfile(dr, name, 0);
					if (f)
						fflush(f);
					dropfile(dr, name);
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
		if (getfile(dr, name, 1)) {
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
		dropfile(dr, name);
		{
			char path[1200];
			if (hostpath(dr, name, path, sizeof(path), 0) &&
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
		dropfile(dr, name);
		{
			char from[1200], to[1200];
			if (hostpath(dr, name, from, sizeof(from), 0)) {
				hostpath(dr, name2, to, sizeof(to), 1);
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

			if (hostpath(dr, name, path, sizeof(path), 0) &&
			    stat(path, &st) == 0)
				recs = (st.st_size + RECLEN - 1) / RECLEN;
			mem[fcb + FCB_R0] = recs & 0xff;
			mem[fcb + FCB_R1] = (recs >> 8) & 0xff;
			mem[fcb + FCB_R2] = (recs >> 16) & 0xff;
			setbytecount(fcb, hostsize(dr, name));
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
		{
			int i, v = 0;

			for (i = 0; i < NDRIVE; i++)
				if (drivedir[i])
					v |= 1 << i;
			retval16(v);
		}
		break;

	case B_GETDSK:
		retval(curdrive);
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
