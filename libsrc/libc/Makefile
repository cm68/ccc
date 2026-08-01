#
# compiler support and standard c library without system call dependencies
#

ARCHIVE = libc.a

AS = asz
# override: the top level drives everything with CC=gcc on the
# command line, and a command-line variable beats a plain
# assignment in every sub-make it reaches.  These are runtime
# sources for the target and gcc cannot build them - a clean
# tree failed here, and only built at all because the objects
# from some earlier run were still lying about.
override CC = zc3
LIB = wslib

DEST ?= ../../root/lib

.SUFFIXES: .c .s .as .o

.c.as:
	$(CC) -I../include -s $<

.c.o:
	$(CC) -I../include -c $<

.s.o:
	$(AS) $< -o $@

SRCS = abs.s allsh.s alrsh.s asallsh.s asalrsh.s asar.s strdup.s exit.s \
	asdiv.s asladd.s asland.s asll.s asllrsh.s aslmul.s \
	aslor.s aslr.s aslsub.s aslxor.s asmod.s asmul.s \
	atoi.s bitfield.s blkclr.s bmove.s csv.s brelop.s \
	wrelop.s getsp.s idiv.s imul.s index.s inout.s \
	iregset.s isalpha.s isdigit.s islower.s isspace.s isupper.s \
	ladd.s land.s lcom.s ldiv.s linc.s lldst.s llrsh.s lmul.s \
	longjmp.s lor.s lrelop.s lsub.s lushr.s lxor.s max.s swap.s  \
	op16.s tramp.s rcsv.s rindex.s sbrk.s shar.s shl16.s \
	shll.s shlr.s strcat.s strchr.s strcmp.s strcpy.s swdisp.s oarg.s \
	strlen.s strncat.s strncmp.s strncpy.s strrchr.s \
	tolower.s toupper.s xtoi.s fputc.s fgetc.s
#
# asfloat.s fbcd.s finc.s float.s frelop.s frexp.s frndint.s ftol.s
# ltof.s are not built.  pass2 no longer emits calls to any of them,
# and fbcd.s and finc.s cannot even assemble: they use deff, which the
# assembler no longer has, because there was never one Z80 float
# format for it to assemble to.
#

CSRCS = \
	rand.c srand1.c mkstemp.c permalloc.c \
	timezone.c ctime.c assert.c calloc.c malloc.c \
	memcmp.c memcpy.c memset.c blkcpy.c ctype_.c cleanup.c \
	fclose.c fflush.c filbuf.c flsbuf.c puts.c \
	fopen.c fread.c freopen.c fwrite.c setbuf.c gets.c ungetc.c \
	buf.c getenv.c pnum.c qsort.c remove.c rewind.c fseek.c ftell.c \
	fprintf.c sprintf.c printf.c doprnt.c putchar.c \
	atol.c fscanf.c scanf.c sscanf.c doscan.c \
	fnum.c atof.c
#
# acos asin atan atan2 ceil cos cosh evalpoly exp fabs floor log sin
# sinh sqrt tan tanh are not built: they do double arithmetic with
# binary operators and there is no float type.  The sources are kept
# as a starting point for a rewrite against a program-supplied float
# representation - see math.h.  fnum.c and atof.c are built, but as
# stubs that a program can override by linking its own.
#

OBJS = $(SRCS:.s=.o) $(CSRCS:.c=.o)

all: $(ARCHIVE)

$(ARCHIVE): $(OBJS)
	rm -f $(ARCHIVE)
	$(LIB) -cr $(ARCHIVE) $(OBJS)

install: $(ARCHIVE)
	@mkdir -p $(DEST)
	cp $(ARCHIVE) $(DEST)

clean:
	rm -f $(OBJS) *.as

clobber: clean
	rm -f $(ARCHIVE)

regression:
	../../tests/regress.sh --filter libsrc/libc/ $(REGRESS_FLAGS)

.PHONY: regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:

