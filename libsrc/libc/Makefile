#
# compiler support and standard c library without system call dependencies
#
# Two build areas, one per compiler: zc3/ holds HiTech-built objects
# and archive, ccc/ holds the same sources built by ccc itself.  The
# .s files assemble identically in both; only the C differs.  Linkers
# pick an area with -L root/lib/zc3 or -L root/lib/ccc, so a zc3-built
# program never mixes with ccc-built runtime C and vice versa.
#
# That the .s files are shared is why the 32-bit runtime comes in two
# sets.  ladd, arelop, almul, aldiv, allsh and the rest are Hi-Tech's,
# take their operands in HL:DE and on the stack, and are named by
# zc3's code generator; the q* set is ccc's, takes them in HL':HL and
# DE':DE, and is named by pass2.  See QLONG.md.  Both are archived
# into both areas and neither program links the other's - the linker
# pulls an object only when something references it.
#
# CC is deliberately unused: the top level drives everything with
# CC=gcc on the command line, and a command-line variable beats any
# assignment in every sub-make it reaches.  These are runtime sources
# for the target and gcc cannot build them.
#

ARCHIVE = libc.a

AS = asz
LIB = wslib
ROOTDIR = $(realpath ../../root)

# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../cruft.mk

DEST ?= ../../root/lib

.SUFFIXES:

SRCS = abs.s allsh.s alrsh.s asallsh.s asalrsh.s asar.s strdup.s exit.s \
	asdiv.s asladd.s asland.s asll.s asllrsh.s aslmul.s \
	aslor.s aslr.s aslsub.s aslxor.s asmod.s asmul.s \
	atoi.s bitfield.s blkclr.s bmove.s csv.s brelop.s \
	wrelop.s getsp.s idiv.s imul.s index.s inout.s \
	iregset.s isalpha.s isdigit.s islower.s isspace.s isupper.s \
	ladd.s land.s lcom.s ldiv.s linc.s lldst.s llrsh.s lmul.s \
	longjmp.s lor.s lrelop.s lsub.s lushr.s lxor.s max.s swap.s  \
	qadd.s qsub.s qand.s qor.s qxor.s qcom.s qcmp.s \
	qshl.s qsar.s qshr.s qinc.s qldst.s qmul.s qdiv.s \
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

all: zc3 ccc

zc3 ccc:
	@$(MAKE) TC=$@ $@/$(ARCHIVE)

# The per-area rules only exist inside a TC= sub-make; without the
# guard, make would need one object list per area at top level too.
ifneq ($(TC),)

OBJS = $(addprefix $(TC)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

# The ccc area is rebuilt when the compiler itself changes: these
# objects exist to be run, and a codegen fix that leaves stale ones
# behind reads as a fix that did not work.  An afternoon went into
# one that had.
ifeq ($(TC),ccc)
ZCC = $(ROOTDIR)/bin/ccc -O
CCDEP = $(ROOTDIR)/bin/c1 $(ROOTDIR)/bin/c0 $(ROOTDIR)/bin/cpp $(ROOTDIR)/bin/peep
else
ZCC = zc3
CCDEP =
endif

$(TC)/%.o: %.c $(CCDEP)
	@mkdir -p $(TC)
	cd $(TC) && $(ZCC) -I../../include -c ../$<

$(TC)/%.o: %.s
	@mkdir -p $(TC)
	$(AS) $< -o $@

$(TC)/$(ARCHIVE): $(OBJS)
	rm -f $@
	$(LIB) -cr $@ $(OBJS)


endif

install: all
	@mkdir -p $(DEST)/zc3 $(DEST)/ccc
	cp zc3/$(ARCHIVE) $(DEST)/zc3/
	cp ccc/$(ARCHIVE) $(DEST)/ccc/

clean:
	rm -rf zc3 ccc
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter libsrc/libc/ $(REGRESS_FLAGS)

.PHONY: all zc3 ccc install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
