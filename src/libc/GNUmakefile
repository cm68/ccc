#
# compiler support and standard c library without system call dependencies
#
# One build area, z80/, installed to destmicronix/lib and destcpm/lib.  There were two
# for as long as the bootstrap needed them - a zc3/ area of the same
# sources built by Hi-Tech C, so a program could link a runtime that
# was known to work while ccc's was still being proven.  ccc builds
# its own now and Hi-Tech is gone from the tree.
#
# One leftover of that: the 32-bit runtime is still here in two sets.
# ladd, arelop, almul, aldiv, allsh and the rest are Hi-Tech's, take
# their operands in HL:DE and on the stack, and were named by zc3's
# code generator; the q* set is ccc's, takes them in HL':HL and
# DE':DE, and is named by pass2.  See QLONG.md.  Nothing emits calls
# to the first set any more, and the linker pulls an object only when
# something references it, so they cost nothing in an image - but
# they are dead weight in the archive and could go.
#
# CC is deliberately unused: the top level drives everything with
# CC=gcc on the command line, and a command-line variable beats any
# assignment in every sub-make it reaches.  These are runtime sources
# for the target and gcc cannot build them.
#

ARCHIVE = libc.a
ARCHIVE280 = libc280.a

# opendir.c IS here and is not built here.  It wants types.h,
# dirent.h and sys/dir.h, which are the micronix tree's headers and
# not this one's - sysinstall says as much about usr/include being
# not ours to own.  The source lives here because there is one libc
# now; the micronix build, which has those headers on its include
# path, is the one that compiles it.  Nothing this compiler runs
# reads a directory.
#
# sbrk is NOT here.  There was a libc/sbrk.s that kept the break in a
# variable and never told the kernel it had moved - it carved memory
# between __Hbss and sp on its own authority.  libc precedes libu in
# every link, so that was the sbrk every Micronix binary got, and
# libu's, which makes the break system call, was shadowed and never
# used.  Under the hardware simulator that earns a segmentation
# violation, the kernel having granted none of the memory malloc was
# handing out.  libu owns sbrk; this does not.
#
# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../GNUmakefile.inc

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
	op16.s tramp.s rcsv.s rindex.s shar.s shl16.s \
	shll.s shlr.s strcat.s strchr.s strcmp.s strcpy.s swdisp.s swdispw.s oarg.s \
	strlen.s strncat.s strncmp.s strncpy.s strrchr.s \
	tolower.s toupper.s xtoi.s fputc.s fgetc.s memcpy.s
#
# asfloat.s float.s frelop.s frexp.s frndint.s ftol.s ltof.s are not
# built.  pass2 no longer emits calls to any of them.  fbcd.s and
# finc.s moved to libf, where their deff constants are now spelled as
# defb.
#

CSRCS = \
	rand.c srand1.c mkstemp.c permaloc.c \
	timezone.c ctime.c assert.c calloc.c malloc.c \
	memcmp.c memset.c blkcpy.c ctype_.c cleanup.c \
	isatty.c mkdir.c mktemp.c system.c \
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

# One build area, ccc/, holding the runtime this compiler built.
# There was a zc3/ area beside it once, the same sources built by
# Hi-Tech C so a ccc-built program could link a runtime known to
# work.  That was the bootstrap and the bootstrap is finished: ccc
# builds its own runtime and nothing here needs another compiler.
# One build area now.  There were two while Hi-Tech C was
# bootstrapping this runtime, one archive per compiler, and a TC=
# sub-make to build the object list twice.  With a single compiler
# the indirection bought nothing.
BUILD = z80
OBJS = $(addprefix $(BUILD)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

# The Z280 variant: same sources, but csv.s -> csv280.s (the startup
# drops the IY frame pointer) and everything built by the Z280 passes.
BUILD280 = z280
SRCS280 = $(filter-out csv.s,$(SRCS)) csv280.s
OBJS280 = $(addprefix $(BUILD280)/,$(SRCS280:.s=.o) $(CSRCS:.c=.o))

all: $(BUILD)/$(ARCHIVE) $(BUILD280)/$(ARCHIVE280)

# Rebuilt when the compiler changes: these objects exist to be run,
# and a codegen fix that leaves stale ones behind reads as a fix that
# did not work.  An afternoon went into one that had.
$(BUILD)/%.o: %.c $(CCDEP)
	@mkdir -p $(BUILD)
	cd $(BUILD) && $(CCC) -O -I../../include -c ../$<

$(BUILD)/%.o: %.s
	@mkdir -p $(BUILD)
	$(AS) $< -o $@

$(BUILD)/$(ARCHIVE): $(OBJS)
	rm -f $@
	$(LIB) -cr $@ $(OBJS)

# Z280 build: the .c compiled by the Z280 passes, the .s assembled by
# the Z280 assembler.  csv280.s takes csv.s's place via SRCS280.
$(BUILD280)/%.o: %.c $(CCDEP280)
	@mkdir -p $(BUILD280)
	cd $(BUILD280) && $(CCC) -O -m z280 -I../../include -c ../$<

$(BUILD280)/%.o: %.s
	@mkdir -p $(BUILD280)
	$(AS280) $< -o $@

$(BUILD280)/$(ARCHIVE280): $(OBJS280)
	rm -f $@
	$(LIB) -cr $@ $(OBJS280)

# Every install gets every target's runtime, because any host can
# cross-build for any target - a Micronix machine builds .com files
# with -m cpm and needs libcpm.a sitting beside its own libu.a.  They
# do not collide: the names differ, and libc.a is the same file for
# both.  If it ever is not, that is a bug - see "make libcheck".
target-install: all
	@mkdir -p $(MXDIR)/lib $(CPMDIR)/lib $(HOSTDIR)/lib
	$(INSTALL) $(BUILD)/$(ARCHIVE) $(MXDIR)/lib
	$(INSTALL) $(BUILD)/$(ARCHIVE) $(CPMDIR)/lib
	$(INSTALL) $(BUILD)/$(ARCHIVE) $(HOSTDIR)/lib
	$(INSTALL) $(BUILD280)/$(ARCHIVE280) $(MXDIR)/lib
	$(INSTALL) $(BUILD280)/$(ARCHIVE280) $(HOSTDIR)/lib
install: target-install

clean:
	rm -rf $(BUILD) $(BUILD280) zc3 ccc
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter src/libc/ $(REGRESS_FLAGS)

.PHONY: all install target-install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
