#
# cp/m system call Library Makefile
# Builds the cp/m call library
#
# Two build areas, one per compiler: zc3/ and ccc/ - see libc/Makefile.
# CC is deliberately unused: the top level drives everything with
# CC=gcc on the command line, and these are target runtime sources.
#

ARCHIVE = libcpm.a

AS = asz
LIB = wslib
ROOTDIR = $(realpath ../../root)

DEST ?= ../../root/lib

.SUFFIXES:

# fakecpcl.s was a stub __cpm_clean for when cleanup.c didn't compile
SRCS = bdos.s bdoshl.s exec.s execl.s exit.s _exit.s \
	fgetc.s getargs.s # zrrtcpm.s fakecpcl.s

CSRCS = abort.c chmod.c cleanup.c close.c creat.c dup.c \
	fcbname.c fseek.c getargs.c getfcb.c getch.c isatty.c \
	open.c read.c rename.c seek.c signal.c stat.c time.c \
	unlink.c write.c

all: zc3 ccc

zc3 ccc:
	@$(MAKE) TC=$@ $@/$(ARCHIVE)

# ccc-check used to be a throwaway compile-stress pass over CSRCS with
# the self-hosted pipeline; the ccc build area does that for real now.
ccc-check: ccc

ifneq ($(TC),)

OBJS = $(addprefix $(TC)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

# The ccc area is rebuilt when the compiler itself changes - see
# libc/Makefile.
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
	rm -rf zc3 ccc ccctest
	rm -f *.o *.as *.1 *.2 *.x *.i *.pp

clobber: clean
	rm -f $(ARCHIVE)

regression:
	../../tests/regress.sh --filter libsrc/libcpm/ $(REGRESS_FLAGS)

.PHONY: all zc3 ccc ccc-check install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
