#
# cp/m system call Library Makefile
# Builds the cp/m call library
#
# One build area, ccc/ - see libc/Makefile.  The zc3/ area beside
# it was the Hi-Tech bootstrap and is gone.
# CC is deliberately unused: the top level drives everything with
# CC=gcc on the command line, and these are target runtime sources.
#

ARCHIVE = libcpm.a

AS = $(ROOTDIR)/bin/asz
LIB = $(ROOTDIR)/bin/wslib
ROOTDIR = $(abspath ../../root)

# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../cruft.mk

DEST ?= ../../root/lib

.SUFFIXES:

# fakecpcl.s was a stub __cpm_clean for when cleanup.c didn't compile
SRCS = bdos.s bdoshl.s exec.s execl.s exit.s _exit.s \
	fgetc.s getargs.s # zrrtcpm.s fakecpcl.s

CSRCS = abort.c chmod.c cleanup.c close.c creat.c dup.c \
	fcbname.c fseek.c getargs.c getfcb.c getch.c isatty.c \
	open.c read.c rename.c seek.c signal.c stat.c time.c \
	unlink.c write.c

# One build area, ccc/, holding the runtime this compiler built.
# There was a zc3/ area beside it once, the same sources built by
# Hi-Tech C so a ccc-built program could link a runtime known to
# work.  That was the bootstrap and the bootstrap is finished: ccc
# builds its own runtime and nothing here needs another compiler.
all: ccc

ccc:
	@$(MAKE) TC=$@ $@/$(ARCHIVE)

# ccc-check used to be a throwaway compile-stress pass over CSRCS with
# the self-hosted pipeline; the ccc build area does that for real now.
ccc-check: ccc

ifneq ($(TC),)

OBJS = $(addprefix $(TC)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

# The ccc area is rebuilt when the compiler itself changes - see
# libc/Makefile.
ZCC = $(ROOTDIR)/bin/ccc -O
CCDEP = $(ROOTDIR)/bin/c1 $(ROOTDIR)/bin/c0 $(ROOTDIR)/bin/cpp $(ROOTDIR)/bin/peep

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
	@mkdir -p $(DEST)/ccc
	cp ccc/$(ARCHIVE) $(DEST)/ccc/

clean:
	rm -rf zc3 ccc ccctest
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter libsrc/libcpm/ $(REGRESS_FLAGS)

.PHONY: all ccc ccc-check install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
