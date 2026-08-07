#
# micronix system call Library Makefile
# Builds the micronix system call library
#
# One build area, ccc/ - see libc/Makefile.  The zc3/ area beside
# it was the Hi-Tech bootstrap and is gone.
# CC is deliberately unused: the top level drives everything with
# CC=gcc on the command line, and these are target runtime sources.
#

ARCHIVE = libu.a

AS = $(ROOTDIR)/bin/asz
LIB = $(ROOTDIR)/bin/wslib
ROOTDIR = $(abspath ../../root)

# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../cruft.mk

DEST ?= ../../root/lib

.SUFFIXES:

SRCS = access.s alarm.s _break.s chdir.s chmod.s chown.s close.s \
	creat.s dup.s errno.s exec.s execv.s exit.s fdpos.s fork.s fstat.s \
	getpid.s getuid.s gtty.s kill.s link.s mknod.s mount.s nice.s open.s \
	pause.s perror.s pipe.s read.s seekraw.s setuid.s _signal.s sleep.s stat.s \
	stime.s sbrk.s stty.s sync.s time.s umount.s uname.s unlink.s wait.s \
	write.s

CSRCS = signal.c lseek.c seek.c

# One build area, ccc/, holding the runtime this compiler built.
# There was a zc3/ area beside it once, the same sources built by
# Hi-Tech C so a ccc-built program could link a runtime known to
# work.  That was the bootstrap and the bootstrap is finished: ccc
# builds its own runtime and nothing here needs another compiler.
all: ccc

ccc:
	@$(MAKE) TC=$@ $@/$(ARCHIVE)

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
	rm -rf zc3 ccc
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter libsrc/libu/ $(REGRESS_FLAGS)

.PHONY: all ccc install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
