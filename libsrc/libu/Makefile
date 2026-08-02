#
# micronix system call Library Makefile
# Builds the micronix system call library
#
# Two build areas, one per compiler: zc3/ and ccc/ - see libc/Makefile.
# CC is deliberately unused: the top level drives everything with
# CC=gcc on the command line, and these are target runtime sources.
#

ARCHIVE = libu.a

AS = asz
LIB = wslib
ROOTDIR = $(realpath ../../root)

DEST ?= ../../root/lib

.SUFFIXES:

SRCS = access.s alarm.s _break.s chdir.s chmod.s chown.s close.s \
	creat.s dup.s errno.s exec.s execv.s exit.s fdpos.s fork.s fstat.s \
	getpid.s getuid.s gtty.s kill.s link.s mknod.s mount.s nice.s open.s \
	pause.s perror.s pipe.s read.s seekraw.s setuid.s _signal.s sleep.s stat.s \
	stime.s sbrk.s stty.s sync.s time.s umount.s uname.s unlink.s wait.s \
	write.s

CSRCS = signal.c lseek.c seek.c

all: zc3 ccc

zc3 ccc:
	@$(MAKE) TC=$@ $@/$(ARCHIVE)

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

$(TC)/%.o: %.c $(CCDEP) | $(TC)
	cd $(TC) && $(ZCC) -I../../include -c ../$<

$(TC)/%.o: %.s | $(TC)
	$(AS) $< -o $@

$(TC)/$(ARCHIVE): $(OBJS)
	rm -f $@
	$(LIB) -cr $@ $(OBJS)

$(TC):
	mkdir -p $(TC)

endif

install: all
	@mkdir -p $(DEST)/zc3 $(DEST)/ccc
	cp zc3/$(ARCHIVE) $(DEST)/zc3/
	cp ccc/$(ARCHIVE) $(DEST)/ccc/

clean:
	rm -rf zc3 ccc
	rm -f *.o *.as *.1 *.2 *.x *.i *.pp

clobber: clean
	rm -f $(ARCHIVE)

regression:
	../../tests/regress.sh --filter libsrc/libu/ $(REGRESS_FLAGS)

.PHONY: all zc3 ccc install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
