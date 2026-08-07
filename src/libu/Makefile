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

# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../tree.mk
include ../../cruft.mk

.SUFFIXES:

SRCS = access.s alarm.s _break.s chdir.s chmod.s chown.s close.s \
	creat.s dup.s errno.s exec.s execv.s exit.s fdpos.s fork.s fstat.s \
	getpid.s getuid.s gtty.s kill.s link.s mknod.s mount.s nice.s open.s \
	pause.s perror.s pipe.s read.s seekraw.s setuid.s _signal.s sleep.s stat.s \
	stime.s sbrk.s stty.s sync.s time.s umount.s unlink.s wait.s \
	write.s

CSRCS = signal.c lseek.c seek.c

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

all: $(BUILD)/$(ARCHIVE) $(BUILD)/crt0.o

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

# The Micronix startup object.  Linked by name, not pulled from the
# archive, so it is built beside it rather than into it.
$(BUILD)/crt0.o: crt0.s
	@mkdir -p $(BUILD)
	$(AS) $< -o $@

target-install: all
	@mkdir -p $(MXDIR)/lib
	cp $(BUILD)/$(ARCHIVE) $(BUILD)/crt0.o $(MXDIR)/lib
install: target-install

clean:
	rm -rf $(BUILD) zc3 ccc
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter src/libu/ $(REGRESS_FLAGS)

.PHONY: all install target-install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
