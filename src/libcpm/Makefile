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

# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../tree.mk
include ../../cruft.mk

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
# One build area now.  There were two while Hi-Tech C was
# bootstrapping this runtime, one archive per compiler, and a TC=
# sub-make to build the object list twice.  With a single compiler
# the indirection bought nothing.
BUILD = z80
OBJS = $(addprefix $(BUILD)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

all: $(BUILD)/$(ARCHIVE) $(BUILD)/crtcpm.o

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

# The CP/M startup object - see libu/Makefile.
$(BUILD)/crtcpm.o: crtcpm.s
	@mkdir -p $(BUILD)
	$(AS) $< -o $@

target-install: all
	@mkdir -p $(CPMDIR)/lib
	cp $(BUILD)/$(ARCHIVE) $(BUILD)/crtcpm.o $(CPMDIR)/lib
install: target-install

clean:
	rm -rf $(BUILD) zc3 ccc
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter src/libcpm/ $(REGRESS_FLAGS)

.PHONY: all install target-install clean clobber regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
