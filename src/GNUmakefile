#
# Everything the tree builds.
#
# The compiler is four passes and a shared support library:
#
#   cpp     preprocessor and token normalizer   -> desthost/lib/pass0
#   pass1   parser, emits the AST               -> desthost/lib/c0
#   pass2   code generator                      -> desthost/lib/c1
#   peep    peephole optimizer                  -> desthost/lib/peep
#   ccclib  support code the passes share       -> libccc.a
#
# tools is the Whitesmith's object toolchain plus the ccc driver and
# astpp; libc, libcpm, libu and include are the target runtime.
#

include ../GNUmakefile.inc

CC = gcc

SUBMAKE = $(MAKE) CC=$(CC) TOP=$(TOP)

#
# THE SUBDIRECTORY LISTS ARE THE SWITCH.
#
# A component that is not being built comes out of the list it is in
# and nothing else has to change - no ifdef, no target to remember,
# and no half-built directory left behind to confuse the next build.
# Putting it back is the same edit in reverse.
#
# CP/M is tabled, so cpm3 is out of HOSTDIRS and libcpm is out of
# TARGETDIRS.  What that costs while it is out: desthost/lib gets no
# libcpm.a or crtcpm.o, so "ccc -m cpm" cannot link, and the "cpm",
# "selfcheck", "selfhost" and "libcheck" targets below all want a
# CP/M runtime that will not be there.  They are left in place because
# tabled is not deleted.
#

# Host: what runs on the build machine, installed into unix/.  ccclib
# comes first because the four passes link against its host archive.
HOSTDIRS = ccclib cpp pass1 pass1z280 pass2 pass2z280 peep peep280 tools
# tabled: cpm3

# Target: the Z80 runtime, compiled by the compiler the host phase
# just installed, into micronix/.
TARGETDIRS = include ccclib libc libu libf
# tabled: libcpm

#
# The order is forced.  The runtime is compiled BY this compiler -
# desthost/bin/ccc driving cpp, c0, c1 and peep, with asz assembling and
# wslib archiving - so the host side has to be built AND installed
# before the target side starts.  ccclib appears in both lists: its
# host archive is linked into the passes, and its z80 archive is
# built later by those same passes.
#
all: host target

host:
	@mkdir -p $(HOSTDIR)/bin $(HOSTDIR)/lib
	@for d in $(HOSTDIRS); do $(SUBMAKE) -C $$d install || exit 1; done

target: host
	@mkdir -p $(MXDIR)/bin $(MXDIR)/lib $(MXDIR)/include
	@mkdir -p $(CPMDIR)/bin $(CPMDIR)/lib $(CPMDIR)/include
	@for d in $(TARGETDIRS); do $(SUBMAKE) -C $$d target-install || exit 1; done

install: all

# The self-hosted passes: ccc compiling its own sources into Micronix
# binaries for the simulator, installed into destmicronix/bin.
# The self-hosted compiler: ccc compiling its own sources into
# Micronix binaries.  tools is in this list and the four passes are
# not enough without it - a root holding c0 and c1 but no driver, no
# assembler and no linker cannot compile anything.
MXDIRS = cpp pass1 pass2 peep tools
micronix: all
	@for d in $(MXDIRS); do $(SUBMAKE) -C $$d mx-install || exit 1; done

# The same four as CP/M .com images, into destcpm/bin.  c1 does not fit
# in the TPA - see RUNTIME.md - so this builds what it can and says
# what it could not.  tools has no com build, so this list is the four
# passes and not MXDIRS.
COMDIRS = cpp pass1 pass2 peep
cpm: all
	@for d in $(COMDIRS); do $(SUBMAKE) -C $$d com-install || exit 1; done

# The compiler running on the CP/M machine, checked against the host.
selfcheck: cpm
	$(SUBMAKE) -C cpm3 selfcheck

# Deliberately NOT pruned.  A tree that built CP/M before still has
# the objects and the build directories, and clean has to reach them
# whether or not CP/M is in the build lists today.  A cleanup list
# that tracks the build list leaves droppings behind exactly when a
# component is switched off, which is when nobody is looking.
ALLDIRS = ccclib cpp pass1 pass1z280 pass2 pass2z280 peep peep280 tools cpm3 libc libcpm libu libf include

clean:
	@for d in $(ALLDIRS); do $(SUBMAKE) -C $$d clean; done
	rm -f $(CRUFT) $(CRUFTASM)

clobber:
	@for d in $(ALLDIRS); do $(SUBMAKE) -C $$d clobber; done
	rm -f $(CRUFT) $(CRUFTASM) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

stage1:
	@for d in cpp pass1 pass2 peep tools; do $(SUBMAKE) -C $$d stage1; done

sizecheck:
	@for d in cpp pass1 pass2 peep; do $(SUBMAKE) -C $$d sizecheck; done

regression:
	../tests/regress.sh --filter src/ $(REGRESS_FLAGS)

.PHONY: all host target install micronix cpm selfcheck clean clobber stage1 sizecheck regression

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
