#
# Target headers.
#
# These are the headers a program compiled for the Z80 is built
# against, not the host's.  Both target trees get the same set: the
# system-dependent part of the runtime is the library - libu for
# Micronix, libcpm for CP/M - and a program includes <stdio.h> the
# same way whichever it links.
#
# There is nothing to build here; the headers are the product.
#

include ../../GNUmakefile.inc

HDRS = $(wildcard *.h)

all:

# One header tree, the same for every target - what differs is handled
# by ifdefs inside the headers, not by two copies that have to be kept
# in step.
#
# A Unix keeps its headers in /usr/include and so does Micronix, so
# that is where the driver looks: libdir/../usr/include.  CP/M has no
# /usr and its install is flat, so there they sit beside the passes -
# libdir/include - which is why every tree gets the set twice.
#
# The unix tree needs BOTH, and used to get only the first.  It is a
# host cross compiler: "ccc -m cpm" out of an install is the ordinary
# way to build a .com, and it resolved its headers to lib/include,
# which nothing created.  Every CP/M compile with an include in it
# died with "cannot find include file: stdio.h" against a header that
# was sitting in usr/include the whole time.  This is the one install
# tree that serves as both targets' toolchain, so it carries both
# layouts.
target-install:
	@mkdir -p $(MXDIR)/include/sys $(CPMDIR)/include/sys
	@mkdir -p $(MXDIR)/usr/include/sys $(CPMDIR)/lib/include/sys
	@mkdir -p $(UNIXDIR)/usr/include/sys $(UNIXDIR)/lib/include/sys
	$(INSTALL) $(HDRS) $(MXDIR)/include
	$(INSTALL) $(HDRS) $(CPMDIR)/include
	$(INSTALL) sys/*.h $(MXDIR)/include/sys
	$(INSTALL) sys/*.h $(CPMDIR)/include/sys
	$(INSTALL) $(HDRS) $(MXDIR)/usr/include
	$(INSTALL) $(HDRS) $(CPMDIR)/lib/include
	$(INSTALL) $(HDRS) $(UNIXDIR)/usr/include
	$(INSTALL) $(HDRS) $(UNIXDIR)/lib/include
	$(INSTALL) sys/*.h $(MXDIR)/usr/include/sys
	$(INSTALL) sys/*.h $(CPMDIR)/lib/include/sys
	$(INSTALL) sys/*.h $(UNIXDIR)/usr/include/sys
	$(INSTALL) sys/*.h $(UNIXDIR)/lib/include/sys

install: target-install

# The .h files here are sources; nothing in CRUFT matches one.
clean:
	rm -f $(CRUFT)

clobber: clean

.PHONY: all install target-install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
