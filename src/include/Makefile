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

include ../../tree.mk
include ../../cruft.mk

HDRS = $(wildcard *.h)

all:

# One header tree, the same for every target - what differs is handled
# by ifdefs inside the headers, not by two copies that have to be kept
# in step.
#
# A Unix keeps its headers in /usr/include and so does Micronix, so
# that is where the driver looks: libdir/../usr/include.  CP/M has no
# /usr and its install is flat, so there they sit beside the passes.
target-install:
	@mkdir -p $(MXDIR)/include/sys $(CPMDIR)/include/sys
	@mkdir -p $(MXDIR)/usr/include/sys $(CPMDIR)/lib/include/sys
	@mkdir -p $(UNIXDIR)/usr/include/sys
	cp $(HDRS) $(MXDIR)/include
	cp $(HDRS) $(CPMDIR)/include
	cp sys/*.h $(MXDIR)/include/sys
	cp sys/*.h $(CPMDIR)/include/sys
	cp $(HDRS) $(MXDIR)/usr/include
	cp $(HDRS) $(CPMDIR)/lib/include
	cp $(HDRS) $(UNIXDIR)/usr/include
	cp sys/*.h $(MXDIR)/usr/include/sys
	cp sys/*.h $(CPMDIR)/lib/include/sys
	cp sys/*.h $(UNIXDIR)/usr/include/sys

install: target-install

# The .h files here are sources; nothing in CRUFT matches one.
clean:
	rm -f $(CRUFT)

clobber: clean

.PHONY: all install target-install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
