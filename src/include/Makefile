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

# One header tree, the same for every target - what differs is
# handled by ifdefs inside the headers, not by two copies that have to
# be kept in step.  It goes in lib/include so the driver finds it
# beside everything else it owns.
target-install:
	@mkdir -p $(MXDIR)/include/sys $(CPMDIR)/include/sys
	@mkdir -p $(MXDIR)/lib/include/sys $(CPMDIR)/lib/include/sys
	@mkdir -p $(UNIXDIR)/lib/include/sys
	cp $(HDRS) $(MXDIR)/include
	cp $(HDRS) $(CPMDIR)/include
	cp sys/*.h $(MXDIR)/include/sys
	cp sys/*.h $(CPMDIR)/include/sys
	cp $(HDRS) $(MXDIR)/lib/include
	cp $(HDRS) $(CPMDIR)/lib/include
	cp $(HDRS) $(UNIXDIR)/lib/include
	cp sys/*.h $(MXDIR)/lib/include/sys
	cp sys/*.h $(CPMDIR)/lib/include/sys
	cp sys/*.h $(UNIXDIR)/lib/include/sys

install: target-install

# The .h files here are sources; nothing in CRUFT matches one.
clean:
	rm -f $(CRUFT)

clobber: clean

.PHONY: all install target-install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
