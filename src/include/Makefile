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

target-install:
	@mkdir -p $(MXDIR)/include/sys $(CPMDIR)/include/sys
	cp $(HDRS) $(MXDIR)/include
	cp $(HDRS) $(CPMDIR)/include
	cp sys/*.h $(MXDIR)/include/sys
	cp sys/*.h $(CPMDIR)/include/sys

install: target-install

# The .h files here are sources; nothing in CRUFT matches one.
clean:
	rm -f $(CRUFT)

clobber: clean

.PHONY: all install target-install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
