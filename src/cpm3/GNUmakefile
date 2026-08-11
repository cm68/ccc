#
# cpm3 - a CP/M 3 machine big enough to run this compiler
#
# A host tool like the rest of src/tools, and it installs beside them
# in desthost/bin.  It is here rather than there because it is a machine,
# not part of the toolchain: nothing in a build reaches for it, and
# the things that do - the self-compile check - name it directly.
#

include ../../GNUmakefile.inc

CC = gcc

# z80.h is vendored and does not compile clean under the warning set
# the rest of the tree uses, so the emulator gets its own rule.  Our
# own code is held to the same standard as everything else.
WARNS = -Wall -Werror -Wno-unused-parameter
CFLAGS = -g -O2 $(WARNS)
EMUFLAGS = -g -O2 -w

OBJECTS = cpm3.o bdos.o
HEADERS = cpm3.h

all: cpm3

# cpm3.c is the only file that instantiates z80.h, and it is the one
# that has to tolerate it.
cpm3.o: cpm3.c $(HEADERS) z80.h
	$(CC) $(EMUFLAGS) -c cpm3.c -o $@

bdos.o: bdos.c $(HEADERS)
	$(CC) $(CFLAGS) -c bdos.c -o $@

cpm3: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS)

# Compile a source with the host compiler and with the CP/M passes
# under this machine, and compare.  Needs "make && make cpm" first:
# the .com images are what it runs.
selfcheck: cpm3
	@sh selfcheck.sh

install: cpm3
	@mkdir -p $(HOSTDIR)/bin
	$(INSTALL) cpm3 $(HOSTDIR)/bin

clean:
	rm -f $(OBJECTS) cpm3 $(CRUFT) $(CRUFTASM)

clobber: clean
	rm -f $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

.PHONY: all selfcheck install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
