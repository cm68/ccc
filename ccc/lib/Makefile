#
# Makefile for libccc.a - shared utilities for ccc compiler
#

DEST = $(abspath ../../root)

include ../../cruft.mk

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -Werror=declaration-after-statement
DEBUG = -g -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS)
LIB = ar
LIBFLAGS = rcs
endif

SOURCES = libutil.c
OBJECTS = $(SOURCES:.c=.o)

# host builds get permalloc from its single source in libsrc/libc;
# z80 links resolve it from libc.a, so keep it out of the z80 libccc
ifeq ($(CC),gcc)
OBJECTS += permalloc.o
endif

HEADERS = libutil.h

# The z80 archive is built into a ccc/ build area that installs to
# root/lib/ccc - the same shape as libsrc.  There was a zc3/ area
# beside it built by Hi-Tech C while this compiler's runtime was
# still being proven; that bootstrap is over.
#
# all is the HOST archive only.  The z80 area is compiled by the
# installed compiler and so cannot be built until it is in root/bin -
# the top-level Makefile runs it as a second phase.
all: libccc.a

z80: ccc

ccc:
	@$(MAKE) TC=$@ $@/libccc.a

ifneq ($(TC),)

#
# The ccc area is rebuilt when the compiler itself changes, for the
# same reason libsrc/libc is: these objects exist to be run, and a
# codegen change that leaves a stale one behind reads as a change that
# did not work.  This rule was missing it, and the 32-bit convention
# change is how that showed - libutil.o went on calling lld, arelop,
# aldiv and almod, so every image linked BOTH runtimes and c0 grew
# five hundred bytes that looked like they came from the new one.
#
ZCC = $(DEST)/bin/ccc -O
CCDEP = $(DEST)/bin/c1 $(DEST)/bin/c0 $(DEST)/bin/cpp $(DEST)/bin/peep

$(TC)/libutil.o: libutil.c $(HEADERS) $(CCDEP)
	@mkdir -p $(TC)
	cd $(TC) && $(ZCC) -I.. -I../../../libsrc/include -c ../libutil.c

$(TC)/libccc.a: $(TC)/libutil.o
	rm -f $@
	$(DEST)/bin/wslib -cr $@ $(TC)/libutil.o


endif

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

permalloc.o: ../../libsrc/libc/permalloc.c
	$(CC) $(CFLAGS) -c $< -o $@

libccc.a: $(OBJECTS)
	$(LIB) $(LIBFLAGS) $@ $(OBJECTS)

# zc3/ and ccc/ are this directory's build areas, not CRUFTDIRS
# entries: the names are shared with the phony targets that fill them.
clean:
	rm -f $(OBJECTS) libccc.a $(CRUFT) $(CRUFTASM)
	rm -rf zc3 ccc

clobber: clean
	rm -f $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

install: z80
	@mkdir -p $(DEST)/lib/ccc
	cp ccc/libccc.a $(DEST)/lib/ccc/

regression:
	../../tests/regress.sh --filter ccc/lib/ $(REGRESS_FLAGS)

.PHONY: all z80 ccc clean clobber install regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
