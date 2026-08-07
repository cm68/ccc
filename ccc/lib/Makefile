#
# Makefile for libccc.a - shared utilities for ccc compiler
#

DEST = $(realpath ../../root)

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

# The z80 archive is built twice, once per compiler, into zc3/ and
# ccc/ build areas that install to root/lib/zc3 and root/lib/ccc -
# the same split as libsrc.  A zc3-linked image takes everything from
# lib/zc3, a ccc-linked one from lib/ccc.
all: libccc.a z80

z80: zc3 ccc

zc3 ccc:
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
ifeq ($(TC),ccc)
ZCC = $(DEST)/bin/ccc -O
CCDEP = $(DEST)/bin/c1 $(DEST)/bin/c0 $(DEST)/bin/cpp $(DEST)/bin/peep
else
ZCC = zc3
CCDEP =
endif

$(TC)/libutil.o: libutil.c $(HEADERS) $(CCDEP)
	@mkdir -p $(TC)
	cd $(TC) && $(ZCC) -I.. -I../../../libsrc/include -c ../libutil.c

$(TC)/libccc.a: $(TC)/libutil.o
	rm -f $@
	wslib -cr $@ $(TC)/libutil.o


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

install: all
	@mkdir -p $(DEST)/lib/zc3 $(DEST)/lib/ccc
	cp zc3/libccc.a $(DEST)/lib/zc3/
	cp ccc/libccc.a $(DEST)/lib/ccc/

regression:
	../../tests/regress.sh --filter ccc/lib/ $(REGRESS_FLAGS)

.PHONY: all z80 zc3 ccc clean clobber install regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
