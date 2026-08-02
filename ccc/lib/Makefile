#
# Makefile for libccc.a - shared utilities for ccc compiler
#

DEST = $(realpath ../../root)

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

ifeq ($(TC),ccc)
ZCC = $(DEST)/bin/ccc -O
else
ZCC = zc3
endif

$(TC)/libutil.o: libutil.c $(HEADERS)
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

clean:
	rm -f $(OBJECTS) libccc.a
	rm -rf zc3 ccc

install: all
	@mkdir -p $(DEST)/lib/zc3 $(DEST)/lib/ccc
	cp zc3/libccc.a $(DEST)/lib/zc3/
	cp ccc/libccc.a $(DEST)/lib/ccc/

regression:
	../../tests/regress.sh --filter ccc/lib/ $(REGRESS_FLAGS)

.PHONY: all z80 zc3 ccc clean install regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
