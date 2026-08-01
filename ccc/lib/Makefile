#
# Makefile for libccc.a - shared utilities for ccc compiler
#

DEST = $(realpath ../../root)

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror
DEBUG = -g -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS)
LIB = ar
LIBFLAGS = rcs
endif

ifeq ($(CC),zc3)
CFLAGS = -I../../libsrc/include
LIB = wslib
LIBFLAGS = -cr
endif


SOURCES = libutil.c
OBJECTS = $(SOURCES:.c=.o)

# host builds get permalloc from its single source in libsrc/libc;
# z80 links resolve it from libc.a, so keep it out of the z80 libccc
ifeq ($(CC),gcc)
OBJECTS += permalloc.o
endif

HEADERS = libutil.h

all: libccc.a

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

permalloc.o: ../../libsrc/libc/permalloc.c
	$(CC) $(CFLAGS) -c $< -o $@

libccc.a: $(OBJECTS)
	$(LIB) $(LIBFLAGS) $@ $(OBJECTS)

clean:
	rm -f $(OBJECTS) libccc.a

ifeq ($(CC),gcc)
install: libccc.a
else
install: libccc.a
	cp libccc.a $(DEST)/lib
endif

regression:
	../../tests/regress.sh --filter ccc/lib/ $(REGRESS_FLAGS)

.PHONY: all clean install regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
