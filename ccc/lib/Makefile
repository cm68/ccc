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

ifeq ($(CC),sdcc)
CFLAGS = -mz80 -DCCC -I../../libsrc/include
endif

SOURCES = libutil.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = libutil.h

all: libccc.a

%.o: %.c $(HEADERS)
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

.PHONY: all clean install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
