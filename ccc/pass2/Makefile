#
# Makefile for ccc compiler pass 2
#

DEST = $(realpath ../../root)

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -std=gnu89
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

ifeq ($(CC),zc3)
CFLAGS = -I../../libsrc/include
LDFLAGS =
endif

CPP = ../../cpp/cpp
INC = -I../../libsrc/include
CCC = $(DEST)/bin/ccc
XDUMP = ../../cpp/xdump
ASTPP = ~/src/ccc/ccc/astpp

OUTDIR = stage1

ifdef TRACE
SETX = set -x ;
else
SETX = @
endif

ifeq ($(CC),ccc)
CFLAGS = -I.
endif

SOURCES = cc2.c astio.c emit.c codegen.c parseast.c \
	emitexpr.c emitcmp.c emitops.c emitincdec.c emitpat.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = cc2.h

all: c1

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

c1: $(OBJECTS)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
else
	ccc -o c1 $(OBJECTS)
endif

clean:
	rm -f $(OBJECTS) c1

clobber: clean
	rm -f ./c1

install: c1
	cp c1 $(DEST)/bin

# Stage1: run cpp, c0, and c1 on each source file
stage1: c1
	mkdir -p $(OUTDIR)
	$(SETX) for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(OUTDIR)/$$b.err ; \
		printf "%-12s" "$$b: "; \
		$(CPP) -DCCC $(INC) -o $(OUTDIR)/$$b $$f 2> $$err && \
		$(XDUMP) $(OUTDIR)/$$b.x > $(OUTDIR)/$$b.xd 2>> $err && \
		../pass1/c0 $(OUTDIR)/$$b.x $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 2>> $$err && \
		$(ASTPP) $(OUTDIR)/$$b.1 > $(OUTDIR)/$$b.pp 2>> $$err && \
      	{ echo "ok"; rm -f $$err; } || { echo "FAIL"; } \
    done

.PHONY: all clean stage1 install clobber

# vim: tabstop=4 shiftwidth=4 noexpandtab:
