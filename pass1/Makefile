#
# Makefile for Ritchie C compiler - Pass 1 (parser/front end)
#

ROOTDIR = ../root

CC = gcc

ifeq ($(CC),gcc)
WARNS = -Wno-implicit-function-declaration -Wno-implicit-int \
	-Wno-return-type -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast \
	-Wno-builtin-declaration-mismatch -std=gnu89
DEBUG = -ggdb3 -O0
# -fcommon needed for K&R style globals in headers
CFLAGS = -m32 $(DEBUG) $(WARNS) -fcommon -I.
LDFLAGS = -m32 $(DEBUG)
endif

CCC = $(ROOTDIR)/bin/ccc
OUTDIR = stage1

ifeq ($(CC),ccc)
CFLAGS = -I.
endif

# Pass 1 (front end) sources
SOURCES = c00.c c01.c c02.c c03.c c04.c c05.c
OBJECTS = $(SOURCES:.c=.o)

# Default target
all: c0 ppic

# Pass 1 - parser
c0: $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)

# Pretty-print intermediate code
ppic: ppic.c
	$(CC) $(LDFLAGS) -o $@ ppic.c

$(OBJECTS): c0.h

# Stage1: compile with cross ccc to Z80 .o files
CCCFLAGS = -I.
stage1:
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) $(CCCFLAGS) -k -P -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.i $$b.x $$b.s $$b.o $$b.ast $$b.pp $(OUTDIR) 2>/dev/null ; \
	done

# Size report for stage1 objects
STAGE1_OBJS = $(SOURCES:.c=.o)
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(ROOTDIR)/bin/wssize $(addprefix $(OUTDIR)/,$(STAGE1_OBJS)) | \
	awk \
		'{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "pass1 total" }' \
	 > sizefile

clean:
	rm -f $(OBJECTS)

clobber: clean
	rm -f c0 ppic
	rm -rf $(OUTDIR)

install: c0 ppic
	cp c0 ppic $(ROOTDIR)/bin

.PHONY: all clean clobber stage1 install sizefile

#
# vim: tabstop=4 shiftwidth=4 expandtab:
#
