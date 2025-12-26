#
# Makefile for Ritchie C compiler - Pass 1 (parser/front end)
#

DEST = $(realpath ../../root)

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

ifeq ($(CC),zc3)
CFLAGS = -I../../libsrc/include
LDFLAGS = 
endif

CCC = $(DEST)/bin/ccc
OUTDIR = stage1

ifeq ($(CC),ccc)
CFLAGS = -I../../libsrc/include
endif

# Pass 1 (front end) sources
SOURCES = c00.c c01.c c02.c c03.c c04.c c05.c
OBJECTS = $(SOURCES:.c=.o)

# Suffix rule for .c -> .o
%.o: %.c c0.h
	$(CC) $(CFLAGS) -c $< -o $@

# Default target
all: c0 ppic

# Pass 1 - parser
c0: $(OBJECTS)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) 
else
	ccc -o c0 $(OBJECTS)
endif

# Pretty-print intermediate code
ppic: ppic.c
	$(CC) $(LDFLAGS) -o $@ ppic.c

$(OBJECTS): c0.h

# Paths to compiler passes
CPP = ../../cpp/cpp
C0 = ./c0
C1 = ../pass2/c1
CPPFLAGS = -I. -I../../libsrc/include

# Stage1: run cpp and c0 on each source file
stage1: c0
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-12s" "$$b: "; \
	  $(CPP) $(CPPFLAGS) $$f -o $(OUTDIR)/$$b 2>$(OUTDIR)/$$b.err && \
	  $(C0) $(OUTDIR)/$$b.x $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 2>>$(OUTDIR)/$$b.err && \
	  { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || { echo "FAIL"; cat $(OUTDIR)/$$b.err; }; \
	done

# Stage1-ccc: compile with cross ccc to Z80 .o files
CCCFLAGS = -I.
stage1-ccc:
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) $(CCCFLAGS) -k -P -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.i $$b.x $$b.1 $$b.2 $$b.pp $$b.s $$b.o $(OUTDIR) 2>/dev/null || true; \
	done

# Size report for stage1 objects
STAGE1_OBJS = $(SOURCES:.c=.o)
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(DEST)/bin/wssize $(addprefix $(OUTDIR)/,$(STAGE1_OBJS)) | \
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
	cp c0 ppic $(DEST)/bin

.PHONY: all clean clobber stage1 stage1-ccc install sizefile

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
