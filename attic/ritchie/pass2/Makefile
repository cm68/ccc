#
# Makefile for Ritchie C compiler - Pass 2 (Z80 code generator)
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
CFLAGS = -I.
endif

# Pass 2 (code generator) sources
SOURCES = c10.c c11.c c12.c c13.c table.c
OBJECTS = $(SOURCES:.c=.o)

# Suffix rule for .c -> .o
%.o: %.c c0.h
	$(CC) $(CFLAGS) -c $< -o $@

ALL_SOURCES = $(SOURCES) cvopt.c
ALL_OBJECTS = $(OBJECTS) table.o

# Default target
all: c1

# Pass 2 - code generator
c1: $(OBJECTS)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
else
	ccc -o c1 $(OBJECTS)
endif

$(OBJECTS): c1.h

table.c: optable cvopt
	./cvopt < optable > table.c

table.o: table.c
	$(CC) $(CFLAGS) -c -o $@ table.c

cvopt: cvopt.c
	cc -o cvopt cvopt.c

# Stage1: compile with cross ccc to Z80 .o files
CCCFLAGS = -I. -i$(ROOTDIR)/usr/include
stage1:
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) $(CCCFLAGS) -k -P -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.i $$b.x $$b.1 $$b.2 $$b.pp $$b.s $$b.o $(OUTDIR) 2>/dev/null || true; \
	done

# Size report for stage1 objects
STAGE1_OBJS = $(ALL_SOURCES:.c=.o)
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(DEST)/bin/wssize $(addprefix $(OUTDIR)/,$(STAGE1_OBJS)) | \
	awk \
		'{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "pass2 total" }' \
	 > sizefile

clean:
	rm -f $(ALL_OBJECTS) cvopt junk.c junk.s *.o *.1 *.2 *.x *.i *.pp

clobber: clean
	rm -f c1
	rm -rf $(OUTDIR)

install: c1
	cp c1 $(DEST)/bin

.PHONY: all clean clobber stage1 install sizefile

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
