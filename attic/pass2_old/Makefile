#
# Makefile for cc2 (pass 2 - code generator)
#
ROOTDIR = ../root

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement \
	-Werror=implicit-function-declaration -Wall -Werror
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

ifeq ($(CC),ccc)
CFLAGS = -I.
CCC1 = $(ROOTDIR)/bin/cc1
CCC2 = $(ROOTDIR)/bin/cc2
ASZ = $(ROOTDIR)/bin/asz
OUTDIR = ../stage1
CCC = $(ROOTDIR)/bin/ccc
endif

SOURCES = pass2.c astio.c parseast.c codegen.c dumpast.c \
	emithelper.c emitexpr.c emitops.c emit.c

OBJECTS = $(SOURCES:.c=.o)

HEADERS = cc2.h astio.h emithelper.h trace2.h

GENERATED = trace2.h tracetags.c

# Default target depends on CC
ifeq ($(CC),gcc)
all: cc2
else ifeq ($(CC),ccc)
all: stage1
endif

cc2: $(OBJECTS) util.o
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) util.o

$(OBJECTS): $(HEADERS)

# util.c comes from parent
util.o: ../util.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Stage1: compile with cross ccc to Z80 .o files
stage1: $(GENERATED)
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) $(CFLAGS) -k -P -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.s $$b.o $$b.ast $$b.pp $(OUTDIR) 2>/dev/null ; \
	done

# Size report for stage1 objects
STAGE1_OBJS = $(SOURCES:.c=.o)
sizefile:
	$(MAKE) stage1 CC=ccc
	@$(ROOTDIR)/bin/wssize $(addprefix ../stage1/,$(STAGE1_OBJS)) | \
	awk \
                '{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
                END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "pass2 total" }' \
         > sizefile

# Generated file rules
trace2.h tracetags.c: ./makedebug2.sh
	./makedebug2.sh

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) util.o cc2 $(GENERATED)

clobber: clean
	rm -f ./cc2

install: cc2
	cp cc2 $(ROOTDIR)/bin

.PHONY: all clean regen stage1 install clobber

# vim: tabstop=4 shiftwidth=4 noexpandtab:
