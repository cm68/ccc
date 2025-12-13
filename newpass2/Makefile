#
# Makefile for newpass2 (clean-sheet code generator)
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
CCC = $(ROOTDIR)/bin/ccc
OUTDIR = ../stage1
endif

SOURCES = cc2.c emit.c
OBJECTS = $(SOURCES:.c=.o)
HEADERS = cc2.h

# Default target depends on CC
ifeq ($(CC),gcc)
all: cc2
else ifeq ($(CC),ccc)
all: stage1
endif

cc2: $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)

$(OBJECTS): $(HEADERS)

# Stage1: compile with cross ccc to Z80 .o files
stage1:
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) $(CFLAGS) -k -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && echo "ok" || echo "FAIL"; \
	  ../astpp.py $$b.ast > $$b.pp ; \
	  mv $$b.s $$b.o $$b.ast $$b.pp $(OUTDIR) 2>/dev/null ; \
	done

# Size report for stage1 objects
STAGE1_OBJS = $(SOURCES:.c=.o)
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(ROOTDIR)/bin/wssize $(addprefix ../stage1/,$(STAGE1_OBJS)) | \
	awk \
		'{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "newpass2 total" }' \
	 > sizefile

clean:
	rm -f $(OBJECTS) cc2

# Test against a single file
test: cc2
	./cc2 ../tests/expr.ast

.PHONY: all clean test stage1 sizefile
