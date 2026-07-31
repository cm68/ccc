#
# Makefile for ccc peephole optimizer
#

DEST = $(realpath ../../root)

CC = gcc

ifeq ($(CC),gcc)
WARNS = -Wall -Werror -std=gnu89
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

ifeq ($(CC),zc3)
CFLAGS = -I../../libsrc/include -I../lib
LDFLAGS =
endif

ifeq ($(CC),ccc)
CFLAGS = -I. -I../lib
endif

INC = -I../../libsrc/include -I../lib
ROOTDIR = $(realpath ../../root)

OUTDIR = stage1

# Z80 compiler used by `sizecheck` (ccc | zc3)
ZCC = ccc
ZCFLAGS = -DCCC $(INC)
ifeq ($(ZCC),zc3)
ZCC_ASM = -S
ZCC_ASMEXT = as
endif
ifeq ($(ZCC),ccc)
ZCC_ASM = -s
ZCC_ASMEXT = s
endif
SIZEDIR = sizecheck-$(ZCC)

SOURCES = peep.c regs.c rules.c pool.c
OBJECTS = $(SOURCES:.c=.o)
HEADERS = peep.h

all: peep

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

peep: $(OBJECTS)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
else
	ccc -o peep $(OBJECTS)
endif

clean:
	rm -f $(OBJECTS) peep *.o *.s *.1 *.2 *.x *.i core vgcore.*

clobber: clean
	rm -f ./peep
	rm -rf sizecheck-* stage1

install: peep
	cp peep $(DEST)/bin

# Stage1: build each source through the compiler being developed
stage1: peep
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: "; \
		if $(ROOTDIR)/bin/ccc -s -DCCC $(INC) -I. $$f \
			> $(OUTDIR)/$$b.err 2>&1 && [ -f $$b.s ]; then \
			mv $$b.s $(OUTDIR)/ ; echo "ok"; \
		else echo "FAIL"; cat $(OUTDIR)/$$b.err; fi \
	done

sizecheck:
	mkdir -p $(SIZEDIR)
	@export ROOTDIR=$(ROOTDIR) ; o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		o="$$o $(SIZEDIR)/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		$(ZCC) -c $(ZCFLAGS) $$f > $(SIZEDIR)/$$b.err 2>&1 \
			&& mv $$b.o $(SIZEDIR)/$$b.o \
			&& echo "ok" || echo "FAIL" ; \
	done ; \
	wssize $$o

regression:
	../../tests/regress.sh --filter ccc/peep/ $(REGRESS_FLAGS)

.PHONY: all clean clobber install stage1 sizecheck regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
