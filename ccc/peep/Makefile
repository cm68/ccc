#
# Makefile for ccc peephole optimizer
#

DEST = $(realpath ../../root)

CC = gcc

ifeq ($(CC),gcc)
WARNS = -Wall -Werror -std=gnu89 -Werror=declaration-after-statement
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

# The peephole built by ccc itself, for the simulator - the same
# differential the other passes have.  It runs on the Z80 too, over
# the assembly c1 emits, so its own footprint is as much a part of
# self-hosting as theirs.  CCCOPT=-O by default: peep optimises its
# own build like everything else.
CCCOPT = -O
PEEPDIR = $(realpath .)
LIBDIR = $(realpath ../lib)
LIBSRCINC = $(realpath ../../libsrc/include)

mx-ccc:
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(ROOTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(ROOTDIR)/bin/ccc $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(PEEPDIR) -I$(LIBDIR) $(PEEPDIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(ROOTDIR)/bin/wsld -s -o peep.mx -Ttext=0x100 \
		$(ROOTDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(ROOTDIR)/lib/ccc -lccc -lc -lu -lc
	@$(ROOTDIR)/bin/wssize mxccc/peep.mx

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

.PHONY: mx-ccc all clean clobber install stage1 sizecheck regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
