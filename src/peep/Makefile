#
# Makefile for ccc peephole optimizer
#


include ../../tree.mk
include ../../cruft.mk

CC = gcc

ifeq ($(CC),gcc)
WARNS = -Wall -Werror -std=gnu89 -Werror=declaration-after-statement
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

ifeq ($(CC),ccc)
CFLAGS = -I. -I../ccclib
endif

INC = -I../include -I../ccclib

OUTDIR = stage1

# Z80 compiler used by `sizecheck`.  ccc is the only one now; zc3
# was the Hi-Tech bootstrap.
ZCC = ccc
ZCFLAGS = -DCCC $(INC)
ZCC_ASM = -s
ZCC_ASMEXT = s
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
	rm -f $(OBJECTS) peep $(CRUFT) $(CRUFTASM)

clobber: clean
	rm -f ./peep $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

install: peep
	cp peep $(UNIXDIR)/bin

# Stage1: build each source through the compiler being developed
stage1: peep
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: "; \
		if $(CCC) -s -DCCC $(INC) -I. $$f \
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
PEEPDIR = $(abspath .)
LIBDIR = $(abspath ../ccclib)
CPPSRC = $(abspath ../cpp)
LIBSRCINC = $(abspath ../include)

mx-ccc:
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(UNIXDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -k -c -DCCC -i$(LIBSRCINC) \
			-I$(PEEPDIR) -I$(LIBDIR) -I$(CPPSRC) $(PEEPDIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			if grep -q XXXXXX $$b.s; then \
				echo "FAIL: XXXXXX marker - pass2 punted on a shape"; \
				grep XXXXXX $$b.s; exit 1; \
			fi; \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(LD) -s -o peep.mx -Ttext=0x100 \
		$(MXDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(MXDIR)/lib -lccc -lc -lu -lc
	@$(SIZE) mxccc/peep.mx

sizecheck:
	mkdir -p $(SIZEDIR)
	@export ROOTDIR=$(TOP) ; o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		o="$$o $(SIZEDIR)/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		$(ZCC) -c $(ZCFLAGS) $$f > $(SIZEDIR)/$$b.err 2>&1 \
			&& mv $$b.o $(SIZEDIR)/$$b.o \
			&& echo "ok" || echo "FAIL" ; \
	done ; \
	$(SIZE) $$o

regression:
	../../tests/regress.sh --filter ccc/peep/ $(REGRESS_FLAGS)

# The self-hosted binary, installed into micronix/bin - the compiler
# as it runs on the machine it targets.
mx-install: mx-ccc
	@mkdir -p $(MXDIR)/bin
	cp mxccc/peep.mx $(MXDIR)/bin

# The CP/M image.  Same sources, same driver, -m cpm: the runtime
# comes out of cpm/ instead of micronix/, crtcpm starts it, libcpm
# turns the calls into BDOS calls, and the driver strips the object
# header so what is left is what CP/M loads at 0x100.
com-ccc: 
	@mkdir -p comccc
	@set -e; cd comccc ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -m cpm -c -DCCC -DCPM -i$(CPMDIR)/include \
			-I$(PEEPDIR) -I$(LIBDIR) -I$(CPPSRC) $(PEEPDIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd comccc && $(CCC) -m cpm -S -o peep.com $$(ls *.o 2>/dev/null) \
		-L$(CPMDIR)/lib -lccc > /dev/null
	@echo "peep.com: `wc -c < comccc/peep.com` bytes"

com-install: com-ccc
	@mkdir -p $(CPMDIR)/bin
	cp comccc/peep.com $(CPMDIR)/bin

.PHONY: mx-ccc all clean clobber install stage1 sizecheck regression mx-install com-ccc com-install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
