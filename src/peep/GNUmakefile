#
# Makefile for ccc peephole optimizer
#


include ../../GNUmakefile.inc

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

# The compile itself is the default rule in GNUmakefile.inc; this
# just says what every object here depends on besides its source.
$(OBJECTS): $(HEADERS)

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
	$(INSTALL) peep $(HOSTDIR)/lib

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

#
# The XXXXXX check that used to be here was dead and is now redundant.
# Dead: it ran "grep XXXXXX $b.s" while compiling with -k -c, and -k
# keeps the .s in /tmp under the compiler's pid rather than beside the
# source - so it grepped a file that was never there, grep returned
# non-zero, and the marker check silently never fired.
#
# Redundant: c1 counts its markers and fails, so a punted shape stops
# the compile and takes the FAIL branch below on its own.  Dropping -k
# with it stops this rule leaving five files per source in /tmp, which
# is what an unlucky pid collision later trips over.
#
mx-ccc:
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(HOSTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(PEEPDIR) -I$(LIBDIR) -I$(CPPSRC) $(PEEPDIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
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

# The self-hosted pass, installed into destmicronix/lib under the
# name the driver runs it by - the compiler as it runs on the machine
# it targets.  A system root, not a pile of .mx files: bin/lib laid
# out the way Micronix wants them, so the tree can be copied straight
# onto a filesystem and used.
mx-install: mx-ccc
	@mkdir -p $(MXDIR)/lib
	$(INSTALL) mxccc/peep.mx $(MXDIR)/lib/peep

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
	$(INSTALL) comccc/peep.com $(CPMDIR)/bin

.PHONY: mx-ccc all clean clobber install stage1 sizecheck regression mx-install com-ccc com-install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
