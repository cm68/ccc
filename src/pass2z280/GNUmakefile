#
# Makefile for ccc compiler pass 2
#


include ../../GNUmakefile.inc

CC = gcc

LIBCCC = ../ccclib/libccc.a

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -std=gnu89 -Werror=declaration-after-statement
DEBUG = -ggdb3 -O0
# -I../cpp for lexeme.h, which pass1, pass2 and astpp all read: the
# token numbers cpp emits are the ones they consume.  It used to be
# reached by spelling the directory in the #include, which is a thing
# a compiler that has to build on CP/M cannot do - there are no
# directories there, and the sources have to be able to say just
# "lexeme.h" and be told where to look.
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I. -I../ccclib -I../cpp
LDFLAGS = -m32 $(DEBUG)
endif

INC = -I../include -I../ccclib -I../cpp
CCC = $(HOSTDIR)/bin/ccc

ASTPP = ../astpp
CPP = ../cpp/cpp
C0 = ../pass1/c0
XDUMP = ../cpp/xdump

OUTDIR = stage1

ifdef TRACE
SETX = set -x
else
SETX = @ true
endif

ifeq ($(CC),ccc)
CFLAGS = -I. -I../ccclib -I../cpp
endif

# Z80 compiler used by `sizecheck`.  ccc is the only one now; zc3
# was the Hi-Tech bootstrap.
ZCC = ccc
# -O, because -O is what ships.  A size report taken without the
# peephole measures a binary nobody builds, and it is the number that
# decides whether a pass fits.
ZCFLAGS = -O -DCCC $(INC)
ZCC_ASM = -s
ZCC_ASMEXT = s

#
# Where a size build lands.  Still keyed to $(ZCC): when there were
# two compilers, one shared directory meant whatever ran last won,
# and reading zc3's objects as ccc's was easy to do and hard to see.
#
SIZEDIR = sizecheck-$(ZCC)

SOURCES = pass2.c astio.c parseast.c expr.c rewrite.c lower.c rules.c \
	ruleidx.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = pass2.h expr.h opcodes.h rules.h

ifeq ($(DEFINES),-DDEBUG)
DBGFILES = dbgtags.c debug.h
else
DBGFILES =
endif

GENERATED = dbgtags.c debug.h ruleidx.c mkruleidx

all: c1280

# Generated file rules
dbgtags.c debug.h: ./makedebug.sh $(SOURCES)
	./makedebug.sh

#
# The rule index, written from the rule table by a program that links
# against it.  Nothing here is maintained by hand: mkruleidx walks
# rules[] and refuses to emit an index at all if any opcode's rules
# are not one unbroken run, so a rule filed in the wrong place stops
# the build instead of never being tried.
#
# ruleidx.c names rules.c as well as the generator.  Editing the table
# does relink mkruleidx by way of rules.o and would regenerate the
# index for that reason alone, but a build should say what it depends
# on rather than arrive at it: the index is a function of the table,
# and that is the edge worth writing down.
#
mkruleidx: mkruleidx.c rules.o $(HEADERS)
	$(CC) $(CFLAGS) -I. -I../ccclib -o mkruleidx mkruleidx.c rules.o

ruleidx.c: mkruleidx rules.c
	./mkruleidx > ruleidx.c || (rm -f ruleidx.c; exit 1)

# The compile itself is the default rule in GNUmakefile.inc; this
# just says what every object here depends on besides its source.
$(OBJECTS): $(HEADERS)

pass2.o: pass2.c $(HEADERS) $(DBGFILES)
	$(CC) $(CFLAGS) -c $< -o $@

c1280: $(OBJECTS) $(LIBCCC)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBCCC)
else
	ccc -o c1280 $(OBJECTS) $(LIBCCC)
endif

$(LIBCCC):
	$(MAKE) -C ../ccclib

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) c1280 $(GENERATED) $(CRUFT) $(CRUFTASM)

clobber: clean
	rm -f ./c1280 $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

install: c1280
	@mkdir -p $(HOSTLIBEXEC)
	$(INSTALL) c1280 $(HOSTLIBEXEC)

# Stage1: run cpp, c0, and c1280 on each source file
stage1: c1280
	mkdir -p $(OUTDIR)
	$(SETX) ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(OUTDIR)/$$b.err ; \
		printf "%-12s" "$$b: "; \
		if ! $(CPP) -DCCC $(INC) -p -o $(OUTDIR)/$$b $$f 2> $$err; then \
			echo "FAIL (cpp)"; \
		elif ! $(C0) $(OUTDIR)/$$b.x $(OUTDIR)/$$b.ast $(OUTDIR)/$$b.dat 2>> $$err; then \
			echo "FAIL (c0)"; \
		elif ! $(ASTPP) $(OUTDIR)/$$b.ast > $(OUTDIR)/$$b.pp 2>> $$err; then \
			echo "FAIL (astpp)"; \
		elif ! ./c1280 $(OUTDIR)/$$b.ast $(OUTDIR)/$$b.dat $(OUTDIR)/$$b.s 2>> $$err; then \
			echo "FAIL (c1280)"; \
		else \
			echo "ok"; \
		fi \
	done

sizecheck: $(GENERATED)
	mkdir -p $(SIZEDIR)
	$(SETX) ; export ROOTDIR=$(TOP) ; o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(SIZEDIR)/$$b.err ; \
		o="$$o $(SIZEDIR)/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		$(ZCC) $(ZCC_ASM) $(ZCFLAGS) $$f >$$err 2>&1 && mv $$b.$(ZCC_ASMEXT) $(SIZEDIR) ; \
		$(ZCC) -c $(ZCFLAGS) $$f >>$$err 2>&1 && mv $$b.o $(SIZEDIR)/$$b.o && \
		{ echo "ok"; } || { echo "FAIL"; } \
	done ; \
	$(SIZE) $$o | awk '{t+=$$1;d+=$$2;b+=$$3;a+=$$4} END {print t,d,b,a}' | tee -a sizefile ; \
	$(LD) -r -o $(SIZEDIR)/c1280 $(MXDIR)/lib/crt0.o $$o -L$(MXDIR)/lib -lccc -lc -lu ; \
	$(SIZE) $(SIZEDIR)/c1280 | tee -a sizefile


# Individual stage1 file rules
.SECONDARY:

$(CPP):
	$(MAKE) -C ../cpp cpp

$(C0):
	$(MAKE) -C ../pass1 c0

$(ASTPP):
	$(MAKE) -C .. astpp

$(OUTDIR)/%.x: %.c $(HEADERS) $(CPP)
	@mkdir -p $(OUTDIR)
	$(CPP) -DCCC $(INC) -p -o $(OUTDIR)/$* $<

$(OUTDIR)/%.ast $(OUTDIR)/%.dat: $(OUTDIR)/%.x $(C0)
	$(C0) $< $(OUTDIR)/$*.ast $(OUTDIR)/$*.dat

$(OUTDIR)/%.pp: $(OUTDIR)/%.ast $(ASTPP)
	$(ASTPP) $< > $@

$(OUTDIR)/%.s: $(OUTDIR)/%.ast $(OUTDIR)/%.dat c1280
	./c1280 $(OUTDIR)/$*.ast $(OUTDIR)/$*.dat $@

regression:
	../../tests/regress.sh --filter ccc/pass2/ $(REGRESS_FLAGS)

# Which rewrite rules ever match.  Needs c1280 built with -DDEBUG, which
# is the default here - the counters are host-side bookkeeping and the
# z80 build has never seen them.  COVERFLAGS=-v also lists the hottest.
coverage: c1280
	python3 ../../tests/rulecover.py $(COVERFLAGS)

# Native Z80 c1280 built by ccc itself, for the self-build run: the
# other two passes have had differential builds for a while, and c1280
# was the one still only proven to compile, never to run.
P2DIR = $(abspath .)
LIBDIR = $(abspath ../ccclib)
CPPSRC = $(abspath ../cpp)
LIBSRCINC = $(abspath ../include)
# The peephole is not optional.  It is a few percent of text on
# ccc-generated code - 610 bytes off cpp, 1109 off c0, 1665 off c1280 -
# and the passes are close enough to the ceiling that a few percent is
# the difference between a source compiling on the Z80 and not.  Build
# it the way it is meant to run; CCCOPT=  turns it off for a
# differential.
CCCOPT = -O


mx-ccc: $(GENERATED)
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(HOSTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(P2DIR) -I$(LIBDIR) -I$(CPPSRC) $(P2DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(LD) -s -o c1280.mx -Ttext=0x100 \
		$(MXDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(MXDIR)/lib -lccc -lc -lu -lc
	@$(SIZE) mxccc/c1280.mx

# The self-hosted pass, installed into destmicronix/lib under the
# name the driver runs it by - the compiler as it runs on the machine
# it targets.  A system root, not a pile of .mx files: bin/lib laid
# out the way Micronix wants them, so the tree can be copied straight
# onto a filesystem and used.
mx-install: mx-ccc
	@mkdir -p $(MXDIR)/lib
	$(INSTALL) mxccc/c1280.mx $(MXDIR)/lib/c1280

# The CP/M image.  Same sources, same driver, -m cpm: the runtime
# comes out of cpm/ instead of micronix/, crtcpm starts it, libcpm
# turns the calls into BDOS calls, and the driver strips the object
# header so what is left is what CP/M loads at 0x100.
com-ccc: $(GENERATED)
	@mkdir -p comccc
	@set -e; cd comccc ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -m cpm -c -DCCC -DCPM -i$(CPMDIR)/include \
			-I$(P2DIR) -I$(LIBDIR) -I$(CPPSRC) $(P2DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd comccc && $(CCC) -m cpm -S -o c1280.com $$(ls *.o 2>/dev/null) \
		-L$(CPMDIR)/lib -lccc > /dev/null
	@echo "c1280.com: `wc -c < comccc/c1280.com` bytes"

com-install: com-ccc
	@mkdir -p $(CPMDIR)/bin
	$(INSTALL) comccc/c1280.com $(CPMDIR)/bin

# The manual goes with what it documents.  man(1) on the target looks
# in /usr/man/man<n> - the path is in the binary - so the staged tree
# carries the pages under usr, the same as the headers.
#
# They live in man1/ rather than here, and have to: ".1" is pass1's
# AST suffix and is swept by CRUFT, so a page beside the source would
# be removed by the next "make clean".
MANPAGES = man1/c1280.1

man-install:
	@mkdir -p $(MXDIR)/usr/man/man1
	$(INSTALL) $(MANPAGES) $(MXDIR)/usr/man/man1

.PHONY: all clean regen stage1 install clobber sizefile sizecheck regression coverage mx-ccc mx-install com-ccc com-install man-install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
