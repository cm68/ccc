#
# Makefile for ccc compiler pass 2
#


include ../../tree.mk
include ../../cruft.mk

CC = gcc

LIBCCC = ../ccclib/libccc.a

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -std=gnu89 -Werror=declaration-after-statement
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I. -I../ccclib
LDFLAGS = -m32 $(DEBUG)
endif

INC = -I../include -I../ccclib
CCC = $(UNIXDIR)/bin/ccc

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
CFLAGS = -I. -I../ccclib
endif

# Z80 compiler used by `sizecheck`.  ccc is the only one now; zc3
# was the Hi-Tech bootstrap.
ZCC = ccc
ZCFLAGS = -DCCC $(INC)
ZCC_ASM = -s
ZCC_ASMEXT = s

#
# Where a size build lands.  Still keyed to $(ZCC): when there were
# two compilers, one shared directory meant whatever ran last won,
# and reading zc3's objects as ccc's was easy to do and hard to see.
#
SIZEDIR = sizecheck-$(ZCC)

SOURCES = pass2.c astio.c parseast.c expr.c rewrite.c lower.c rules.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = pass2.h expr.h opcodes.h rules.h

ifeq ($(DEFINES),-DDEBUG)
DBGFILES = debugtags.c debug.h
else
DBGFILES =
endif

GENERATED = debugtags.c debug.h

all: c1

# Generated file rules
debugtags.c debug.h: ./makedebug.sh $(SOURCES)
	./makedebug.sh

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

pass2.o: pass2.c $(HEADERS) $(DBGFILES)
	$(CC) $(CFLAGS) -c $< -o $@

c1: $(OBJECTS) $(LIBCCC)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBCCC)
else
	ccc -o c1 $(OBJECTS) $(LIBCCC)
endif

$(LIBCCC):
	$(MAKE) -C ../ccclib

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) c1 $(GENERATED) $(CRUFT) $(CRUFTASM)

clobber: clean
	rm -f ./c1 $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

install: c1
	cp c1 $(UNIXDIR)/bin

# Stage1: run cpp, c0, and c1 on each source file
stage1: c1
	mkdir -p $(OUTDIR)
	$(SETX) ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(OUTDIR)/$$b.err ; \
		printf "%-12s" "$$b: "; \
		if ! $(CPP) -DCCC $(INC) -p -o $(OUTDIR)/$$b $$f 2> $$err; then \
			echo "FAIL (cpp)"; \
		elif ! $(C0) $(OUTDIR)/$$b.x $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 2>> $$err; then \
			echo "FAIL (c0)"; \
		elif ! $(ASTPP) $(OUTDIR)/$$b.1 > $(OUTDIR)/$$b.pp 2>> $$err; then \
			echo "FAIL (astpp)"; \
		elif ! ./c1 $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 $(OUTDIR)/$$b.s 2>> $$err; then \
			echo "FAIL (c1)"; \
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
	$(LD) -r -o $(SIZEDIR)/c1 $(MXDIR)/lib/crt0.o $$o -L$(MXDIR)/lib -lccc -lc -lu ; \
	$(SIZE) $(SIZEDIR)/c1 | tee -a sizefile


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

$(OUTDIR)/%.1 $(OUTDIR)/%.2: $(OUTDIR)/%.x $(C0)
	$(C0) $< $(OUTDIR)/$*.1 $(OUTDIR)/$*.2

$(OUTDIR)/%.pp: $(OUTDIR)/%.1 $(ASTPP)
	$(ASTPP) $< > $@

$(OUTDIR)/%.s: $(OUTDIR)/%.1 $(OUTDIR)/%.2 c1
	./c1 $(OUTDIR)/$*.1 $(OUTDIR)/$*.2 $@

regression:
	../../tests/regress.sh --filter ccc/pass2/ $(REGRESS_FLAGS)

# Which rewrite rules ever match.  Needs c1 built with -DDEBUG, which
# is the default here - the counters are host-side bookkeeping and the
# z80 build has never seen them.  COVERFLAGS=-v also lists the hottest.
coverage: c1
	python3 ../../tests/rulecover.py $(COVERFLAGS)

# Native Z80 c1 built by ccc itself, for the self-build run: the
# other two passes have had differential builds for a while, and c1
# was the one still only proven to compile, never to run.
P2DIR = $(abspath .)
LIBDIR = $(abspath ../ccclib)
LIBSRCINC = $(abspath ../include)
# The peephole is not optional.  It is a few percent of text on
# ccc-generated code - 610 bytes off cpp, 1109 off c0, 1665 off c1 -
# and the passes are close enough to the ceiling that a few percent is
# the difference between a source compiling on the Z80 and not.  Build
# it the way it is meant to run; CCCOPT=  turns it off for a
# differential.
CCCOPT = -O


mx-ccc: $(GENERATED)
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(UNIXDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(P2DIR) -I$(LIBDIR) $(P2DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(LD) -s -o c1.mx -Ttext=0x100 \
		$(MXDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(MXDIR)/lib -lccc -lc -lu -lc
	@$(SIZE) mxccc/c1.mx

# The self-hosted binary, installed into micronix/bin - the compiler
# as it runs on the machine it targets.
mx-install: mx-ccc
	@mkdir -p $(MXDIR)/bin
	cp mxccc/c1.mx $(MXDIR)/bin

.PHONY: all clean regen stage1 install clobber sizefile sizecheck regression coverage mx-ccc mx-install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
