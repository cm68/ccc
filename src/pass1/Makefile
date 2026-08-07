#
# Makefile for ccc compiler pass 1 
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
C1 = ../pass2/c1

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

SOURCES = pass1.c error.c lexread.c fold.c pfx.c post.c name.c init.c istream.c eutil.c \
	expr.c parse.c pblock.c swcnt.c decl.c type.c tparse.c declare.c outast.c outh.c outfn.c regalloc.c util.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = cc1.h token.h enumlist.h error.h debug.h

GENERATED = enumlist.h error.h debug.h debugtags.c

all: c0

# Generated file rules
enumlist.h: token.h Makefile
	@echo '/* generated from token.h - DO NOT EDIT */' >enumlist.h
	tr ',' '\n' < token.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum / { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >>enumlist.h

debugtags.c debug.h: ./makedebug.sh
	./makedebug.sh

error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

c0: $(OBJECTS) $(LIBCCC)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) $(LIBCCC)
else
	ccc -o c0 $(OBJECTS) $(LIBCCC)
endif

$(LIBCCC):
	$(MAKE) -C ../ccclib

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

# pass1.[0-9]* is what -v dumps land in when the trace flags are on.
clean:
	rm -f $(OBJECTS) c0 $(GENERATED) $(CRUFT) $(CRUFTASM) pass1.[0-9]*

clobber: clean
	rm -f ./c0 $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

install: c0
	cp c0 $(UNIXDIR)/bin

# Stage1: run cpp, c0, and c1 on each source file
stage1: c0
	mkdir -p $(OUTDIR)
	$(SETX) ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(OUTDIR)/$$b.err ; \
		printf "%-12s" "$$b: "; \
		if ! ../cpp/cpp -DCCC $(INC) -p -o $(OUTDIR)/$$b $$f 2> $$err; then \
			echo "FAIL (cpp)"; \
		elif ! ./c0 $(OUTDIR)/$$b.x $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 2>> $$err; then \
			echo "FAIL (c0)"; \
		elif ! $(ASTPP) $(OUTDIR)/$$b.1 > $(OUTDIR)/$$b.pp 2>> $$err; then \
			echo "FAIL (astpp)"; \
		elif ! $(C1) -v 8 $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 $(OUTDIR)/$$b.s 2>> $$err; then \
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
	$(LD) -r -o $(SIZEDIR)/c0 -Ttext=0x100 $(MXDIR)/lib/crt0.o $$o -L$(MXDIR)/lib -lccc -lc -lu ; \
	$(SIZE) $(SIZEDIR)/c0 | tee -a sizefile


# Individual stage1 file rules
.SECONDARY:

$(CPP):
	$(MAKE) -C ../cpp cpp

$(C1):
	$(MAKE) -C ../pass2 c1

$(ASTPP):
	$(MAKE) -C .. astpp

$(OUTDIR)/%.x: %.c $(HEADERS) $(CPP)
	@mkdir -p $(OUTDIR)
	$(CPP) -DCCC $(INC) -p -o $(OUTDIR)/$* $<

$(OUTDIR)/%.1 $(OUTDIR)/%.2: $(OUTDIR)/%.x c0
	./c0 $< $(OUTDIR)/$*.1 $(OUTDIR)/$*.2

$(OUTDIR)/%.pp: $(OUTDIR)/%.1 $(ASTPP)
	$(ASTPP) $< > $@

$(OUTDIR)/%.s: $(OUTDIR)/%.1 $(OUTDIR)/%.2 $(C1)
	$(C1) -v 8 $(OUTDIR)/$*.1 $(OUTDIR)/$*.2 $@

regression:
	../../tests/regress.sh --filter ccc/pass1/ $(REGRESS_FLAGS)

# The self-build: ccc's own chain compiles c0 and links it as a
# Micronix binary for the simulator.  mx-zc3 built the same thing
# with Hi-Tech C so the two could be run against each other; that
# was the bootstrap and it is over.
P1DIR = $(abspath .)
LIBDIR = $(abspath ../ccclib)
LIBSRCINC = $(abspath ../include)

# The peephole is not optional.  It is a few percent of text on
# ccc-generated code - 610 bytes off cpp, 1109 off c0, 1665 off c1 -
# and the passes are close enough to the ceiling that a few percent is
# the difference between a source compiling on the Z80 and not.  Build
# it the way it is meant to run; CCCOPT=  turns it off for a
# differential.
CCCOPT = -O


# The same build done by ccc itself.  tests/diffpass1.sh runs the two
# c0.mx binaries against each other on identical lexeme streams, so
# both have to exist and both have to be current; this target was a
# sequence of commands typed by hand until a stale mxccc quietly
# answered a question about a fix that had never been built into it.
mx-ccc: $(GENERATED)
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(UNIXDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(P1DIR) -I$(LIBDIR) $(P1DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(LD) -s -o c0.mx -Ttext=0x100 \
		$(MXDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(MXDIR)/lib -lccc -lc -lu -lc
	@$(SIZE) mxccc/c0.mx

# The self-hosted binary, installed into micronix/bin - the compiler
# as it runs on the machine it targets.
mx-install: mx-ccc
	@mkdir -p $(MXDIR)/bin
	cp mxccc/c0.mx $(MXDIR)/bin

.PHONY: all clean regen stage1 install clobber sizefile sizecheck regression mx-ccc mx-install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
