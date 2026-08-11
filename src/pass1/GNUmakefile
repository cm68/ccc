#
# Makefile for ccc compiler pass 1 
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
C1 = ../pass2/c1

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
	expr.c parse.c pblock.c pstmt2.c swcnt.c decl.c type.c tparse.c declare.c outast.c outh.c outfn.c regalloc.c util.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = cc1.h token.h enumlist.h error.h debug.h

GENERATED = enumlist.h error.h debug.h dbgtags.c

all: c0

# Generated file rules
enumlist.h: token.h GNUmakefile
	@echo '/* generated from token.h - DO NOT EDIT */' >enumlist.h
	tr ',' '\n' < token.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum / { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >>enumlist.h

dbgtags.c debug.h: ./makedebug.sh
	./makedebug.sh

error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h

# The compile itself is the default rule in GNUmakefile.inc; this
# just says what every object here depends on besides its source.
$(OBJECTS): $(HEADERS)

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

install: c0 man-install
	$(INSTALL) c0 $(HOSTDIR)/lib

# Stage1: run cpp, c0, and c1 on each source file
stage1: c0
	mkdir -p $(OUTDIR)
	$(SETX) ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(OUTDIR)/$$b.err ; \
		printf "%-12s" "$$b: "; \
		if ! ../cpp/cpp -DCCC $(INC) -p -o $(OUTDIR)/$$b $$f 2> $$err; then \
			echo "FAIL (cpp)"; \
		elif ! ./c0 $(OUTDIR)/$$b.x $(OUTDIR)/$$b.ast $(OUTDIR)/$$b.dat 2>> $$err; then \
			echo "FAIL (c0)"; \
		elif ! $(ASTPP) $(OUTDIR)/$$b.ast > $(OUTDIR)/$$b.pp 2>> $$err; then \
			echo "FAIL (astpp)"; \
		elif ! $(C1) -v 8 $(OUTDIR)/$$b.ast $(OUTDIR)/$$b.dat $(OUTDIR)/$$b.s 2>> $$err; then \
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

$(OUTDIR)/%.ast $(OUTDIR)/%.dat: $(OUTDIR)/%.x c0
	./c0 $< $(OUTDIR)/$*.ast $(OUTDIR)/$*.dat

$(OUTDIR)/%.pp: $(OUTDIR)/%.ast $(ASTPP)
	$(ASTPP) $< > $@

$(OUTDIR)/%.s: $(OUTDIR)/%.ast $(OUTDIR)/%.dat $(C1)
	$(C1) -v 8 $(OUTDIR)/$*.ast $(OUTDIR)/$*.dat $@

regression:
	../../tests/regress.sh --filter ccc/pass1/ $(REGRESS_FLAGS)

# The self-build: ccc's own chain compiles c0 and links it as a
# Micronix binary for the simulator.  mx-zc3 built the same thing
# with Hi-Tech C so the two could be run against each other; that
# was the bootstrap and it is over.
P1DIR = $(abspath .)
LIBDIR = $(abspath ../ccclib)
CPPSRC = $(abspath ../cpp)
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
	@set -e; cd mxccc ; export PATH=$(HOSTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(CCC) $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(P1DIR) -I$(LIBDIR) -I$(CPPSRC) $(P1DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(LD) -s -o c0.mx -Ttext=0x100 \
		$(MXDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(MXDIR)/lib -lccc -lc -lu -lc
	@$(SIZE) mxccc/c0.mx

# The self-hosted pass, installed into destmicronix/lib under the
# name the driver runs it by - the compiler as it runs on the machine
# it targets.  A system root, not a pile of .mx files: bin/lib laid
# out the way Micronix wants them, so the tree can be copied straight
# onto a filesystem and used.
mx-install: mx-ccc
	@mkdir -p $(MXDIR)/lib
	$(INSTALL) mxccc/c0.mx $(MXDIR)/lib/c0

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
			-I$(P1DIR) -I$(LIBDIR) -I$(CPPSRC) $(P1DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd comccc && $(CCC) -m cpm -S -o c0.com $$(ls *.o 2>/dev/null) \
		-L$(CPMDIR)/lib -lccc > /dev/null
	@echo "c0.com: `wc -c < comccc/c0.com` bytes"

com-install: com-ccc
	@mkdir -p $(CPMDIR)/bin
	$(INSTALL) comccc/c0.com $(CPMDIR)/bin

# The manual goes with what it documents.  man(1) on the target looks
# in /usr/man/man<n> - the path is in the binary - so the staged tree
# carries the pages under usr, the same as the headers.
#
# They live in man1/ rather than here, and have to: ".1" is pass1's
# AST suffix and is swept by CRUFT, so a page beside the source would
# be removed by the next "make clean".
MANPAGES = man1/c0.1

man-install:
	@mkdir -p $(MXDIR)/usr/man/man1
	$(INSTALL) $(MANPAGES) $(MXDIR)/usr/man/man1

.PHONY: all clean regen stage1 install clobber sizefile sizecheck regression mx-ccc mx-install com-ccc com-install man-install

# vim: tabstop=4 shiftwidth=4 noexpandtab:
