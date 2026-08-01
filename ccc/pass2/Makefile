#
# Makefile for ccc compiler pass 2
#

DEST = $(realpath ../../root)

CC = gcc

LIBCCC = ../lib/libccc.a

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -std=gnu89
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I. -I../lib
LDFLAGS = -m32 $(DEBUG)
endif

ifeq ($(CC),zc3)
CFLAGS = -I../../libsrc/include -I../lib
LDFLAGS =
endif


INC = -I../../libsrc/include -I../lib
CCC = $(DEST)/bin/ccc

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
CFLAGS = -I. -I../lib
endif

# Z80 compiler used by `sizecheck` (ccc | zc3)
ZCC = ccc
ZCFLAGS = -DCCC $(INC)
ROOTDIR = $(realpath ../../root)
ifeq ($(ZCC),zc3)
ZCC_ASM = -S
ZCC_ASMEXT = as
endif
ifeq ($(ZCC),ccc)
ZCC_ASM = -s
ZCC_ASMEXT = s
endif

#
# Where a size build lands.  Keyed to the compiler that produced it:
# these directories hold objects, and one directory shared between two
# compilers means whatever ran last wins.  Reading zc3's objects and
# calling them ccc's is an easy mistake to make and a hard one to see.
#
SIZEDIR = sizecheck-$(ZCC)

SOURCES = pass2.c astio.c parseast.c expr.c rewrite.c rules.c
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
	$(MAKE) -C ../lib

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) c1 $(GENERATED) *.o *.rel *.sym *.lst *.asm *.ihx *.lk *.map *.mem *.noi *.1 *.2 *.x *.i *.pp core vgcore.*

clobber: clean
	rm -f ./c1
	rm -rf sizecheck sizecheck-* stage1 stage2

install: c1
	cp c1 $(DEST)/bin

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

# CP/M .com footprint, built the way the shipped binary is: -O peephole
# (~23% smaller text than sizecheck's unoptimised build).  This is the
# number to weigh against the TPA, not sizecheck's.
ZC3 = zc3
ZC3FLAGS = -O
LIBSRCINC = $(realpath ../../libsrc/include)

com: $(GENERATED)
	@mkdir -p comzc3
	@set -e; cd comzc3 ; export PATH=$(ROOTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(ZC3) -c $(ZC3FLAGS) -CPM -DCCC -I.. -I../../lib -I$(LIBSRCINC) \
			../$$f > $$b.log 2>&1 \
			&& { [ -f $$b.o ] || [ -f $$b.obj ]; }; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd comzc3 && $(ROOTDIR)/bin/wsld -s -o c1.ws \
		-Ttext=0x100 -Tdata=0x100 -Tbss=0x100 \
		$(ROOTDIR)/lib/crtcpm.o $$(ls *.o *.obj 2>/dev/null) \
		-L$(ROOTDIR)/lib -lccc -lcpm -lc -lu -lc
	cd comzc3 && tail -c +17 c1.ws > c1-zc3.com
	@echo "--- per object (text data bss) ---"
	@cd comzc3 && $(ROOTDIR)/bin/wssize $$(ls *.o *.obj 2>/dev/null)
	@echo "--- linked image ---"
	@$(ROOTDIR)/bin/wssize comzc3/c1.ws | sed 's/c1.ws/c1-zc3.com/'

sizecheck: $(GENERATED)
	mkdir -p $(SIZEDIR)
	$(SETX) ; export ROOTDIR=$(ROOTDIR) ; o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(SIZEDIR)/$$b.err ; \
		o="$$o $(SIZEDIR)/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		$(ZCC) $(ZCC_ASM) $(ZCFLAGS) $$f >$$err 2>&1 && mv $$b.$(ZCC_ASMEXT) $(SIZEDIR) ; \
		$(ZCC) -c $(ZCFLAGS) $$f >>$$err 2>&1 && mv $$b.o $(SIZEDIR)/$$b.o && \
		{ echo "ok"; } || { echo "FAIL"; } \
	done ; \
	wssize $$o | awk '{t+=$$1;d+=$$2;b+=$$3;a+=$$4} END {print t,d,b,a}' | tee -a sizefile ; \
	wsld -r -o $(SIZEDIR)/c1 $(ROOTDIR)/lib/crt0.o $$o -L$(ROOTDIR)/lib -lccc -lc -lu ; \
	wssize $(SIZEDIR)/c1 | tee -a sizefile


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

.PHONY: all clean regen stage1 install clobber sizefile sizecheck regression coverage

# vim: tabstop=4 shiftwidth=4 noexpandtab:
