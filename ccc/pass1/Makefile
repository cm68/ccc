#
# Makefile for ccc compiler pass 1 
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
C1 = ../pass2/c1

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

SOURCES = pass1.c error.c lexread.c \
	expr.c parse.c decl.c type.c declare.c outast.c regalloc.c util.c
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
	$(MAKE) -C ../lib

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) c0 $(GENERATED) *.o *.rel *.sym *.lst *.asm *.ihx *.lk *.map *.mem *.noi *.1 *.2 *.x *.i *.pp

clobber: clean
	rm -f ./c0
	rm -rf sizecheck sizecheck-* stage1 stage2

install: c0
	cp c0 $(DEST)/bin

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
	wsld -r -o $(SIZEDIR)/c0 -Ttext=0x100 $(ROOTDIR)/lib/crt0.o $$o -L$(ROOTDIR)/lib -lccc -lc -lu ; \
	wssize $(SIZEDIR)/c0 | tee -a sizefile


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

# Native Z80 build for simulator testing - same pattern as cpp's com-zc3.
# Micronix flavor: crt0.o + libu syscalls; run with root/sim -d / mxzc3/c0.mx.
# -O: zc3 peephole optimizer (parseExpr was split so optim3 can cope).
ZC3 = zc3
ZC3FLAGS = -O
P1DIR = $(realpath .)
LIBDIR = $(realpath ../lib)
LIBSRCINC = $(realpath ../../libsrc/include)

mx-zc3: $(GENERATED)
	@mkdir -p mxzc3
	@set -e; cd mxzc3 ; export PATH=$(ROOTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(ZC3) -c $(ZC3FLAGS) -CPM -DCCC -I$(P1DIR) -I$(LIBDIR) -I$(LIBSRCINC) \
			$(P1DIR)/$$f > $$b.log 2>&1 \
			&& { [ -f $$b.o ] || [ -f $$b.obj ]; }; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxzc3 && $(ROOTDIR)/bin/wsld -s -o c0.mx -Ttext=0x100 \
		$(ROOTDIR)/lib/crt0.o $$(ls *.o *.obj 2>/dev/null) \
		-L$(ROOTDIR)/lib -lccc -lc -lu -lc
	@$(ROOTDIR)/bin/wssize mxzc3/c0.mx

# The same build done by ccc itself.  tests/diffpass1.sh runs the two
# c0.mx binaries against each other on identical lexeme streams, so
# both have to exist and both have to be current; this target was a
# sequence of commands typed by hand until a stale mxccc quietly
# answered a question about a fix that had never been built into it.
mx-ccc: $(GENERATED)
	@mkdir -p mxccc
	@set -e; cd mxccc ; export PATH=$(ROOTDIR)/bin:$$PATH ; \
	for f in $(SOURCES); do \
		b=$${f%.c}; \
		printf "%-12s" "$$b: " ; \
		if $(ROOTDIR)/bin/ccc $(CCCOPT) -c -DCCC -i$(LIBSRCINC) \
			-I$(P1DIR) -I$(LIBDIR) $(P1DIR)/$$f > $$b.log 2>&1 \
			&& [ -f $$b.o ]; then \
			echo ok; \
		else echo FAIL; cat $$b.log; exit 1; fi \
	done
	cd mxccc && $(ROOTDIR)/bin/wsld -s -o c0.mx -Ttext=0x100 \
		$(ROOTDIR)/lib/crt0.o $$(ls *.o 2>/dev/null) \
		-L$(ROOTDIR)/lib -lccc -lc -lu -lc
	@$(ROOTDIR)/bin/wssize mxccc/c0.mx

.PHONY: all clean regen stage1 install clobber sizefile sizecheck regression mx-zc3 mx-ccc

# vim: tabstop=4 shiftwidth=4 noexpandtab:
