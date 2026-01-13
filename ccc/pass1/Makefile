#
# Makefile for ccc compiler pass 1 
#

DEST = $(realpath ../../root)

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -std=gnu89
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

ifeq ($(CC),zc3)
CFLAGS = -I../../libsrc/include
LDFLAGS =
endif

ifeq ($(CC),sdcc)
CFLAGS = -mz80 -DCCC -I../../libsrc/include
SDCC_OBJS = $(SOURCES:.c=.rel)
endif

INC = -I../../libsrc/include
CCC = $(DEST)/bin/ccc
ASTPP = ../astpp

OUTDIR = stage1

ifdef TRACE
SETX = set -x
else
SETX = @ true
endif

ifeq ($(CC),ccc)
CFLAGS = -I.
endif

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

%.rel: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $<

ifeq ($(CC),sdcc)
c0: $(SDCC_OBJS)
	@echo "SDCC compile complete. $(words $(SDCC_OBJS)) object files ready."
	@echo "Link with: sdcc -mz80 --no-std-crt0 -o c0.ihx $(SDCC_OBJS) <libc.rel>"
else
c0: $(OBJECTS)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
else
	ccc -o c0 $(OBJECTS)
endif
endif

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) c0 $(GENERATED) *.o *.rel *.sym *.lst *.asm *.ihx *.lk *.map *.mem *.noi *.1 *.2 *.x *.i *.pp

clobber: clean
	rm -f ./c0
	rm -rf sizecheck sizecheck1 stage1 stage2

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
		elif ! ../pass2/c1 -v 8 $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 $(OUTDIR)/$$b.s 2>> $$err; then \
			echo "FAIL (c1)"; \
		else \
			echo "ok"; \
		fi \
	done

sizecheck: $(GENERATED)
	mkdir -p sizecheck
	$(SETX) ; o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=sizecheck/$$b.err ; \
		o="$$o sizecheck/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		zc3 -S -DCCC $(INC) $$f && mv $$b.as sizecheck ; \
		zc3 -c -DCCC $(INC) -o sizecheck/$$b.o $$f && \
		{ echo "ok"; } || { echo "FAIL"; } \
	done ; \
	wssize $$o | awk '{t+=$$1;d+=$$2;b+=$$3;a+=$$4} END {print t,d,b,a}' | tee -a sizefile ; \
	wsld -r -o sizecheck/c0 ../../root/lib/crt0.o $$o ../../root/lib/libc.a ../../root/lib/libu.a ; \
	wssize sizecheck/c0 | tee -a sizefile

sizecheck1: $(GENERATED)
	mkdir -p sizecheck1
	$(SETX) ; o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=sizecheck/$$b.err ; \
		o="$$o sizecheck1/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		sdcc --sdcccall 0 -c -mz80 -DCCC -I../../libsrc/include $$f && mv $$b.??? sizecheck1 ; \
		{ echo "ok"; } || { echo "FAIL"; } \
	done ; \
	sdcc -mz80 -o sizecheck1/c0 sizecheck1/*.rel

# Individual stage1 file rules
.SECONDARY:

CPP = ../cpp/cpp
C1 = ../pass2/c1

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

.PHONY: all clean regen stage1 install clobber sizefile sizecheck sizecheck1

# vim: tabstop=4 shiftwidth=4 noexpandtab:
