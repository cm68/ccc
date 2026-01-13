#
# Makefile for ccc compiler pass 2
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

CPP = ../cpp/cpp
INC = -I../../libsrc/include
CCC = $(DEST)/bin/ccc
XDUMP = ../cpp/xdump
ASTPP = ../astpp
C0 = ../pass1/c0

OUTDIR = stage1

ifdef TRACE
SETX = set -x ;
else
SETX = @
endif

ifeq ($(CC),ccc)
CFLAGS = -I.
endif

SOURCES = pass2.c astio.c parseast.c expr.c rewrite.c
OBJECTS = $(SOURCES:.c=.o)

HEADERS = pass2.h expr.h opcodes.h

ifeq ($(DEFINES),-DDEBUG)
DBGFILES = debugtags.c debug.h
else
DBGFILES =
endif

all: c1

debugtags.c debug.h: ./makedebug.sh $(SOURCES)
	./makedebug.sh

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

pass2.o: pass2.c $(HEADERS) $(DBGFILES)
	$(CC) $(CFLAGS) -c $< -o $@

c1: $(OBJECTS)
ifeq ($(CC),gcc)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
else
	ccc -o c1 $(OBJECTS)
endif

clean:
	rm -f $(OBJECTS) c1 *.o *.1 *.2 *.x *.i *.pp core vgcore.* debugtags.c debug.h

clobber: clean
	rm -f ./c1
	rm -rf stage1 stage1 sizecheck sizecheck1

install: c1
	cp c1 $(DEST)/bin

# Stage1: run cpp, c0, c1 on each source file
stage1: c1
	mkdir -p $(OUTDIR)
	$(SETX) for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=$(OUTDIR)/$$b.err ; \
		printf "%-12s" "$$b: "; \
		$(CPP) -DCCC $(INC) -o $(OUTDIR)/$$b $$f 2> $$err && \
		$(XDUMP) $(OUTDIR)/$$b.x > $(OUTDIR)/$$b.xd 2>> $$err && \
		../pass1/c0 $(OUTDIR)/$$b.x $(OUTDIR)/$$b.1 $(OUTDIR)/$$b.2 2>> $$err && \
		$(ASTPP) $(OUTDIR)/$$b.1 > $(OUTDIR)/$$b.pp 2>> $$err && \
		./c1 -p $(OUTDIR)/$$b.1 -o $(OUTDIR)/$$b.s 2>> $$err && \
      	{ echo "ok"; rm -f $$err; } || { echo "FAIL"; } \
    done

sizecheck:
	mkdir -p sizecheck
	$(SETX) o="" ; for f in $(SOURCES); do \
		b=$${f%.c}; \
		err=sizecheck/$$b.err ; \
		o="$$o sizecheck/$$b.o" ; \
		printf "%-12s" "$$b: " ; \
		zc3 -S -DCCC $(INC) $$f && mv $$b.as sizecheck ; \
		zc3 -c -DCCC $(INC) -o sizecheck/$$b.o $$f && \
		{ echo "ok"; } || { echo "FAIL"; } \
	done ; \
	wssize $$o | awk '{t+=$$1;d+=$$2;b+=$$3;a+=$$4} END {print t,d,b,a}' | tee -a sizefile ; \
	wsld -r -o sizecheck/c1 ../../root/lib/crt0.o $$o ../../root/lib/libc.a ../../root/lib/libu.a ; \
	wssize sizecheck/c1 | tee -a sizefile

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

.PHONY: all clean stage1 install clobber sizecheck

# vim: tabstop=4 shiftwidth=4 noexpandtab:
