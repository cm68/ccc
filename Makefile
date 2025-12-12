#
# Top-level Makefile for ccc compiler
#
# Invokes sub-makes for pass1 (cc1) and pass2 (cc2)
#
CC = gcc

ASM = root/bin/asz
ASMOPTS =

CCCFLAGS = -DCCC -DASMKWLOOK -ilibsrc/include -I.

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement \
	-Werror=implicit-function-declaration -Wall -Werror
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS)
LDFLAGS = -m32 $(DEBUG) -o
LD = gcc
endif

ROOTDIR = root

# Source files for stage1 building
CC1_SOURCES = pass1/pass1.c pass1/error.c pass1/lex.c pass1/io.c pass1/macro.c \
	pass1/kw.c pass1/expr.c pass1/parse.c pass1/type.c pass1/declare.c pass1/outast.c
CC2_SOURCES = pass2/pass2.c pass2/astio.c pass2/parseast.c pass2/codegen.c \
	pass2/dumpast.c pass2/emithelper.c pass2/emitexpr.c pass2/emitops.c pass2/emit.c

# All C source files
CFILES = $(CC1_SOURCES) $(CC2_SOURCES) util.c ccc.c \
	pass1/tokenlist.c pass1/debugtags.c pass2/tracetags.c

BINS = pass1/cc1 pass2/cc2 ccc

# Documentation files
DOCFILES = README.md TODO.md CLAUDE.md AST_FORMAT.md libsrc/README.md libsrc/libc/README.md
LIBSRCS = libsrc/Makefile libsrc/*/*.s libsrc/*/*.c libsrc/include/*.h

all: cc1 cc2 ccc install

cc1:
	$(MAKE) -C pass1

cc2:
	$(MAKE) -C pass2

ccc: ccc.o
	$(LD) $(LDFLAGS) ccc ccc.o

install: cc1 cc2 ccc
	mkdir -p $(ROOTDIR)/bin
	$(MAKE) -C pass1 install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C pass2 install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C ws install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C libsrc install ROOTDIR=$(CURDIR)/$(ROOTDIR)

# Suffix rules using installed binaries
%.ast: %.c cc1
	./$(ROOTDIR)/bin/cc1 $(CCCFLAGS) -o $@ $<

%.s: %.ast cc2
	./$(ROOTDIR)/bin/cc2 $<

%.pp: %.ast astpp.py
	python3 ./astpp.py $< > $@

%.obj: %.s
	$(ASM) $(ASMOPTS) -o $@ $<

# Pattern rules for stage1 directory
.PRECIOUS: stage1/%.ast stage1/%.s

stage1/%.ast: %.c cc1 FORCE
	@mkdir -p stage1
	./$(ROOTDIR)/bin/cc1 $(CCCFLAGS) -o $@ $<

stage1/%.s: stage1/%.ast cc2 FORCE
	./$(ROOTDIR)/bin/cc2 -o $@ $<

stage1/%.o: stage1/%.s FORCE
	$(ASM) $(ASMOPTS) -o $@ $<

FORCE:

.PHONY: test tests valgrind FORCE cc1 cc2
test: cc1 cc2 install
	$(MAKE) -C tests test

tests: cc1 cc2 install
	$(MAKE) -C tests tests

valgrind: cc1 cc2
	$(MAKE) -C tests valgrind

sizecheck:
	$(MAKE) -C pass1 sizefile
	$(MAKE) -C pass2 sizefile
	@cat pass1/sizefile pass2/sizefile | tee cur.size
	@diff -N cur.size prev.size
	@cp cur.size prev.size

.PHONY: unit-tests
unit-tests:
	$(MAKE) -C pass1 regen
	$(MAKE) -C unit_test tests

.PHONY: stage1
stage1: cc1 cc2 install
	@echo "Building stage1 with cross ccc"
	$(MAKE) -C pass1 CC=ccc ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C pass2 CC=ccc ROOTDIR=$(CURDIR)/$(ROOTDIR)
	@echo "Stage1 build complete"

regen:
	$(MAKE) -C pass1 regen
	$(MAKE) -C pass2 regen

tags:
	ctags pass1/*.c pass2/*.c *.c

doc.pdf: $(CFILES) $(DOCFILES) Makefile
	{ for f in $(DOCFILES); do \
	    pandoc -f gfm -t plain "$$f" | \
		iconv -f utf-8 -t Latin1//TRANSLIT | \
		enscript -2rG --title="$$f" -p -; \
	  done; \
	  enscript -2rG -p - Makefile $(CFILES) $(LIBSRCS) ; } | \
		ps2pdf - doc.pdf

clean:
	$(MAKE) -C pass1 clean
	$(MAKE) -C pass2 clean
	rm -f ccc.o ccc tests/*.i *.ast *.s *.pp *.i
	rm -rf stage1
	$(MAKE) -C unit_test clean

clobber: clean
	rm -f $(BINS) tags doc.pdf prev.size

.PHONY: all install clean clobber regen tags
