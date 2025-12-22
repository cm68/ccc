#
# Top-level Makefile for ccc compiler
#
# Invokes sub-makes for pass1 (cc1) and pass2 (cc2)
#

# Cancel built-in lex rule - .x files are lexeme dumps, not lex sources
%.c: %.x

CC = gcc

ASM = root/bin/asz
ASMOPTS =

CPPFLAGS = -DCCC -DASMKWLOOK -ilibsrc/include -I.

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
CC2_SOURCES = pass2/astio.c pass2/codegen.c pass2/emit.c pass2/emitcmp.c \
	pass2/emitexpr.c pass2/emitincdec.c pass2/emitops.c pass2/cc2.c \
	pass2/parseast.c

# Source files for documentation
CFILES = $(CC1_SOURCES) $(CC2_SOURCES) util.c ccc.c \
	pass1/tokenlist.c pass1/debugtags.c
HFILES = pass2/cc2.h pass1/cc1.h pass1/token.h

BINS = cpp/cpp pass1/cc1 pass2/cc2 ccc

# Documentation files
DOCFILES = README.md TODO.md CLAUDE.md AST_FORMAT.md ASTPP.md \
	pass2/NEWPASS2.md pass2/CONDITIONS.md pass2/STACK.md \
	ws/README.md ws/ASZ.md ws/WS.md \
	libsrc/README.md libsrc/libc/README.md
LIBSRCS = libsrc/Makefile libsrc/*/*.s libsrc/*/*.c libsrc/include/*.h

all: cpp cc1 cc2 ccc install

cpp:
	$(MAKE) -C cpp

cc1:
	$(MAKE) -C pass1

cc2:
	$(MAKE) -C pass2

ccc.o: ccc.c
	$(CC) $(CFLAGS) -DROOTDIR=$(CURDIR)/$(ROOTDIR) -c -o $@ $<

ccc: ccc.o
	$(LD) $(LDFLAGS) ccc ccc.o

install: cpp cc1 cc2 ccc
	mkdir -p $(ROOTDIR)/bin
	cp ccc $(ROOTDIR)/bin
	$(MAKE) -C cpp install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C pass1 install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C pass2 install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C ws install ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C libsrc install ROOTDIR=$(CURDIR)/$(ROOTDIR)

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
	@diff -N cur.size prev.size ; true

.PHONY: stage1
stage1: cc1 cc2 install
	@echo "Building stage1 with cross ccc"
	$(MAKE) -C ws CC=ccc ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C cpp CC=ccc ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C pass1 CC=ccc ROOTDIR=$(CURDIR)/$(ROOTDIR)
	$(MAKE) -C pass2 CC=ccc ROOTDIR=$(CURDIR)/$(ROOTDIR)
	@echo "Stage1 build complete"

tags:
	ctags pass1/*.c pass2/*.c *.c

doc.pdf: $(CFILES) $(HFILES) $(DOCFILES) Makefile
	{ for f in $(DOCFILES); do \
	    pandoc -f gfm -t plain "$$f" | \
		iconv -f utf-8 -t Latin1//TRANSLIT | \
		enscript -2rG --title="$$f" -p -; \
	  done; \
	  enscript -2rG -p - Makefile $(CFILES) $(HFILES) $(LIBSRCS) ; } | \
		ps2pdf - doc.pdf

clean:
	$(MAKE) -C pass1 clean
	$(MAKE) -C pass2 clean
	rm -f ccc.o ccc tests/*.i tests/*.x *.ast *.s *.pp *.i *.x
	rm -rf stage1

clobber: clean
	rm -f $(BINS) tags doc.pdf prev.size

.PHONY: all install clean clobber regen tags
#
# vim: tabstop=4 shiftwidth=4 expandtab:
#
