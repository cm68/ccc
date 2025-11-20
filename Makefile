#
# makefile for native and cross z80 compiler
#
# embedded tests are for incremental development
#
# this following is only to get an idea of size
# the idea eventual is that it is both cross and native.
#
# we self-generate some of the files to keep things consistent
#
CC = gcc
#CC = sdcc
#CC = ccc

ASM = asz
ASMOPTS =

ifeq ($(CC),cc)
DEFINES= -DCCC
DEBUG=
CFLAGS = $(DEBUG) $(DEFINES)
LD = echo
LDFLAGS= 
endif

ifeq ($(CC),sdcc)
DEFINES= -DSDCC
DEBUG=
CFLAGS = $(DEBUG) $(DEFINES) -mz80
LD = sdldz80
LDFLAGS= -l /usr/share/sdcc/lib/z80/z80.lib -m -w -i -y
endif

ifeq ($(CC),gcc)
DEFINES= -DDEBUG
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement -Werror=implicit-function-declaration -Wall
DEBUG= -ggdb3 -O0
CFLAGS = $(DEBUG) $(DEFINES) $(WARNS)
LDFLAGS= $(DEBUG) -o
LD= gcc
endif

CC1OBJECTS = cc1.o error.o lex.o io.o macro.o kw.o util.o tokenlist.o \
	unixlib.o expr.o parse.o type.o declare.o outast.o

CC2OBJECTS = cc2.o util.o astio.o parseast.o codegen.o emit.o

HEADERS = cc1.h token.h
GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c op_pri.h

# All C source files (generated + corresponding to .o files)
CFILES = cc1.c error.c lex.c io.c macro.c kw.c util.c unixlib.c \
	expr.c parse.c type.c declare.c outast.c \
	cc2.c astio.c parseast.c codegen.c emit.c ccc.c \
	tokenlist.c debugtags.c

# All header files (manually written + generated)
HFILES = $(HEADERS) astio.h enumlist.h error.h debug.h op_pri.h

# All source files
SOURCES = $(CFILES) $(HFILES)

LIBSRCS = lib/Makefile lib/ccclib.s

# Documentation files (in reading order)
DOCFILES = README.md TODO.md CLAUDE.md AST_FORMAT.md lib/README.md

BINS = enumcheck cc1 cc2 ccc maketokens genop_pri

#VERBOSE=-v 3

all: cc1 cc2 ccc doc.pdf

cc1: $(CC1OBJECTS)
	$(LD) $(LDFLAGS) cc1 $(CC1OBJECTS)

cc2: $(CC2OBJECTS)
	$(LD) $(LDFLAGS) cc2 $(CC2OBJECTS)

ccc: ccc.o
	$(LD) $(LDFLAGS) ccc ccc.o

$(CC1OBJECTS): $(HFILES)

# Suffix rule to generate .ast files from .c files
%.ast: %.c cc1
	./cc1 -DCCC -i./include -I. -E -o $@ $<

# Suffix rule to generate .s assembly files from .ast files
%.s: %.ast cc2
	./cc2 $<

# Suffix rule to generate .pp (pretty-printed) files from .ast files
%.pp: %.ast astpp.lisp
	./astpp.lisp $< > $@

# Suffix rule to assemble to .obj files
%.obj: %.s
	$(ASM) $(ASMOPTS) -o $@ $<
	
.PHONY: test tests valgrind
test: cc1 tests/runtest.sh
	$(MAKE) -C tests test

tests: cc1 tests/runtest.sh
	$(MAKE) -C tests tests

valgrind: cc1 tests/runvalgrind.sh
	$(MAKE) -C tests valgrind

.PHONY: unit-tests
unit-tests: $(GENERATED)
	$(MAKE) -C unit_test tests

.PHONY: stage1
stage1: cc1 cc2
	@echo "Building stage1 compiler binaries"
	@mkdir -p stage1
	@for f in $(CFILES); do \
	  if [ -f "$$f" ]; then \
	    b=$$(basename $$f .c) ; \
	    printf "%-30s" "$$f: "; \
	    ./cc1 -DCCC -i./include -I. -E -o stage1/$$b.ast "$$f" \
		2>stage1/$$b.err ; \
	    ret1=$$?; \
	    mv $$b.i stage1/$$b.i; \
	    if [ $$ret1 -ne 0 ]; then \
	      echo "FAIL (parse error)"; \
	    else \
	      ./astpp.lisp stage1/$$b.ast >stage1/$$b.pp 2>>stage1/$$b.err ; \
	      ./cc2 stage1/$$b.ast >stage1/$$b.s 2>>stage1/$$b.err ; \
	      ret2=$$?; \
	      if [ $$ret2 -ne 0 ]; then \
	        echo "FAIL (codegen error)"; \
	      else \
	        $(ASM) $(ASMOPTS) -o stage1/$$b.o stage1/$$b.s \
		    >>stage1/$$b.err 2>&1 ; \
	        ret3=$$?; \
	        if [ $$ret3 -ne 0 ]; then \
	          echo "FAIL (asm error)"; \
	        else \
	          echo "PASS"; \
	          rm -f stage1/$$b.err; \
	        fi; \
	      fi; \
	    fi; \
	  fi; \
	done
	@echo "Stage1 build complete: stage1/*.o files ready for linking"

#
# check size of compiled objects
#
sizecheck: stage1
	@echo cc1 size 
	@for o in $(CC1OBJECTS) ; do wssize stage1/$$o ; done | \
		tr ':' ' ' | \
	awk 'NF!=1{text+=$$6 ; data+=$$8;bss+=$$10}END{print text, data, bss}'
	@echo cc2 size 
	@for o in $(CC2OBJECTS) ; do wssize stage1/$$o ; done | \
		tr ':' ' ' | \
	awk 'NF!=1{text+=$$6 ; data+=$$8;bss+=$$10}END{print text, data, bss}'

#
# process the cc1.h file, extracting the enum tags for the tokens
#
enumlist.h: cc1.h Makefile
	@echo '/* generated from token.h - DO NOT EDIT */' >enumlist.h
	tr ',' '\n' < token.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum / { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >>enumlist.h

#
# generate the operator priority table
#
op_pri.h: ./genop_pri
	./genop_pri

genop_pri: genop_pri.c
	cc -o genop_pri genop_pri.c

#
# generate token names from the enumlist.h file
#
tokenlist.c: enumlist.h maketokens
	./maketokens >tokenlist.c
	
maketokens: maketokens.c token.h enumlist.h
	cc -o maketokens maketokens.c

#
# generate the debug.h file from the shell script makedebug.sh
# which grunges through all the c sources looking for VERBOSE(tag)
#
debug.h debugtags.c: ./makedebug.sh
	./makedebug.sh

#
# process the errorcodes file, which generates error.h, containing the
# error codes and corresponding error strings
#
error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h
	
enumcheck: enumlist.h enumcheck.c
	cc -o enumcheck enumcheck.c
	./enumcheck

regen:
	rm -f $(GENERATED)
	make $(GENERATED)

tags:
	ctags *.c

doc.pdf: $(SOURCES) $(DOCFILES) Makefile
	{ for f in $(DOCFILES); do \
	    pandoc -f gfm -t plain "$$f" | \
		iconv -f utf-8 -t Latin1//TRANSLIT | \
		enscript -2rG --title="$$f" -p -; \
	  done; \
	  enscript -2rG -p - Makefile $(SOURCES) $(LIBSRCS) ; } | \
		ps2pdf - doc.pdf

clean:
	rm -f $(CC1OBJECTS) cc2.o ccc.o $(GENERATED) tests/*.i \
		*.ast *.s *.pp *.asm *.lst *.sym *.map *.cdb *.ihx *.i
	rm -rf stage1
	$(MAKE) -C unit_test clean

clobber: clean
	rm -f $(BINS) tags doc.pdf

cc1.o: debugtags.c
parse.o: parse.c
type.o: type.c
main.o: main.c
io.o: io.c
macro.o: macro.c
kw.o: kw.c
error.o: error.c
util.o: util.c
tokenlist.o: tokenlist.c
expr.o: expr.c op_pri.h
outast.o: outast.c
astio.o: astio.c astio.h cc2.h
parseast.o: parseast.c astio.h cc2.h
