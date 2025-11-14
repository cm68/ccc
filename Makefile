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
DEBUG= -ggdb3 -O0
CFLAGS = $(DEBUG) $(DEFINES) -Wno-implicit-function-declaration -Wall
LDFLAGS= $(DEBUG) -o
LD= gcc
endif

CC1OBJECTS = cc1.o error.o lex.o io.o macro.o kw.o util.o tokenlist.o unixlib.o \
	expr.o parse.o type.o declare.o outast.o

HEADERS = cc1.h token.h
GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c op_pri.h

# All C source files (generated + corresponding to .o files)
CFILES = cc1.c error.c lex.c io.c macro.c kw.c util.c unixlib.c \
	expr.c parse.c type.c declare.c outast.c \
	cc2.c parseast.c ccc.c \
	tokenlist.c debugtags.c

# All header files (manually written + generated)
HFILES = $(HEADERS) enumlist.h error.h debug.h op_pri.h

# All source files
SOURCES = $(CFILES) $(HFILES)

# Documentation files (in reading order)
DOCFILES = README.md TODO.md CLAUDE.md AST_FORMAT.md

BINS = enumcheck cc1 cc2 ccc maketokens genop_pri

#VERBOSE=-v 3

all: cc1 cc2 ccc doc.pdf

cc1: $(CC1OBJECTS)
	$(LD) $(LDFLAGS) cc1 $(CC1OBJECTS)

cc2: cc2.o util.o parseast.o
	$(LD) $(LDFLAGS) cc2 cc2.o util.o parseast.o

ccc: ccc.o
	$(LD) $(LDFLAGS) ccc ccc.o

$(CC1OBJECTS): $(HFILES)

# Suffix rule to generate .ast files from .c files
%.ast: %.c cc1
	./cc1 -DCCC -i./include -I. -E $< > $@ 2>&1

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

.PHONY: sizecheck
sizecheck: clean clobber
	$(MAKE) CC=sdcc cc1

.PHONY: selfcheck
selfcheck: cc1
	@echo "Testing compiler on its own sources (full pipeline: cpp + parse)..."
	@for f in $(CFILES); do \
	  if [ -f "$$f" ]; then \
	    printf "%-30s" "$$f: "; \
	    timeout 10 ./cc1 -DCCC -i./include -I. -E "$$f" > /dev/null 2>&1; \
	    ret=$$?; \
	    if [ $$ret -eq 124 ]; then \
	      echo "FAIL (timeout)"; \
	    elif [ $$ret -ne 0 ]; then \
	      echo "FAIL (parse error)"; \
	    else \
	      echo "PASS"; \
	    fi; \
	  fi; \
	done

.PHONY: fullcheck
fullcheck: cc1 cc2
	@echo "Testing compiler on its own sources (complete pipeline: cpp + parse + codegen)..."
	@for f in $(CFILES); do \
	  if [ -f "$$f" ]; then \
	    printf "%-30s" "$$f: "; \
	    timeout 10 ./cc1 -DCCC -i./include -I. -E "$$f" > /tmp/$$f.ast 2>&1; \
	    ret1=$$?; \
	    if [ $$ret1 -eq 124 ]; then \
	      echo "FAIL (parse timeout)"; \
	    elif [ $$ret1 -ne 0 ]; then \
	      echo "FAIL (parse error)"; \
	    else \
	      timeout 10 ./cc2 /tmp/$$f.ast > /dev/null 2>&1; \
	      ret2=$$?; \
	      if [ $$ret2 -eq 124 ]; then \
	        echo "FAIL (codegen timeout)"; \
	      elif [ $$ret2 -ne 0 ]; then \
	        echo "FAIL (codegen error)"; \
	      else \
	        echo "PASS"; \
	      fi; \
	    fi; \
	    rm -f /tmp/$$f.ast; \
	  fi; \
	done

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
	    pandoc -f gfm -t plain "$$f" | iconv -f utf-8 -t Latin1//TRANSLIT | enscript -2rG --title="$$f" -p -; \
	  done; \
	  enscript -2rG -p - Makefile $(CFILES) $(HFILES); } | ps2pdf - doc.pdf

clean:
	rm -f $(CC1OBJECTS) cc2.o ccc.o $(GENERATED) tests/*.i *.ast.* \
		*.asm *.lst *.sym *.map *.cdb *.ihx *.i
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
