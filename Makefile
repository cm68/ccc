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
DEFINES= -DDEBUG
DEBUG= -ggdb3 -O0

CC = gcc
#CC = sdcc
ifeq ($(CC),sdcc)
DEFINES=
CFLAGS = -DSDCC $(DEFINES) -mz80
LD = sdldz80
LDFLAGS= -l /usr/share/sdcc/lib/z80/z80.lib -m -w -i -y
endif

ifeq ($(CC),gcc)
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

BINS = enumcheck cc1 cc2 ccc maketokens genop_pri

#VERBOSE=-v 3

all: cc1 cc2 ccc

cc1: $(CC1OBJECTS)
	$(LD) $(LDFLAGS) cc1 $(CC1OBJECTS)

cc2: cc2.o util.o parseast.o
	$(LD) $(LDFLAGS) cc2 cc2.o util.o parseast.o

ccc: ccc.o
	$(LD) $(LDFLAGS) ccc ccc.o

$(CC1OBJECTS): $(HFILES)

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

#
# process the cc1.h file, extracting the enum tags for the tokens
#
enumlist.h: cc1.h Makefile
	tr ',' '\n' < token.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum / { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >enumlist.h

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

doc.pdf: $(SOURCES)
	enscript -2r -p - $(CFILES) $(HFILES) | ps2pdf - doc.pdf

clean:
	rm -f $(CC1OBJECTS) cc2.o ccc.o $(GENERATED) tests/*.i *.ast.* \
		*.asm *.lst *.sym *.map *.cdb *.ihx
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
