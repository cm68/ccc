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
CC = sdcc
CFLAGS = -mz80 --fomit-frame-pointer
CC = gcc
CFLAGS = -Wno-implicit-function-declaration -g

OBJECTS = ccc.o error.o parse.o type.o main.o lex.o io.o macro.o kw.o util.o tokenlist.o \
	lextest.o iotest.o
HEADERS = ccc.h error.h expr.h type.h
GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c
CFILES = iotest.c lextest.c kw.c io.c macro.c util.c error.c

BINS = enumcheck cc1 cc2 lextest maketokens iotest
TESTS = -v 3 m1.c

all: lextest iotest

cc1: cc1.o lex.o kw.o io.o macro.o util.o error.o tokenlist.o
lextest: lex.o cc1.o kw.o io.o macro.o util.o error.o tokenlist.o nullexpr.o
	cc -g -o lextest tokenlist.o cc1.o lex.o kw.o io.o macro.o util.o error.o nullexpr.o

iotest: io.o util.o iotest.o
	cc -g -o iotest io.o iotest.o util.o

ccc: $(OBJECTS)
	cc -o ccc $(OBJECTS)

$(OBJECTS): $(HEADERS)

.PHONY: test tests
test: iotest lextest runtest.sh
	./runtest.sh $(TESTS)

tests: iotest lextest runtest.sh
	./runtest.sh

#
# process the ccc.h file, extracting the enum tags for the tokens
#
enumlist.h: ccc.h
	tr ',' '\n' < ccc.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum token/ { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >enumlist.h

#
# generate token names from the enumlist.h file
#
tokenlist.c: enumlist.h maketokens.c
	cc -o maketokens maketokens.c
	./maketokens >tokenlist.c
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

clean:
	rm -f $(OBJECTS) lextest.o $(GENERATED) tests/*.i

clobber: clean
	rm -f $(BINS)

lextest.o: tokenlist.c lextest.c

parse.o: parse.c
type.o: type.c
main.o: main.c
io.o: io.c
macro.o: macro.c
kw.o: kw.c
error.o: error.c
util.o: util.c
tokenlist.o: tokenlist.c
lextest.o: lextest.c
iotest.o: iotest.c
