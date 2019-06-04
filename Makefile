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
DEFINES= -DLEXTEST -DDEBUG

CC = sdcc
CFLAGS = -mz80 --fomit-frame-pointer
LD = sdldz80
LDFLAGS= -l /usr/share/sdcc/lib/z80/z80.lib -m -w -i -y

CC = gcc
CFLAGS = $(DEFINES) -Wno-implicit-function-declaration -g
LDFLAGS= -g -o
LD= gcc

CC1OBJECTS = \
cc1.o \
error.o \
lex.o \
io.o \
macro.o \
kw.o \
util.o \
tokenlist.o \
nullexpr.o
NOFILES= \
main.o \
type.o \
parse.o \
foo.o

HEADERS = ccc.h error.h expr.h type.h
GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c
CFILES = kw.c io.c macro.c util.c error.c

BINS = enumcheck cc1 cc2 maketokens

all: cc1

cc1: $(CC1OBJECTS)
	$(LD) $(LDFLAGS) cc1 $(CC1OBJECTS)

$(CC1OBJECTS): $(HEADERS)

.PHONY: test tests
test: cc1 runtest.sh
	./runtest.sh $(TESTS)

tests: cc1 runtest.sh
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
tokenlist.c: enumlist.h maketokens
	./maketokens >tokenlist.c
	
maketokens: maketokens.c token.h
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

clean:
	rm -f $(CC1OBJECTS) $(GENERATED) tests/*.i \
		*.asm *.lst *.sym *.map *.cdb *.ihx

clobber: clean
	rm -f $(BINS)

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
