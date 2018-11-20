#
# makefile for native and cross z80 compiler
#
CC = gcc
CFLAGS = -Wno-implicit-function-declaration -g
OBJECTS = error.o parse.o type.o main.o lex.o io.o macro.o kw.o util.o
HEADERS = ccc.h error.h expr.h type.h
GENERATED = enumlist.h tokenlist.c error.h

BINS = enumcheck cc1 cc2 lextest maketokens

lextest: lex.o lextest.o kw.o io.o macro.o util.o error.o tokenlist.c
	cc -g -o lextest lextest.o lex.o kw.o io.o macro.o util.o error.o

ccc: $(OBJECTS)
	cc -o ccc $(OBJECTS)

$(OBJECTS): $(HEADERS)

test: lextest testfile.c
	./lextest -v -1 testfile.c
	
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
# process the errorcodes file, which generates error.h, containing the
# error codes and corresponding error strings
#
error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h
	
enumcheck: enumlist.h enumcheck.c
	cc -o enumcheck enumcheck.c
	./enumcheck

clean:
	rm -f $(OBJECTS) lextest.o $(GENERATED)

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
