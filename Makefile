#
# makefile for native and cross z80 compiler
#
CC = gcc
CFLAGS = -Wno-implicit-function-declaration -g
OBJECTS = error.o parse.o type.o main.o lex.o io.o macro.o kw.o util.o
HEADERS = ccc.h error.h expr.h type.h
GENERATED = enumlist.h
BINS = enumcheck cc1 cc2


ccc: $(OBJECTS)
	cc -o ccc $(OBJECTS)

$(OBJECTS): $(HEADERS)

test: ccc testfile.c
	./ccc -v -1 testfile.c
	
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
# process the errorcodes file, which generates error.h, containing the
# error codes and corresponding error strings
#
error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h
	
enumcheck: enumlist.h enumcheck.c
	cc -o enumcheck enumcheck.c
	./enumcheck

clean:
	rm -f $(OBJECTS) enumlist.h

clobber: clean
	rm -f $(BINS)

parse.o: parse.c
type.o: type.c
main.o: main.c
io.o: io.c
macro.o: macro.c
kw.o: kw.c
error.o: error.c
util.o: util.c
