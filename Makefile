#
# makefile for native and cross z80 compiler
#
CFLAGS = -Wno-implicit-function-declaration
OBJECTS = error.o parse.o type.o main.o lex.o io.o macro.o kw.o util.o
HEADERS = lex.h type.h
GENERATED = enumlist.h
BINS = enumcheck cc1 cc2

ccc: $(OBJECTS)
	cc -o ccc $(OBJECTS)

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
