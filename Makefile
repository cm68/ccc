#
# makefile for native and cross z80 compiler
#
CFLAGS = -Wno-implicit-function-declaration
OBJECTS = parse.o type.o main.o lex.o io.o macro.o kw.o
OBJECTS = main.o lex.o io.o kw.o
HEADERS = lex.h type.h

ccc: $(OBJECTS)
	cc -o ccc $(OBJECTS)

test: ccc testfile.c
	./ccc -v -1 testfile.c
	
clean:
	rm -f $(OBJECTS)

parse.o: parse.c
type.o: type.c
main.o: main.c
io.o: io.c
macro.o: macro.c
kw.o: kw.c
