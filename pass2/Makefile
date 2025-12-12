#
# Makefile for cc2 (pass 2 - code generator)
#
CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement \
	-Werror=implicit-function-declaration -Wall -Werror
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

OBJECTS = pass2.o util.o astio.o parseast.o codegen.o dumpast.o \
	emithelper.o emitexpr.o emitops.o emit.o

HEADERS = cc2.h astio.h emithelper.h trace2.h

GENERATED = trace2.h tracetags.c

all: cc2

cc2: $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)

$(OBJECTS): $(HEADERS)

# util.c comes from parent
util.o: ../util.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Generated file rules
trace2.h tracetags.c: ./makedebug2.sh
	./makedebug2.sh

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) cc2 $(GENERATED)

clobber: clean
	rm -f ./cc2

install: cc2
	cp cc2 ../root/bin
	
.PHONY: all clean regen
