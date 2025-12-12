#
# Makefile for cc1 (pass 1 - parser)
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

OBJECTS = pass1.o error.o lex.o io.o macro.o kw.o util.o tokenlist.o \
	expr.o parse.o type.o declare.o outast.o

HEADERS = cc1.h token.h enumlist.h error.h debug.h

GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c

all: cc1

cc1: $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)

$(OBJECTS): $(HEADERS)

# util.c comes from parent
util.o: ../util.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Generated file rules
enumlist.h: token.h Makefile
	@echo '/* generated from token.h - DO NOT EDIT */' >enumlist.h
	tr ',' '\n' < token.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum / { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >>enumlist.h

tokenlist.c: enumlist.h maketokens
	./maketokens >tokenlist.c

maketokens: maketokens.c token.h enumlist.h
	cc -o maketokens maketokens.c

debug.h debugtags.c: ./makedebug.sh
	./makedebug.sh

error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h

regen:
	rm -f $(GENERATED)
	$(MAKE) $(GENERATED)

clean:
	rm -f $(OBJECTS) cc1 maketokens $(GENERATED)

clobber: clean
	rm -f ./cc1

install: cc1
	cp cc1 ../root/bin

.PHONY: all clean regen
