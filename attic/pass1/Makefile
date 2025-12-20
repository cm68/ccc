#
# Makefile for cc1 (pass 1 - parser)
#
# Disable built-in lex rule (.x -> .c) to avoid conflicts with lexeme files
%.c: %.x

ROOTDIR = ../root

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement \
	-Werror=implicit-function-declaration -Wall -Werror
DEBUG = -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS) -I.
LDFLAGS = -m32 $(DEBUG)
endif

CCC = $(ROOTDIR)/bin/ccc
OUTDIR = stage1

ifeq ($(CC),ccc)
CFLAGS = -I.
endif

SOURCES = pass1.c error.c lexread.c tokenlist.c \
	expr.c parse.c type.c declare.c outast.c

OBJECTS = $(SOURCES:.c=.o)

HEADERS = cc1.h token.h enumlist.h error.h debug.h

GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c

# Default target depends on CC
ifeq ($(CC),gcc)
all: cc1
else ifeq ($(CC),ccc)
all: stage1
endif

cc1: $(OBJECTS) util.o
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS) util.o

$(OBJECTS): $(HEADERS)

# util.c comes from parent
util.o: ../util.c $(HEADERS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Stage1: compile with cross ccc to Z80 .o files
# Use -I. flag only - not gcc flags like -m32
CCCFLAGS = -I.
stage1: $(GENERATED)
	mkdir -p $(OUTDIR)
	@for f in $(SOURCES); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) $(CCCFLAGS) -k -P -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.i $$b.x $$b.s $$b.o $$b.ast $$b.pp $(OUTDIR) 2>/dev/null ; \
	done
	@printf "%-20s" "../util.c: "
	@$(CCC) $(CCCFLAGS) -k -P -c ../util.c 2> $(OUTDIR)/util.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/util.err; } || echo "FAIL"
	@mv util.i util.x util.s util.o util.ast util.pp $(OUTDIR) 2>/dev/null

# Size report for stage1 objects
STAGE1_OBJS = $(SOURCES:.c=.o) util.o
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(ROOTDIR)/bin/wssize $(addprefix $(OUTDIR)/,$(STAGE1_OBJS)) | \
	awk \
		'{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "pass1 total" }' \
	 > sizefile

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
	rm -f $(OBJECTS) util.o cc1 maketokens $(GENERATED)

clobber: clean
	rm -f ./cc1

install: cc1

.PHONY: all clean regen stage1 install clobber sizefile

#
# vim: tabstop=4 shiftwidth=4 expandtab:
#	cp cc1 $(ROOTDIR)/bin

