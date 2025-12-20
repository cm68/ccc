# cpp - C Preprocessor
#
# Builds the preprocessor that produces .x and .i files

# Disable built-in lex rule (.x -> .c) to avoid conflicts with lexeme output files
%.c: %.x

ROOTDIR = ../root

CC ?= gcc
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement \
-Werror=implicit-function-declaration -Wall -Werror
DEBUG = -ggdb3 -O0
CFLAGS = -m32 -ggdb3 -O0 -Wall $(DEBUG) $(WARNS)

CCC = $(ROOTDIR)/bin/ccc
OUTDIR = stage1

# When building with ccc, use different flags
ifeq ($(CC),ccc)
CFLAGS = -DCCC
endif

SRCS = cpp.c lex.c io.c macro.c kw.c emit.c util.c
OBJS = $(SRCS:.c=.o)

all: cpp

cpp: $(OBJS)
	$(CC) $(CFLAGS) -o cpp $(OBJS)

%.o: %.c cpp.h
	$(CC) $(CFLAGS) -c -o $@ $<

# Stage: compile with cross ccc to Z80 .o files
stage1:
	mkdir -p $(OUTDIR)
	@for f in $(SRCS); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) -DCCC -k -P -c $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.x $$b.i $$b.s $$b.o $$b.ast $$b.pp $(OUTDIR) 2>/dev/null ; \
	done

# Size report for stage1 objects
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(ROOTDIR)/bin/wssize $(addprefix $(OUTDIR)/,$(OBJS)) | \
	awk \
		'{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "cpp total" }' \
	 > sizefile

clean:
	rm -f cpp *.o

install: cpp
	cp cpp $(ROOTDIR)/bin

.PHONY: all clean install stage1 sizefile
#
# vim: tabstop=4 shiftwidth=4 expandtab:
#
