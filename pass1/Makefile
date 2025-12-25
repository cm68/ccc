# pass1 - C Compiler Pass 1 (Parser)
#
# Builds the parser that produces intermediate code

DEST ?= $(CURDIR)/../root

CC = gcc

CCC = $(DEST)/bin/ccc
OUTDIR = stage1

# Compiler-specific flags
ifeq ($(CC),gcc)
WARNS = -Wall -Werror -pedantic \
-Wdeclaration-after-statement -Werror=declaration-after-statement \
-Werror=implicit-function-declaration
CFLAGS = -m32 -std=c89 -g -O0 $(WARNS)
else ifeq ($(CC),ccc)
CFLAGS = -DCCC
endif

SRCS = p1cclass.c p1emit.c p1expr.c p1lex.c p1main.c p1memchk.c p1op.c \
       p1program.c p1stmt.c p1sym.c p1type.c
OBJS = $(SRCS:.c=.o)
HDRS = p1.h p1cclass.h p1tok.h

all: c0

c0: $(OBJS)
	$(CC) $(CFLAGS) -o c0 $(OBJS)

%.o: %.c $(HDRS)
	$(CC) $(CFLAGS) -c -o $@ $<

# Stage: compile with cross ccc to Z80 .o files
stage1:
	mkdir -p $(OUTDIR)
	@for f in $(SRCS); do \
	  b=$${f%.c}; \
	  printf "%-20s" "$$f: "; \
	  $(CCC) -DCCC -k -P -s $$f 2> $(OUTDIR)/$$b.err 1>&2 && { echo "ok"; rm -f $(OUTDIR)/$$b.err; } || echo "FAIL"; \
	  mv $$b.i $$b.x $$b.1 $$b.2 $$b.pp $$b.s $$b.o $(OUTDIR) 2>/dev/null ; \
	done

# Size report for stage1 objects
sizefile: Makefile
	$(MAKE) stage1 CC=ccc
	@$(DEST)/bin/wssize $(addprefix $(OUTDIR)/,$(OBJS)) | \
	awk \
		'{ t+=$$1 ; d+=$$2 ; b+=$$3 ; a+=$$4; print } \
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "pass1 total" }' \
	 > sizefile

clean:
	rm -f *.o

clobber: clean
	rm -f c0
	rm -rf stage1

install: c0
	cp c0 $(DEST)/bin

.PHONY: all clean clobber install stage1 sizefile
#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
