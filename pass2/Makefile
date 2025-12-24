# pass2 - C Compiler Pass 2 (Code Generator)
#
# Builds the code generator that produces Z80 assembly

DEST ?= $(CURDIR)/../root

CC ?= gcc

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

SRCS = cgcode.c cgdata.c cggen1.c cggen2.c cggen3.c cggetchar.c \
       cglex1.c cglex2.c cglocal1.c cglocal2.c cgmain.c cgmalloc.c \
       cgsym1.c cgsym2.c cgsym3.c cgtree1.c cgtree2.c cgtree3.c
OBJS = $(SRCS:.c=.o)
HDRS = cgen.h

all: c1

c1: $(OBJS)
	$(CC) $(CFLAGS) -o c1 $(OBJS)

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
		END {printf "%7d %7d %7d %7d %19s\n", t, d, b, a, "pass2 total" }' \
	 > sizefile

clean:
	rm -f *.o

clobber: clean
	rm -f c1

install: c1
	cp c1 $(DEST)/bin

.PHONY: all clean clobber install stage1 sizefile
#
# vim: tabstop=4 shiftwidth=4 expandtab:
#
