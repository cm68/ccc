# Whitesmith's tools
# asz    - Z80 assembler
# wsld   - linker
# wsnm   - object/archive dump
# wslib  - archive librarian
# wssize - display object sizes

DEST ?= $(CURDIR)/../root
CCC = $(DEST)/bin/ccc
OUTDIR = stage1

CC ?= gcc

ifeq ($(CC),gcc)
CFLAGS = -m32 -g -Wall -std=c90 -pedantic -Dlinux -std=c89
LD = gcc

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -c $<
endif

ifeq ($(CC),zxc)

.SUFFIXES: .c .o
CFLAGS =
.c.o:
	zxc $(CFLAGS) -s $<
	asz $(basename $<).as

endif

ALL = asz wsld wsnm wslib wssize ccc

all: $(ALL)

# Header dependencies
asz.o: asm.h
asm.o: asm.h wsobj.h
asmz80.o: asm.h
wsobj.o: wsobj.h
wsld.o: wsobj.h
wsnm.o: wsobj.h
wslib.o: wsobj.h
wssize.o: wsobj.h

# Link targets
asz: asz.o asm.o asmz80.o wsobj.o
	$(CC) -o $@ asz.o asm.o asmz80.o wsobj.o

wsld: wsld.o wsobj.o
	$(CC) -o $@ wsld.o wsobj.o

wsnm: wsnm.o wsobj.o
	$(CC) -o $@ wsnm.o wsobj.o

wslib: wslib.o
	$(CC) -o $@ wslib.o

wssize: wssize.o
ifeq ($(CC),zxc)
	./wsld -o wssize wssize.o /usr/local/lib/cpm/lib80/libc.lib
else
	$(CC) -o $@ wssize.o
endif

ccc.o: ccc.c
	$(CC) $(CFLAGS) -DROOTDIR=$(DEST) -c -o $@ $<

ccc: ccc.o
	$(CC) -o $@ ccc.o

test:

clean:
	rm -f *.a *.o tests/*.out tests/*.a tests/*.o
	rm -f stage1/* core

clobber: clean
	rm -f $(ALL)

install: all
	mkdir -p $(DEST)/bin
	cp $(ALL) $(DEST)/bin

CCCFLAGS = -DCCC -i../native/include

SRCS = asm.c asmz80.c asz.c wsld.c wslib.c wsnm.c wsobj.c wssize.c

stage1: 
	mkdir -p stage1
	@for f in $(SRCS); do \
		b=$${f%.c}; \
		printf "%-20s" "$$f: "; \
		$(CCC) -DCCC -k -P -c $$f 2> stage1/$$b.err 1>&2 && { echo "ok"; rm -f stage1/$$b.err; } || echo "FAIL"; \
        mv $$b.i $$b.x $$b.1 $$b.2 $$b.pp $$b.s $$b.o $(OUTDIR) 2>/dev/null ; true ;\
	done
	@echo "Stage1 complete"

sizecheck: stage1
	@echo "=== tools stage1 sizes ==="
	@./wssize $(STAGE1_OBJS) | \
	awk '($$1 != "text") {printf "%-12s %5d %5d %5d = %5d\n", substr($$6, 8), $$1, $$2, $$3, $$1+$$2+$$3}'

# Link stage1 tools with libccc.a
WSLD = ./wsld
LIBCCC = ../native/lib/libccc.a

stage1/asz: stage1/asz.o stage1/asm.o stage1/asmz80.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/asz.o stage1/asm.o stage1/asmz80.o stage1/wsobj.o $(LIBCCC)

stage1/wsld: stage1/wsld.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wsld.o stage1/wsobj.o $(LIBCCC)

stage1/wsnm: stage1/wsnm.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wsnm.o stage1/wsobj.o $(LIBCCC)

stage1/wslib: stage1/wslib.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wslib.o $(LIBCCC)

stage1/wssize: stage1/wssize.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wssize.o $(LIBCCC)

link: stage1/asz stage1/wsld stage1/wsnm stage1/wslib stage1/wssize
	@echo "=== Linked stage1 tools ==="
	@ls -la stage1/asz stage1/wsld stage1/wsnm stage1/wslib stage1/wssize

.PHONY: all clean clobber test install stage1 sizecheck link

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
