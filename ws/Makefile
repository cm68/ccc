# Whitesmith's tools
# asz    - Z80 assembler
# wsld   - linker
# wsnm   - object/archive dump
# wslib  - archive librarian
# wssize - display object sizes

ROOTDIR = ../root
CCC = ../root/bin/ccc
OUTDIR = stage1

CC = gcc
CFLAGS = -m32 -g -Wall -std=c90 -pedantic -Dlinux -std=c89

ALL = asz wsld wsnm wslib wssize

all: $(ALL)

asz: asz.o asm.o asmz80.o wsobj.o
	$(CC) $(CFLAGS) -o asz asz.o asm.o asmz80.o wsobj.o

asmz80.o: asmz80.c asm.h
	$(CC) $(CFLAGS) -c asmz80.c

asz.o: asz.c asm.h
	$(CC) $(CFLAGS) -c asz.c

asm.o: asm.c asm.h wsobj.h
	$(CC) $(CFLAGS) -c asm.c

wsobj.o: wsobj.c wsobj.h
	$(CC) $(CFLAGS) -c wsobj.c

wsld: wsld.o wsobj.o
	$(CC) $(CFLAGS) -o wsld wsld.o wsobj.o

wsld.o: wsld.c wsobj.h
	$(CC) $(CFLAGS) -c wsld.c

wsnm: wsnm.c wsobj.o wsobj.h
	$(CC) $(CFLAGS) -o wsnm wsnm.c wsobj.o

wslib: wslib.c wsobj.h
	$(CC) $(CFLAGS) -o wslib wslib.c

wssize: wssize.c wsobj.h
	$(CC) $(CFLAGS) -o wssize wssize.c

test: all
	@echo "=== Testing asz ==="
	./asz tests/ctest.s -o tests/ctest.o
	@echo ""
	@echo "=== Testing wsnm on object ==="
	./wsnm tests/ctest.o
	@echo ""
	@echo "=== Testing wsnm on archive ==="
	./wsnm tests/libu.a 2>&1 | head -50
	@echo ""
	@echo "=== Testing wslib -t ==="
	./wslib -tv tests/libu.a | head -20
	@echo ""
	@echo "=== Testing wslib -c (create) ==="
	./wslib -cv tests/test.a tests/chdr.o tests/ctest.o
	./wslib -tv tests/test.a
	@echo ""
	@echo "=== Testing wslib -x (extract) ==="
	mkdir -p tests/tmp && cd tests/tmp && ../../wslib -xv ../test.a
	ls -la tests/tmp/
	rm -rf tests/tmp
	@echo ""
	@echo "=== Testing wsld ==="
	./wsld -v -o tests/test.out tests/chdr.o tests/ctest.o
	./wsnm tests/test.out

clean:
	rm -f *.a *.o tests/*.out tests/*.a tests/*.o
	rm -f stage1/* core

clobber: clean
	rm -f $(ALL)

install: all
	mkdir -p $(ROOTDIR)/bin
	cp $(ALL) $(ROOTDIR)/bin

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
	@echo "=== ws tools stage1 sizes ==="
	@../ws/wssize $(STAGE1_OBJS) | \
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
# vim: tabstop=4 shiftwidth=4 expandtab:
#
