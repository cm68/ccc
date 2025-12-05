# Whitesmith's tools
# asz    - Z80 assembler
# wsld   - linker
# wsnm   - object/archive dump
# wslib  - archive librarian
# wssize - display object sizes

CC = gcc
CFLAGS = -m32 -g -Wall -std=c90 -pedantic -Dlinux

ALL = asz wsld wsnm wslib wssize

all: $(ALL)

asz: asz.o asm.o
	$(CC) $(CFLAGS) -o asz asz.o asm.o

asz.o: asz.c asm.h
	$(CC) $(CFLAGS) -c asz.c

asm.o: asm.c asm.h
	$(CC) $(CFLAGS) -c asm.c

wsld: wsld.c
	$(CC) $(CFLAGS) -o wsld wsld.c

wsnm: wsnm.c
	$(CC) $(CFLAGS) -o wsnm wsnm.c

wslib: wslib.c
	$(CC) $(CFLAGS) -o wslib wslib.c

wssize: wssize.c
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
	rm -f $(ALL) *.o tests/*.out tests/test.a tests/ctest.o

install: all
	sudo cp $(ALL) /usr/local/bin/

.PHONY: all clean test install
