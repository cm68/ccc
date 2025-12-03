# wsld - Whitesmith's linker

CC = gcc
CFLAGS = -g -Wall -std=c90 -pedantic -Dlinux
ASZ = ../asz/asz

wsld: main.c
	$(CC) $(CFLAGS) -o wsld main.c

test1.o: test1.s
	$(ASZ) -o test1.o test1.s

test2.o: test2.s
	$(ASZ) -o test2.o test2.s

test: wsld test1.o test2.o
	@echo "=== linking test1.o test2.o ==="
	./wsld -v test1.o test2.o -o linked.out
	@echo ""
	@echo "=== linking with base addresses ==="
	./wsld -v test1.o test2.o -Ttext 0x100 -o linked100.out
	@echo ""
	@echo "=== verifying output ==="
	@xxd linked100.out | head -5

clean:
	rm -f wsld *.o

.PHONY: clean test
