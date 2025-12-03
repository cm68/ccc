# wsnm - Whitesmith's object file dump utility

CC = gcc
CFLAGS = -g -Wall -std=c90 -pedantic -Dlinux

wsnm: main.c
	$(CC) $(CFLAGS) -o wsnm main.c

test: wsnm
	./wsnm ../wsld/test1.o
	@echo ""
	./wsnm -d ../wsld/test1.o

clean:
	rm -f wsnm

.PHONY: clean test
