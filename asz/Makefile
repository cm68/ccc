#
# native Makefile for asz assembler
#
# /usr/src/cmd/asz/Makefile 
#
# Changed: <2023-07-05 22:46:38 curt>
#
# vim: tabstop=4 shiftwidth=4 noexpandtab:

all: asz

asz: main.o asm.o
	cc -g -o asz main.o asm.o

main.o:	main.c
	cc -g -c main.c

asm.o:	asm.c
	cc -g -c asm.c

test: asz
	./asz util.s
	./asz expr.s

clean:
	rm *.o

clobber: clean
	rm -f asz

