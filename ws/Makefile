# Whitesmith's tools
# asz    - Z80 assembler
# wsld   - linker
# wsnm   - object/archive dump
# wslib  - archive librarian
# wssize - display object sizes

CC = gcc
CFLAGS = -m32 -g -Wall -std=c90 -pedantic -Dlinux -std=c89

ALL = asz wsld wsnm wslib wssize

all: $(ALL)

asz: asz.o asm.o wsobj.o
	$(CC) $(CFLAGS) -o asz asz.o asm.o wsobj.o

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
	rm -f $(ALL) *.o tests/*.out tests/test.a tests/ctest.o
	rm -f stage1/*.ast stage1/*.s stage1/*.o

install: all
	sudo cp $(ALL) /usr/local/bin/

# Stage1 build - compile ws tools to native Z80
# Uses ../cc1, ../cc2, ./asz from host build

CC1 = ../cc1
CC2 = ../cc2
ASZ = ./asz
CCCFLAGS = -DCCC -i../native/include

STAGE1_SRCS = asm.c asz.c wsld.c wslib.c wsnm.c wsobj.c wssize.c
STAGE1_OBJS = $(addprefix stage1/, $(STAGE1_SRCS:.c=.o))

stage1: $(CC1) $(CC2) $(ASZ)
	@mkdir -p stage1
	for f in $(STAGE1_SRCS); do \
	  b=$$(basename $$f .c) ; \
	  printf "%-12s" "$$f:"; \
	  $(CC1) $(CCCFLAGS) -o stage1/$$b.ast $$f 2>stage1/$$b.err ; \
	  if [ $$? -ne 0 ]; then \
	    echo "FAIL (parse)"; \
	  else \
	    $(CC2) -o stage1/$$b.s stage1/$$b.ast 2>>stage1/$$b.err ; \
	    if [ $$? -ne 0 ]; then \
	      echo "FAIL (codegen)"; \
	    else \
	      $(ASZ) -o stage1/$$b.o stage1/$$b.s 2>>stage1/$$b.err ; \
	      if [ $$? -ne 0 ]; then \
	        echo "FAIL (asm)"; \
	      else \
	        echo "PASS"; \
	      fi; \
	    fi; \
	  fi; \
	done
	@echo "Stage1 complete"

sizecheck: stage1
	@echo "=== ws tools stage1 sizes ==="
	@../ws/wssize $(STAGE1_OBJS) | \
	awk '($$1 != "text") {printf "%-12s %5d %5d %5d = %5d\n", substr($$6, 8), $$1, $$2, $$3, $$1+$$2+$$3}'

# Link stage1 tools with libccc.a
WSLD = ./wsld
LIBCCC = ../native/lib/libccc.a

stage1/asz: stage1/asz.o stage1/asm.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/asz.o stage1/asm.o stage1/wsobj.o $(LIBCCC)

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

.PHONY: all clean test install stage1 sizecheck link
