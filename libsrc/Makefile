# CCC Runtime Library Makefile
# Builds the Z80 runtime libraries for programs compiled with ccc

DIRS = libc libu

.PHONY: all clean install

all:
	@for d in $(DIRS) ; do $(MAKE) -C $$d all ; done

install: all
	mkdir -p $(ROOTDIR)/lib
	mkdir -p $(ROOTDIR)/usr/include
	cp libc/crt0.o libc/libc.a libu/libu.a $(ROOTDIR)/lib
	cp include/*.h $(ROOTDIR)/usr/include

clean:
	@for d in $(DIRS) ; do $(MAKE) -C $$d clean ; done
