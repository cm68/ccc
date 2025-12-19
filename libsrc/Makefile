# CCC Runtime Library Makefile
# Builds the Z80 runtime libraries for programs compiled with ccc

ROOTDIR ?= ../root
DIRS = libc libu

.PHONY: all clean clobber install

all:
	@for d in $(DIRS) ; do $(MAKE) -C $$d all ; done

install: all
	mkdir -p $(ROOTDIR)/lib
	mkdir -p $(ROOTDIR)/usr/include
	cp libc/crt0.o libc/libc.a libu/libu.a $(ROOTDIR)/lib
	cp include/*.h $(ROOTDIR)/usr/include

clobber:
	@for d in $(DIRS) ; do $(MAKE) -C $$d clobber ; done
	
clean:
	@for d in $(DIRS) ; do $(MAKE) -C $$d clean ; done
#
# vim: tabstop=4 shiftwidth=4 expandtab:
#
