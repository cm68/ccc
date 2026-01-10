#
# Makefile for ccc C compiler
#
# Builds cpp, pass1 (c0), pass2 (c1), and astpp
#

DEST = $(realpath ../root)

CC = gcc
CFLAGS = -m32 -g -O0 -Wall -Werror

all:
	$(MAKE) -C cpp
	$(MAKE) -C pass1
	$(MAKE) -C pass2
	$(MAKE) astpp

astpp: astpp.c
	$(CC) $(CFLAGS) -o $@ $<

install: astpp
	$(MAKE) -C cpp DEST=$(DEST) install
	$(MAKE) -C pass1 DEST=$(DEST) install
	$(MAKE) -C pass2 DEST=$(DEST) install
	cp astpp ../root/bin
	
clean:
	$(MAKE) -C cpp clean
	$(MAKE) -C pass1 clean
	$(MAKE) -C pass2 clean

clobber:
	$(MAKE) -C cpp clobber
	$(MAKE) -C pass1 clobber
	$(MAKE) -C pass2 clobber
	rm -f astpp

stage1:
	$(MAKE) -C cpp DEST=$(DEST) stage1
	$(MAKE) -C pass1 DEST=$(DEST) stage1
	$(MAKE) -C pass2 DEST=$(DEST) stage1

sizecheck:
	$(MAKE) -C cpp sizecheck
	$(MAKE) -C pass1 sizecheck
	$(MAKE) -C pass2 sizecheck

.PHONY: all install clean clobber stage1 sizecheck

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
