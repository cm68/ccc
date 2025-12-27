#
# Makefile for ccc C compiler
#
# Builds pass1 (c0), pass2 (c1), and astpp
#

DEST = $(realpath ../root)

CC = gcc
CFLAGS = -m32 -g -O0 -Wall -Werror

all:
	$(MAKE) -C pass1
	$(MAKE) -C pass2
	$(MAKE) astpp

astpp: astpp.c
	$(CC) $(CFLAGS) -o $@ $<

install:
	$(MAKE) -C pass1 DEST=$(DEST) install
	$(MAKE) -C pass2 DEST=$(DEST) install

clean:
	$(MAKE) -C pass1 clean
	$(MAKE) -C pass2 clean

clobber:
	$(MAKE) -C pass1 clobber
	$(MAKE) -C pass2 clobber
	rm -f astpp

.PHONY: all install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
