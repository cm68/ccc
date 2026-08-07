#
# Makefile for hitech C compiler
#
# Builds both pass1 (c0) and pass2 (c1)
#

DEST ?= ../root

all:
	$(MAKE) -C pass1
	$(MAKE) -C pass2

install:
	$(MAKE) -C pass1 DEST=$(DEST) install
	$(MAKE) -C pass2 DEST=$(DEST) install

clean:
	$(MAKE) -C pass1 clean
	$(MAKE) -C pass2 clean

clobber:
	$(MAKE) -C pass1 clobber
	$(MAKE) -C pass2 clobber

.PHONY: all install clean clobber

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
