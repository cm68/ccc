#
# Makefile for ccc C compiler
#
# Builds cpp, pass1 (c0), pass2 (c1), and astpp
#

DEST = $(abspath ../root)

include ../cruft.mk

CC = gcc
CFLAGS = -m32 -g -O0 -Wall -Werror

all:
	$(MAKE) -C lib
	$(MAKE) -C cpp
	$(MAKE) -C pass1
	$(MAKE) -C pass2
	$(MAKE) -C peep
	$(MAKE) astpp

astpp: astpp.c
	$(CC) $(CFLAGS) -o $@ $<

# The host passes only.  lib's install is NOT here: it builds the z80
# archives with the compiler this target is in the middle of putting
# in place, so it would need c0, c1, cpp and peep before any of them
# had been copied.  That knot is what "runtime" below untangles.
install: astpp
	$(MAKE) -C cpp DEST=$(DEST) install
	$(MAKE) -C pass1 DEST=$(DEST) install
	$(MAKE) -C pass2 DEST=$(DEST) install
	$(MAKE) -C peep DEST=$(DEST) install
	cp astpp $(DEST)/bin

# The z80 support archive, built by the compiler install just placed.
runtime:
	$(MAKE) -C lib DEST=$(DEST) install

clean:
	$(MAKE) -C lib clean
	$(MAKE) -C cpp clean
	$(MAKE) -C pass1 clean
	$(MAKE) -C pass2 clean
	$(MAKE) -C peep clean
	rm -f $(CRUFT) $(CRUFTASM)

clobber:
	$(MAKE) -C lib clobber
	$(MAKE) -C cpp clobber
	$(MAKE) -C pass1 clobber
	$(MAKE) -C pass2 clobber
	$(MAKE) -C peep clobber
	rm -f $(CRUFT) $(CRUFTASM) $(CRUFTLOG) astpp
	rm -rf $(CRUFTDIRS)

stage1:
	$(MAKE) -C cpp DEST=$(DEST) stage1
	$(MAKE) -C pass1 DEST=$(DEST) stage1
	$(MAKE) -C pass2 DEST=$(DEST) stage1
	$(MAKE) -C peep DEST=$(DEST) stage1

# Native (Z80) compile of cpp/c0/c1.
# Override Z80 compiler with: make sizecheck ZCC=zc3   (HiTech) or ZCC=ccc (default).
ZCC = ccc
sizecheck:
	$(MAKE) ZCC=$(ZCC) -C cpp sizecheck
	$(MAKE) ZCC=$(ZCC) -C pass1 sizecheck
	$(MAKE) ZCC=$(ZCC) -C pass2 sizecheck
	$(MAKE) ZCC=$(ZCC) -C peep sizecheck

# Regression - all ccc/ subtrees (cpp, pass1, pass2, lib, astpp)
regression:
	../tests/regress.sh --filter ccc/ $(REGRESS_FLAGS)

.PHONY: all install clean clobber stage1 sizecheck regression

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
