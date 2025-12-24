#
# Top-level Makefile for ccc compiler
#
# Orchestrates builds across subdirectories
#

CC = gcc

# Installation destination - propagated to all submakes
DEST = $(CURDIR)/root

# Subdirectories to build
DIRS = cpp pass1 pass2 tools libsrc

SUBMAKE = $(MAKE) CC=$(CC) DEST=$(DEST)

all:
	@for d in $(DIRS); do $(SUBMAKE) -C $$d all; done

install:
	@mkdir -p $(DEST)/bin $(DEST)/lib
	@for d in $(DIRS); do $(SUBMAKE) -C $$d install; done

clean:
	@for d in $(DIRS); do $(SUBMAKE) -C $$d clean; done
	rm -f *.ast *.s *.pp *.i *.x
	rm -rf stage1

clobber:
	@for d in $(DIRS); do $(SUBMAKE) -C $$d clobber; done
	rm -f tags doc.pdf prev.size cur.size

stage1: install
	@echo "Building stage1 with cross ccc"
	@for d in $(DIRS); do $(MAKE) CC=ccc DEST=$(DEST) -C $$d stage1; done
	@echo "Stage1 build complete"

test: install
	$(SUBMAKE) -C tests test

tests: install
	$(SUBMAKE) -C tests tests

valgrind: install
	$(SUBMAKE) -C tests valgrind

tags:
	ctags cpp/*.c pass1/*.c pass2/*.c tools/*.c

sizecheck:
	@true

.PHONY: all install clean clobber stage1 test tests valgrind tags sizecheck
#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
