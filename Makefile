#
# Top-level Makefile for ccc
#
# Orchestrates src/ and holds the install trees.  See tree.mk for the
# layout and README.md for what lives where.
#

CC = gcc

include tree.mk
include cruft.mk

# Directories cleaned from here.  src does its own orchestration;
# tests is not built by "all" but is where most of the tree's
# droppings land, so clean and clobber have to reach it.
CLEANDIRS = src tests

SUBMAKE = $(MAKE) CC=$(CC) TOP=$(TOP)

#
# The build has two phases and the order is forced by what the thing
# is.  The target runtime is compiled BY this compiler, so the host
# side has to be built and installed into unix/ before the target
# side can begin.  src/Makefile runs them in that order.
#
all:
	$(SUBMAKE) -C src all

host:
	$(SUBMAKE) -C src host

target:
	$(SUBMAKE) -C src target

install: all

clean:
	@for d in $(CLEANDIRS); do $(SUBMAKE) -C $$d clean; done
	rm -f $(CRUFT) $(CRUFTASM)

# The install trees go too: unlike the old root/, they hold nothing
# but what a build put there.  root/ used to carry a hand-made symlink
# to the simulator, which is why it was spared; that now lives in
# tests/ where the harness that uses it can find it.
clobber:
	@for d in $(CLEANDIRS); do $(SUBMAKE) -C $$d clobber; done
	rm -f $(CRUFT) $(CRUFTASM) $(CRUFTLOG) doc.pdf
	rm -rf $(CRUFTDIRS) $(UNIXDIR) $(MXDIR) $(CPMDIR)

stage1: all
	@echo "Building stage1 with cross ccc"
	$(MAKE) CC=ccc TOP=$(TOP) -C src stage1
	@echo "Stage1 build complete"

test: all
	$(SUBMAKE) -C src/tools test
	$(SUBMAKE) -C tests test

tests: all
	$(SUBMAKE) -C tests tests

# Leaks are not what this is for: these programs read a file, write a
# file and exit.  A field read before it is written is the thing that
# matters, because it changes what the compiler emits and does it
# differently depending on what was compiled before.
valgrind: all
	sh tests/vgsweep.sh

tags:
	ctags src/cpp/*.c src/pass1/*.c src/pass2/*.c src/peep/*.c src/tools/*.c

# Native (Z80) compile of cpp/c0/c1, for the size report.
sizecheck:
	$(MAKE) TOP=$(TOP) -C src sizecheck

# The self-hosted passes, built by ccc for the simulator and
# installed into micronix/bin.
micronix: all
	$(SUBMAKE) -C src micronix

# The same, as CP/M .com images into cpm/bin.
cpm: all
	$(SUBMAKE) -C src cpm

# Run the compiler's own passes under the CP/M 3 machine in
# src/cpm3 and check they agree with the host.
selfcheck:
	$(SUBMAKE) -C src selfcheck

# Run the cpp regression harness over the full corpus.
# Pass REGRESS_FLAGS=--bless to regenerate the baseline.
regression:
	./tests/regress.sh $(REGRESS_FLAGS)

# The production-coverage suite: generate the operator x width x
# residence corpus, prove no shape lacks a rule, run it native and
# under the simulator, hold the rule-coverage baseline, and compile
# the corpus with the SIMULATED c0/c1, byte-compared against the host.
prodtest: all
	$(SUBMAKE) -C tests/gen

.PHONY: all host target install clean clobber stage1 test tests valgrind \
	tags sizecheck micronix cpm selfcheck regression prodtest

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
