#
# Top-level GNUmakefile for ccc - everything driven from the build
# host: the host build ("all") and the cross build ("micronix",
# "cpm").  The native build has its own Makefiles and is not run from
# here.
#
# Orchestrates src/ and holds the install trees.  See GNUmakefile.inc
# for the three builds, the layout, and what the command names mean;
# README.md for what lives where.
#

CC = gcc

include GNUmakefile.inc

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

# INSTALLING IS TWO STEPS, and only the second one needs a password.
#
#   install     the tree walk.  Every directory copies what it built
#               into $(UNIXDIR) - bin, lib and usr/include - and the
#               result is a complete, working toolchain that happens
#               to be sitting in the build tree.  No privilege, no
#               destination outside the tree.
#   sysinstall  that directory, copied onto $(PREFIX).
#
# "all" already runs the walk, because the target runtime is compiled
# by the toolchain being installed and so has to come out of an
# install tree rather than a scatter of build directories.  So this is
# a no-op after a build, which is the point: sysinstall refreshes
# $(PREFIX) without rebuilding anything.
#
# It used to depend on micronix and cpm as well - a full Z80 self-host
# and a CP/M image build - to install a host cross compiler that
# copies neither of them anywhere.  Those have their own targets.
install: all
	@echo "installed into $(UNIXDIR)"

# The system install.  A plain recursive copy of the tree that
# "install" just built: unix/bin -> $(PREFIX)/bin, unix/lib ->
# $(PREFIX)/lib, unix/usr/include -> $(PREFIX)/usr/include.  The
# driver works out where its passes are from its own path - libdir is
# bin/../lib, see tools/ccc.c - so the layout carries over unchanged
# and nothing has to be rebuilt for the new location.
#
# sudo, because $(PREFIX) is not ours.  SUDO= runs it without, for a
# DESTDIR you already own.
sysinstall: install
	$(SUDO) $(MKDIR) $(DESTDIR)$(PREFIX)
	$(SUDO) $(CP) -r $(UNIXDIR)/. $(DESTDIR)$(PREFIX)
	@echo "installed: $(DESTDIR)$(PREFIX)/bin/ccc, libraries in $(PREFIX)/lib"

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

# libc.a and libccc.a are system-independent by construction: the
# system-call layer is libu.a on Micronix and libcpm.a on CP/M, and
# nothing target-specific belongs above that line.  Every install
# holds one copy of each under the same name, so if they ever diverge
# the two targets silently get different code from the same filename.
# That is a bug, and this is where it gets caught.
libcheck:
	@cmp micronix/lib/libc.a   cpm/lib/libc.a && \
	 cmp micronix/lib/libccc.a cpm/lib/libccc.a && \
	 echo "libcheck: libc.a and libccc.a are the same for both targets"

# THE CROSS-TARGET GATE.  Compile every source of every pass three
# ways - host, Micronix under the usersim, CP/M 3 on the cpm3 machine -
# and assert the two simulated legs emit exactly what the host does.
#
# This is what makes a self-hosted build worth having.  A compiler that
# runs on the target but emits something subtly different is worse than
# one that will not run at all, because its output looks right.
#
# Needs all three builds present, so it depends on them.  LEGS=mx or
# LEGS=cpm runs one target; TPA= sizes the CP/M machine.
selfhost: all micronix cpm
	@sh src/cpm3/selfhost.sh

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
	tags sizecheck micronix cpm selfcheck selfhost libcheck \
	regression prodtest

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
