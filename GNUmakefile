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
# side can begin.  src/GNUmakefile runs them in that order.
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
#               into $(HOSTDIR) - bin, lib and usr/include - and the
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
	@echo "installed into $(HOSTDIR)"

# The system install.  A plain recursive copy of the tree that
# "install" just built: desthost/bin -> $(PREFIX)/bin, desthost/lib ->
# $(PREFIX)/lib, desthost/usr/include -> $(PREFIX)/usr/include.  The
# driver works out where its passes are from its own path - libdir is
# bin/../lib, see tools/ccc.c - so the layout carries over unchanged
# and nothing has to be rebuilt for the new location.
#
# sudo, because $(PREFIX) is not ours.  SUDO= runs it without, for a
# DESTDIR you already own.
# A copy never deletes, so anything that leaves the tree stays behind
# in $(PREFIX) for ever.  That is not hypothetical: lib/cpp outlived
# the rename to pass0, crtcpm.o and libcpm.a outlived the split into
# destcpm, and a stub sys/stat.h outlived being deleted - and the
# driver goes looking in lib/include, so a header nobody ships any
# more was still being found there.
#
# It cannot be fixed by emptying $(PREFIX) first.  These are shared
# directories: /usr/local/bin has twenty other programs in it and
# /usr/local/lib has python3.13, none of which are ours to remove.
#
# So the install writes down what it owns.  lib/ccc.manifest lists
# every file in the tree, is shipped as part of it, and the next
# sysinstall removes everything the last one listed before copying the
# new tree over.  A file that has left the tree leaves $(PREFIX) with
# it, and nothing that was never ours is touched.
#
# Two guards on the way in, because this deletes: a path out of the
# manifest is dropped if it is absolute or contains "..", and the
# removal is rm -f and never rm -r - so the worst a damaged manifest
# can do is leave an empty directory behind.
MANIFEST = lib/ccc.manifest

sysinstall: install
	@cd $(HOSTDIR) && find . -type f | sed 's|^\./||' | \
	    grep -v '^$(MANIFEST)$$' | sort > $(MANIFEST).tmp && \
	    { cat $(MANIFEST).tmp; echo $(MANIFEST); } > $(MANIFEST) && \
	    rm -f $(MANIFEST).tmp
	$(SUDO) $(MKDIR) $(DESTDIR)$(PREFIX)
	@# lib/include is wholly ours and is written out fresh by every
	@# build, so it goes wholesale rather than a file at a time.  That
	@# covers what the manifest cannot: a header installed before
	@# there was a manifest to list it - which is exactly how the stub
	@# sys/stat.h went on being found there after it was deleted.
	@#
	@# Only this one.  usr/include is NOT ours to empty: the real
	@# Micronix sys/stat.h lives there and this tree does not ship it,
	@# so it is not in the manifest and must not be swept up.
	$(SUDO) rm -rf $(DESTDIR)$(PREFIX)/lib/include
	@if [ -f $(DESTDIR)$(PREFIX)/$(MANIFEST) ]; then \
	    echo "removing what the last install left"; \
	    sed -e '/^\//d' -e '/\.\./d' $(DESTDIR)$(PREFIX)/$(MANIFEST) | \
	    sed 's|^|$(DESTDIR)$(PREFIX)/|' | \
	    $(SUDO) xargs -r $(RM); \
	fi
	$(SUDO) $(CP) -r $(HOSTDIR)/. $(DESTDIR)$(PREFIX)
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
	rm -rf $(CRUFTDIRS) $(HOSTDIR) $(MXDIR) $(CPMDIR)

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
# installed into destmicronix/bin.
micronix: all
	$(SUBMAKE) -C src micronix

#
# DORMANT (2026-08-13).  The CP/M target is parked, and is expected to
# stay parked for a long time.  Do not work on it.
#
# It is not parked because it broke.  It is parked because the passes do
# not fit: CP/M 3 leaves about 61K of TPA and there is no room in it for
# what these have become.  A serious slimming exercise comes first, and
# until that has happened there is nothing to be gained by making the
# link work - a pass that fits nothing is not progress.
#
# So this target's failures are not findings.  The one you will hit
# first is
#
#	wsld: cannot open desthost/lib/crtcpm.o
#
# which no rule builds - src/libcpm/crtcpm.s is the only crtcpm in the
# tree - and which the install note at the top of this file already
# records as having outlived the split into destcpm.  Leave it.
#
# CPM=1 builds it anyway, for whoever picks the slimming up.
#
cpm: all
	@if [ -z "$(CPM)" ]; then \
		echo "cpm: dormant - the passes do not fit the TPA yet."; \
		echo "     Slimming comes first.  CPM=1 to build it anyway."; \
	else \
		$(SUBMAKE) -C src cpm ; \
	fi

# Run the compiler's own passes under the CP/M 3 machine in
# src/cpm3 and check they agree with the host.
#
# DORMANT with the cpm target above, which it is built on.
selfcheck:
	@if [ -z "$(CPM)" ]; then \
		echo "selfcheck: dormant with the cpm target - see GNUmakefile."; \
	else \
		$(SUBMAKE) -C src selfcheck ; \
	fi

# libc.a and libccc.a are system-independent by construction: the
# system-call layer is libu.a on Micronix and libcpm.a on CP/M, and
# nothing target-specific belongs above that line.  Every install
# holds one copy of each under the same name, so if they ever diverge
# the two targets silently get different code from the same filename.
# That is a bug, and this is where it gets caught.
libcheck:
	@cmp $(MXDIR)/lib/libc.a   $(CPMDIR)/lib/libc.a && \
	 cmp $(MXDIR)/lib/libccc.a $(CPMDIR)/lib/libccc.a && \
	 echo "libcheck: libc.a and libccc.a are the same for both targets"

# THE CROSS-TARGET GATE.  Compile every source of every pass three
# ways - host, Micronix under the usersim, CP/M 3 on the cpm3 machine -
# and assert the two simulated legs emit exactly what the host does.
#
# This is what makes a self-hosted build worth having.  A compiler that
# runs on the target but emits something subtly different is worse than
# one that will not run at all, because its output looks right.
#
# Two ways for now, not three: the CP/M leg is dormant (see the cpm
# target above), so this runs LEGS=mx and the Micronix leg is the gate.
# It is still the gate that matters - a pass that runs on the target and
# emits something subtly different is worse than one that will not run.
#
# LEGS=cpm or LEGS="mx cpm" asks for the dormant leg back, once there is
# a CP/M build to compare against; TPA= sizes the CP/M machine.
selfhost: all micronix
	@LEGS=$${LEGS:-mx} sh src/cpm3/selfhost.sh

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
