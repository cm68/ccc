# Host tools.
#
# The Whitesmith's object toolchain:
#   asz    - Z80 assembler
#   wsld   - linker
#   wsnm   - object/archive dump
#   wslib  - archive librarian
#   wssize - display object sizes
#
# plus the two that belong to the compiler:
#   ccc    - the driver: runs cpp, c0, c1, peep, asz and wsld
#   astpp  - AST pretty-printer, for reading what pass1 emitted
#
# All of them run on the build host and install into unix/bin.

OUTDIR = stage1

include ../../GNUmakefile.inc

CC = gcc

# The compile itself is the default rule in GNUmakefile.inc.  There
# was a .c.o here as well, and a second one under ifeq($(CC),zxc) for
# the Hi-Tech bootstrap: both were dead, because GNU make prefers a
# pattern rule to a suffix rule and the one in the include always won.
# The bootstrap is over and zxc is gone from the tree; only the flags
# are left, and they are still guarded because "make stage1" runs this
# directory with CC=ccc.
ifeq ($(CC),gcc)
WARNS = -Wall -Werror -pedantic \
-Wdeclaration-after-statement -Werror=declaration-after-statement \
-Werror=implicit-function-declaration
CFLAGS = -m32 -std=c99 -g -O0 $(WARNS) -Dlinux
endif

ALL = asz wsld wsnm wslib wssize ccc astpp

all: $(ALL)

# Header dependencies
asz.o: asm.h
asm.o: asm.h wsobj.h
asmz80.o: asm.h
wsobj.o: wsobj.h
wsld.o: wsobj.h
wsnm.o: wsobj.h
wslib.o: wsobj.h
wssize.o: wsobj.h

# Link targets
asz: asz.o asm.o asmz80.o wsobj.o
	$(CC) $(CFLAGS) -o $@ asz.o asm.o asmz80.o wsobj.o

wsld: wsld.o wsobj.o
	$(CC) $(CFLAGS) -o $@ wsld.o wsobj.o

wsnm: wsnm.o wsobj.o
	$(CC) $(CFLAGS) -o $@ wsnm.o wsobj.o

wslib: wslib.o
	$(CC) $(CFLAGS) -o $@ wslib.o

wssize: wssize.o
	$(CC) $(CFLAGS) -o $@ wssize.o

# The driver finds the passes and the runtime from argv[0] - see the
# comment on libdir in ccc.c - so nothing about where the tree lives
# is compiled in.  DEFTARGET names the system this driver produces
# binaries for when -m does not say; a driver built to run ON Micronix
# would set it to micronix, one built for CP/M to cpm.
# DEFLIB is only reached when argv[0] has no slash - a bare "ccc"
# typed at a shell, which is what a real installation gets.  A driver
# for Micronix wants /lib; a host install wants the prefix it was
# installed under.
#
# So it is built for PREFIX, not for the build tree, and there is only
# one of it.  That is what lets "make sysinstall" be a plain recursive
# copy with nothing to rebuild: the driver sitting in unix/bin is
# already the driver that belongs in $(PREFIX)/bin.
#
# Nothing in the tree is worse off for it.  Every rule here runs the
# driver as $(CCC) - an absolute path - so argv[0] has a slash, libdir
# comes out of the path, and DEFLIB is never consulted in-tree.
CCCPREFIX ?= $(PREFIX)

ccc: ccc.c
	$(CC) $(CFLAGS) -DDEFTARGET='"micronix"' \
		-DDEFLIB='"$(CCCPREFIX)/lib"' -o $@ $<

# astpp walks the AST pass1 writes; format.h is the shape of it and
# lives in ccclib because pass2 reads it too.
#
# It gets its own flags.  Everything else here is held to c99 with
# declarations first, because it has to stay compilable by this
# compiler; astpp is a host-only debugging aid that never has to
# survive that, and it was written mixing declarations and code.
ASTPPFLAGS = -m32 -g -O0 -Wall -Werror

astpp: astpp.c ../ccclib/format.h
	$(CC) $(ASTPPFLAGS) -I../ccclib -I../cpp -o $@ $<

test: asz
	@sh tests/check-dollar.sh

# tests/ holds hand-written .s assembler cases, so its cleanup names
# what the check script produces rather than sweeping the suffix.
clean:
	rm -f $(CRUFT) $(CRUFTASM)
	rm -f tests/*.out tests/*.a tests/*.o tests/*.n tests/*.lst tests/core

clobber: clean
	rm -f $(ALL) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

# The driver is the user command; the assembler, the linker and the
# AST printer are things the driver runs, so they go in lib beside the
# passes.  wslib, wsnm and wssize are ordinary tools a person uses on
# an object file and stay on the path.
LIBTOOLS = asz wsld astpp
BINTOOLS = ccc wslib wsnm wssize

install: all
	@mkdir -p $(UNIXDIR)/bin $(UNIXDIR)/lib
	$(INSTALL) $(BINTOOLS) $(UNIXDIR)/bin
	$(INSTALL) $(LIBTOOLS) $(UNIXDIR)/lib

CCCFLAGS = -DCCC -i../include

SRCS = asm.c asmz80.c asz.c wsld.c wslib.c wsnm.c wsobj.c wssize.c

stage1: 
	mkdir -p stage1
	@for f in $(SRCS); do \
		b=$${f%.c}; \
		printf "%-20s" "$$f: "; \
		$(CCC) -DCCC -k -c $$f 2> stage1/$$b.err 1>&2 && { echo "ok"; rm -f stage1/$$b.err; } || echo "FAIL"; \
        mv $$b.i $$b.x $$b.1 $$b.2 $$b.pp $$b.s $$b.o $(OUTDIR) 2>/dev/null ; true ;\
	done
	@echo "Stage1 complete"

sizecheck: stage1
	@echo "=== tools stage1 sizes ==="
	@./wssize $(STAGE1_OBJS) | \
	awk '($$1 != "text") {printf "%-12s %5d %5d %5d = %5d\n", substr($$6, 8), $$1, $$2, $$3, $$1+$$2+$$3}'

# Link stage1 tools with libccc.a
WSLD = ./wsld
LIBCCC = ../ccclib/libccc.a

stage1/asz: stage1/asz.o stage1/asm.o stage1/asmz80.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/asz.o stage1/asm.o stage1/asmz80.o stage1/wsobj.o $(LIBCCC)

stage1/wsld: stage1/wsld.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wsld.o stage1/wsobj.o $(LIBCCC)

stage1/wsnm: stage1/wsnm.o stage1/wsobj.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wsnm.o stage1/wsobj.o $(LIBCCC)

stage1/wslib: stage1/wslib.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wslib.o $(LIBCCC)

stage1/wssize: stage1/wssize.o $(LIBCCC)
	$(WSLD) -r -o $@ stage1/wssize.o $(LIBCCC)

link: stage1/asz stage1/wsld stage1/wsnm stage1/wslib stage1/wssize
	@echo "=== Linked stage1 tools ==="
	@ls -la stage1/asz stage1/wsld stage1/wsnm stage1/wslib stage1/wssize

regression:
	../../tests/regress.sh --filter src/tools/ $(REGRESS_FLAGS)

.PHONY: all clean clobber test install stage1 sizecheck link regression

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
