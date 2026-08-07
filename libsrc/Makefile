#
# generate the libraries and c startup objects
#

DEST ?= $(CURDIR)/../root

# CRUFTASM is not used anywhere under libsrc: crt0.s, crtcpm.s and
# every .s in the three libraries are sources.
include ../cruft.mk

# The assembler is named by path, not found on PATH.  It is built by
# tools/ in this same tree and installed a phase earlier; relying on
# PATH meant the build only worked on a machine that already had an
# older install of it sitting there.
AS = $(DEST)/bin/asz

# Each library builds one area, ccc/, and installs it to
# root/lib/ccc.  There was a zc3/ area beside it while Hi-Tech C was
# bootstrapping this compiler's runtime; that is finished.
DIRS = libu libc libcpm
STARTUP = crtcpm.o crt0.o

all: $(STARTUP)
	for i in $(DIRS); do \
		$(MAKE) -C $$i ; \
	done

crtcpm.o: crtcpm.s
	$(AS) -o crtcpm.o crtcpm.s

crt0.o: crt0.s
	$(AS) -o crt0.o crt0.s

install: all
	mkdir -p $(DEST)/lib
	for i in $(DIRS); do \
		$(MAKE) -C $$i DEST=$(DEST)/lib install ; \
	done
	cp $(STARTUP) $(DEST)/lib
	mkdir -p $(DEST)/usr/include
	cp include/*.h $(DEST)/usr/include
	cp -r include/sys $(DEST)/usr/include

clobber: clean
	rm -f $(STARTUP) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)
	for i in $(DIRS); do \
		$(MAKE) -C $$i clobber ; \
	done

# old/ is superseded runtime kept for reference and has no place in
# DIRS, but a build ran there once and left its objects behind.  The
# .s files beside them are the reference and stay.
clean:
	rm -f $(CRUFT) $(CRUFTASMGEN)
	rm -f include/*.i include/*.x
	rm -f old/*/*.o old/*/*.a
	for i in $(DIRS); do \
		$(MAKE) -C $$i clean ; \
	done

regression:
	../tests/regress.sh --filter libsrc/ $(REGRESS_FLAGS)

.PHONY: all install clobber clean regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
