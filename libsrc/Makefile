#
# generate the libraries and c startup objects
#

DEST ?= $(CURDIR)/../root

AS = asz

# Each library builds two areas, zc3/ and ccc/ - one archive per
# compiler - and installs them to root/lib/zc3 and root/lib/ccc.
# Links pick an area with -L; the two runtimes never mix in one image.
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
	rm -f $(STARTUP)
	for i in $(DIRS); do \
		$(MAKE) -C $$i clobber ; \
	done

clean:
	for i in $(DIRS); do \
		$(MAKE) -C $$i clean ; \
	done

regression:
	../tests/regress.sh --filter libsrc/ $(REGRESS_FLAGS)

.PHONY: all install clobber clean regression

# vim: tabstop=4 shiftwidth=4 noexpandtab:
