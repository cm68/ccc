#
# generate the libraries and c startup objects
#

AS = asz

DIRS = libu libc libcpm
STARTUP = crtcpm.o crt0.o
DEST = $(realpath ../root/lib)

all: $(STARTUP)
	for i in $(DIRS); do \
		$(MAKE) -C $$i ; \
	done

crtcpm.o: crtcpm.s
	$(AS) -o crtcpm.o crtcpm.s

crt0.o: crt0.s
	$(AS) -o crt0.o crt0.s

install: all
	mkdir -p $(DEST)
	for i in $(DIRS); do \
		$(MAKE) -C $$i DEST=$(DEST) install ; \
	done
	cp $(STARTUP) $(DEST)

clobber: clean
	rm -f $(STARTUP)
	for i in $(DIRS); do \
		$(MAKE) -C $$i clobber ; \
	done

clean: 
	for i in $(DIRS); do \
		$(MAKE) -C $$i clean ; \
	done

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
