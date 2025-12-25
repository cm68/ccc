#
# produce the hitech c library into a whitesmith object file
#
DIRS = libu libc

all: crtcpm.o crt0.o
	for i in $(DIRS); do \
		$(MAKE) -C $$i ; \
	done

crtcpm.o: crtcpm.s
	asz -o crtcpm.o crtcpm.s

crt0.o: crt0.s
	asz -o crt0.o crt0.s

install: crt0.o
	for i in $(DIRS); do \
		$(MAKE) -C $$i install ; \
	done
	cp crt0.o ../../../root/lib

clobber: clean
	for i in $(DIRS); do \
		$(MAKE) -C $$i clobber ; \
	done

clean:
	for i in $(DIRS); do \
		$(MAKE) -C $$i clean ; \
	done
	rm *.o

# vim: tabstop=4 shiftwidth=4 noexpandtab:
