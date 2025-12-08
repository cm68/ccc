# CCC Runtime Library Makefile
# Builds the Z80 runtime libraries for programs compiled with ccc

DIRS = libc libu

.PHONY: all clean install

all:
	@for d in $(DIRS) ; do $(MAKE) -C $$d all ; done

install:
	@for d in $(DIRS) ; do $(MAKE) -C $$d install ; done

clean:
	@for d in $(DIRS) ; do $(MAKE) -C $$d clean ; done
