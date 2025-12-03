ASZ = ../../asz
AFLAGS = -8

SRCS = $(wildcard *.s)
OBJS = $(SRCS:.s=.o)

all: $(OBJS)

%.o: %.s
	$(ASZ) $(AFLAGS) $<

clean:
	rm -f $(OBJS)

.PHONY: all clean
