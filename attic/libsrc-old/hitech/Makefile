#
# produce the hitech c library into a whitesmith object file
#
all: crtcpm.o crt0.o

crtcpm.o: crtcpm.s
	asz -o crtcpm.o crtcpm.s

crt0.o: crt0.s
	asz -o crt0.o crt0.s

clean:
	rm *.o

# vim: tabstop=4 shiftwidth=4 noexpandtab:
