#!/bin/bash
# makedebug2.sh - Generate trace flags for cc2
# Scans cc2 source files for TRACE() calls and generates trace2.h and tracetags.c

CC2_SOURCES="parseast.c astio.c codegen.c emit.c emitexpr.c emitops.c emithelper.c"

echo "/* created by makedebug2.sh */" > trace2.h
echo "/* created by makedebug2.sh */" > tracetags.c
echo "char *topts[] = {" >> tracetags.c
k=1
tags=($(grep 'TRACE(' $CC2_SOURCES 2>/dev/null | sed -e 's/^.*TRACE(//' -e 's/).*$//' | sort -u))
for tag in ${tags[@]}; do
	printf "#define $tag 0x%02x\n" $k >> trace2.h
	printf "\"$tag\",\n" >> tracetags.c
	(( k = k << 1 ))
done
echo "0 };" >> tracetags.c
