/*
 * Runtime correctness tests.
 *
 * Each test file is a set of checks and a main() that returns the
 * number of the first check that failed, or zero if all passed.  The
 * exit status names the failure on its own, which matters because the
 * Z80 paths have no output worth linking - there is nothing to parse
 * and nothing to keep in step across three toolchains.
 *
 * Use short, not int, for anything whose width matters.  int is 16
 * bits on the Z80 and 32 on the host, so an int-based check would
 * compute different answers in the two places and the native build
 * would be no kind of reference.  short is 16 bits everywhere.
 *
 * Keep to the tree's restrictions: K&R declarations, no auto aggregate
 * initialisers, no struct assignment, no const or signed keywords.
 * These files have to compile under gcc, zc3 and ccc alike.
 */
#ifndef RT_H
#define RT_H

#define CHECK(n, got, want)	if ((got) != (want)) return (n)

#endif
