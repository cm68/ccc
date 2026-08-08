/*
 * data structures for the compiler (pass 1).
 *
 * we don't have any other non-generated includes.
 * so, everything is right here
 *
 * nested includes are a bit ugly, but it means that i can just include cc1.h
 */

#include <stdlib.h>
/* stdio is DEBUG furniture: one setvbuf in pass1.c is the only
 * non-DEBUG use, and a header's macro and intern load is charged
 * to every file that includes it - see the footprint gate */
#ifndef CCC
#include <stdio.h>
#endif
#include <string.h>
#ifdef CCC
#include <unixio.h>
#endif

/*
 * generated files
 */
#include "debug.h"
#include "token.h"

#include "p1base.h"

/*
 * we just want the error symbols
 * error.h is generated, and contains actual error strings if DEF_ERRMSG.
 */
#undef DEF_ERRMSG
#include "error.h"

/*
 * Allocation counters for tracking memory usage
 */
#ifdef DEBUG
extern int nameAllocCnt;
extern int nameCurCnt;
extern int nameHighWater;
extern int exprAllocCnt;
extern int exprCurCnt;
extern int exprHighWater;
#endif

/*
 * The pieces.  cc1.h is the whole of pass1's world, as it always was;
 * a source that wants less includes the pieces it needs instead, and
 * leaves cpp the room to hold the source itself.
 */
#include "p1expr.h"
#include "p1type.h"
#include "p1name.h"
#include "p1stmt.h"
#include "p1lex.h"
