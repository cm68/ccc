/*
 * what every pass1 source needs, and no more
 *
 * The system headers, the generated ones, the shared typedefs and the
 * error codes.  Everything past this is a part of the compiler rather
 * than its furniture, and lives in a p1*.h a source includes only if
 * it uses it - see p1expr.h for why that matters.
 */
#ifndef _P1CORE_H
#define _P1CORE_H

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

#endif
