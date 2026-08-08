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
/*
 * Compiler-wide control.
 *
 * These were in p1stmt.h, beside the switch tables, and every source
 * that wanted to know which parsing phase it was in had to take the
 * statement machinery with it - which is most of them, and is what
 * made p1stmt.h the expensive include.  They are not statements;
 * they are the state the whole pass runs under.
 */
struct name;			/* a pointer to one is all this needs */

/* Global context for static variable name mangling */
extern struct name *curFunc;
extern unsigned char staticCtr;  // file-global counter for static variable names
extern unsigned char shadowCtr;  // counter for shadowed locals

/* AST output control */
extern unsigned char astFd;         // where to write AST output
extern unsigned char asmFd;         // where to write global data assembly

/* Two-phase parsing control */
extern unsigned char phase;         // 1 = build symbol table, 2 = emit AST

#endif
#include <string.h>
#ifdef CCC
#include <unixio.h>
/*
 * Compiler-wide control.
 *
 * These were in p1stmt.h, beside the switch tables, and every source
 * that wanted to know which parsing phase it was in had to take the
 * statement machinery with it - which is most of them, and is what
 * made p1stmt.h the expensive include.  They are not statements;
 * they are the state the whole pass runs under.
 */
struct name;			/* a pointer to one is all this needs */

/* Global context for static variable name mangling */
extern struct name *curFunc;
extern unsigned char staticCtr;  // file-global counter for static variable names
extern unsigned char shadowCtr;  // counter for shadowed locals

/* AST output control */
extern unsigned char astFd;         // where to write AST output
extern unsigned char asmFd;         // where to write global data assembly

/* Two-phase parsing control */
extern unsigned char phase;         // 1 = build symbol table, 2 = emit AST

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
/*
 * Compiler-wide control.
 *
 * These were in p1stmt.h, beside the switch tables, and every source
 * that wanted to know which parsing phase it was in had to take the
 * statement machinery with it - which is most of them, and is what
 * made p1stmt.h the expensive include.  They are not statements;
 * they are the state the whole pass runs under.
 */
struct name;			/* a pointer to one is all this needs */

/* Global context for static variable name mangling */
extern struct name *curFunc;
extern unsigned char staticCtr;  // file-global counter for static variable names
extern unsigned char shadowCtr;  // counter for shadowed locals

/* AST output control */
extern unsigned char astFd;         // where to write AST output
extern unsigned char asmFd;         // where to write global data assembly

/* Two-phase parsing control */
extern unsigned char phase;         // 1 = build symbol table, 2 = emit AST

#endif

/*
 * Compiler-wide control.
 *
 * These were in p1stmt.h, beside the switch tables, and every source
 * that wanted to know which parsing phase it was in had to take the
 * statement machinery with it - which is most of them, and is what
 * made p1stmt.h the expensive include.  They are not statements;
 * they are the state the whole pass runs under.
 */
struct name;			/* a pointer to one is all this needs */

/* Global context for static variable name mangling */
extern struct name *curFunc;
extern unsigned char staticCtr;  // file-global counter for static variable names
extern unsigned char shadowCtr;  // counter for shadowed locals

/* AST output control */
extern unsigned char astFd;         // where to write AST output
extern unsigned char asmFd;         // where to write global data assembly

/* Two-phase parsing control */
extern unsigned char phase;         // 1 = build symbol table, 2 = emit AST

#endif
