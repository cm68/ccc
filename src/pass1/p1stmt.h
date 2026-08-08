/*
 * statements: blocks, locals and switches
 *
 * Split out of cc1.h, which every source included for everything.
 * That cost more than tidiness: cpp builds a table for every name it
 * sees, and the whole of cc1.h left it room for forty-six more
 * declarations - not enough for any real source, which is why pass1
 * could not be compiled on CP/M at all.  A file that wants types and
 * not statements now says so and pays for what it uses.
 *
 * cc1.h includes all of these, so anything not yet narrowed still
 * gets what it always did.
 */
#ifndef _P1STMT_H
#define _P1STMT_H

#include "p1base.h"

/* swcnt.c - switch bookkeeping and statement counters */
int atSwBodyStmt(void);
void parseBlockEx(int emitHdr);
void parseBlock(void);
struct local *mklocal(struct name *n);
struct local *capLocals(void);
char *getAsmText(void);
struct swcase *nextCase(void);
void stIf2(void);
void stRet2(void);
void stSwitch2(void);
void stExpr2(void);
void stGoto2(void);
int reserveCount(void);
void patchCount(int slot, char c);
/* outh.c - AST-writer helpers */
struct local *findInLocals(struct name *want);
int isAssignOp(unsigned char op);
char dchainreg(struct expr *e);
int truncok(unsigned char op);
int bytevalued(struct expr *e);
int candemote(struct expr *e, int size);
void demote(struct expr *e, struct type *t);
int iscmpop(unsigned char op);
unsigned char valwidth(struct type *t);
struct type *opwidth(struct expr *e);
char typeSfx(struct type *t);
char *mkLbl(char *base, char *suffix);
void emitLabel(char *base, char *suffix);
void emitGoto(char *base, char *suffix);
int cntCondLbls(struct expr *e);
struct name *findMemberOff(struct name *members, int offset);

/*
 * Switch statement table tracking (phase 1)
 * Accumulates case values and labels for each switch statement.
 * Nested switches use swStack to track which switch is current.
 * Dynamically allocated - no fixed limits on switch count.
 */
#define MAX_SWDEPTH 8       /* max switch nesting */
#define SW_INIT_CASES 8     /* initial cases per switch, and the step it grows by */
#define SW_INIT_SWS   8     /* initial switches per function, likewise */

/*
 * A case, as phase 1 records it.  Not its value: phase 2 re-parses
 * that from the token stream as an expression, so the four bytes this
 * used to carry were written twice and read never.  A big switch runs
 * to a couple of hundred cases and the array is live for the whole
 * function.
 */
/*
 * A local as register allocation and emission need it.
 *
 * Phase 1 used to hand phase 2 a whole struct name per local - forty
 * bytes on the host, twenty-three on the Z80 - of which thirteen
 * fields are ever read.  The symbol-table chain, the tag and emitted
 * flags and the initialiser union are all dead the moment the copy is
 * taken.
 *
 * And these do not come and go with each function: phase 1 captures
 * every function's before phase 2 frees any, so the whole file's worth
 * is live at the turn between them.  Six bytes apiece is worth having.
 */
struct local {
	unsigned short id;
	struct type *type;
	struct local *next;
	kind kind;
	unsigned char level;
	unsigned char sclass;
	unsigned char static_id;
	unsigned char ref_count;  /* reference count (capped at 255) */
	unsigned char agg_refs;   /* struct member accesses, for IX */
	unsigned char reg;        /* 0=none, 1=B, 2=C, 3=BC, 4=IX */
	unsigned char addr_taken;
	unsigned char blkid;      /* declaring block, with level walks scopes */
	short frm_off;            /* params positive, locals negative */
};

struct swcase {
    unsigned char is_default; /* 1 if default, 0 if case */
    unsigned char stmts;    /* statement count for this case section */
};

struct swtab {
    struct swcase *cases;   /* allocated case array for this switch */
    unsigned char count;    /* number of cases */
    unsigned char capacity; /* allocated size of cases array */
    unsigned char num;      /* switch number (for labels) */
    unsigned char base_stmts; /* stmt_count at start of current case */
    unsigned char final_cnt;  /* stmt_count when switch body ends */
    unsigned char emitIdx;  /* phase 2: current case being emitted */
    unsigned char cslot;    /* reserved count-queue slot (phase 1) */
};

/* Dynamic switch list */
extern struct swtab *swList;
extern unsigned char swCount;       /* number of switches in function */
extern unsigned char swCapacity;    /* allocated size of swList */
extern unsigned char swStack[];     /* nesting stack (indices into swList) */
extern unsigned char swDepth;       /* nesting depth */
extern unsigned char swEmitIdx;     /* phase 2: next switch to emit */
extern unsigned char swEmitStack[]; /* phase 2: stack of switch indices */
extern unsigned char swEmitDepth;   /* phase 2: emit stack depth */

void resetSwitch(void);             /* reset for new function */
void pushSwitch(void);              /* enter switch statement */
void popSwitch(void);               /* exit switch statement */
void addCase(unsigned char stmt_cnt);  /* add case to current switch */
void addDefault(unsigned char stmt_cnt);           /* add default to current switch */
void finishCase(unsigned char stmt_cnt);           /* finalize current case stmt count */


/*
 * Count storage for streaming AST emission
 * Phase 1 computes counts (args, cases, stmts), phase 2 retrieves them.
 * Reset between functions, flip after phase 1 for LIFO retrieval.
 */
void pushCount(char c);
char popCount(void);
void resetCounts(void);
void resetCountIdx(void);
void resetSpanCnts(void);  /* Reset read pointer for phase 2 */

/* Block statement counts (phase 1 -> phase 2) */
void enterBlkCnt(void);  /* call when entering block in phase 1 */
void pushBlkCnt(unsigned char n);
unsigned char popBlkCnt(void);
void flipBlkCnts(void);  /* prepare for phase 2 */
void resetBlkCnts(void);
void pushFuncCnt(unsigned char n);
unsigned char popFuncCnt(void);
void resetFuncIdx(void);

#endif
