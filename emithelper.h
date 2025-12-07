/*
 * emithelper.h - Helper functions for code emission
 */
#ifndef EMITHELPER_H
#define EMITHELPER_H

#include "cc2.h"

/* Assembly string emission */
void emit(unsigned char idx);
void emitByteLoad(unsigned char reg);
void emitByteStore(unsigned char reg);
void emitWordLoad(unsigned char reg);
void emitAddHLReg(unsigned char reg);
const char *byteRegName(unsigned char reg);
const char *wordRegName(unsigned char reg);
unsigned char bcOrCIdx(unsigned char reg);

/* String/name helpers */
const char *stripDollar(const char *symbol);
const char *stripVarPfx(const char *name);
void freeNode(struct expr *e);
int isLocalSym(const char *name);  /* Returns 1 if symbol is local (no _ prefix) */
const char *getRegName(enum register_id reg);
/* findVar is declared in cc2.h */

/* IY-indexed memory access */
void loadWordIY(char offset);
void loadBCIY(char offset);
void storeWordIY(char offset);
void loadByteIY(char offset, char is_param);
void storeByteIY(char offset, char is_param);

/* IX-indexed memory access */
void loadWordIX(char offset);
void storeWordIX(char offset);

/* Variable load/store with cache management */
void loadVar(const char *sym, char sz, char docache);
void storeVar(const char *sym, char sz, char docache);
int varIYOfs(struct local_var *var);

/* Register tracking for callee-save */
int getUsedRegs(struct local_var *locals);
int calleeSavSz(int used);

/* Function prolog emission */
void emitFnProlog(char *name, char *params, char *rettype, int frame_size,
                  struct local_var *locals);

/* Comparison function detection */
int isCmpFunc(const char *fname);

/* Expression cache helper - creates (M $sym) node */
struct expr *mkVarCache(const char *sym, int size);

/* Cache management */
void clearHL();
void clearDE();
void clearA();
void pushStack();
void popStack();
void invalStack();

/* Helper for binary op detection */
int isBinopWAccum(unsigned char op);

/* Simple load - uses scheduler's loc/dest fields */
int emitSimplLd(struct expr *e);

/* Expression emission - main function and helpers */
void emitExpr(struct expr *e);
void emitIncDec(struct expr *e);
void emitAssign(struct expr *e);
void emitBinop(struct expr *e);
void emitCall(struct expr *e);
void emitTernary(struct expr *e);
void emitGlobDrf(struct expr *e);
void emitRegVarDrf(struct expr *e);
void emitStackDrf(struct expr *e);
void emitBCIndir(void);

/* Label/jump optimization */
void addLabelMap(int from, int to, enum jump_type type);
int resolveLabel(int label);
void refLabel(int label);
int getLabelRef(int label);
void scanExprJumps(struct expr *e);
void scanLabJumps(struct stmt *s);
void emitJump(const char *instr, const char *prefix, int label);

/* Assembly string indices */
#define S_EXX 0
#define S_IXHL 1
#define S_BCHL 2
#define S_HLDE 3
#define S_DEA 4
#define S_SBCHLDE 5
#define S_AHL 6
#define S_INCDE 7
#define S_EXXBCHL 8
#define S_ADDHLDE 9
#define S_HLPIX 10
#define S_ORA 11
#define S_HLD 12
#define S_ESAVE 13
#define S_CA 14
#define S_BCHLX 15
#define S_BA 16
#define S_AL 17
#define S_AHORL 18
#define S_AH 19
#define S_JRNZ3 20
#define S_INCHL 21
#define S_EXDEHL 22
#define S_DESAVE S_EXDEHL
#define S_EXBCHL S_EXDEHL
#define S_EXXBC 24
#define S_LDABC 25
#define S_LDAIXZ 26
#define S_LDAB 27
#define S_LDAC 28
#define S_LDBA 29
#define S_LDCA 30
#define S_EXXLDAB 31
#define S_EXXLDAC 32
#define S_EXXLDBA 33
#define S_EXXLDCA 34
#define S_ADDHLBC 35
#define S_CALLFA 36
#define S_CALLGL 37
#define S_CALLL32I 38
#define S_CALLPL 39
#define S_ERRPARS 40
#define S_CACHESWP S_EXDEHL
#define S_DEADR S_EXDEHL
#define S_PUSHTOS S_EXDEHL
#define S_EXXABCORC 44
#define S_EXXBCPOPHL 45
#define S_FBKNORM 46
#define S_JPFF 47
#define S_ABCORC 48
#define S_LDEDHLSWP 49
#define S_HLZERO 50
#define S_POPAFRET 51
#define S_POPDE 52
#define S_POPDEADR S_POPDE
#define S_POPDERES S_POPDE
#define S_POPHL 54
#define S_POPHPOST S_POPHL
#define S_POPHLLOW S_POPHL
#define S_POPHLUPP S_POPHL
#define S_POPHLRET S_POPHL
#define S_PUSHAFSV 58
#define S_PUSHBC 59
#define S_PUSHDE 60
#define S_PUSHDESV S_PUSHDE
#define S_PUSHDESP S_PUSHDE
#define S_PUSHHL 62
#define S_PUSHHLLOW S_PUSHHL
#define S_PUSHHLOV S_PUSHHL
#define S_PUSHHLUPP S_PUSHHL
#define S_PUSHIX 65
#define S_IXSWPHL 66
#define S_RET 67
#define S_WARNBPTR 68
#define S_ZEXTSL 69
#define S_LOCVAR 70
#define S_EMPTY 71
#define S_NEWLINE 72
#define S_POPIX 73
#define S_POPBC 74
#define S_EXXPOPBC 75

/* Global label map for jump optimization */
#define MAX_LBLMAP 256
extern int lblMapCnt;

#endif /* EMITHELPER_H */
