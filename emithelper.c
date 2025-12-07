/*
 * emithelper.c - Helper functions for code emission
 *
 * Contains: assembly string table, register helpers, variable load/store,
 * label/jump optimization, function prolog, cache management
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"
#include "regcache.h"

/* Common assembly strings - indexed for compact emission */
static const char *asmstr[] = {
    "\texx\n",                          /* S_EXX */
    "\tpush ix\n\tpop hl\n",            /* S_IXHL */
    "\tld h, b\n\tld l, c\n",           /* S_BCHL */
    "\tld (hl), e\n",                   /* S_HLDE */
    "\tld (de), a\n",                   /* S_DEA */
    "\tor a\n\tsbc hl, de\n",           /* S_SBCHLDE */
    "\tld a, (hl)\n",                   /* S_AHL */
    "\tinc de\n",                       /* S_INCDE */
    "\texx\n\tpush bc\n\texx\n\tpop hl\n", /* S_EXXBCHL */
    "\tadd hl, de\n",                   /* S_ADDHLDE */
    "\tpush hl\n\tpop ix\n",            /* S_HLPIX */
    "\tor a\n",                         /* S_ORA */
    "\tld (hl), d\n",                   /* S_HLD */
    "\tld e, a\n",			/* S_ESAVE */
    "\tld c, a\n",                      /* S_CA */
    "\tld b, h\n\tld c, l\n",           /* S_BCHLX */
    "\tld b, a\n",                      /* S_BA */
    "\tld a, l\n",                      /* S_AL */
    "\tld a, h\n\tor l\n",              /* S_AHORL */
    "\tld a, h\n",                      /* S_AH */
    "\tjr nz, $+3\n",                   /* S_JRNZ3 */
    "\tinc hl\n",                       /* S_INCHL */
    "\tex de, hl\n", 			/* S_DESAVE */
    "\tex de, hl\n",                    /* S_EXBCHL */
    "\texx\n\tpush bc\n\texx\n",        /* S_EXXBC */
    "\tld a, (bc)\n",                   /* S_LDABC */
    "\tld a, (ix+0)\n",                 /* S_LDAIXZ */
    "\tld a, b\n",                      /* S_LDAB */
    "\tld a, c\n",                      /* S_LDAC */
    "\tld b, a\n",                      /* S_LDBA */
    "\tld c, a\n",                      /* S_LDCA */
    "\texx\n\tld a, b\n\texx\n",        /* S_EXXLDAB */
    "\texx\n\tld a, c\n\texx\n",        /* S_EXXLDAC */
    "\texx\n\tld b, a\n\texx\n",        /* S_EXXLDBA */
    "\texx\n\tld c, a\n\texx\n",        /* S_EXXLDCA */
    "\tadd hl, bc\n",                   /* S_ADDHLBC */
    "\tcall framealloc\n",              /* S_CALLFA */
    "\tcall getlong\n",                 /* S_CALLGL */
    "\tcall load32i\n",                 /* S_CALLL32I */
    "\tcall putlong\n",                 /* S_CALLPL */
    "\t; ERROR: emit parse error\n", /* S_ERRPARS */
    "\tex de, hl\n", 			/* S_CACHESWP */
    "\tex de, hl\n",			/* S_DEADR */
    "\tex de, hl\n",			/* S_PUSHTOS */
    "\texx\n\tld a, b\n\tor c\n\texx\n", /* S_EXXABCORC */
    "\texx\n\tpush bc\n\texx\n\tpop de\n\tadd hl, de\n", /* S_EXXBCPOPHL */
    "\t; fall back to normal indirect\n", /* S_FBKNORM */
    "\tjp framefree\n",                 /* S_JPFF */
    "\tld a, b\n\tor c\n",              /* S_ABCORC */
    "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl\n", /* S_LDEDHLSWP */
    "\tld hl, 0\n",			/* S_HLZERO */
    "\tpop af\n",  			/* S_POPAFRET */
    "\tpop de\n",      			/* S_POPDEADR */
    "\tpop de\n", 			/* S_POPDERES */
    "\tpop hl\n", 			/* S_POPHPOST */
    "\tpop hl\n", 			/* S_POPHLLOW */
    "\tpop hl\n", 			/* S_POPHLUPP */
    "\tpop hl\n",  			/* S_POPHLRET */
    "\tpush af\n",   			/* S_PUSHAFSV */
    "\tpush bc\n",                      /* S_PUSHBC */
    "\tpush de\n",     			/* S_PUSHDESV */
    "\tpush de\n", 			/* S_PUSHDESP */
    "\tpush hl\n",  			/* S_PUSHHLLOW */
    "\tpush hl\n",   			/* S_PUSHHLOV */
    "\tpush hl\n",  			/* S_PUSHHLUPP */
    "\tpush ix\n",                      /* S_PUSHIX */
    "\tpush ix\n\tpop de\n\tadd hl, de\n", /* S_IXSWPHL */
    "\tret\n",                          /* S_RET */
    "\t; WARNING: byte reg holds pointer?\n", /* S_WARNBPTR */
    "\t; zero-extend short to long\n", /* S_ZEXTSL */
    "; Local variables:\n",             /* S_LOCVAR */
    "",                                 /* S_EMPTY */
    "\n",                               /* S_NEWLINE */
    "\tpop ix\n",                       /* S_POPIX */
    "\tpop bc\n",                       /* S_POPBC */
    "\texx\n\tpop bc\n\texx\n"          /* S_EXXPOPBC */
};

void emit(unsigned char idx) {
    fdprintf(outFd, "%s", asmstr[idx]);
}

/* Register lookup tables - indexed by register_id enum */
static const char * const regNameTab[] = {
    NULL, "B", "C", "BC", "IX"
};
static const char * const byteRegTab[] = {
    NULL, "b", "c", NULL, NULL
};
static const char * const wordRegTab[] = {
    NULL, NULL, NULL, "bc", "ix"
};
static const unsigned char byteLoadTab[] = {
    0, S_LDAB, S_LDAC, 0, 0
};
static const unsigned char byteStoreTab[] = {
    0, S_BA, S_CA, 0, 0
};
static const unsigned char wordLoadTab[] = {
    0, 0, 0, S_BCHL, S_IXHL
};
static const unsigned char wordStoreTab[] = {
    0, 0, 0, S_BCHLX, S_HLPIX
};
static const unsigned char addHLRegTab[] = {
    0, 0, 0, S_ADDHLBC, S_IXSWPHL
};
/* Callee-save bitmask: bit0=BC/B/C, bit1=IX */
static const unsigned char saveMaskTab[] = {
    0, 1, 1, 1, 2
};
/* BC or-c test: S_ABCORC for BC, 0 otherwise */
static const unsigned char bcOrCTab[] = {
    0, 0, 0, S_ABCORC, 0
};

const char *
getRegName(enum register_id reg)
{
    return reg <= REG_IX ? regNameTab[reg] : "???";
}

void emitByteLoad(unsigned char reg) { emit(byteLoadTab[reg]); }
void emitByteStore(unsigned char reg) { emit(byteStoreTab[reg]); }
void emitWordLoad(unsigned char reg) { emit(wordLoadTab[reg]); }
void emitAddHLReg(unsigned char reg) { emit(addHLRegTab[reg]); }
const char *byteRegName(unsigned char reg) { return byteRegTab[reg]; }
const char *wordRegName(unsigned char reg) { return wordRegTab[reg]; }
unsigned char bcOrCIdx(unsigned char reg) { return bcOrCTab[reg]; }

const char *
stripVarPfx(const char *name)
{
    return (name && name[0] == '$') ? name + 1 : name;
}

/* stripDollar is an alias for stripVarPfx */
const char *stripDollar(const char *s) { return stripVarPfx(s); }

void freeNode(struct expr *e) {
    xfree(e->cleanup_block);
    free(e);
}

/* isLocalSym: symbols not starting with '_' are local/static */
int isLocalSym(const char *name) {
    return name && name[0] != '_';
}

/* Get IY-indexed offset for a local/param variable */
int varIYOfs(struct local_var *var) {
    return var->offset + (var->is_param ? 1 : 0);
}

/* findVar is defined in codegen.c */

/* IY-indexed memory access - helper for sign handling */
static void iyOp(const char *fmt, char offset, int adj) {
    if (offset >= 0)
        fdprintf(outFd, fmt, '+', offset + adj);
    else
        fdprintf(outFd, fmt, '-', -offset - adj);
}

void loadWordIY(char offset) {
    if (fnIYHLValid && fnIYHLOfs == offset) return;
    iyOp("\tld l, (iy %c %d)\n", offset, 0);
    iyOp("\tld h, (iy %c %d)\n", offset, 1);
    clearHL();
    fnIYHLOfs = offset;
    fnIYHLValid = 1;
}

void loadBCIY(char offset) {
    iyOp("\tld c, (iy %c %d)\n", offset, 0);
    iyOp("\tld b, (iy %c %d)\n", offset, 1);
}

void storeWordIY(char offset) {
    iyOp("\tld (iy %c %d), l\n", offset, 0);
    iyOp("\tld (iy %c %d), h\n", offset, 1);
    fnIYHLOfs = offset;
    fnIYHLValid = 1;
    fnIXHLOfs = -1;
}

void loadByteIY(char offset, char is_param) {
    iyOp("\tld a, (iy %c %d)\n", offset, is_param && offset >= 0 ? 1 : 0);
}

void storeByteIY(char offset, char is_param) {
    iyOp("\tld (iy %c %d), a\n", offset, is_param && offset >= 0 ? 1 : 0);
}

/* IX-indexed memory access */
void loadWordIX(char offset) {
    if (fnIXHLOfs == offset) return;  /* Already in HL */
    fdprintf(outFd, "\tld l, (ix + %d)\n", offset);
    fdprintf(outFd, "\tld h, (ix + %d)\n", offset + 1);
    clearHL();
    fnIXHLOfs = offset;
}

void storeWordIX(char offset) {
    fdprintf(outFd, "\tld (ix + %d), l\n", offset);
    fdprintf(outFd, "\tld (ix + %d), h\n", offset + 1);
}

/*
 * Label map for jump optimization
 *
 * Maps labels to their jump targets to enable chain resolution.
 * When a label immediately precedes an unconditional jump, we record
 * the mapping (label -> target). During emission, resolveLabel()
 * follows these chains to find the final destination, eliminating
 * redundant jumps like:
 *
 *   jp L1      becomes    jp L3
 *   ...
 *   L1: jp L2
 *   L2: jp L3
 *
 * Also tracks reference counts to identify unused labels.
 */
#define MAX_LABELS 256
static struct labelMap labelMap[MAX_LABELS];
int lblMapCnt = 0;

void addLabelMap(int from, int to, enum jump_type type) {
    if (lblMapCnt < MAX_LABELS) {
        labelMap[lblMapCnt].label = from;
        labelMap[lblMapCnt].target = to;
        labelMap[lblMapCnt].jump_type = type;
        labelMap[lblMapCnt].refcnt = 0;
        lblMapCnt++;
    }
}

/* Find or create label entry, return index */
static int findLabel(int label) {
    int i;
    for (i = 0; i < lblMapCnt; i++) {
        if (labelMap[i].label == label) return i;
    }
    /* Create new entry */
    if (lblMapCnt < MAX_LABELS) {
        labelMap[lblMapCnt].label = label;
        labelMap[lblMapCnt].target = 255;  /* 255 = no target */
        labelMap[lblMapCnt].jump_type = JMP_UNCOND;
        labelMap[lblMapCnt].refcnt = 0;
        return lblMapCnt++;
    }
    return -1;
}

/* Increment reference count for a label */
void refLabel(int label) {
    int idx = findLabel(label);
    if (idx >= 0) labelMap[idx].refcnt++;
}

/* Get reference count for a label */
int getLabelRef(int label) {
    int i;
    for (i = 0; i < lblMapCnt; i++) {
        if (labelMap[i].label == label) return labelMap[i].refcnt;
    }
    return 0;
}

int resolveLabel(int label) {
    int i, j;
    int visited[100];
    int vcnt = 0;
    int current = label;

    while (vcnt < 100) {
        for (j = 0; j < vcnt; j++) {
            if (visited[j] == current) return current;
        }
        visited[vcnt++] = current;

        for (i = 0; i < lblMapCnt; i++) {
            if (labelMap[i].label == current &&
                labelMap[i].jump_type == JMP_UNCOND &&
                labelMap[i].target != 255) {
                current = labelMap[i].target;
                break;
            }
        }
        if (i == lblMapCnt) return current;
    }
    return label;
}

void scanExprJumps(struct expr *e) {
    if (!e) return;
    if (e->jump && e->label > 0)
        addLabelMap(e->label, e->jump->target_label, e->jump->type);
    if (e->left) scanExprJumps(e->left);
    if (e->right) scanExprJumps(e->right);
}

void scanLabJumps(struct stmt *s) {
    if (!s) return;

    /* Handle numeric end-label (type 'Y') followed by jump */
    if (s->type == 'Y' && s->next && s->next->jump) {
        addLabelMap(s->label, s->next->jump->target_label, s->next->jump->type);
    }

    if (s->jump && s->label > 0)
        addLabelMap(s->label, s->jump->target_label, s->jump->type);

    if (s->expr) scanExprJumps(s->expr);
    if (s->expr2) scanExprJumps(s->expr2);
    if (s->expr3) scanExprJumps(s->expr3);

    if (s->then_branch) scanLabJumps(s->then_branch);
    if (s->else_branch) scanLabJumps(s->else_branch);
    if (s->next) scanLabJumps(s->next);
}

void emitJump(const char *instr, const char *prefix, int label) {
    if (label < 0) {
        /* label < 0 means prefix is the complete label symbol */
        fdprintf(outFd, "\t%s %s\n", instr, prefix);
    } else {
        int resolved = resolveLabel(label);
        fdprintf(outFd, "\t%s %s%d\n", instr, prefix, resolved);
    }
}

/* Callee-save helpers */
int getUsedRegs(struct local_var *locals) {
    struct local_var *var;
    int used = 0;
    for (var = locals; var; var = var->next)
        used |= saveMaskTab[var->reg];
    return used;
}

int calleeSavSz(int used) {
    int size = 0;
    if (used & 1) size += 2;  /* BC */
    if (used & 2) size += 2;  /* IX */
    if (used & 4) size += 2;  /* BC' */
    return size;
}

/* Function prolog emission */
void emitFnProlog(char *name, char *params, char *rettype, int frame_size,
                  struct local_var *locals) {
    int has_params = (params && params[0]);
    struct local_var *var;

    fdprintf(outFd, "; Function: %s", name);
    if (has_params) fdprintf(outFd, "(%s)", params);
    else fdprintf(outFd, "()");
    if (rettype && rettype[0]) fdprintf(outFd, " -> %s", rettype);
    emit(S_NEWLINE);

    if (locals) {
        emit(S_LOCVAR);
        for (var = locals; var; var = var->next) {
            const char *regname = getRegName(var->reg);

            fdprintf(outFd, ";   %s: ", var->name);
            if (var->first_label == 255) {
                if (regname)
                    fdprintf(outFd, "unused, reg=%s\n", regname);
                else
                    fdprintf(outFd, "unused, (iy%+d)\n", var->offset);
            } else {
                if (regname)
                    fdprintf(outFd, "reg=%s\n", regname);
                else
                    fdprintf(outFd, "(iy%+d)\n", var->offset);
            }
        }
    }

    fdprintf(outFd, "%s:\n", name);  /* Name already has _ prefix if public */

    if (frame_size > 0 || has_params) {
        if (frame_size == 0) {
            fdprintf(outFd, "\txor a\n");
        } else {
            fdprintf(outFd, "\tld a, %d\n", frame_size);
        }
        emit(S_CALLFA);
    }

    {
        int used = getUsedRegs(locals);
        if (used & 1) emit(S_PUSHBC);
        if (used & 2) emit(S_PUSHIX);
    }

    for (var = locals; var; var = var->next) {
        if (var->is_param && var->reg != REG_NO) {
            if (var->size == 2) {
                if (var->reg == REG_BC) {
                    loadBCIY(var->offset);
                } else {
                    loadWordIY(var->offset);
                    if (var->reg == REG_IX) {
                        char sym[64];
                        struct expr *cache;
                        emit(S_HLPIX);
                        /* HL still has the value - set cache */
                        snprintf(sym, sizeof(sym), "$%s", var->name);
                        cache = mkVarCache(sym, 2);
                        clearHL();
                        cacheSetHL(cache);
                        freeExpr(cache);
                    }
                }
            } else if (var->size == 1 && var->reg <= REG_C) {
                static const char bc[] = "?bc";
                fdprintf(outFd, "\tld %c, (iy + %d)\n",
                    bc[var->reg], var->offset + 1);
            }
        }
    }
}

/* Comparison function detection */
static const char *cmpPats[] = {
    "eq", "ne", "lt", "gt", "le", "ge", "and", "or", 0
};

int isCmpFunc(const char *fname) {
    int i;
    if (!fname) return 0;
    for (i = 0; cmpPats[i]; i++) {
        if (strstr(fname, cmpPats[i])) return 1;
    }
    return 0;
}

/* freeExpr is defined in parseast.c */

struct expr *mkVarCache(const char *symbol, int size) {
    struct expr *sym_node, *deref_node;

    sym_node = malloc(sizeof(struct expr));
    if (!sym_node) return NULL;
    memset(sym_node, 0, sizeof(struct expr));
    sym_node->op = '$';
    sym_node->symbol = (char *)symbol;
    sym_node->size = 2;  /* SYM node is always pointer size (2 bytes) */

    deref_node = malloc(sizeof(struct expr));
    if (!deref_node) { free(sym_node); return NULL; }
    memset(deref_node, 0, sizeof(struct expr));
    deref_node->op = 'M';
    deref_node->size = size;  /* DEREF size is the dereferenced value size */
    deref_node->left = sym_node;
    return deref_node;
}

/* Cache management */
void clearHL() {
    fnIXHLOfs = -1;
    fnIYHLValid = 0;
    cacheInvalHL();
}

void clearDE() {
    cacheInvalDE();
}

void clearA() {
    fnIXAOfs = -1;
    fnABCValid = 0;
    fnAZero = 0;
    cacheInvalA();
}

void pushStack() {
    if (0) return;

    if (fnDEValid) {
        emit(S_PUSHDESP);
        fnDESaveCnt++;
        clearDE();
    }
    emit(S_PUSHTOS);
    fnDEValid = 1;
    fnZValid = 0;
    cachePushHL();  /* Move HL cache to DE */
}

void popStack() {
    if (0) return;
    fnDEValid = 0;
    fnZValid = 0;
    clearDE();
}

void invalStack() {
    if (0) return;
    fnDEValid = 0;
    fnZValid = 0;
    clearHL();
    clearDE();
    clearA();
}

int isBinopWAccum(unsigned char op) {
    switch (op) {
    case '+': case '-': case '*': case '/': case '%':
    case '&': case '|': case '^': case 'y': case 'w':
    case '>': case '<': case 'g': case 'L':
    case 'Q': case 'n':
        return 1;
    default:
        return 0;
    }
}

/* Helper: set cache for variable after load/store */
static void setVarCache(const char *sym, char sz, char isA) {
    struct expr *cache = mkVarCache(sym, sz);
    if (isA) {
        char saveAZero = fnAZero;  /* Preserve A-is-zero flag across store */
        clearA();
        fnAZero = saveAZero;
        cacheSetA(cache);
    } else {
        clearHL();
        cacheSetHL(cache);
    }
    freeExpr(cache);
}

/* Helper: emit global long (4-byte) load/store */
static void globLong(const char *s, char isStore) {
    if (isStore) {
        fdprintf(outFd, "\tld (%s), hl\n", s);
        emit(S_EXX);
        fdprintf(outFd, "\tld (%s+2), hl\n", s);
    } else {
        fdprintf(outFd, "\tld hl, (%s)\n", s);
        emit(S_EXX);
        fdprintf(outFd, "\tld hl, (%s+2)\n", s);
    }
    emit(S_EXX);
}

/* Variable load/store with cache management */
void loadVar(const char *sym, char sz, char docache) {
    const char *vn = stripVarPfx(sym);
    struct local_var *v;
    if (TRACE(T_VAR)) {
        fdprintf(2, "  loadVar: sym=%s (1)\n", sym);
    }
    v = findVar(vn);
    if (TRACE(T_VAR)) {
        fdprintf(2, "  loadVar: findVar returned %p (2)\n", (void*)v);
    }

    if (v && v->reg != REG_NO) {
        if (TRACE(T_VAR)) {
            fdprintf(2, "  loadVar: branch A\n");
        }
        emit(sz == 1 ? byteLoadTab[v->reg] : wordLoadTab[v->reg]);
    } else if (v) {
        if (TRACE(T_VAR)) {
            fdprintf(2, "  loadVar: branch B\n");
        }
        if (sz == 1) loadByteIY(v->offset, v->offset >= 0);
        else if (sz == 2) loadWordIY(v->offset);
        else if (sz == 4) {
            fdprintf(outFd, "\tld a, %d\n", v->offset);
            emit(S_CALLGL);
        }
    } else {
        const char *s = stripDollar(sym);
        if (TRACE(T_VAR)) {
            fdprintf(2, "  loadVar: branch C (global)\n");
        }
        addRefSym(s);
        if (sz == 1) fdprintf(outFd, "\tld a, (%s)\n", s);
        else if (sz == 2) fdprintf(outFd, "\tld hl, (%s)\n", s);
        else if (sz == 4) globLong(s, 0);
    }
    if (docache && sz >= 2) setVarCache(sym, sz, 0);
}

void storeVar(const char *sym, char sz, char docache) {
    const char *vn = stripVarPfx(sym);
    struct local_var *v;
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: sym=%s\n", sym);
    }
    v = findVar(vn);
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: findVar returned %p\n", (void*)v);
    }

    if (v && v->reg != REG_NO) {
        if (sz == 1) {
            emit(byteStoreTab[v->reg]);
            /* After ld c,a or ld b,a, A still has the value */
            if (docache) setVarCache(sym, sz, 1);
        } else {
            emit(wordStoreTab[v->reg]);
            if (docache) setVarCache(sym, sz, 0);
        }
    } else if (v) {
        if (sz == 1) storeByteIY(v->offset, v->offset >= 0);
        else if (sz == 2) storeWordIY(v->offset);
        else if (sz == 4) {
            fdprintf(outFd, "\tld a, %d\n", v->offset);
            emit(S_CALLPL);
        }
    } else {
        const char *s = stripDollar(sym);
        if (TRACE(T_VAR)) {
            fdprintf(2, "  storeVar: global store sz=%d\n", sz);
        }
        if (sz == 1) {
            fdprintf(outFd, "\tld (%s), a\n", s);
            if (docache) setVarCache(sym, 1, 1);
        } else if (sz == 2) {
            fdprintf(outFd, "\tld (%s), hl\n", s);
            if (docache) setVarCache(sym, sz, 0);
        } else if (sz == 4) {
            globLong(s, 1);
        }
        if (TRACE(T_VAR)) {
            fdprintf(2, "  storeVar: done\n");
        }
    }
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: returning\n");
    }
}

/*
 * Emit simple word load based on scheduler's loc and dest fields.
 * Handles: LOC_REG (BC, IX), LOC_MEM, LOC_STACK, LOC_IX
 * Target register from e->dest: R_HL, R_DE, R_BC
 * Returns 1 if handled, 0 if not a simple case.
 */
int emitSimplLd(struct expr *e)
{
    const char *sym;
    int ofs;
    char hi, lo;            /* register halves */
    const char *rp;         /* register pair name */

    if (!e || e->size != 2) return 0;

    /* Set register names based on destination */
    switch (e->dest) {
    case R_HL: hi = 'h'; lo = 'l'; rp = "hl"; break;
    case R_DE: hi = 'd'; lo = 'e'; rp = "de"; break;
    case R_BC: hi = 'b'; lo = 'c'; rp = "bc"; break;
    default: return 0;
    }

    switch (e->loc) {
    case LOC_CONST:
        fdprintf(outFd, "\tld %s, %ld\n", rp, e->value);
        break;

    case LOC_REG:
        if (e->reg == R_BC && e->dest != R_BC) {
            fdprintf(outFd, "\tld %c, b\n\tld %c, c\n", hi, lo);
        } else if (e->reg == R_IX) {
            fdprintf(outFd, "\tpush ix\n\tpop %s\n", rp);
        } else if (e->reg == R_HL && e->dest == R_DE) {
            fdprintf(outFd, "\tex de, hl\n");
        } else if (e->reg == R_DE && e->dest == R_HL) {
            fdprintf(outFd, "\tex de, hl\n");
        } else {
            return 0;
        }
        break;

    case LOC_MEM:
        if (!e->left || !e->left->symbol) return 0;
        sym = stripDollar(e->left->symbol);
        addRefSym(sym);
        fdprintf(outFd, "\tld %s, (%s)\n", rp, sym);
        break;

    case LOC_STACK:
        ofs = e->offset;
        fdprintf(outFd, "\tld %c, (iy %c %d)\n\tld %c, (iy %c %d)\n",
                 lo, ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs,
                 hi, ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
        break;

    case LOC_IX:
        ofs = e->offset;
        fdprintf(outFd, "\tld %c, (ix %c %d)\n\tld %c, (ix %c %d)\n",
                 lo, ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs,
                 hi, ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
        break;

    default:
        return 0;
    }

    /* Update state based on destination */
    if (e->dest == R_DE) fnDEValid = 1;
    else if (e->dest == R_HL) clearHL();

    freeExpr(e);
    return 1;
}

/*
 * Emit DEREF of global variable with cache check
 * Pattern: (M $global) with OP_GLOBAL opflag set
 */
void emitGlobDrf(struct expr *e)
{
    const char *sym;
    const char *s;

    if (!e || !e->left || !e->left->symbol) return;

    sym = e->left->symbol;
    s = stripDollar(sym);
    addRefSym(s);

    if (e->size == 1) {
        if (cacheFindByte(e) == 'A') {
            /* A already holds this value - skip load */
        } else {
            fdprintf(outFd, "\tld a, (%s)\n", s);
            clearA();
            cacheSetA(e);
        }
    } else if (e->size == 2) {
        if (e->dest == R_DE) {
            /* Scheduler says load to DE */
            fdprintf(outFd, "\tld de, (%s)\n", s);
            fnDEValid = 1;
        } else if (cacheFindWord(e) == 'H') {
            /* HL already holds this value - skip load */
        } else {
            fdprintf(outFd, "\tld hl, (%s)\n", s);
            clearHL();
            cacheSetHL(e);
        }
    } else if (e->size == 4) {
        fdprintf(outFd, "\tld hl, (%s)\n", s);
        emit(S_EXX);
        fdprintf(outFd, "\tld hl, (%s+2)\n", s);
        emit(S_EXX);
        clearHL();
    }

    /* Free the expression tree */
    freeExpr(e);
}

/*
 * Emit DEREF of register-allocated variable with cache check
 * Pattern: (M $regvar) where opflags has OP_REGVAR
 */
void emitRegVarDrf(struct expr *e)
{
    struct local_var *var;
    const char *sym;

    if (!e || !e->left || !e->left->symbol) return;

    sym = e->left->symbol;
    if (sym[0] == '$') sym++;
    var = findVar(sym);
    if (!var) {
        /* Fallback to global */
        emitGlobDrf(e);
        return;
    }

    if (e->size == 1) {
        /* Byte register variable */
        if (cacheFindByte(e) == 'A') {
            /* A already holds this value - skip load */
        } else {
            if (var->reg == REG_B || var->reg == REG_BC)
                fdprintf(outFd, "\tld a, b\n");
            else if (var->reg == REG_C)
                fdprintf(outFd, "\tld a, c\n");
            cacheSetA(e);
        }
    } else {
        /* Word register variable */
        if (e->dest == R_DE) {
            /* Scheduler says load to DE */
            if (var->reg == REG_BC)
                fdprintf(outFd, "\tld d, b\n\tld e, c\n");
            else if (var->reg == REG_IX)
                fdprintf(outFd, "\tpush ix\n\tpop de\n");
            fnDEValid = 1;
        } else if (cacheFindWord(e) == 'H') {
            /* HL already holds this value - skip load */
        } else {
            if (var->reg == REG_BC)
                fdprintf(outFd, "\tld h, b\n\tld l, c\n");
            else if (var->reg == REG_IX)
                emit(S_IXHL);
            clearHL();
            cacheSetHL(e);
        }
    }

    freeExpr(e);
}

/*
 * Emit DEREF of stack variable (IY-indexed) with cache check
 * Pattern: (M $stackvar) where opflags has OP_IYMEM
 */
void emitStackDrf(struct expr *e)
{
    struct local_var *var;
    const char *sym;
    int ofs;

    if (!e || !e->left || !e->left->symbol) return;

    sym = e->left->symbol;
    if (sym[0] == '$') sym++;
    var = findVar(sym);
    if (!var) {
        /* Fallback to global */
        emitGlobDrf(e);
        return;
    }

    ofs = var->offset;

    if (e->size == 1) {
        if (cacheFindByte(e) == 'A') {
            /* A already holds this value - skip load */
        } else {
            loadByteIY(ofs, var->is_param);
            cacheSetA(e);
        }
    } else if (e->size == 2) {
        if (e->dest == R_DE) {
            /* Scheduler says load to DE */
            fdprintf(outFd, "\tld e, (iy %c %d)\n\tld d, (iy %c %d)\n",
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs,
                     ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
            fnDEValid = 1;
        } else if (cacheFindWord(e) == 'H') {
            /* HL already holds this value - skip load */
        } else {
            loadWordIY(ofs);
            clearHL();
            cacheSetHL(e);
        }
    } else if (e->size == 4) {
        fdprintf(outFd, "\tld l, (iy %c %d)\n\tld h, (iy %c %d)\n",
                 ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs,
                 ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
        emit(S_EXX);
        fdprintf(outFd, "\tld l, (iy %c %d)\n\tld h, (iy %c %d)\n",
                 ofs + 2 >= 0 ? '+' : '-', ofs + 2 >= 0 ? ofs + 2 : -(ofs + 2),
                 ofs + 3 >= 0 ? '+' : '-', ofs + 3 >= 0 ? ofs + 3 : -(ofs + 3));
        emit(S_EXX);
        clearHL();
    }

    freeExpr(e);
}

/*
 * Emit byte load from (BC) with caching
 * Creates a synthetic expression representing *(bc) for cache matching
 */
void emitBCIndir(void)
{
    static struct expr bcIndir = { 'B', 1 };  /* Synthetic BC indirect marker */

    /* Check if A already holds (bc) */
    if (cacheFindByte(&bcIndir) == 'A') {
        /* A already has (bc) - skip load */
        return;
    }
    fdprintf(outFd, "\tld a, (bc)\n");
    cacheInvalA();
    cacheSetA(&bcIndir);
}
