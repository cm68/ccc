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
    "\tex de, hl\n", 			/* S_EXDEHL */
    "\tor a\n\tscf\n",                  /* S_ORASCF */
    "\texx\n\tpush bc\n\texx\n",        /* S_EXXBC */
    "\tld a, (bc)\n",                   /* S_LDABC */
    "\tld a, (ix+0)\n",                 /* S_LDAIXZ */
    "\tld a, b\n",                      /* S_LDAB */
    "\tld a, c\n",                      /* S_LDAC */
    "\tex de, hl\n\tcall ilsh\n",       /* S_CALLILSH */
    "\tex de, hl\n\tcall idiv\n",       /* S_CALLIDIV */
    "\texx\n\tld a, b\n\texx\n",        /* S_EXXLDAB */
    "\texx\n\tld a, c\n\texx\n",        /* S_EXXLDAC */
    "\texx\n\tld b, a\n\texx\n",        /* S_EXXLDBA */
    "\texx\n\tld c, a\n\texx\n",        /* S_EXXLDCA */
    "\tadd hl, bc\n",                   /* S_ADDHLBC */
    "\tcall framealloc\n",              /* S_CALLFA */
    "\tcall getLiy\n",                  /* S_CALLGL */
    "\tcall load32i\n",                 /* S_CALLL32I */
    "\tcall putLiy\n",                  /* S_CALLPL */
    "\t; ERR\n", /* S_ERRPARS */
    "\tld a, l\n\tor c\n\tld c, a\n",   /* S_ALORCC */
    "\tld a, h\n\tor b\n\tld b, a\n",   /* S_AHORBBA */
    "\tld a, (hl)\n\tpush af\n",        /* S_LDAHLPUSH */
    "\texx\n\tld a, b\n\tor c\n\texx\n", /* S_EXXABCORC */
    "\texx\n\tpush bc\n\texx\n\tpop de\n\tadd hl, de\n", /* S_EXXBCPOPHL */
    "\tcp (ix + 0)\n",                  /* S_CPIXZ */
    "\tjp framefree\n",                 /* S_JPFF */
    "\tld a, b\n\tor c\n",              /* S_ABCORC */
    "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl\n", /* S_LDEDHLSWP */
    "\tld hl, 0\n",			/* S_HLZERO */
    "\tpop af\n",  			/* S_POPAFRET */
    "\tpop de\n",      			/* S_POPDE */
    "\tld a, ixh\n\tbit 7, a\n",        /* S_IXHBIT7 */
    "\tpop hl\n", 			/* S_POPHL */
    "\tadd hl, bc\n\tld b, h\n\tld c, l\n", /* S_ADDHLBCBC */
    "\tex de, hl\n\tpop hl\n",          /* S_EXDEHLPOPHL */
    "\tpush hl\n\tld h, b\n\tld l, c\n", /* S_PUSHHLBCHL */
    "\tpush af\n",   			/* S_PUSHAFSV */
    "\tpush bc\n",                      /* S_PUSHBC */
    "\tpush de\n",     			/* S_PUSHDE */
    "\tpush hl\n\tex de, hl\n",         /* S_PUSHHLEXDE */
    "\tpush hl\n",  			/* S_PUSHHL */
    "\tld a, l\n\txor e\n\tld l, a\n\tld a, h\n\txor d\n\tld h, a\n", /* S_XORHLDE */
    "\tld a, l\n\tor e\n\tld l, a\n\tld a, h\n\tor d\n\tld h, a\n",  /* S_ORHLDE */
    "\tpush ix\n",                      /* S_PUSHIX */
    "\tpush ix\n\tpop de\n\tadd hl, de\n", /* S_IXSWPHL */
    "\tret\n",                          /* S_RET */
    "\tld a, l\n\tand e\n\tld l, a\n\tld a, h\n\tand d\n\tld h, a\n", /* S_ANDHLDE */
    "", /* S_ZEXTSL */
    "\tjr nz, $+4\n\tinc hl\n",         /* S_JRNZ4INC */
    "\tjr nc, $+3\n",                   /* S_JRNC3 */
    "\tld (hl), d\n\tdec hl\n\tld (hl), e\n", /* S_STDEHL */
    "\tpop ix\n",                       /* S_POPIX */
    "\tpop bc\n",                       /* S_POPBC */
    "\texx\n\tpop bc\n\texx\n",         /* S_EXXPOPBC */
    "\tjp nz, $+8\n",                   /* S_JPNZ8 */
    "\tcall lor32\n",                   /* S_CALLLOR32 */
    "\tbit 7, h\n",                     /* S_BIT7H */
    "\txor a\n",                        /* S_XORA */
    "\tld e, l\n\tld d, h\n",           /* S_HLTODE */
    "\tbit 7, d\n",                     /* S_BIT7D */
    "\tbit 7, b\n",                     /* S_BIT7B */
    "\tadd hl, hl\n",                   /* S_ADDHLHL */
    "\tld (hl), a\n",                   /* S_LDHLA */
    "\tex de, hl\n\tcall irsh\n",       /* S_CALLIRSH */
    "\tdec hl\n",                       /* S_DECHL */
    "\tld c, (hl)\n\tinc hl\n\tld b, (hl)\n",  /* S_LDCHL */
    "",                                 /* UNUSED88 */
    "\tld d, b\n\tld e, c\n",           /* S_LDDEBC */
    "\tld a, (hl)\n\tld c, a\n\tinc hl\n",  /* S_LDAHLINC */
    "\tld a, (hl)\n\tld h, a\n\tld l, c\n", /* S_LDAHLHIGH */
    "\tld l, a\n\tld h, 0\n",               /* S_WIDEN */
    "\tld a, (hl)\n\tinc hl\n\tld h, (hl)\n\tld l, a\n", /* S_LDHLIND */
    "\tpush iy\n\tpop hl\n",                /* S_IYHL */
    "\tpush ix\n\tpop de\n",                /* S_IXDE */
    "\texx\n\tld hl, 0\n\texx\n",           /* S_EXX0 */
    "\tcall imul\n",                        /* S_CALLIMUL */
    "\tpush ix\n\tpop bc\n",                /* S_IXBC */
    "\tld l, a\n\trla\n\tsbc a, a\n\tld h, a\n",  /* S_SEXT */
    "\tadd ix, de\n",                       /* S_ADDIXDE */
    "\tadd ix, bc\n",                       /* S_ADDIXBC */
    "\tor c\n\tld c, a\n",                  /* S_ORCCA */
    "\tinc a\n",                            /* S_INCA */
    "\tdec a\n",                            /* S_DECA */
    "\tld a, h\n\tadc a, 0\n\tld h, a\n"    /* S_ADCH0 */
};

/* Format strings with single %d - for emit1() */
static const char *fmtstr[] = {
    "\tld a, %d\n",                         /* F_LDA */
    "\tcp %d\n",                            /* F_CP */
    "\tadd a, %d\n",                        /* F_ADDA */
    "\tsub %d\n",                           /* F_SUB */
    "\tand %d\n",                           /* F_AND */
    "\tor %d\n",                            /* F_OR */
    "\txor %d\n",                           /* F_XOR */
    "no%d:\n",                              /* F_IFEND */
    "yes%d:\n",                             /* F_IFTHEN */
    "el%d:\n",                              /* F_IF */
    "orE%d:\n",                             /* F_OREND */
    "anE%d:\n",                             /* F_ANDEND */
    "tF%d:\n",                              /* F_TERNF */
    "tE%d:\n",                              /* F_TERNE */
    "\tld a, %d\n\tcall leaiy\n"            /* F_LEAIY */
};

/* Format strings with single %s - for emitS() */
static const char *sfmtstr[] = {
    "\tld hl, (%s)\n",                      /* FS_LDHLM */
    "\tld (%s), hl\n",                      /* FS_STHLM */
    "\tld a, (%s)\n",                       /* FS_LDAM */
    "\tld (%s), a\n",                       /* FS_STAM */
    "\tcall %s\n",                          /* FS_CALL */
    "\tld hl, %s\n",                        /* FS_LDHL */
    "\tld de, %s\n",                        /* FS_LDDE */
    "%s:\n",                                /* FS_LABEL */
    "\tjp %s\n",                            /* FS_JP */
    "\tld de, (%s)\n"                       /* FS_LDDEM */
};

void emit(unsigned char idx) {
    fdputs(outFd, asmstr[idx]);
}

void emit1(unsigned char idx, char val) {
    /* For label format strings (F_IFEND through F_TERNE), include fnIndex */
    if (idx >= F_IFEND && idx <= F_TERNE) {
        static const char *labfmt[] = {
            "no%d_%d:\n",    /* F_IFEND - if-end (false path done) */
            "yes%d_%d:\n",   /* F_IFTHEN - if-then (true path) */
            "el%d_%d:\n",    /* F_IF - else branch */
            "orE%d_%d:\n",   /* F_OREND - or short-circuit end */
            "anE%d_%d:\n",   /* F_ANDEND - and short-circuit end */
            "tF%d_%d:\n",    /* F_TERNF - ternary false */
            "tE%d_%d:\n"     /* F_TERNE - ternary end */
        };
        /* Labels use unsigned val as label number */
        fdprintf(outFd, labfmt[idx - F_IFEND], fnIndex, (unsigned char)val);
    } else {
        /* Instructions use signed val for offsets */
        fdprintf(outFd, fmtstr[idx], val);
    }
}

void emitS(unsigned char idx, const char *s) {
    fdprintf(outFd, sfmtstr[idx], s);
}

/* Format strings with two %s args - for emit2S() */
static const char *s2fmtstr[] = {
    "\tld a, %s\n\tor %s\n",               /* FS2_LDAOR */
    "\t%s %s\n"                            /* FS2_OP */
};

void emit2S(unsigned char idx, const char *s1, const char *s2) {
    fdprintf(outFd, s2fmtstr[idx], s1, s2);
}

/* Output string to assembly output - saves passing outFd at each call site */
void out(const char *s) {
    fdputs(outFd, s);
}


/* Register lookup tables - indexed by register_id enum */
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
char varIYOfs(struct local_var *var) {
    return var->offset + (var->is_param ? 1 : 0);
}

/* findVar is defined in codegen.c */

/* IY-indexed memory access - helper for sign handling */
/* Unified index register ops: pass 'x' for IX, 'y' for IY
 * fmt should have: first %c for low byte dest (or none), then i%c for register,
 * %c for sign, %d for offset
 * For simple forms like "\tld a, (i%c %c %d)\n"
 * For dest-taking forms like "\tld %c, (i%c %c %d)\n"
 */
static void idxOp(const char *fmt, char arg1, int offset, int adj) {
    int o = offset + adj;
    if (o >= 0)
        fdprintf(outFd, fmt, arg1, '+', o);
    else
        fdprintf(outFd, fmt, arg1, '-', -o);
}

/* 4-arg version for destination register forms */
static void idxOp2(const char *fmt, char dest, char reg, int offset, int adj) {
    int o = offset + adj;
    if (o >= 0)
        fdprintf(outFd, fmt, dest, reg, '+', o);
    else
        fdprintf(outFd, fmt, dest, reg, '-', -o);
}

/* IY shorthand using legacy signature */
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
    fnIXHL32 = 0;
}

void loadByteIY(char offset, char is_param) {
    (void)is_param;  /* Byte params are at exact offset, no adjustment */
    iyOp("\tld a, (iy %c %d)\n", offset, 0);
}

void storeByteIY(char offset, char is_param) {
    (void)is_param;  /* Byte params are at exact offset, no adjustment */
    iyOp("\tld (iy %c %d), a\n", offset, 0);
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
    fnIXHLOfs = offset;  /* HL still holds the stored value */
    fnIXHL32 = 0;        /* Only 2-byte value, not 4 */
    fnIYHLValid = 0;     /* IY cache is no longer valid */
}

/*
 * Emit a jump instruction.
 * If label == 255, prefix is used as the complete symbol name.
 * Otherwise, constructs prefix + fnIndex + _ + label.
 */
void emitJump(const char *instr, const char *prefix, unsigned char label) {
    if (label == 255) {
        fdprintf(outFd, "\t%s %s\n", instr, prefix);
    } else {
        fdprintf(outFd, "\t%s %s%d_%d\n", instr, prefix, fnIndex, label);
    }
}

/* Callee-save helpers */
char getUsedRegs(struct local_var *locals) {
    struct local_var *var;
    int used = 0;
    for (var = locals; var; var = var->next)
        used |= saveMaskTab[var->reg];
    return used;
}

char calleeSavSz(char used) {
    int size = 0;
    if (used & 1) size += 2;  /* BC */
    if (used & 2) size += 2;  /* IX */
    if (used & 4) size += 2;  /* BC' */
    return size;
}

/* Function prolog emission */
void
emitFnProlog(char *name, char *rettype, char frame_size,
             struct local_var *locals, int has_params)
{
    struct local_var *var;

    fdprintf(outFd, "; %s\n", name);

#ifdef DEBUG
    /* Emit local variable info as comments */
    if (locals) {
        static const char *regnames[] = { "-", "B", "C", "BC", "IX" };
        fdprintf(outFd, "; frame=%d\n", frame_size);
        for (var = locals; var; var = var->next) {
            const char *rn = (var->reg < 5) ? regnames[var->reg] : "?";
            fdprintf(outFd, ";   %s %s: size=%d iy%+d refs=%d reg=%s\n",
                var->is_param ? "param" : "local",
                var->name, var->size, var->offset, var->ref_count, rn);
        }
    }
#endif

    emitS(FS_LABEL, name);

    if (frame_size > 0 || has_params) {
        if (frame_size == 0) {
            emit(S_XORA);
        } else {
            emit1(F_LDA, frame_size);
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
                        emit(S_HLPIX);
                        clearHL();
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

char isCmpFunc(const char *fname) {
    int i;
    if (!fname) return 0;
    for (i = 0; cmpPats[i]; i++) {
        if (strstr(fname, cmpPats[i])) return 1;
    }
    return 0;
}

/* freeExpr is defined in parseast.c */

struct expr *mkVarCache(const char *symbol, char size) {
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
    fnIXHL32 = 0;
    fnIYHLValid = 0;
    fnHLZero = 0;
}

void clearDE() {
}

void clearA() {
    fnIXAOfs = -1;
    fnABCValid = 0;
    fnAZero = 0;
    fnARegvar = 0;
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

char isBinopWAccum(unsigned char op) {
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

/* Helper: emit global long (4-byte) load/store */
static void globLong(const char *s, char isStore) {
    if (isStore) {
        emitS(FS_STHLM, s);
        emit(S_EXX);
        fdprintf(outFd, "\tld (%s+2), hl\n", s);
    } else {
        emitS(FS_LDHLM, s);
        emit(S_EXX);
        fdprintf(outFd, "\tld hl, (%s+2)\n", s);
    }
    emit(S_EXX);
}

/* Variable load/store with cache management */
void loadVar(const char *sym, char sz, char docache) {
    const char *vn = stripVarPfx(sym);
    struct local_var *v;
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "  loadVar: sym=%s (1)\n", sym);
    }
#endif
    v = findVar(vn);
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "  loadVar: findVar returned %p (2)\n", (void*)v);
    }
#endif

    if (v && v->reg != REG_NO) {
#ifdef DEBUG
        if (TRACE(T_VAR)) {
            fdprintf(2, "  loadVar: branch A\n");
        }
#endif
        if (sz == 1) {
            /* Check if A already has this regvar's value */
            if ((v->reg == REG_B || v->reg == REG_C) && fnARegvar == v->reg) {
                /* A already has this regvar - skip load */
            } else {
                emit(byteLoadTab[v->reg]);
                /* Track that A now has this regvar's value */
                if (v->reg == REG_B || v->reg == REG_C)
                    fnARegvar = v->reg;
            }
        } else {
            emit(wordLoadTab[v->reg]);
        }
    } else if (v) {
#ifdef DEBUG
        if (TRACE(T_VAR)) {
            fdprintf(2, "  loadVar: branch B\n");
        }
#endif
        if (sz == 1) loadByteIY(v->offset, v->offset >= 0);
        else if (sz == 2) loadWordIY(v->offset);
        else if (sz == 4) {
            emit1(F_LDA, v->offset);
            emit(S_CALLGL);
        }
    } else {
        const char *s = stripDollar(sym);
#ifdef DEBUG
        if (TRACE(T_VAR)) {
            fdprintf(2, "  loadVar: branch C (global)\n");
        }
#endif
        if (sz == 1) emitS(FS_LDAM, s);
        else if (sz == 2) emitS(FS_LDHLM, s);
        else if (sz == 4) globLong(s, 0);
    }
}

void storeVar(const char *sym, char sz, char docache) {
    const char *vn = stripVarPfx(sym);
    struct local_var *v;
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: sym=%s\n", sym);
    }
#endif
    v = findVar(vn);
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: findVar returned %p\n", (void*)v);
    }
#endif

    if (v && v->reg != REG_NO) {
        if (sz == 1) {
            emit(byteStoreTab[v->reg]);
        } else {
            emit(wordStoreTab[v->reg]);
        }
    } else if (v) {
        if (sz == 1) storeByteIY(v->offset, v->offset >= 0);
        else if (sz == 2) storeWordIY(v->offset);
        else if (sz == 4) {
            emit1(F_LDA, v->offset);
            emit(S_CALLPL);
        }
    } else {
        const char *s = stripDollar(sym);
#ifdef DEBUG
        if (TRACE(T_VAR)) {
            fdprintf(2, "  storeVar: global store sz=%d\n", sz);
        }
#endif
        if (sz == 1) {
            emitS(FS_STAM, s);
        } else if (sz == 2) {
            emitS(FS_STHLM, s);
        } else if (sz == 4) {
            globLong(s, 1);
        }
#ifdef DEBUG
        if (TRACE(T_VAR)) {
            fdprintf(2, "  storeVar: done\n");
        }
#endif
    }
#ifdef DEBUG
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: returning\n");
    }
#endif
}

/*
 * Emit simple word load based on scheduler's loc and dest fields.
 * Handles: LOC_REG (BC, IX), LOC_MEM, LOC_STACK, LOC_IX
 * Target register from e->dest: R_HL, R_DE, R_BC
 * Returns 1 if handled, 0 if not a simple case.
 */
char
emitSimplLd(struct expr *e)
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
            emit(S_EXDEHL);
        } else if (e->reg == R_DE && e->dest == R_HL) {
            emit(S_EXDEHL);
        } else {
            return 0;
        }
        break;

    case LOC_MEM:
        if (!e->left || !e->left->symbol) return 0;
        sym = stripDollar(e->left->symbol);
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
void
emitGlobDrf(struct expr *e)
{
    const char *sym;
    const char *s;

    if (!e || !e->left || !e->left->symbol) return;

    sym = e->left->symbol;
    s = stripDollar(sym);

    if (e->size == 1) {
        emitS(FS_LDAM, s);
        clearA();
    } else if (e->size == 2) {
        if (e->dest == R_DE) {
            /* Scheduler says load to DE */
            emitS(FS_LDDEM, s);
            fnDEValid = 1;
        } else {
            emitS(FS_LDHLM, s);
            clearHL();
        }
    } else if (e->size == 4) {
        emitS(FS_LDHLM, s);
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
void
emitRegVarDrf(struct expr *e)
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
        if (var->reg == REG_B || var->reg == REG_BC)
            emit(S_LDAB);
        else if (var->reg == REG_C)
            emit(S_LDAC);
    } else {
        /* Word register variable */
        if (e->dest == R_DE) {
            /* Scheduler says load to DE */
            if (var->reg == REG_BC)
                emit(S_LDDEBC);
            else if (var->reg == REG_IX)
                emit(S_IXDE);
            fnDEValid = 1;
        } else if (e->dest == R_BC) {
            /* Scheduler says load to BC */
            if (var->reg == REG_IX)
                emit(S_IXBC);
            /* BC->BC is a no-op */
        } else {
            if (var->reg == REG_BC)
                emit(S_BCHL);
            else if (var->reg == REG_IX)
                emit(S_IXHL);
            clearHL();
        }
    }

    freeExpr(e);
}

/*
 * Emit DEREF of stack variable (IY-indexed) with cache check
 * Pattern: (M $stackvar) where opflags has OP_IYMEM
 */
/*
 * Unified index register dereference - works for both IY and IX
 * reg: 'x' for IX, 'y' for IY
 * ofs: offset from index register
 * size: 1, 2, or 4 bytes
 * dest: target register (R_HL, R_DE, R_BC)
 * e: expression for cache tracking (may be NULL)
 */
void
emitIndexDrf(char reg, char ofs, char size, char dest, struct expr *e)
{
    char hi, lo;

    if (size == 1) {
        /* Load byte to A, B, or C based on dest */
        if (dest == R_B) {
            idxOp("\tld b, (i%c %c %d)\n", reg, ofs, 0);
        } else if (dest == R_C) {
            idxOp("\tld c, (i%c %c %d)\n", reg, ofs, 0);
        } else {
            idxOp("\tld a, (i%c %c %d)\n", reg, ofs, 0);
            fnAZero = 0;
            clearA();
        }
    } else if (size == 2) {
        switch (dest) {
        case R_BC: hi = 'b'; lo = 'c'; break;
        case R_DE: hi = 'd'; lo = 'e'; break;
        default:   hi = 'h'; lo = 'l'; break;
        }
        if (dest == R_DE) {
            idxOp("\tld e, (i%c %c %d)\n", reg, ofs, 0);
            idxOp("\tld d, (i%c %c %d)\n", reg, ofs, 1);
            fnDEValid = 1;
        } else {
            /* Load to HL (or BC) */
            if (reg == 'y' && (dest == R_HL || dest == 0)) {
                /* Use cached IY load */
                loadWordIY(ofs);
            } else {
                idxOp2("\tld %c, (i%c %c %d)\n", lo, reg, ofs, 0);
                idxOp2("\tld %c, (i%c %c %d)\n", hi, reg, ofs, 1);
            }
            if (dest == R_HL || dest == 0) {
                clearHL();
            }
        }
    } else if (size == 4) {
        /* Check if full 32-bit value already in HL/HL' (IX cache) */
        if (reg == 'x' && fnIXHLOfs == ofs && fnIXHL32) {
            /* Both low and high words cached - skip entire load */
        } else if (reg == 'x' && fnIXHLOfs == ofs) {
            /* Low word cached - just load high word */
            emit(S_EXX);
            idxOp("\tld l, (i%c %c %d)\n", reg, ofs, 2);
            idxOp("\tld h, (i%c %c %d)\n", reg, ofs, 3);
            emit(S_EXX);
            fnIXHL32 = 1;
        } else {
            idxOp("\tld l, (i%c %c %d)\n", reg, ofs, 0);
            idxOp("\tld h, (i%c %c %d)\n", reg, ofs, 1);
            emit(S_EXX);
            idxOp("\tld l, (i%c %c %d)\n", reg, ofs, 2);
            idxOp("\tld h, (i%c %c %d)\n", reg, ofs, 3);
            emit(S_EXX);
            if (reg == 'x') {
                fnIXHLOfs = ofs;
                fnIXHL32 = 1;
            }
        }
    }
}

/*
 * Emit DEREF of stack variable (IY-indexed) with cache check
 * Pattern: (M $stackvar) where opflags has OP_IYMEM
 */
void
emitStackDrf(struct expr *e)
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

    emitIndexDrf('y', var->offset, e->size, e->dest, e);
    freeExpr(e);
}

/*
 * Emit byte load from (BC)
 */
void
emitBCIndir(void)
{
    emit(S_LDABC);
}
