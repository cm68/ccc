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
    "\tld e, a  ; save byte value\n",  /* S_ESAVE */
    "\tld c, a\n",                      /* S_CA */
    "\tld b, h\n\tld c, l\n",           /* S_BCHLX */
    "\tld b, a\n",                      /* S_BA */
    "\tld a, l\n",                      /* S_AL */
    "\tld a, h\n\tor l\n",              /* S_AHORL */
    "\tld a, h\n",                      /* S_AH */
    "\tjr nz, $+3\n",                   /* S_JRNZ3 */
    "\tinc hl\n",                       /* S_INCHL */
    "\tex de, hl  ; save word value in DE\n", /* S_DESAVE */
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
    "\t; ERROR: failed to parse INCDEC_PLACEHOLDER\n", /* S_ERRPARS */
    "\tex de, hl  ; cached value in DE, swap to HL\n", /* S_CACHESWP */
    "\tex de, hl  ; DE = address\n",    /* S_DEADR */
    "\tex de, hl  ; push TOS to 2nd entry\n", /* S_PUSHTOS */
    "\texx\n\tld a, b\n\tor c\n\texx\n", /* S_EXXABCORC */
    "\texx\n\tpush bc\n\texx\n\tpop de\n\tadd hl, de\n", /* S_EXXBCPOPHL */
    "\t; fall back to normal indirect\n", /* S_FBKNORM */
    "\tjp framefree\n",                 /* S_JPFF */
    "\tld a, b\n\tor c\n",              /* S_ABCORC */
    "\tld e, (hl)\n\tinc hl\n\tld d, (hl)\n\tex de, hl\n", /* S_LDEDHLSWP */
    "\tld hl, 0  ; upper 16 bits = 0\n", /* S_HLZERO */
    "\tpop af  ; return old value\n",  /* S_POPAFRET */
    "\tpop de  ; DE = address\n",      /* S_POPDEADR */
    "\tpop de  ; restore from nested op\n", /* S_POPDERES */
    "\tpop hl  ; old value for postfix\n", /* S_POPHPOST */
    "\tpop hl  ; restore lower word\n", /* S_POPHLLOW */
    "\tpop hl  ; restore upper word\n", /* S_POPHLUPP */
    "\tpop hl  ; return old value\n",  /* S_POPHLRET */
    "\tpush af  ; save old value\n",   /* S_PUSHAFSV */
    "\tpush bc\n",                      /* S_PUSHBC */
    "\tpush de  ; save address\n",     /* S_PUSHDESV */
    "\tpush de  ; spill 2nd stack entry\n", /* S_PUSHDESP */
    "\tpush hl  ; save lower word\n",  /* S_PUSHHLLOW */
    "\tpush hl  ; save old value\n",   /* S_PUSHHLOV */
    "\tpush hl  ; save upper word\n",  /* S_PUSHHLUPP */
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

const char *
getRegName(enum register_id reg)
{
    switch (reg) {
        case REG_NO:  return NULL;
        case REG_B:   return "B";
        case REG_C:   return "C";
        case REG_Bp:  return "B'";
        case REG_Cp:  return "C'";
        case REG_BC:  return "BC";
        case REG_BCp: return "BC'";
        case REG_IX:  return "IX";
        default:      return "???";
    }
}

const char *
stripDollar(const char *symbol)
{
    if (symbol && symbol[0] == '$') {
        return symbol + 1;
    }
    return symbol;
}

const char *
stripVarPfx(const char *name)
{
    if (name && name[0] == '$') name++;
    if (name && name[0] == 'A') name++;
    return name;
}

void freeNode(struct expr *e) {
    if (e->asm_block) free(e->asm_block);
    if (e->cleanup_block) free(e->cleanup_block);
    free(e);
}

int
isMangledName(const char *name)
{
    int len, i;

    if (!name) return 0;
    len = strlen(name);
    if (len < 4) return 0;

    for (i = len - 4; i < len; i++) {
        char c = name[i];
        if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')))
            return 0;
    }
    return 1;
}

/* findVar is defined in codegen.c */

/* IY-indexed memory access */
void loadWordIY(char offset) {
    if (offset >= 0) {
        fdprintf(outFd, "\tld l, (iy + %d)\n", offset);
        fdprintf(outFd, "\tld h, (iy + %d)\n", offset + 1);
    } else {
        fdprintf(outFd, "\tld l, (iy - %d)\n", -offset);
        fdprintf(outFd, "\tld h, (iy - %d)\n", -offset - 1);
    }
}

void storeWordIY(char offset) {
    if (offset >= 0) {
        fdprintf(outFd, "\tld (iy + %d), l\n", offset);
        fdprintf(outFd, "\tld (iy + %d), h\n", offset + 1);
    } else {
        fdprintf(outFd, "\tld (iy - %d), l\n", -offset);
        fdprintf(outFd, "\tld (iy - %d), h\n", -offset - 1);
    }
}

void loadByteIY(char offset, char is_param) {
    if (offset >= 0) {
        fdprintf(outFd, "\tld a, (iy + %d)\n", is_param ? offset + 1 : offset);
    } else {
        fdprintf(outFd, "\tld a, (iy - %d)\n", -offset);
    }
}

void storeByteIY(char offset, char is_param) {
    if (offset >= 0) {
        fdprintf(outFd, "\tld (iy + %d), a\n", is_param ? offset + 1 : offset);
    } else {
        fdprintf(outFd, "\tld (iy - %d), a\n", -offset);
    }
}

/* IX-indexed memory access */
void loadWordIX(char offset) {
    fdprintf(outFd, "\tld l, (ix + %d)\n", offset);
    fdprintf(outFd, "\tld h, (ix + %d)\n", offset + 1);
}

void storeWordIX(char offset) {
    fdprintf(outFd, "\tld (ix + %d), l\n", offset);
    fdprintf(outFd, "\tld (ix + %d), h\n", offset + 1);
}

/* Label map for jump optimization */
#define MAX_LABELS 1000
static struct labelMap labelMap[MAX_LABELS];
int lblMapCnt = 0;

void addLabelMap(int from, int to, enum jump_type type) {
    if (lblMapCnt < MAX_LABELS) {
        labelMap[lblMapCnt].label = from;
        labelMap[lblMapCnt].target = to;
        labelMap[lblMapCnt].jump_type = type;
        lblMapCnt++;
    }
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
                labelMap[i].target != -1) {
                current = labelMap[i].target;
                break;
            }
        }
        if (i == lblMapCnt) return current;
    }
    return label;
}

int extLabelNum(const char *asm_text) {
    const char *p;
    int num;

    if (!asm_text) return -1;

    if ((p = strstr(asm_text, "_tern_false_")) != NULL) {
        if (sscanf(p + 12, "%d:", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "_tern_end_")) != NULL) {
        if (sscanf(p + 10, "%d:", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "_end_")) != NULL) {
        if (sscanf(p + 5, "%d:", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "_if_")) != NULL) {
        if (sscanf(p + 4, "%d:", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "_while_")) != NULL && !strstr(asm_text, "_end_")) {
        if (sscanf(p + 7, "%d:", &num) == 1) return num;
    }
    return -1;
}

int extJumpTarget(const char *asm_text, enum jump_type *jtype) {
    const char *p;
    int num;

    if (!asm_text) return -1;

    if ((p = strstr(asm_text, "jp ")) != NULL) {
        *jtype = JMP_UNCOND;
        if (sscanf(p + 3, "_tern_end_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_tern_false_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_if_end_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_if_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_while_%d", &num) == 1) return num;
        if (sscanf(p + 3, "_while_end_%d", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "jp z,")) != NULL) {
        *jtype = JUMP_IF_ZERO;
        if (sscanf(p + 6, "_tern_false_%d", &num) == 1) return num;
        if (sscanf(p + 6, "_tern_end_%d", &num) == 1) return num;
        if (sscanf(p + 6, "_if_end_%d", &num) == 1) return num;
        if (sscanf(p + 6, "_if_%d", &num) == 1) return num;
    }
    else if ((p = strstr(asm_text, "jp nz,")) != NULL) {
        *jtype = JMP_IF_NOT_Z;
        if (sscanf(p + 7, "_tern_false_%d", &num) == 1) return num;
        if (sscanf(p + 7, "_tern_end_%d", &num) == 1) return num;
        if (sscanf(p + 7, "_if_end_%d", &num) == 1) return num;
        if (sscanf(p + 7, "_if_%d", &num) == 1) return num;
    }
    return -1;
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

    if (s->type == 'A' && s->asm_block) {
        int label_num = extLabelNum(s->asm_block);
        if (label_num >= 0) {
            if (s->next && s->next->type == 'A' && s->next->asm_block) {
                enum jump_type jtype;
                int target = extJumpTarget(s->next->asm_block, &jtype);
                if (target >= 0) addLabelMap(label_num, target, jtype);
            }
        }
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
    int resolved = resolveLabel(label);
    fdprintf(outFd, "\t%s %s%d\n", instr, prefix, resolved);
}

/* Callee-save helpers */
int getUsedRegs(struct local_var *locals) {
    struct local_var *var;
    int used = 0;
    for (var = locals; var; var = var->next) {
        if (var->reg == REG_BC || var->reg == REG_B || var->reg == REG_C)
            used |= 1;
        if (var->reg == REG_IX)
            used |= 2;
        if (var->reg == REG_BCp || var->reg == REG_Bp || var->reg == REG_Cp)
            used |= 4;
    }
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
            char offset_str[32];
            if (var->offset >= 0)
                snprintf(offset_str, sizeof(offset_str), "(iy+%d)", var->offset);
            else
                snprintf(offset_str, sizeof(offset_str), "(iy%d)", var->offset);

            fdprintf(outFd, ";   %s: ", var->name);
            if (var->first_label == -1) {
                if (regname)
                    fdprintf(outFd, "unused (0 refs, 0 agg_refs, %s, reg=%s)\n",
                             offset_str, regname);
                else
                    fdprintf(outFd, "unused (0 refs, 0 agg_refs, %s)\n", offset_str);
            } else {
                if (regname)
                    fdprintf(outFd, "labels %d-%d (%d refs, %d agg_refs, %s, reg=%s)\n",
                             var->first_label, var->last_label, var->ref_count,
                             var->agg_refs, offset_str, regname);
                else
                    fdprintf(outFd, "labels %d-%d (%d refs, %d agg_refs, %s)\n",
                             var->first_label, var->last_label, var->ref_count,
                             var->agg_refs, offset_str);
            }
        }
    }

    if (isMangledName(name)) fdprintf(outFd, "%s:\n", name);
    else fdprintf(outFd, "_%s:\n", name);

    if (frame_size > 0 || has_params) {
        fdprintf(outFd, "\tld a, %d\n", frame_size);
        emit(S_CALLFA);
    }

    {
        int used = getUsedRegs(locals);
        if (used & 1) emit(S_PUSHBC);
        if (used & 2) emit(S_PUSHIX);
        if (used & 4) emit(S_EXXBC);  /* save BC' via exx; push bc; exx */
    }

    for (var = locals; var; var = var->next) {
        if (var->is_param && var->reg != REG_NO) {
            if (var->size == 2) {
                loadWordIY(var->offset);
                if (var->reg == REG_BC) emit(S_BCHLX);
                else if (var->reg == REG_IX) emit(S_HLPIX);
            } else if (var->size == 1) {
                if (var->reg == REG_B || var->reg == REG_C) {
                    fdprintf(outFd, "\tld a, (iy + %d)\n", var->offset + 1);
                    if (var->reg == REG_B) emit(S_BA);
                    else emit(S_CA);
                }
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

/* Expression cache helpers */
struct expr *shallowCopy(struct expr *e) {
    struct expr *copy;
    if (!e) return NULL;
    copy = malloc(sizeof(struct expr));
    if (!copy) return NULL;
    memcpy(copy, e, sizeof(struct expr));
    copy->left = NULL;
    copy->right = NULL;
    copy->asm_block = NULL;
    copy->cleanup_block = NULL;
    copy->jump = NULL;
    return copy;
}

/* freeExpr is defined in parseast.c */

static int cache_depth = 0;
int matchesCache(struct expr *e1, struct expr *e2) {
    int result;
    cache_depth++;
    if (TRACE(T_CACHE)) {
        fdprintf(2, "    matchesCache depth=%d e1=%p e2=%p\n", cache_depth, (void*)e1, (void*)e2);
    }
    if (cache_depth > 100) {
        fdprintf(2, "matchesCache: depth > 100, loop?\n");
        exit(1);
    }
    if (!e1 || !e2) { cache_depth--; return 0; }
    if (e1->op != e2->op) { cache_depth--; return 0; }
    if (e1->size != e2->size) { cache_depth--; return 0; }

    if (e1->op == '$') {
        if (!e1->symbol || !e2->symbol) { cache_depth--; return 0; }
        result = strcmp(e1->symbol, e2->symbol) == 0;
        cache_depth--;
        return result;
    }
    if (e1->op == 'M') {
        result = matchesCache(e1->left, e2->left);
        cache_depth--;
        return result;
    }
    cache_depth--;
    return 0;
}

struct expr *mkVarCache(const char *symbol, int size) {
    struct expr *sym_node, *deref_node;

    sym_node = malloc(sizeof(struct expr));
    if (!sym_node) return NULL;
    memset(sym_node, 0, sizeof(struct expr));
    sym_node->op = '$';
    sym_node->symbol = (char *)symbol;
    sym_node->size = size;

    deref_node = malloc(sizeof(struct expr));
    if (!deref_node) { free(sym_node); return NULL; }
    memset(deref_node, 0, sizeof(struct expr));
    deref_node->op = 'M';
    deref_node->size = size;
    deref_node->left = sym_node;
    return deref_node;
}

/* Cache management */
void clearHL() {
    if (0) return;
    if (fnHLCache) {
        freeExpr(fnHLCache);
        fnHLCache = NULL;
    }
}

void clearDE() {
    if (0) return;
    if (fnDECache) {
        freeExpr(fnDECache);
        fnDECache = NULL;
    }
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

    if (fnDECache) freeExpr(fnDECache);
    fnDECache = fnHLCache;
    fnHLCache = NULL;
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
}

int isBinopWAccum(unsigned char op) {
    switch (op) {
    case '+': case '-': case '*': case '/': case '%':
    case '&': case '|': case '^': case 'w':
    case '>': case '<': case 'g': case 'L':
    case 'Q': case 'n':
        return 1;
    default:
        return 0;
    }
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
        if (sz == 1) {
            if (v->reg == REG_B) emit(S_LDAB);
            else if (v->reg == REG_C) emit(S_LDAC);
            else if (v->reg == REG_Bp) emit(S_EXXLDAB);
            else if (v->reg == REG_Cp) emit(S_EXXLDAC);
        } else {
            if (v->reg == REG_BC) emit(S_BCHL);
            else if (v->reg == REG_BCp) emit(S_EXXBCHL);
            else if (v->reg == REG_IX) emit(S_IXHL);
        }
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
        else if (sz == 4) {
            fdprintf(outFd, "\tld hl, (%s)\n", s);
            emit(S_EXX);
            fdprintf(outFd, "\tld hl, (%s+2)\n", s);
            emit(S_EXX);
        }
    }
    if (docache  && sz >= 2) {
        clearHL();
        fnHLCache = mkVarCache(sym, sz);
    }
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
            if (v->reg == REG_B) emit(S_BA);
            else if (v->reg == REG_C) emit(S_CA);
            else if (v->reg == REG_Bp) emit(S_EXXLDBA);
            else if (v->reg == REG_Cp) emit(S_EXXLDCA);
        } else {
            if (v->reg == REG_BC) emit(S_BCHLX);
            else if (v->reg == REG_IX) emit(S_HLPIX);
            if (docache ) {
                clearHL();
                fnHLCache = mkVarCache(sym, sz);
            }
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
        if (sz == 1) fdprintf(outFd, "\tld (%s), a\n", s);
        else if (sz == 2) {
            fdprintf(outFd, "\tld (%s), hl\n", s);
            if (docache ) {
                clearHL();
                fnHLCache = mkVarCache(sym, sz);
            }
        }
        else if (sz == 4) {
            fdprintf(outFd, "\tld (%s), hl\n", s);
            emit(S_EXX);
            fdprintf(outFd, "\tld (%s+2), hl\n", s);
            emit(S_EXX);
        }
        if (TRACE(T_VAR)) {
            fdprintf(2, "  storeVar: done\n");
        }
    }
    if (TRACE(T_VAR)) {
        fdprintf(2, "  storeVar: returning\n");
    }
}
