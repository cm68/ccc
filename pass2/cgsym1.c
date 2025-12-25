#include "cgen.h"

/*
 * File - sym1.c
 */
/*
 * parseVariable - Parse "[v" variable declaration
 */
void parseVariable() {
    node_t *exprNode;
    int storageClass;
    register member_t *symbol;

    symbol         = declareSymbol(getToken(), VAR);
    symbol->type = parseTypeSpec(getToken(), &symbol->refl);
    exprNode        = optimizeExpr(parseExpr());
    if (exprNode->op != CONST)
        fatalErr("Bad element count expr");
    symbol->nelem = (uint16_t)exprNode->info.l;
    freeExprTree(exprNode);
    storageClass = *getToken();
    expect(']');

    switch (storageClass) {
    case 'A': /* auto		*/
    case 'a':
        symbol->tflag = 1;
        break;
    case 'R': /* register	*/
    case 'r':
        symbol->sflags |= B_SLOC_REGVAR;
    case 'P': /* pointer	*/
    case 'p':
        symbol->tflag = 5;
        break;
    case 'S': /* static 	*/
    case 's':
        symbol->tflag = 4;
        break;
    case 'E': /* extern	*/
    case 'e':
        symbol->tflag = 3;
        break;
    case 'T': /* typedef	*/
    case 't':
        symbol->sclass = TYPE;
        if (symbol->refl != 0) {
            symbol->offset  = 0;
            symbol->tflag = 2;
        } else {
            symbol->offset  = symbol->type->offset;
            symbol->tflag = symbol->type->tflag;
        }
        if (symbol->nelem != 1)
            symbol->tflag = 0;
        break;
    default:
        fatalErr("Bad storage class");
    }
    symbol->size = varSize(symbol);
    if (symbol->sclass != TYPE) {
        if (symbol->refl == 0 && symbol->size == 0 && symbol->type->size == 0)
            prError("Bad dimensions");

        if (symbol->tflag == 5 || symbol->tflag == 1)
            allocVar(symbol, storageClass);
        else
            symbol->offset = newLocal(); /* swTableCnt++ */

        if ((symbol->refl & B_REFL_FUNC) && symbol->nelem != 0) {
            if (++lvlidx == MAXFUN)
                fatalErr("Functions nested too deep");

            funcScopeDepth[lvlidx] = nstdpth; /* Save current nesting depth */
            funcParmOff[lvlidx] = 6;
            funcLocalSize[lvlidx] = 0;
            funcSymbol[lvlidx] = symbol;
        }
    }
}

typedef struct {
    uint16_t cnt;
    member_t *vals[257];
} memberb_t;

typedef struct {
    uint16_t cnt;
    int16_t vals[257];
} memberi_t;

/*
 * parseMembers - Parse struct/union member declarations
 */
void parseMembers(int classCode) {

    size_t size;
    member_t *structSymbol;
    memberb_t *pMember;
    node_t *exprNode;
    char *token;
    memberb_t members;
    register member_t *memberSym;

    structSymbol          = declareSymbol(getToken(), classCode);
    pMember      = &members;
    pMember->cnt = 0;
    for (;;) {
        token = getToken();
        if (*token == ']')
            break;
        memberSym          = (member_t *)allocMem(sizeof(member_t)); /* Create member_t */
        memberSym->sclass = MEMBER;
        if (*token == ':') {
            memberSym->bWidth = atoi(token + 1);
            memberSym->sflags |= B_SLOC_BITFIELD;
            token = getToken();
        }
        memberSym->type = parseTypeSpec(token, &memberSym->refl);
        exprNode        = optimizeExpr(parseExpr());
        if (exprNode->op != CONST)
            fatalErr("Strucdecl - bad\tnelem");
        memberSym->nelem = (uint16_t)exprNode->info.l;
        freeExprTree(exprNode);
        memberSym->size                    = varSize(memberSym);
        pMember->vals[pMember->cnt++] = memberSym;
    }
    /* note in the size calculation below an extra member_t pointer is reserved
     * in the decompilation I have assumed struct _memb (defined in struct bbb)
     * used a dummy array size of [1] for the vals, this gives the extra pointer
     */
    size    = (pMember->cnt - 1) * sizeof(member_t *) + sizeof(struct _memb);
    pMember = allocMem(size);
    bmove(&members, pMember, size);
    structSymbol->u.mlist = (struct _memb *)pMember;
    layoutStruct(structSymbol);
}

/*
 * parseEnum - Parse enum type declaration
 */
void parseEnum() {
    node_t *exprNode;
    memberi_t *pMember;
    int lobnd;
    int hibnd;
    size_t size;
    memberi_t members;
    register member_t *symbol;

    lobnd        = MININT; /* arithmetic overflow in constant expression */
    hibnd        = MAXINT;
    symbol           = declareSymbol(getToken(), ENUM);
    pMember      = &members;
    pMember->cnt = 0;
    for (;;) {
        exprNode = parseExpr();
        if (exprNode->op == DOT_DOT) {
            freeExprTree(exprNode);
            size    = (pMember->cnt - 1) * sizeof(uint16_t) + sizeof(struct _memi);
            pMember = allocMem(size);
            bmove(&members, pMember, size);
            symbol->u.ilist = (struct _memi *)pMember;
            setEnumSize(symbol, hibnd, lobnd);
            expect(']');
            return;
        }
        exprNode                           = optimizeExpr(exprNode);

        pMember->vals[pMember->cnt++] = (uint16_t)exprNode->info.l;

        if (lobnd < exprNode->info.l)
            lobnd = exprNode->info.l;
        if (exprNode->info.l < hibnd)
            hibnd = exprNode->info.l;
        freeExprTree(exprNode);
    }
}

/*
 * alignOffset OK++ PMO			Used in: layoutStruct
 *
 * Rounds offset up to alignment boundary.
 * alignOffset(off, align-1) aligns to 'align' bytes.
 * e.g., alignOffset(5, 1) = 6 (align to 2-byte boundary)
 */
int alignOffset(int p1, int p2) {

    return (p1 + p2) & ~p2;
}

/*
 * varSize OK++ PMO   Used in: parseInit, parseVariable, parseMembers
 *
 * Calculate total size of a variable in bytes.
 * For pointers (b_refl & B_REFL_PTR): nelem * 2 (pointer size)
 * For other types: type->size * nelem (base type size * element count)
 */
int varSize(register member_t *symbol) {

    if (symbol->refl & B_REFL_PTR)
        return symbol->nelem * 2;
    return symbol->type->size * symbol->nelem;
}

/*
 * memberAlign OK++ PMO	       Used in: emitInitData, layoutStruct
 *
 * Returns alignment requirement for a struct member.
 * Pointers (b_refl != 0) have no alignment requirement.
 * Otherwise returns the type's alignment from b_off.
 */
int memberAlign(register member_t *symbol) {

    if (symbol->refl != 0)
        return 0;
    return symbol->type->offset;
}

/*
 * layoutStruct OK++ PMO		Used in: parseMembers
 *
 * Computes struct/union layout: member offsets and total size.
 * - Unions: all members at offset 0, size = max member size
 * - Structs: sequential layout with alignment, bitfield packing
 */
void layoutStruct(register member_t *symbol) {
    int offset;
    int memberIdx;
    int bitOffset;
    member_t *memberPtr;
    offset = 0;

    for (memberIdx = bitOffset = 0; memberIdx != symbol->u.mlist->cnt; memberIdx++) {

        memberPtr = symbol->u.mlist->vals[memberIdx];
        if (symbol->sclass == UNION) {
            offset         = max(offset, memberPtr->size);
            memberPtr->offset = 0;
        } else {
            offset = memberPtr->offset = alignOffset(offset, memberAlign(memberPtr));
            if (memberPtr->sflags & B_SLOC_BITFIELD) {
                if (16 < bitOffset + memberPtr->bWidth || memberPtr->bWidth == 0) {
                    if (bitOffset != 0)
                        memberPtr->offset = (offset += 2);
                    bitOffset = 0;
                }
                memberPtr->bOffset = bitOffset;
                bitOffset += memberPtr->bWidth;
                if (bitOffset == 32) {
                    bitOffset = 0;
                    offset += 2;
                }
            } else {
                if (bitOffset != 0) {
                    bitOffset         = 0;
                    memberPtr->offset = (offset += 2);
                }
                offset += memberPtr->size;
            }
        }
    }
    if (bitOffset != 0) {
        bitOffset = 0;
        offset += 2;
    }
    symbol->size  = alignOffset(offset, 0); /* m10: */
    symbol->offset   = 0;
    symbol->sclass = STRUCT;
}

/*
 * saveRegVars OK++ PMO			Used in: genExprCode
 *
 * Emits code to save register variables in function prologue.
 * Scans symbol table for variables at current scope that are:
 *   - VAR class (b_class == VAR)
 *   - Register variables (b_flag == 2)
 *   - Marked for saving (b_sloc & B_SLOC_SAVE)
 * Emits push instructions via prIXnPush() for each.
 */
void saveRegVars() {
    member_t **hashPtr;
    register member_t *symbol;

    hashPtr = hashtab;
    do {
        for (symbol = *hashPtr; symbol != 0 && symbol->depth == nstdpth; symbol = symbol->next)
            if (symbol->sclass == VAR && symbol->tflag == 2 && (symbol->sflags & B_SLOC_SAVE))
                prIXnPush(symbol);
    } while (++hashPtr != &hashtab[HASHTABSIZE]);
}

/* end of file sym1.c */

/* vim: tabstop=4 shiftwidth=4 noexpandtab: */
